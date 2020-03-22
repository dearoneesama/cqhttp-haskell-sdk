{-# LANGUAGE  TemplateHaskell
            , LiberalTypeSynonyms #-}

module CoolQ.HTTP.Api.Internal.Generate where

import Language.Haskell.TH
import Data.Char
  ( isUpper
  , toLower )
import CoolQ.HTTP.Api.Monad
  ( ApiT )
import CoolQ.HTTP.Api.Caller
  ( ApiCaller )
import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Reader
  ( ReaderT (ReaderT) )
import Data.Aeson
  ((.=))

callApi :: ApiCaller (ApiT o m)
callApi method payload = ReaderT $ \(api, _) ->
  api method payload

toSnakeCase :: String -> String
toSnakeCase (x : xs)
  | isUpper x = '_' : toLower x : toSnakeCase xs
  | otherwise = x : toSnakeCase xs
toSnakeCase "" = ""

toRegularType :: Type -> Type
toRegularType (AppT (AppT ArrowT (AppT tx (LitT (StrTyLit _)))) y) =
  AppT (AppT ArrowT tx) (toRegularType y)
toRegularType x =
  ForallT [PlainTV o, PlainTV m] [AppT (ConT ''MonadIO) (VarT m)]
  (AppT (AppT (AppT (ConT ''ApiT) (VarT o)) (VarT m)) x)
  where
  o = mkName "o"
  m = mkName "m"

getParams :: Type -> [String]
getParams (AppT (AppT ArrowT (AppT _ (LitT (StrTyLit nx)))) y) =
  nx : getParams y
getParams x = []

generateApiCall :: Name -> [String] -> Exp
generateApiCall apiName params =
  AppE (AppE (VarE 'callApi) (LitE (StringL (toSnakeCase (nameBase apiName)))))
    (ListE (fmap (\x -> AppE (AppE (VarE '(.=)) (LitE (StringL (toSnakeCase x)))) (VarE (mkName x))) params))

defineIndividual :: Dec -> [Dec]
defineIndividual (ValD (VarP apiName) (NormalB (SigE fun typ)) _) =
  [ (SigD apiName (toRegularType typ))
  , (FunD apiName [(Clause ((VarP . mkName) <$> params) (NormalB (AppE fun (generateApiCall apiName params))) [])]) ]
  where
  params = getParams typ

defineApi :: Q [Dec] -> Q [Dec]
defineApi ds = (defineIndividual =<<) <$> ds
