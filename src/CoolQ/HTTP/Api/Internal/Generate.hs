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
toRegularType (AppT (AppT ArrowT tx) y) =
  AppT (AppT ArrowT tx) (toRegularType y)
toRegularType x =
  ForallT [PlainTV o, PlainTV m] [AppT (ConT ''MonadIO) (VarT m)]
  (AppT (AppT (AppT (ConT ''ApiT) (VarT o)) (VarT m)) x)
  where
  o = mkName "o"
  m = mkName "m"

generateApiCall :: Name -> [String] -> Exp
generateApiCall apiName params =
  AppE (AppE (VarE 'callApi) (LitE (StringL realApiName)))
    (ListE (fmap (\x -> AppE (AppE (VarE '(.=)) (LitE (StringL (toSnakeCase x)))) (VarE (mkName x))) params))
  where
  realApiName = (toSnakeCase (nameBase apiName))

defineIndividual :: Dec -> [Dec]
defineIndividual (FunD apiName [Clause params (NormalB (SigE fun typ)) _]) =
  [ (SigD apiName apiType)
  , (FunD apiName [(Clause params (NormalB apiCallExpr) [])]) ]
  where
  apiType = toRegularType typ
  apiCallExpr = (AppE fun (generateApiCall apiName stringifiedParams))
  stringifiedParams = (\x -> let (VarP nm) = x in nameBase nm) <$> params
defineIndividual (ValD (VarP apiName) body _) =
  defineIndividual (FunD apiName [Clause [] body []])

defineApi :: Q [Dec] -> Q [Dec]
defineApi ds = (defineIndividual =<<) <$> ds
