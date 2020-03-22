module CoolQ.HTTP.Api.Internal.Util where

import Data.Scientific
  ( Scientific
  , toRealFloat )
import Data.Time
  ( NominalDiffTime
  , UTCTime
  , parseTimeM
  , defaultTimeLocale )
import Data.Maybe
  ( fromMaybe )

toIntegral :: Integral a => Scientific -> a
toIntegral = floor . toRealFloat

toMilliseconds :: Integral a => NominalDiffTime -> a
toMilliseconds = floor . (* 1000)

toUTC :: (Integral a, Show a) => a -> UTCTime
toUTC x = fromMaybe panic $ parseTimeM False defaultTimeLocale "%s" $ show x
  where
  panic = error "timestamp is not convertible to decimal integer representation. this should not happen"
