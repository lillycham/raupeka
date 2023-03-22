module Language.Raupeka.Pretty (RaupekaPretty (..)) where

import Data.Text (Text, pack)

class RaupekaPretty a where
  rpretty :: a -> String
  rpretty' :: a -> Text
  rpretty' = pack . rpretty
