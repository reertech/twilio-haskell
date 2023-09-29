{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Capability
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Types.Capability where

import Control.Monad
import Data.Aeson
import Data.Set (Set)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as T

type Capabilities = Set Capability

data Capability
  = Voice
  | SMS
  | MMS
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance {-# OVERLAPPING #-} FromJSON Capabilities where
  parseJSON (Object obj)
    = let map' = fmap (\value -> case value of
                        Bool bool     -> bool
                        _             -> False) obj
      in  return $ foldr (\capability set ->
            if KM.lookup (K.fromString $ show capability) map' == Just True
              then Set.insert capability set
              else set
          ) Set.empty [Voice, SMS, MMS]
  parseJSON _ = mzero
