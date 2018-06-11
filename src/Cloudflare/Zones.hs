{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Cloudflare.Zones where

import Control.Monad.Trans
import Control.Monad.Except

import Data.Monoid ((<>))
import Data.Aeson
import Data.Aeson.Types

import GHC.Generics
import qualified Data.Text as T

import Cloudflare.Internals

type ZoneID = T.Text

data Zone = Zone
    { zoneID     :: ZoneID
    , zoneName   :: T.Text
    , zoneStatus :: T.Text
    , zoneType   :: T.Text
    }
    deriving (Show, Generic)

instance FromJSON Zone where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 4
        }

getZones :: MonadIO m => CloudflareAPI m [Zone]
getZones = getCloudflare "/zones"

getZoneIDByName :: MonadIO m => T.Text -> CloudflareAPI m ZoneID
getZoneIDByName name = do
    m <- filter ((== name) . zoneName) <$> getZones
    case m of
        [] -> throwError $ NotFound ("No zones with ID " <> name)
        (z:_) -> return (zoneID z)

