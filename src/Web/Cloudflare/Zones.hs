{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Cloudflare.Zones where

import Control.Monad.Except

import Data.Monoid ((<>))
import Data.Aeson
import Data.Aeson.Types

import GHC.Generics
import qualified Data.Text as T

import Web.Cloudflare.Internals

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

getZones :: Cloudflare [Zone]
getZones = getCloudflare "/zones"

getZoneIDByName :: T.Text -> Cloudflare (Maybe ZoneID)
getZoneIDByName name = do
    m <- filter ((== name) . zoneName) <$> getZones
    case m of
        [] -> return Nothing
        (z:_) -> return (Just $ zoneID z)
