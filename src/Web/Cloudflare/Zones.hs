{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Cloudflare.Zones where

import Control.Monad.Reader

import Data.Monoid ((<>))
import Data.Aeson
import Data.Aeson.Types

import GHC.Generics
import qualified Data.Text as T

import Web.Cloudflare.Internals

import Data.HashMap.Strict (HashMap(..))
import qualified Data.HashMap.Strict as HM

type ZoneID = T.Text

data ZonePatch = ZonePatch
    { zonePatchName :: T.Text
    , zonePatchJumpStart :: Maybe Bool
    , zonePatchOrganization :: Maybe Organization
    }
    deriving (Show, Generic)

data Zone = Zone
    { zoneID     :: ZoneID
    , zoneName   :: T.Text
    , zoneStatus :: T.Text
    , zoneType   :: T.Text
    }
    deriving (Show, Generic)

data Organization = Organization
    { orgID     :: T.Text
    , orgName   :: T.Text
    , orgStatus :: T.Text
    }
    deriving (Show, Generic)

instance ToJSON Organization where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3 }

instance FromJSON Organization where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3 }

instance ToJSON ZonePatch where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 9 }

instance FromJSON Zone where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 4 }

getZones :: Cloudflare [Zone]
getZones = getCloudflare "zones"

createZone :: ZonePatch -> Cloudflare [Zone]
createZone = postCloudflare "zones"

zoneDetails :: ZoneID -> Cloudflare Zone
zoneDetails zid = getCloudflare ("zones" </> zid)

deleteZone :: ZoneID -> Cloudflare ()
deleteZone zid = deleteCloudflare ("zones" </> zid)

data PurgeFile = PurgeFile
    { purgeUrl     :: T.Text
    , purgeHeaders :: HashMap T.Text T.Text
    , purgeTags    :: [T.Text]
    , purgeHosts   :: [T.Text]
    }
    deriving (Generic)

data Purge = Purge
    { purgePurgeEverything :: Maybe Bool
    , purgeFiles :: [PurgeFile]
    }
    deriving (Generic)

instance ToJSON PurgeFile where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Purge where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 5 }

purgeAllFiles :: ZoneID -> Purge -> Cloudflare ZoneID
purgeAllFiles zid = postCloudflare ("zones" </> zid </> "purge_cache")

getZoneIDByName :: T.Text -> Cloudflare (Maybe ZoneID)
getZoneIDByName name = do
    m <- filter ((== name) . zoneName) <$> getZones
    case m of
        [] -> return Nothing
        (z:_) -> return (Just $ zoneID z)
