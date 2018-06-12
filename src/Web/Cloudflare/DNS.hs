{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Cloudflare.DNS where

import Control.Monad.Except

import Data.Monoid ((<>))
import Data.Aeson
import Data.Aeson.Types

import GHC.Generics
import qualified Data.Text as T

import Web.Cloudflare.Internals
import Web.Cloudflare.Zones

type DNSRecordID = T.Text

data DNSRecordPatch = DNSRecordPatch
    { dnsPatchType    :: T.Text
    , dnsPatchName    :: T.Text
    , dnsPatchContent :: T.Text
    , dnsPatchTTL     :: Int
    }
    deriving (Show, Generic)

data DNSRecord = DNSRecord
    { dnsId      :: DNSRecordID
    , dnsType    :: T.Text
    , dnsName    :: T.Text
    , dnsContent :: T.Text
    , dnsTTL     :: Int
    }
    deriving (Show, Generic)

instance FromJSON DNSRecordPatch where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON DNSRecordPatch where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 8 }

instance FromJSON DNSRecord where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3 }

instance ToJSON DNSRecord where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3 }


listDNSRecords :: ZoneID -> Cloudflare [DNSRecord]
listDNSRecords zid = getCloudflare ("zones" </> zid </> "dns_records")

createDNSRecord :: ZoneID -> DNSRecordPatch -> Cloudflare DNSRecord
createDNSRecord zid = postCloudflare ("zones" </> zid </> "dns_records")

dnsRecordDetails :: ZoneID -> DNSRecordID -> Cloudflare DNSRecord
dnsRecordDetails zid did = getCloudflare ("zones" </> zid </> "dns_records" </> did)

updateDNSRecord :: ZoneID -> DNSRecordID -> DNSRecordPatch -> Cloudflare DNSRecord
updateDNSRecord zid did =
    putCloudflare ("zones" </> zid <> "dns_records" </> did)

deleteDNSRecord :: ZoneID -> DNSRecordID -> Cloudflare ()
deleteDNSRecord zid did =
    deleteCloudflare ("zones" </> zid </> "dns_records" </> did)
