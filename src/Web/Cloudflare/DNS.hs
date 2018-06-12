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

data DNSRecord = DNSRecord
    { dnsId      :: Maybe T.Text
    , dnsType    :: T.Text
    , dnsName    :: T.Text
    , dnsContent :: T.Text
    , dnsTTL     :: Int
    }
    deriving (Show, Generic)

instance FromJSON DNSRecord where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3
        }

instance ToJSON DNSRecord where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3
        }


listDNSRecords :: ZoneID -> Cloudflare [DNSRecord]
listDNSRecords zid = getCloudflare ("/zones/" <> T.unpack zid <> "/dns_records")

createDNSRecord :: ZoneID -> DNSRecord -> Cloudflare DNSRecord
createDNSRecord zid = postCloudflare ("/zones/" <> T.unpack zid <> "/dns_records")
