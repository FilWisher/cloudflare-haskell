{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Cloudflare.DNS where

import Control.Monad.Trans
import Control.Monad.Except

import Data.Monoid ((<>))
import Data.Aeson
import Data.Aeson.Types

import GHC.Generics
import qualified Data.Text as T

import Cloudflare.Internals
import Cloudflare.Zones

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


listDNSRecords :: MonadIO m => ZoneID -> CloudflareAPI m [DNSRecord]
listDNSRecords zid = getCloudflare ("/zones/" <> T.unpack zid <> "/dns_records")

createDNSRecord :: MonadIO m => ZoneID -> DNSRecord -> CloudflareAPI m DNSRecord
createDNSRecord zid record =
    postCloudflare ("/zones/" <> T.unpack zid <> "/dns_records") record
