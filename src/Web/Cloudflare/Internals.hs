{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Cloudflare.Internals where

import GHC.Generics

import Control.Monad.Reader

import Data.Monoid ((<>))

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Lens
import Data.Aeson
import Network.Wreq

type Email = BS.ByteString
type APIKey = BS.ByteString

type Path = String

baseurl :: String
baseurl = "https://api.cloudflare.com/client/v4"

data Account = Account
    { accountEmail  :: Email
    , accountAPIKey :: APIKey
    }
    deriving (Show)

data APIResponse a = APIResponse
    { success  :: Bool
    , errors   :: [T.Text]
    , messages :: [T.Text]
    , result   :: a
    }
    deriving (Show, Generic)

instance FromJSON a => FromJSON (APIResponse a)

type Cloudflare a = ReaderT Account IO a

runCloudflare :: Cloudflare a -> Account -> IO a
runCloudflare = runReaderT

getCloudflare :: FromJSON a => Path -> Cloudflare a
getCloudflare path = ReaderT $ \acc -> do
    r <- asJSON =<< getWith (opts acc) (baseurl <> path)
    return (result $ r ^. responseBody)
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]

postCloudflare :: (ToJSON a, FromJSON b) => Path -> a -> Cloudflare b
postCloudflare path body = ReaderT $ \acc -> do
    r <- asJSON =<< postWith (opts acc) (baseurl <> path) (toJSON body)
    return (result $ r ^. responseBody)
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]
