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

type Path = T.Text

baseurl :: T.Text
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

runCloudflare :: Account -> Cloudflare a -> IO a
runCloudflare = flip runReaderT

getCloudflare :: FromJSON a => Path -> Cloudflare a
getCloudflare path = ReaderT $ \acc -> do
    r <- asJSON =<< getWith (opts acc) (T.unpack $ baseurl </> path)
    return (result $ r ^. responseBody)
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]

postCloudflare :: (ToJSON a, FromJSON b) => Path -> a -> Cloudflare b
postCloudflare path body = ReaderT $ \acc -> do
    r <- asJSON =<< postWith (opts acc) (T.unpack $ baseurl </> path) (toJSON body)
    return (result $ r ^. responseBody)
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]

putCloudflare :: (ToJSON a, FromJSON b) => Path -> a -> Cloudflare b
putCloudflare path body = ReaderT $ \acc -> do
    r <- asJSON =<< putWith (opts acc) (T.unpack $ baseurl </> path) (toJSON body)
    return (result $ r ^. responseBody)
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]

deleteCloudflare :: Path -> Cloudflare ()
deleteCloudflare path = ReaderT $ \acc -> do
    deleteWith (opts acc) (T.unpack $ baseurl </> path)
    return ()
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]

(</>) :: T.Text -> T.Text -> T.Text
(</>) bef aft = bef <> "/" <> aft
