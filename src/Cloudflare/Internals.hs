{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cloudflare.Internals where

import GHC.Generics

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans

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

type CloudflareAPI m a = ReaderT Account (ExceptT APIError m) a

runCloudflareAPI :: CloudflareAPI m a -> Account -> m (Either APIError a)
runCloudflareAPI acc = runExceptT . runReaderT acc

getCloudflare :: (MonadIO m, FromJSON a) => Path -> CloudflareAPI m a
getCloudflare path = ReaderT $ \acc -> do
    r <- liftIO $ asJSON =<< getWith (opts acc) (baseurl <> path)
    return (result $ r ^. responseBody)
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]

postCloudflare :: (MonadIO m, ToJSON a, FromJSON b) => Path -> a -> CloudflareAPI m b
postCloudflare path body = ReaderT $ \acc -> do
    r <- liftIO $ asJSON =<< postWith (opts acc) (baseurl <> path) (toJSON body)
    return (result $ r ^. responseBody)
    where
        opts (Account email apikey) = defaults
            & header "X-Auth-Email" .~ [email]
            & header "X-Auth-Key"   .~ [apikey]
            & header "Content-Type" .~ ["application/json"]

data APIError
    = NotFound T.Text
    | NetworkError T.Text -- ^ Returned by the client
    | HTTPError T.Text -- ^ Returned by the API because of bad HTTP
    | CloudflareError T.Text -- ^ Returned by the API
    deriving (Show)
