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
import Network.Wreq hiding (Auth)

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

data Auth = TokenAuth BS.ByteString | AccountAuth Account

instance FromJSON a => FromJSON (APIResponse a)

type Cloudflare a = ReaderT Auth IO a

runCloudflare :: Auth -> Cloudflare a -> IO a
runCloudflare = flip runReaderT

getCloudflare :: FromJSON a => Path -> Cloudflare a
getCloudflare path = ReaderT $ \auth -> do
    r <- asJSON =<< getWith (doAuth auth) (T.unpack $ baseurl </> path)
    return (result $ r ^. responseBody)

doAuth :: Auth -> Network.Wreq.Options
doAuth (AccountAuth (Account email apikey)) = 
    defaults & header "X-Auth-Email" .~ [email]
             & header "X-Auth-Key"   .~ [apikey]
             & header "Content-Type" .~ ["application/json"]
doAuth (TokenAuth token) =
    defaults & header "Authorization" .~ ["Bearer " <> token]

postCloudflare :: (ToJSON a, FromJSON b) => Path -> a -> Cloudflare b
postCloudflare path body = ReaderT $ \auth -> do
    r <- asJSON =<< postWith (doAuth auth) (T.unpack $ baseurl </> path) (toJSON body)
    return (result $ r ^. responseBody)

putCloudflare :: (ToJSON a, FromJSON b) => Path -> a -> Cloudflare b
putCloudflare path body = ReaderT $ \auth -> do
    r <- asJSON =<< putWith (doAuth auth) (T.unpack $ baseurl </> path) (toJSON body)
    return (result $ r ^. responseBody)

deleteCloudflare :: Path -> Cloudflare ()
deleteCloudflare path = ReaderT $ \auth -> do
    deleteWith (doAuth auth) (T.unpack $ baseurl </> path)
    return ()

(</>) :: T.Text -> T.Text -> T.Text
(</>) bef aft = bef <> "/" <> aft
