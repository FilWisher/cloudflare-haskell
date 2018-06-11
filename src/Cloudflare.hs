{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cloudflare
    ( module Cloudflare.Zones
    , module Cloudflare.DNS
    ) where

import Cloudflare.Zones
import Cloudflare.DNS
