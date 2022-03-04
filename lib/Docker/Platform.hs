{-# LANGUAGE OverloadedStrings #-}

module Docker.Platform
  ( Platform (..)
  , getDisplayName
  )
  where

import Control.Applicative ( (<|>) )
import Data.Maybe ( catMaybes )
import Data.Text ( Text )
import qualified Data.Text as T

data Platform =
  -- The field descriptions below are from the documentation for the Docker
  -- Image Manifest Version 2, Schema 2.
  --
  -- Some of the fields, such as platformArchitecture and platformOS should always
  -- be defined, but we make them optional so that we can still display
  -- information about images that have somewhat malformed manifests.
  Platform {
    -- | The architecture field specifies the CPU architecture, for example
    -- amd64 or ppc64le.
    getPlatformArchitecture :: Maybe Text,
    -- | The os field specifies the operating system, for example linux or
    -- windows.
    getPlatformOS :: Maybe Text,
    -- | The optional os.version field specifies the operating system version,
    -- for example 10.0.10586.
    getPlatformOSVersion :: Maybe Text,
    -- | The optional os.features field specifies an array of strings, each
    -- listing a required OS feature (for example on Windows win32k).
    getPlatformOSFeatures :: Maybe [Text],
    -- | The optional variant field specifies a variant of the CPU, for example
    -- armv6l to specify a particular CPU variant of the ARM CPU.
    getPlatformVariant :: Maybe Text,
    -- | The optional features field specifies an array of strings, each listing
    -- a required CPU feature (for example sse4 or aes).
    getPlatformFeatures :: Maybe [Text]
  }

getDisplayName :: Platform -> Text
getDisplayName platform =
  T.intercalate " " .  catMaybes $
    [ getPlatformOS platform <|> Just "Unknown OS"
    , getPlatformOSVersion $ platform
    , Just "on"
    , getPlatformArchitecture platform <|> Just "Unknown Architecture"
    ]
