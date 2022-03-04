{-# LANGUAGE OverloadedStrings #-}

module Docker.Fetch.MediaTypes
  ( manifestListMediaType
  , manifestV2ManifestMediaType
  )
  where

import qualified Data.ByteString as BS

manifestListMediaType :: BS.ByteString
manifestListMediaType =
  "application/vnd.docker.distribution.manifest.list.v2+json"

manifestV2ManifestMediaType :: BS.ByteString
manifestV2ManifestMediaType =
  "application/vnd.docker.distribution.manifest.v2+json"
