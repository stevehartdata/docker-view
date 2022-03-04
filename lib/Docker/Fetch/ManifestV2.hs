module Docker.Fetch.ManifestV2
  ( ManifestV2 (ManifestV2)
  , FSLayer (..)
  , getFSLayers
  , getConfigUrl
  )
  where

import Network.URI ( URI )
import Data.Text ( Text )

import Docker.Registry ( Registry )
import Docker.Repository ( Repository )
import Docker.Fetch.Urls ( getBlobUrl )

data ManifestV2 = ManifestV2 {
  _getMediaType :: Text,
  -- ^ Media type for the config file
  _getDigest :: Text,
  -- ^ Digest of the config file
  getFSLayers :: [FSLayer]
}

data FSLayer = FSLayer {
  getLayerDownloadSize :: Maybe Integer,
  getLayerDownloadUrls :: Maybe [Text],
  getLayerDigest :: Text
}

-- Returns the expected content type and URL for the config file.
getConfigUrl :: Registry -> Repository -> ManifestV2 -> (Text, URI)
getConfigUrl registry repo (ManifestV2 mediaType digest _) =
  (mediaType, getBlobUrl registry repo digest)
