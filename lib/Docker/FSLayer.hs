module Docker.FSLayer
  ( FSLayer (..)
  , getDownloadUrl
  )
  where

import Data.Text ( Text )
import Data.Time ( UTCTime )
import qualified Data.Text as T
import Network.URI ( uriToString )

import Docker.Fetch.Urls ( getBlobUrl )
import Docker.Registry ( Registry )
import Docker.Repository ( Repository )

data FSLayer = FSLayer {
  getBaseRegistry :: Registry,
  getBaseRepository :: Repository,
  getDownloadUrls :: Maybe [Text],
  getDownloadSize :: Maybe Integer,
  getDigest :: Text,
  -- Size in bytes
  getCreated :: Maybe UTCTime,
  getCreatedBy :: Maybe Text
}

-- | Get a download URL for the layer
getDownloadUrl :: FSLayer -> Text
getDownloadUrl layer =
  case getDownloadUrls layer of
    Nothing -> downloadUrlFromDigest
    Just [] -> downloadUrlFromDigest
    Just (url:_) -> url
  where
    downloadUrlFromDigest =
      T.pack
      . flip (uriToString id) ""
      . getBlobUrl (getBaseRegistry layer) (getBaseRepository layer)
      $ getDigest layer
