module Docker.Fetch.Urls
  ( getBlobUrl
  )
  where

import Data.Text ( Text )
import qualified Data.Text as T
import Network.URI ( URI, uriPath )

import Docker.Registry ( Registry, getRegistryUrl )
import Docker.Repository ( Repository )
import qualified Docker.Repository as Repo

getBlobUrl :: Registry -> Repository -> Text -> URI
getBlobUrl registry repo blobDigest =
  (getRegistryUrl registry) {
    uriPath = "/v2/" ++ Repo.getRepositoryName repo ++ "/blobs/" ++ T.unpack blobDigest
  }
