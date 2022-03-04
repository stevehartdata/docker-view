module Docker.Fetch.ImageConfig
  ( ImageConfig (..)
  , FSLayer (..)
  )
  where

import Data.Text ( Text )
import Data.Time ( UTCTime )

import Docker.ContainerConfig ( ContainerConfig )

data ImageConfig = ImageConfig {
  getID :: Maybe Text,
  getParent :: Maybe Text,
  getComment :: Maybe Text,
  getCreated :: Maybe UTCTime,
  getDockerVersion :: Maybe Text,
  getAuthor :: Maybe Text,
  getSize :: Maybe Text,
  getArchitecture :: Maybe Text,
  getVariant :: Maybe Text,
  getOS :: Maybe Text,
  getOSVersion :: Maybe Text,
  getOSFeatures :: Maybe [Text],
  getDefaultContainerConfig :: Maybe ContainerConfig,
  getHistory :: Maybe [FSLayer]
}

data FSLayer = FSLayer {
  getLayerCreated :: Maybe UTCTime,
  getLayerCreatedBy :: Maybe Text
}
