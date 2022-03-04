module Docker.SingleDockerImage
  ( SingleDockerImage (..)
  )
  where

import Data.Text ( Text )
import Data.Time ( UTCTime )

import Docker.ContainerConfig ( ContainerConfig )
import Docker.FSLayer ( FSLayer )

data SingleDockerImage = SingleDockerImage {
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
  getFilesystemLayers :: [FSLayer]
}
