{-# LANGUAGE OverloadedStrings #-}

module Docker
  ( FullyQualifiedImageName
  , getRegistryHostname, getRegistryPort, getRepositoryName, getTagOrDigest
  , parseImageName

  , getImage

  , DockerImage (DockerImage, MultiPlatformDockerImage)
  , SingleDockerImage

  , ContainerConfig
  , FSLayer
  , Platform
  )
  where

import Docker.ContainerConfig ( ContainerConfig )
import Docker.DockerImage ( DockerImage (..) )
import Docker.Fetch ( getImage )
import Docker.FSLayer ( FSLayer )
import Docker.FullyQualifiedImageName
import Docker.Platform ( Platform )
import Docker.SingleDockerImage ( SingleDockerImage )
