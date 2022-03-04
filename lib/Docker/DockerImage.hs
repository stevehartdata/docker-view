module Docker.DockerImage
  ( DockerImage (DockerImage, MultiPlatformDockerImage)
  )
  where

import Docker.Platform ( Platform )
import Docker.SingleDockerImage ( SingleDockerImage )

data DockerImage
  -- | A standard Docker image
  = DockerImage SingleDockerImage
  -- | A Docker image that supports multiple platforms
  | MultiPlatformDockerImage [(Platform, SingleDockerImage)]

