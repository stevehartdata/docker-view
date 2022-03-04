module Docker.Fetch.ImageManifestReference
  ( ImageManifestReference (..)
  )
  where

import Data.Text ( Text )

import Docker.Platform ( Platform )

-- Note: The documentation for each field below comes from
-- <https://docs.docker.com/registry/spec/manifest-v2-2/>. We only include the
-- fields that we actually need. Additional fields may be added later.
data ImageManifestReference = ImageManifestReference {
  mediaType :: Text,
  -- ^ The MIME type of the referenced object. This will generally be
  -- application/vnd.docker.distribution.manifest.v2+json, but it could also be
  -- application/vnd.docker.distribution.manifest.v1+json if the manifest list
  -- references a legacy schema-1 manifest.
  digest :: Text,
  -- ^ The digest of the content, as defined by the Registry V2 HTTP API
  -- Specificiation.
  platform :: Platform
  -- ^ The platform supported by the image defined by the image manifest
  }
