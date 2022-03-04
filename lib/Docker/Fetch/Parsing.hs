{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Fetch.Parsing
  ( eitherDecodeWith'
  , parseManifestListV2
  , parseV2Manifest
  , parseImageV1
  )
  where

import Control.Monad ( forM )
import Control.Monad.Error.Class ( MonadError, liftEither )
import Data.Aeson ( (.:), (.:?), Value, withObject )
import qualified Data.Aeson as Aeson
import Data.Aeson.Internal ( formatError, iparse )
import Data.Aeson.Parser ( eitherDecodeWith, jsonEOF )
import Data.Aeson.Types ( Parser )
import qualified Data.ByteString.Lazy as BSL

import Docker.ContainerConfig ( ContainerConfig (ContainerConfig) )
import Docker.Fetch.ImageConfig ( ImageConfig (ImageConfig) )
import qualified Docker.Fetch.ImageConfig as Config
import Docker.Fetch.ImageManifestReference
  ( ImageManifestReference (ImageManifestReference) )
import Docker.Fetch.ManifestV2 ( ManifestV2 (ManifestV2) )
import qualified Docker.Fetch.ManifestV2 as Manifest
import Docker.Platform ( Platform (Platform) )

parseManifestListV2 :: BSL.ByteString -> Either String [ImageManifestReference]
parseManifestListV2 = eitherDecodeWith' $
  withObject "Manifest List" $ \v -> do
    manifests <- v .: "manifests"
    forM manifests $ \manifest ->
      ImageManifestReference
        <$> manifest .: "mediaType"
        <*> manifest .: "digest"
        <*> (manifest .: "platform" >>= mkPlatform)

  where
    mkPlatform :: Aeson.Object -> Parser Platform
    mkPlatform v =
      Platform
        <$> v .:? "architecture"
        <*> v .:? "os"
        <*> v .:? "os.version"
        <*> v .:? "os.features"
        <*> v .:? "variant"
        <*> v .:? "features"

parseV2Manifest :: BSL.ByteString -> Either String ManifestV2
parseV2Manifest = eitherDecodeWith' $
  withObject "V2 Manifest" $ \v -> do
    (configObj :: Aeson.Object) <- v .: "config"
    ManifestV2
      <$> configObj .: "mediaType"
      <*> configObj .: "digest"
      <*> (v .: "layers" >>= mapM parseFSLayer)
  where
    parseFSLayer :: Aeson.Object -> Parser Manifest.FSLayer
    parseFSLayer v =
      Manifest.FSLayer
        <$> v .:? "size"
        <*> v .:? "urls"
        <*> v .: "digest"

-- The format for this parser is defined mostly here:
-- <https://github.com/moby/moby/blob/a5c757555091df6bd1e9c60c175d167dcdfde83c/image/image.go#L35>
parseImageV1 :: BSL.ByteString -> Either String ImageConfig
parseImageV1 = eitherDecodeWith' $
  withObject "Image Config" $ \v ->
    ImageConfig
      <$> v .:? "id"
      <*> v .:? "parent"
      <*> v .:? "comment"
      <*> v .:? "created"
      <*> v .:? "docker_version"
      <*> v .:? "author"
      <*> v .:? "size"
      <*> v .:? "architecture"
      <*> v .:? "variant"
      <*> v .:? "os"
      <*> v .:? "os.version"
      <*> v .:? "os.features"
      <*> (v .:? "config" >>= mapM parseContainerConfig)
      <*> (v .:? "history" >>= mapM (mapM parseHistory))
  where
    parseContainerConfig v =
      ContainerConfig
        <$> v .:? "Hostname"
        <*> v .:? "Domainname"
        <*> v .:? "User"
        <*> v .:? "AttachStdin"
        <*> v .:? "AttachStdout"
        <*> v .:? "AttachStderr"
        <*> v .:? "ExposedPorts"
        <*> v .:? "Tty"
        <*> v .:? "OpenStdin"
        <*> v .:? "StdinOnce"
        <*> v .:? "Env"
        <*> v .:? "Cmd"
        -- Don't support HealthCheck yet.
        -- <*> v .:? "Healthcheck"
        <*> v .:? "ArgsEscaped"
        <*> v .:? "Image"
        <*> v .:? "Volumes"
        <*> v .:? "WorkingDir"
        <*> v .:? "Entrypoint"
        <*> v .:? "NetworkDisabled"
        <*> v .:? "MacAddress"
        <*> v .:? "OnBuild"
        <*> v .:? "Labels"
        <*> v .:? "StopSignal"
        <*> v .:? "StopTimeout"
        <*> v .:? "Shell"

    parseHistory v =
      Config.FSLayer
        <$> v .:? "created"
        <*> v .:? "created_by"

eitherDecodeWith' :: MonadError String m
                  => (Value -> Parser a) -> BSL.ByteString -> m a
eitherDecodeWith' parser bs =
  liftEither . eitherFormatError . eitherDecodeWith jsonEOF (iparse parser) $ bs
  where
    -- From the aeson implementation
    eitherFormatError = either (Left . uncurry formatError) Right