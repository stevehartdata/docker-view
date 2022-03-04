{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Render
  ( renderImage
  , renderNoImage
  , renderError
  )
  where

import Prelude hiding ( head, id, div )
import qualified Prelude
import Control.Monad ( forM_ )
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Time ( UTCTime )
import Data.Time.Format.ISO8601 ( iso8601Show )
import NeatInterpolation ( trimming )
import qualified Text.Blaze.Renderer.Utf8 as Blaze
import Text.Blaze.Html5 hiding ( i )
import qualified Text.Blaze.Html5.Attributes as A

import Docker
  ( DockerImage (DockerImage, MultiPlatformDockerImage), ContainerConfig,
    FSLayer, Platform )
import qualified Docker.ContainerConfig as Config
import qualified Docker.Fetch as Fetch
import qualified Docker.FSLayer as Layer
import Docker.SingleDockerImage
import qualified Docker.Platform as Platform

renderNoImage :: [Text] -> BSL.ByteString
renderNoImage suggestedImages =
  Blaze.renderMarkup $ renderCommon mempty suggestedImages $ do
    p $ do
      text "Welcome to Docker View, a service of "
      a ! A.href "https://americandata.consulting" $ text "American Data"
      text "!"

    p . text $ [trimming|
Docker View is designed to give you a clear understanding of a Docker image by
showing the actual properties of the image. Once you understand how a Docker
image is defined, this view is more helpful than looking at the Dockerfile that
was used to produce the image. This view is also helpful when you don't have
access to the Dockerfile that was used to produce the image.
|]

    h2 . text $ "Limitations"
    p . text $ "Docker View is a new service, so you might run into some limitations:"

    ul $ do
      li $ do
        text $ [trimming|
Currently, only the main Docker registry and Microsoft's registry are supported.
Feel free to
|]
        a ! A.href "mailto:contact@americandata.consulting" $ text " contact us"
        text " if you need support for another registry."
      li . text $ [trimming|
We are also limited by Docker's quota for registry access, so you may receive an error
if you submit many requests, or if other users are submitting many requests. We may consider
increasing the quota in the future.
|]

renderCommon :: Text -> [Text] -> Html -> Html
renderCommon titleText suggestedImages inner =
  docTypeHtml $ do
    head $ do
      meta ! A.charset "utf-8"
      title $ text titleText
      link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "styles.css"
      link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.media "screen and (min-width: 768px)" ! A.href "styles-desktop.css"

    body $ do
      h1 ! A.id "page-title" $ do
        a ! A.href "/" $ text "Docker View"
        if not . T.null $ titleText
          then text $ ": " <> titleText
          else mempty

      div ! A.id "new-image-form" ! A.class_ "sidebar" $ do

        form $ do
          h2 $ text "View Another Image"
          label ! A.for "new-image-name" $ text "Image Name"
          input ! A.id "new-image-name" ! A.type_ "text" ! A.name "i"
          button ! A.type_ "submit" $ text "View"

        form ! A.id "suggested-images" $ do
          h3 $ text "You Might Like"
          ul $ forM_ suggestedImages $ \imageName -> do
            li $ button ! A.type_ "submit" ! A.name "i" ! A.value (toValue imageName)
                  $ text imageName

      inner

renderImage :: Text -> [Text] -> DockerImage -> BSL.ByteString
renderImage imageName suggestedImages =
  Blaze.renderMarkup . renderImageList imageName suggestedImages

renderImageList :: Text -> [Text] -> DockerImage -> Html
renderImageList imageName suggestedImages dockerImage =
  renderCommon imageName suggestedImages $ do
      section ! A.class_ "main" $ do
        renderImage' dockerImage

renderImage' :: DockerImage -> Markup
renderImage' (DockerImage image) = do
  section ! A.class_ "image" $ renderSingleImage h2 image
renderImage' (MultiPlatformDockerImage images) = do
  section ! A.class_ "platforms" $ do
    text "This image supports the following platforms:"
    forM_ images (uncurry renderImageForPlatform)

renderImageForPlatform :: Platform -> SingleDockerImage -> Markup
renderImageForPlatform platform dockerImage =
  section ! A.class_ "image" $ do
    h2 $ text $ Platform.getDisplayName platform
    renderSingleImage h3 dockerImage

renderSingleImage :: (Markup -> Markup) -> SingleDockerImage -> Markup
renderSingleImage h3' dockerImage = do
  section ! A.class_ "overview" $ dl $ do
    renderAttribute "ID" $ getID dockerImage
    renderAttribute "Parent" $ getParent dockerImage
    renderAttribute "Comment" $ getComment dockerImage
    renderAttribute "Created" $ getCreated dockerImage
    renderAttribute "Docker Version" $ getDockerVersion dockerImage
    renderAttribute "Author" $ getAuthor dockerImage
    renderAttribute "Size" $ getSize dockerImage

  section ! A.class_ "image-platform" $ do
    h3' $ text "About the Platform This Image Can Run On"
    dl $ do
      renderAttribute "Architecutre" $ getArchitecture dockerImage
      renderAttribute "Variant" $ getVariant dockerImage
      renderAttribute "OS" $ getOS dockerImage
      renderAttribute "OS Version" $ getOSVersion dockerImage
      renderAttribute "OS Features" $ getOSFeatures dockerImage

  section ! A.class_ "image-default-config" $ do
    h3' $ text "Default Configuration for Containers Based on This Image"
    p $ text "This configuration controls how Docker starts a container based on this image."
    fromMaybe mempty $ fmap renderConfig . getDefaultContainerConfig $ dockerImage

  section ! A.class_ "image-fs-layers" $ do
    h3' $ text "Filesystem Layers"
    ol $ renderFSLayers $ getFilesystemLayers dockerImage

  where
    renderAttribute :: ImageAttributeValue a => Text -> Maybe a -> Markup
    renderAttribute label' = renderAttribute' label' Nothing

    renderAttributeWithHelpText :: ImageAttributeValue a
                                => Text -> Text -> Maybe a -> Markup
    renderAttributeWithHelpText label' helpText =
      renderAttribute' label' $ Just helpText

    renderAttribute' :: ImageAttributeValue a
                      => Text -> Maybe Text -> Maybe a -> Markup
    renderAttribute' label' helpText value = div $ do
      let helpTextAttribute =
            case helpText of
              Nothing -> mempty
              Just helpText' -> A.title $ toValue helpText'
      dt ! helpTextAttribute $ text label'
      dd ! A.class_ "value" $ fromMaybe mempty . fmap toText $ value

    renderConfig :: ContainerConfig -> Markup
    renderConfig config =
      dl $ do
        renderAttribute "Host Name" $ Config.getHostName config
        renderAttribute "Domain Name" $ Config.getDomainName config
        renderAttributeWithHelpText
          "User"
          "The user that will run the command(s) inside the container. A group can also be specified, as in user:group."
          $ Config.getUser config
        renderAttribute "Attach STDIN" $ Config.getAttachStdin config
        renderAttribute "Attach STDOUT" $ Config.getAttachStdout config
        renderAttribute "Attach STDERR" $ Config.getAttachStderr config
        renderAttribute "Exposed Ports" $ Config.getExposedPorts config
        renderAttributeWithHelpText
          "TTY"
          "Attach standard streams to a TTY, including stdin if it is not closed."
          $ Config.getTty config
        renderAttribute "Open STDIN" $ Config.getOpenStdin config
        renderAttributeWithHelpText
          "STDIN Once"
          "Automatically close STDIN after a single attached client disconnects."
          $ Config.getStdinOnce config
        renderAttributeWithHelpText
          "Environment Variables"
          "Environment variables to set for the commands that will run in the container."
          $ Config.getEnvironmentVariables config
        renderAttributeWithHelpText
          "Command"
          "The command to run when starting the container."
          $ Config.getCommand config
        -- Don't support health checks yet.
        -- renderAttributeWithHelpText
        --   "Health Check"
        --   "Describes how to check the container's health."
        --   $ Config.getHealthCheck config
        renderAttributeWithHelpText
          "Args Escaped"
          "For Windows containers, indicates whether the command is already escaped. If it is, then it will be treated as a command line."
          $ Config.getArgsEscaped config
        renderAttributeWithHelpText
          "Image"
          "Name of the image as it was passed by the operator."
          $ Config.getImage config
        renderAttribute "Volumes" $ Config.getVolumes config
        renderAttribute "Working Directory" $ Config.getWorkingDirectory config
        renderAttributeWithHelpText
          "Entrypoint"
          "Entrypoint to run when starting the container."
          $ Config.getEntrypoint config
        renderAttribute "Networking Disabled"
          $ Config.getNetworkingDisabled config
        renderAttribute "MAC Address" $ Config.getMacAddress config
        renderAttributeWithHelpText
          "ONBUILD"
          "ONBUILD metadata that were defined on the image Dockerfile."
          $ Config.getOnBuild config
        renderAttributeWithHelpText
          "Labels"
          "List of labels set for this container"
          $ Config.getLabels config
        renderAttributeWithHelpText
          "Stop Signal"
          "Signal to stop the container."
          $ Config.getStopSignal config
        renderAttributeWithHelpText
          "Stop Timeout"
          "Timeout (in seconds) before the container will be stopped."
          $ Config.getStopTimeout config
        renderAttributeWithHelpText
          "Shell"
          "The shell to use for the shell form of RUN, CMD, and ENTRYPOINT."
          $ Config.getShell config

    renderFSLayers :: [FSLayer] -> Markup
    renderFSLayers = go 1
      where
        go :: Int -> [FSLayer] -> Markup
        go _ [] = mempty
        go i (l:ls) = do
          li $ do
            a
              ! (A.href . fromString . T.unpack . Layer.getDownloadUrl $ l)
              $ string $ "Layer " ++ show i
            text $ " ("
                    <> (fromMaybe "Unknown size"
                        . fmap prettyPrintSize
                        . Layer.getDownloadSize $ l)
                    <> ")"
            dl $ do
              renderAttribute "Created" $ Layer.getCreated l
              renderAttribute "Created By" $ Layer.getCreatedBy l
          go (i + 1) ls

renderError :: [Text] -> Fetch.Error -> BSL.ByteString
renderError suggestedImages e =
  Blaze.renderMarkup . renderCommon "Uh oh!" suggestedImages $ do
    p . text $ "Oh no! An error occurred! " <> getErrorMessage e

  where
    getErrorMessage :: Fetch.Error -> Text
    getErrorMessage (Fetch.ImageUsesAnUnsupportedManifestType) =
      "It looks like the image you requested uses a manifest type that we can't understand."
    getErrorMessage Fetch.ManifestDidNotIncludeAMediaType =
      "The server did not return a media type with one of the image manifests."
    getErrorMessage Fetch.CouldNotParseManifestList =
      "We were not able to parse one of the image manifest lists."
    getErrorMessage Fetch.CouldNotParseManifest =
      "We were not able to parse one of the image manifests."
    getErrorMessage Fetch.CouldNotParseImageConfig =
      "We were not able to parse one of the image configurations."
    getErrorMessage Fetch.AccessToHostIsNotPermitted =
      "Your image is hosted on a registry that is not supported by Docker Viewer."
    getErrorMessage Fetch.FetchFailedError =
      "We were unable to fetch an image artifact."
    getErrorMessage (Fetch.CouldNotAuthorizeRequest _) =
      "We were not able to authorize a required request to the registry that hosts the image you requested."
    getErrorMessage (Fetch.FetchFailedWithAuthorization _) =
      "We were unable to fetch an image artifact, even after gaining authorization."
    getErrorMessage (Fetch.LowLevelFetchFailed _) =
      "A low-level HTTP error occurred."
    getErrorMessage Fetch.TooManyRequests =
      "We've exceeded our quota for requests to the registry that hosts the image you requested. You may be able to view images in a different registry, but you should not try viewing images in this registry for a little while.."

prettyPrintSize :: Integer -> Text
-- TODO: Make more human-friendly output
prettyPrintSize i = (T.pack . show $ i) <> " bytes"

class ImageAttributeValue a where
  toText :: a -> Markup

instance ImageAttributeValue Bool where
  toText True = "True"
  toText False = "False"

instance  ImageAttributeValue Integer where
  toText = string . show

instance ImageAttributeValue Text where
  toText = text . Prelude.id

instance ImageAttributeValue [Text] where
  toText xs = ul $ forM_ xs $ li . text

instance ImageAttributeValue [(Text, Text)] where
  toText = (.) dl $ mapM_ $ \(k, v) -> do
        dd . text $ k
        dl . text $ v

instance ImageAttributeValue UTCTime where
  toText = string . iso8601Show
