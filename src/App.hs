{-# LANGUAGE OverloadedStrings #-}

module App ( app ) where

import Control.Monad ( join )
import Control.Monad.Except ( runExceptT )
import Data.Text.Encoding ( decodeUtf8' )
import Data.Text ( Text, unpack )
import Data.Text.Lazy ( pack )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types ( status200, status400, status404, status500 )
import Network.Wai
  ( Request, Response, ResponseReceived, pathInfo, queryString,
    responseFile, responseLBS )

import Docker ( getImage, parseImageName )

import Render ( renderImage, renderNoImage, renderError )

app :: Http.Manager -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app manager =
  let getResponseForRequest' = getResponseForRequest manager
  in \req respond ->
        getResponseForRequest' req >>= respond

getResponseForRequest :: Http.Manager -> Request -> IO Response
getResponseForRequest manager request = do
  case pathInfo request of
    [] ->
      case join . lookup "i" . queryString $ request of
        Nothing ->
          return $ responseLBS status200 [] $ renderNoImage suggestedImages
        Just imageName -> case decodeUtf8' imageName of
          Left _ -> return $ responseLBS status400 [] "Invalid image name"
          Right imageName' -> getResponseForQuery manager imageName'
    ["styles.css"] ->
      return $ responseFile status200 [] "html/styles.css" Nothing
    ["styles-desktop.css"] ->
      return $ responseFile status200 [] "html/styles-desktop.css" Nothing
    _ -> return $ responseLBS status404 [] "Resource not found"

getResponseForQuery :: Http.Manager -> Text -> IO Response
getResponseForQuery manager imageName =
  case (parseImageName . unpack $ imageName) of
    Nothing ->
      return $ responseLBS status400 [] "Image name could not be parsed"
    Just parsedImageName -> do
      img <- runExceptT $ getImage manager permittedRegistryHosts parsedImageName
      return $ case img of
        Left e -> responseLBS status500 [] . renderError suggestedImages $ e
        Right img' ->
          responseLBS status200 [] . renderImage imageName suggestedImages $ img'

permittedRegistryHosts :: [String]
permittedRegistryHosts = ["registry-1.docker.io", "mcr.microsoft.com"]

suggestedImages :: [Text]
suggestedImages = [ "ubuntu:latest"
                  , "nginx:latest"
                  , "mcr.microsoft.com/dotnet/aspnet:6.0"
                  ]
