{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Fetch
  ( Error(..)
  , getImage
  , parseChallenge )
  where

import Prelude hiding ( rem )
import Control.Applicative ( (<|>), many )
import Control.Monad ( forM, guard, unless, void )
import Control.Monad.Except ( MonadError, liftEither, runExceptT, throwError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State ( MonadState, StateT, evalStateT )
import qualified Data.Attoparsec.ByteString as Attoparsec ( satisfy, skipWhile, takeWhile1 )
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec hiding ( satisfy, skipWhile, takeWhile1 )
import qualified Data.ByteString as BS hiding ( split )
import qualified Data.ByteString.Char8 as BS ( split )
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Combinators ( mapLeft, maybeToRight )
import Data.Maybe ( fromMaybe, fromJust, isJust )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
  ( BodyReader, Response,
    withResponse, getOriginalRequest, requestFromURI,
    requestHeaders, applyBearerAuth,
    responseBody, responseHeaders, responseStatus )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types
  ( HeaderName, hAccept, hContentType, status401, status429, statusIsSuccessful )
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.Header ( hWWWAuthenticate )
import Network.URI ( URI, uriAuthority, uriPath, uriRegName )

import Docker.DockerImage ( DockerImage (..) )
import Docker.Fetch.Http ( brReadBounded )
import qualified Docker.Fetch.ImageConfig as Config
import Docker.Fetch.ImageManifestReference
  ( ImageManifestReference (ImageManifestReference) )
import qualified Docker.Fetch.ManifestV2 as Manifest
import Docker.Fetch.MediaTypes
import Docker.Fetch.OAuthContext
  ( OAuthContext, startingOAuthContext, tryAuthorizeScopes )
import Docker.Fetch.Parsing ( parseManifestListV2, parseV2Manifest, parseImageV1 )
import Docker.FSLayer ( FSLayer (FSLayer) )
import Docker.FullyQualifiedImageName
  ( FullyQualifiedImageName (FullyQualifiedImageName),
    TagOrDigest (TagOrDigest) )
import Docker.Registry ( Registry, getRegistryUrl )
import Docker.Repository ( Repository )
import qualified Docker.Repository as Repo
import Docker.SingleDockerImage ( SingleDockerImage (SingleDockerImage) )


data Error
  = ImageUsesAnUnsupportedManifestType
  | ManifestDidNotIncludeAMediaType
  | CouldNotParseManifestList
  | CouldNotParseManifest
  | CouldNotParseImageConfig
  | AccessToHostIsNotPermitted
  | FetchFailedError
  | CouldNotAuthorizeRequest String
  | FetchFailedWithAuthorization FetchResult
  | LowLevelFetchFailed String
  | TooManyRequests

getImage :: forall m. (MonadError Error m, MonadIO m)
          => Http.Manager
          -> [String]
          -> FullyQualifiedImageName
          -> m DockerImage
getImage manager permittedHosts =
    flip evalStateT startingOAuthContext . imageName
  where
    imageName :: FullyQualifiedImageName -> StateT OAuthContext m DockerImage
    imageName imageName'@(FullyQualifiedImageName registry repo _) = do
      let url = getManifestUrl imageName'
      (contentType, resp) <- fetch' [manifestV2ManifestMediaType, manifestListMediaType] url
      case contentType of
        Just tp
          | tp == manifestListMediaType -> imageManifestListV2 registry repo resp
          | tp == manifestV2ManifestMediaType -> imageManifestV2 registry repo resp
          | otherwise -> throwError ImageUsesAnUnsupportedManifestType
        _ -> throwError ManifestDidNotIncludeAMediaType

    imageManifestListV2 :: Registry
                        -> Repository
                        -> BSL.ByteString
                        -> StateT OAuthContext m DockerImage
    imageManifestListV2 registry repo manifestList = do
      manifestRefs <- liftEither
                      . mapLeft (const CouldNotParseManifestList)
                      . parseManifestListV2
                      $ manifestList

      fmap MultiPlatformDockerImage
        . forM manifestRefs $ \ref@(ImageManifestReference _ _ platform) -> do
            img <- imageManifestReference registry repo ref
            return (platform, img)

    imageManifestReference :: Registry
                            -> Repository
                            -> ImageManifestReference
                            -> StateT OAuthContext m SingleDockerImage
    imageManifestReference registry repo (ImageManifestReference mediaType digest _platform) = do
      let url = getManifestUrl' registry repo digest
      (_contentType, resp) <- fetch' [T.encodeUtf8 mediaType] url
      imageManifestV2' registry repo resp

    imageManifestV2 :: Registry
                    -> Repository
                    -> BSL.ByteString
                    -> StateT OAuthContext m DockerImage
    imageManifestV2 registry repo = fmap DockerImage . imageManifestV2' registry repo

    imageManifestV2' :: Registry
                      -> Repository
                      -> BSL.ByteString
                      -> StateT OAuthContext m SingleDockerImage
    imageManifestV2' registry repo rawManifest = do
      manifest <- liftEither
                  . mapLeft (const CouldNotParseManifest)
                  . parseV2Manifest
                  $ rawManifest

      let (configContentType, configUrl) = Manifest.getConfigUrl registry repo manifest
      (_contentType, rawConfig) <- fetch' [T.encodeUtf8 configContentType] configUrl

      config <- liftEither
                . mapLeft (const CouldNotParseImageConfig)
                . parseImageV1
                $ rawConfig

      return $ SingleDockerImage
        (Config.getID config)
        (Config.getParent config)
        (Config.getComment config)
        (Config.getCreated config)
        (Config.getDockerVersion config)
        (Config.getAuthor config)
        (Config.getSize config)
        (Config.getArchitecture config)
        (Config.getVariant config)
        (Config.getOS config)
        (Config.getOSVersion config)
        (Config.getOSFeatures config)
        (Config.getDefaultContainerConfig config)
        (mergeLayerInfo
          registry
          repo
          (Manifest.getFSLayers manifest)
          (Config.getHistory config))

    fetch' :: [BS.ByteString]
            -> URI
            -> StateT OAuthContext m (Maybe BS.ByteString, BSL.ByteString)
    fetch' acceptContentTypes url = do
      unless
        (let host = fmap uriRegName . uriAuthority $ url
          in isJust host && (fromJust host `elem` permittedHosts))
        $ throwError AccessToHostIsNotPermitted

      res <- runExceptT $ fetch manager Nothing acceptContentTypes url
      case res of
        Left e -> throwError $ LowLevelFetchFailed e
        Right (FetchFailed status _)
          | status == status429 -> throwError TooManyRequests
          | otherwise -> throwError FetchFailedError
        Right (FetchSucceeded contentType contentBody) ->
          return (contentType, contentBody)
        Right (FetchAuthorizationRequired challenge) -> do
          authToken <- runExceptT $ tryAuthorize manager challenge
          case authToken of
            Left e -> throwError $ CouldNotAuthorizeRequest e
            Right token -> do
              -- Note: this fetch could fail if enough time passes between
              -- getting the token and sending the request.
              res' <- runExceptT $ fetch manager (Just token) acceptContentTypes url
              case res' of
                Left e -> throwError $ LowLevelFetchFailed e
                Right (FetchFailed status _)
                  | status == status429 -> throwError TooManyRequests
                  | otherwise -> throwError FetchFailedError
                Right (FetchSucceeded contentType contentBody) ->
                  return (contentType, contentBody)
                Right res'' -> throwError $ FetchFailedWithAuthorization res''

-- | Try to get an authorization token to respond to a challenge.
tryAuthorize :: (MonadError String m, MonadIO m, MonadState OAuthContext m)
              => Http.Manager
              -> Maybe BS.ByteString -- ^ Challenge
              -> m Text
tryAuthorize _manager Nothing =
  throwError "Cannot authorize without a challenge"
tryAuthorize manager (Just challenge) = do
  (realm, service, scopes) <-
    liftEither
    . mapLeft (\e -> "Could not parse challenge: " ++ e)
    . parseChallenge
    $ challenge

  token <- runExceptT $ tryAuthorizeScopes manager realm service scopes
  case token of
    Left _e -> throwError "Could not authorize scopes."
    Right token' -> return token'

-- | Returns (realm, service, scopes) if the challenge is a Docker Bearer
-- challenge.
parseChallenge :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString, [BS.ByteString])
parseChallenge bs = do
  (scheme, params) <- flip Attoparsec.parseOnly bs $ do
    authScheme' <- authScheme
    void $ Attoparsec.many1 $ Attoparsec.char8 ' '
    authParams <- commaSepBy1 authParam
    return (authScheme', authParams)

  maybeToRight "Challenge is not for Bearer scheme." $ guard $ scheme == "Bearer"
  realm <- maybeToRight "No realm found" . lookup "realm" $ params
  service <- maybeToRight "No service found" . lookup "service" $ params
  -- TODO: This may not be the right way to split. I'm not sure if it handles
  -- multiple adjacent spaces properly.
  scopes <- BS.split ' ' <$> (maybeToRight "No scope found" . lookup "scope" $ params)
  return (realm, service, scopes)
  where
    authScheme = token
    authParam = do
      paramName <- token
      void $ Attoparsec.char8 '='
      paramValue <- token <|> quotedString
      return (paramName, paramValue)

    token = Attoparsec.takeWhile1 (\c -> isChar c && (not . isSeparator $ c))

    quotedString = do
      void $ Attoparsec.char8 '"'
      res <- many (qtdtext <|> quotedPair)
      void $ Attoparsec.char8 '"'
      return $ BS.pack res

    qtdtext = Attoparsec.satisfy isQtdtext
    quotedPair = Attoparsec.char8 '\\' >> Attoparsec.satisfy isChar

    separators = BS.unpack "()<>@,;:\\\"/[]?={} " ++ [9] -- horizontal tab

    isChar c = c >= 0 && c <= 127
    isCtl c = (c >= 0 && c <= 31) || c == 127
    isSeparator c = c `elem` separators
    -- The rules for text are a bit complex. CRLF is allowed as part of a header
    -- field continuation. Since we are only parsing the actual value of the
    -- header at this point, we should not encounter any CRLFs.
    --
    -- Furthermore, linear whitespace "has the same semantics as SP," so
    -- sequences of linear whitespace may have been replaced by a single space,
    -- but this is not required by HTTP/1.1, so we need to be prepared to handle
    -- sequences of whitespace.
    isText c = (not . isCtl $ c) || isSpace c || c == 9 -- horizontal tab
    isQtdtext c = isText c && c /= 34 -- double-quote mark
    isSpace c = c == 32

    commaSepBy1 = flip Attoparsec.sepBy1 commaAndOptionalWhitespace
      where commaAndOptionalWhitespace = do
              Attoparsec.skipWhile (\c -> isSpace c || c == 9)
              void $ Attoparsec.char8 ','
              Attoparsec.skipWhile (\c -> isSpace c || c == 9)
              return ()

fetch :: (MonadIO m, MonadError String m)
      => Http.Manager
      -> Maybe Text
      -> [BS.ByteString]
      -> URI
      -> m FetchResult
fetch manager bearerToken acceptContentTypes url = do
    req <- case requestFromURI url of
      Left _e -> throwError "Could not build request for URL."
      Right r -> return . addAuthHeader . addAcceptHeaders $ r

    liftIO $ withResponse req manager handleResponse
  where
    handleResponse :: Response BodyReader -> IO FetchResult
    handleResponse resp
      | statusIsSuccessful . responseStatus $ resp = do
          rawManifest <- brReadBounded targetMaxBodySize $ responseBody resp
          let contentType = lookup hContentType . responseHeaders $ resp
          return $ FetchSucceeded contentType rawManifest
      | responseStatus resp == status401 = do
          let challenge = lookup hWWWAuthenticate . responseHeaders $ resp
          return $ FetchAuthorizationRequired challenge
      | otherwise =
          return $ FetchFailed (responseStatus resp) (getOriginalRequest resp)

    targetMaxBodySize :: Int
    targetMaxBodySize = 1024 * 1024

    addAcceptHeaders :: Http.Request -> Http.Request
    addAcceptHeaders =
      addHeaders $ fmap (\ct -> (hAccept, ct)) acceptContentTypes

    addAuthHeader :: Http.Request -> Http.Request
    addAuthHeader = case bearerToken of
      Nothing -> id
      Just bearerToken' ->
        -- TODO: We encode the token with UTF-8. We should check that this is
        -- the correct encoding to use.
        applyBearerAuth $ T.encodeUtf8 bearerToken'

    addHeaders :: [(HeaderName, BS.ByteString)] -> Http.Request -> Http.Request
    addHeaders newHeaders req =
      let headers = requestHeaders req
      in req {
            requestHeaders = headers ++ newHeaders
          }

data FetchResult
  = FetchSucceeded
      (Maybe BS.ByteString) -- ^ Content type
      BSL.ByteString -- ^ Content body
  | FetchAuthorizationRequired
      (Maybe BS.ByteString) -- ^ WWW-Authenticate header
  | FetchFailed HttpTypes.Status Http.Request
  deriving (Show)


-- Computing URLs
-- --------------

getManifestUrl :: FullyQualifiedImageName -> URI
getManifestUrl (FullyQualifiedImageName registry repo tagOrDigest) =
  getManifestUrl' registry repo (toText tagOrDigest)
    where
      toText Nothing = "latest"
      toText (Just (TagOrDigest str)) = T.pack str

getManifestUrl' :: Registry
                -- ^ The registry that contains the manifest
                -> Repository
                -- ^ The repository that contains the manifest
                -> Text
                -- ^ The tag or digest of the manifest
                -> URI
getManifestUrl' registry repo tagOrDigest =
  (getRegistryUrl registry) {
    uriPath = "/v2/" ++ Repo.getRepositoryName repo ++ "/manifests/" ++ T.unpack tagOrDigest
  }



mergeLayerInfo :: Registry
                -> Repository
                -> [Manifest.FSLayer]
                -> Maybe [Config.FSLayer]
                -> [FSLayer]
mergeLayerInfo registry repo m_layers c_layers =
  merge m_layers (fromMaybe [] c_layers)
  where
    merge [] _ = []
    merge (m:ms) [] =
      (:merge ms []) $ FSLayer
        registry
        repo
        (Manifest.getLayerDownloadUrls m)
        (Manifest.getLayerDownloadSize m)
        (Manifest.getLayerDigest m)
        Nothing
        Nothing
    merge (m:ms) (c:cs) =
      (:merge ms cs) $ FSLayer
        registry
        repo
        (Manifest.getLayerDownloadUrls m)
        (Manifest.getLayerDownloadSize m)
        (Manifest.getLayerDigest m)
        (Config.getLayerCreated c)
        (Config.getLayerCreatedBy c)
