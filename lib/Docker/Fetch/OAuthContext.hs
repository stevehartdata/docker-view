{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Docker.Fetch.OAuthContext
  ( OAuthContext
  , startingOAuthContext
  , tryAuthorizeScopes
  ) where

import Control.Exception ( SomeException )
import qualified Control.Exception as E
import Control.Monad ( unless )
import Control.Monad.Except ( ExceptT, MonadError, liftEither, runExceptT, throwError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State ( MonadState, get, put )
import Control.Monad.Trans.Maybe ( MaybeT(..), runMaybeT )
import Data.Aeson ( (.:), (.:?), withObject )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Combinators ( mapLeft )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock ( UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime )
import Network.HTTP.Client ( Response, withResponse )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types ( statusIsSuccessful )
import Network.URI ( URI, parseURI )

import Docker.Fetch.Http ( brReadBounded )
import Docker.Fetch.Parsing ( eitherDecodeWith' )


data OAuthContext = OAuthContext (Map Key Token)

startingOAuthContext :: OAuthContext
startingOAuthContext = OAuthContext Map.empty

data Key = Key
  BS.ByteString -- ^ Realm
  BS.ByteString -- ^ Service
  [BS.ByteString] -- ^ Scopes
  deriving (Eq, Ord)

getTokenFromContext :: OAuthContext
                    -> BS.ByteString -- ^ Realm
                    -> BS.ByteString -- ^ Service
                    -> [BS.ByteString] -- ^ Scopes
                    -> IO (Maybe Text)
getTokenFromContext (OAuthContext contextMap) realm service scopes =
  runMaybeT $ do
    token <- hoistMaybe $ Map.lookup (Key realm service scopes) contextMap
    now <- liftIO $ getCurrentTime
    if now >= getTokenExpiration token
      then hoistMaybe Nothing
      else return $ getTokenValue token
  where
    -- This function was added in a recent version of transformers
    hoistMaybe = MaybeT . pure

addTokenToContext :: OAuthContext
                  -> BS.ByteString -- ^ Realm
                  -> BS.ByteString -- ^ Service
                  -> [BS.ByteString] -- ^ Scopes
                  -> Token -- ^ Token
                  -> OAuthContext
addTokenToContext (OAuthContext contextMap) realm service scopes token =
  OAuthContext $ Map.insert (Key realm service scopes) token contextMap

-- | Returns a token for the requested scopes, if authorization can be obtained.
-- Existing authorizations in the context may be used, or an access token may be
-- requested from the token endpoint.
tryAuthorizeScopes :: (MonadError Error m, MonadIO m, MonadState OAuthContext m)
                    => Http.Manager
                    -> BS.ByteString -- ^ Realm
                    -> BS.ByteString -- ^ Service
                    -> [BS.ByteString] -- ^ Scopes
                    -> m Text
tryAuthorizeScopes manager realm service scopes = do
  -- Docker uses the realm to specify the token endpoint.
  realm' <- case T.decodeUtf8' realm of
    Left _e -> throwError CouldNotDecodeRealmAsText
    Right t -> return $ T.unpack t

  tokenEndpoint <-
    liftMaybe CouldNotParseRealmAsTokenEndpoint $ parseURI realm'

  -- Check if we have a valid token in the OAuthContext already before we
  -- request a fresh one.
  context <- get
  existingToken <- liftIO $ getTokenFromContext context realm service scopes
  case existingToken of
    Nothing -> do
      t <- getToken manager tokenEndpoint service scopes
      put $ addTokenToContext context realm service scopes t
      return $ getTokenValue t
    (Just t) -> return t

  where
    liftMaybe :: (MonadError e m) => e -> Maybe a -> m a
    liftMaybe s mb = case mb of
      Nothing -> throwError s
      Just x -> return x

data Token = Token {
  getTokenExpiration :: UTCTime,
  getTokenValue :: Text
}

-- | Get a token from the token endpoint.
getToken :: (MonadError Error m, MonadIO m)
          => Http.Manager
          -> URI -- ^ Token endpoint
          -> BS.ByteString -- ^ Service
          -> [BS.ByteString] -- ^ Scopes
          -> m Token
getToken manager tokenEndpoint service scopes = do
  req <- convertExceptionToError
          InvalidTokenEndpointUri
          $ Http.requestFromURI tokenEndpoint

  -- TODO: I need to check, but I think existing query parameters are supposed
  -- to be preserved. For now, we don't support token endpoints with query
  -- parameters.
  let req' = Http.setQueryString
                (("service", Just service):map (\s -> ("scope", Just s)) scopes)
                req

  withResponse' req' manager $ \resp -> do
    unless (statusIsSuccessful . Http.responseStatus $ resp)
      $ throwError TokenRetrievalFailed

    tokenResponse <- brReadBounded' $ Http.responseBody resp

    liftIO getCurrentTime >>= getTokenFromTokenResponse tokenResponse

  where
    withResponse' :: (MonadError Error m, MonadIO m)
                  => Http.Request
                  -> Http.Manager
                  -> (Response Http.BodyReader -> ExceptT Error IO a)
                  -> m a
    withResponse' req mgr f = do
      res <- liftIO
              . E.handle (return . Left . ExceptionOccurredDuringHttpRequest)
              $ withResponse req mgr (\resp -> runExceptT . f $ resp)
      liftEither res

    brReadBounded' :: (MonadError Error m, MonadIO m)
                    => Http.BodyReader -> m BSL.ByteString
    brReadBounded' reader = do
      res <- liftIO
              . E.handle (return . Left . ExceptionOccurredWhileReadingResponse)
              . fmap Right
              $ brReadBounded targetMaxBodySize reader
      liftEither res

    targetMaxBodySize :: Int
    targetMaxBodySize = 1024 * 1024

    getTokenFromTokenResponse :: MonadError Error m
                              => BSL.ByteString -> UTCTime -> m Token
    getTokenFromTokenResponse tokenResponse now =
      liftEither
      . mapLeft CouldNotDecodeTokenResponse
      $ flip eitherDecodeWith' tokenResponse $
          withObject "Token response" $ \v -> do
            -- Tokens expire in 60 seconds if the expiration time is not
            -- specified.
            expiresIn <- fmap (secondsToNominalDiffTime . fromMaybe 60)
                            (v .:? "expires_in")
            Token (addUTCTime expiresIn now) <$> (v .: "access_token")

data Error
  = InvalidTokenEndpointUri SomeException
  | TokenRetrievalFailed
  | CouldNotDecodeTokenResponse String
  | ExceptionOccurredDuringHttpRequest Http.HttpException
  | ExceptionOccurredWhileReadingResponse Http.HttpException
  | CouldNotDecodeRealmAsText
  | CouldNotParseRealmAsTokenEndpoint

convertExceptionToError :: (MonadError error m)
                        => (exception -> error)
                        -> Either exception a
                        -> m a
convertExceptionToError f (Left e) = throwError $ f e
convertExceptionToError _ (Right x) = return x
