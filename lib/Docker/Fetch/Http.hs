module Docker.Fetch.Http
  ( brReadBounded )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client ( BodyReader )

-- | Continuously call 'brRead', building up a lazy ByteString until a chunk is
-- constructed that is at least as many bytes as requested. Throws an exception
-- if there are remaining chunks in the response.
--
-- See also 'brReadSome'.

-- This implementation is based on brReadSome in http-client.
--
-- Note that the odd 'front' logic is used to make the function tail recursive.
-- Adding IO into the mix of the naive implementation results in an
-- implementation that is not tail recursive:
--
-- brReadBounded rem brRead' =
--     BSL.fromChunks <$> loop rem
--   where
--     loop rem'
--       | rem' <= 0 = do
--           bs <- brRead'
--           if BS.null bs
--               then return []
--               else throwIO OverlongResponseBodyException
--       | otherwise = do
--           bs <- brRead'
--           if BS.null bs
--               then return []
--               else fmap (bs:) $ loop (rem' - BS.length bs)
brReadBounded :: Int
                -- ^ Stop reading after this many bytes have been read
                -> BodyReader
                -> IO BSL.ByteString
brReadBounded rem brRead' =
    loop id rem
  where
    loop front rem'
      | rem' <= 0 = do
          bs <- brRead'
          if BS.null bs
              then produce front
              else error "Response was too long"
      | otherwise = do
          bs <- brRead'
          if BS.null bs
              then produce front
              else loop (front . (bs:)) (rem' - BS.length bs)

    produce front = return $ BSL.fromChunks $ front []