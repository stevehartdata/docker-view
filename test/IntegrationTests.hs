module Main (main)
where

import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( ReaderT, ask, runReaderT )
import Data.Maybe ( fromJust )
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Tls

import Docker

main = do
  manager <- newManager

  getImageManifest manager
    $ fromJust $ parseImageName "mcr.microsoft.com/dotnet/framework/runtime:4.8"

newManager :: IO Http.Manager
newManager = Http.newManager Tls.tlsManagerSettings
