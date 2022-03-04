module Main where

import Network.Wai.Handler.Warp ( run )
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Tls
import System.Environment ( getArgs )

import App ( app )

main :: IO ()
main = do
  [portStr] <- getArgs
  let port = read portStr
  manager <- Http.newManager Tls.tlsManagerSettings
  run port (app manager)
