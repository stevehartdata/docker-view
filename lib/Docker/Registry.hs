module Docker.Registry
  ( Registry (DefaultRegistry, LocalhostRegistry, HostnameRegistry)
  , getRegistryHostname, getRegistryPort, getRegistryProtocol, getRegistryUrl
  )
  where

import Network.URI ( URI (URI), URIAuth (URIAuth) )

data Registry
  = DefaultRegistry
  | LocalhostRegistry
  | HostnameRegistry
      String -- ^ Hostname
      String -- ^ Port
  deriving (Eq, Show)

getRegistryHostname :: Registry -> String
getRegistryHostname DefaultRegistry = "registry-1.docker.io"
getRegistryHostname LocalhostRegistry = "localhost"
getRegistryHostname (HostnameRegistry hostname _port) = hostname

getRegistryPort :: Registry -> String
getRegistryPort (HostnameRegistry _hostname port) = port
getRegistryPort _ = ""

-- For now, we only support HTTPS, except for localhost
getRegistryProtocol :: Registry -> String
getRegistryProtocol LocalhostRegistry = "http:"
getRegistryProtocol _ = "https:"

getRegistryUrl :: Registry -> URI
getRegistryUrl reg =
  URI (getRegistryProtocol reg)
      (Just $ URIAuth "" (getRegistryHostname reg) (getRegistryPort reg))
      "" "" ""
