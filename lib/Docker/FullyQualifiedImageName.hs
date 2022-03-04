module Docker.FullyQualifiedImageName
  ( FullyQualifiedImageName (FullyQualifiedImageName)
  , getRegistryHostname, getRegistryPort, getRepositoryName, getTagOrDigest

  , TagOrDigest (TagOrDigest)
  , parseImageName
  )
  where

import Prelude hiding ( rem )

import Docker.Registry
  ( Registry (DefaultRegistry, LocalhostRegistry, HostnameRegistry) )
import qualified Docker.Registry as Registry
import Docker.Repository ( Repository (RepositoryName) )
import qualified Docker.Repository as Repo
data FullyQualifiedImageName =
  FullyQualifiedImageName Registry Repository (Maybe TagOrDigest)
  deriving (Eq, Show)

data TagOrDigest
  = TagOrDigest String
  deriving (Eq, Show)

getRegistryHostname :: FullyQualifiedImageName -> String
getRegistryHostname (FullyQualifiedImageName registry _ _) =
  Registry.getRegistryHostname registry

getRegistryPort :: FullyQualifiedImageName -> String
getRegistryPort (FullyQualifiedImageName registry _ _) =
  Registry.getRegistryPort registry

getRepositoryName :: FullyQualifiedImageName -> String
getRepositoryName (FullyQualifiedImageName _ repo _) =
  Repo.getRepositoryName repo

getTagOrDigest :: FullyQualifiedImageName -> Maybe String
getTagOrDigest (FullyQualifiedImageName _ _ tagOrDigest) =
  fmap (\(TagOrDigest s) -> s) tagOrDigest

-- | Make a 'FullyQualifiedImageName' from a string like
-- `mcr.microsoft.com/dotnet/framework/runtime:4.8`. The format is
-- `registry-hostname/repository-name:tag-or-digest`.
parseImageName :: String -> Maybe FullyQualifiedImageName
parseImageName s = do
  (registry, repoName, tagOrDigest) <- parse s
  let repoName' =
        case registry of
          DefaultRegistry
            | '/' `elem` repoName -> repoName
            | otherwise -> "library/" ++ repoName
          _ -> repoName
  return $ FullyQualifiedImageName registry (RepositoryName repoName') tagOrDigest

  where
    -- TODO: These parsing functions can use stricter validation

    parse = parseLocalhost

    parseLocalhost = parseString handleFail handleMatch "localhost/" []
      where
        handleMatch = parseRepoName LocalhostRegistry []
        handleFail = parseRepoName DefaultRegistry

    parseString _ handleMatch [] _ rem = handleMatch rem
    parseString handleFail handleMatch (t0:ts) acc s'@(x0:rem)
      | x0 == t0 = parseString handleFail handleMatch ts (t0:acc) rem
      | otherwise = handleFail acc s'
    parseString handleFail _handleMatch (_t0:_ts) acc s'@[] = handleFail acc s'

    parseRepoName reg acc [] =
      Just (reg, reverse $ acc, Nothing)
    parseRepoName DefaultRegistry acc s'@('.':_rem) = parseDomain acc s'
    parseRepoName _reg _acc ('.':_rem) = Nothing
    parseRepoName reg acc (':':rem) =
      parseTagOrDigest reg (reverse acc) [] rem
    parseRepoName reg acc (x0:rem) =
      parseRepoName reg (x0:acc) rem

    parseDomain _ [] = Nothing
    parseDomain acc ('/':xs) =
      parseRepoName (HostnameRegistry (reverse acc) "") [] xs
    parseDomain acc (':':xs) = parsePort (reverse acc) [] xs
    parseDomain acc (x:xs) = parseDomain (x:acc) xs

    parsePort _ _ [] = Nothing
    parsePort hostname acc ('/':xs) =
      parseRepoName (HostnameRegistry hostname (reverse acc)) [] xs
    parsePort hostname acc (x:xs) =
      parsePort hostname (x:acc) xs

    parseTagOrDigest hostname repoName acc [] =
      Just (hostname, repoName, Just. TagOrDigest $ reverse acc)
    parseTagOrDigest DefaultRegistry repoName acc s'@('/':_rem) =
      parsePort repoName [] (reverse acc ++ s')
    parseTagOrDigest hostname repoName acc (x0:rem) =
      parseTagOrDigest hostname repoName (x0:acc) rem
