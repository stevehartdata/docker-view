module Docker.Repository
  ( Repository (RepositoryName)
  , getRepositoryName
  )
  where

data Repository =
  RepositoryName { getRepositoryName :: String }
  deriving (Eq, Show)
