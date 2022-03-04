module Docker.ContainerConfig
  ( ContainerConfig (..)
  )
  where

import Data.Text ( Text )

data ContainerConfig = ContainerConfig {
  getHostName :: Maybe Text,
  getDomainName :: Maybe Text,
  getUser :: Maybe Text,
  getAttachStdin :: Maybe Bool,
  getAttachStdout :: Maybe Bool,
  getAttachStderr :: Maybe Bool,
  getExposedPorts :: Maybe [Text],
  getTty :: Maybe Bool,
  getOpenStdin :: Maybe Bool,
  getStdinOnce :: Maybe Bool,
  getEnvironmentVariables :: Maybe [Text],
  getCommand :: Maybe [Text],
  -- Don't support health checks yet.
  -- getHealthCheck :: Maybe HealthConfig,
  getArgsEscaped :: Maybe Text,
  getImage :: Maybe Text,
  getVolumes :: Maybe [Text],
  getWorkingDirectory :: Maybe Text,
  getEntrypoint :: Maybe [Text],
  getNetworkingDisabled :: Maybe Bool,
  getMacAddress :: Maybe Text,
  getOnBuild :: Maybe [Text],
  getLabels :: Maybe [(Text, Text)],
  getStopSignal :: Maybe Text,
  getStopTimeout :: Maybe Integer,
  getShell :: Maybe [Text]
}