{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main ( main ) where

import Control.Monad ( unless )
import Data.Either ( isRight, fromLeft )
import Data.Maybe ( fromJust )
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Stack ( HasCallStack )
import NeatInterpolation
import Test.HUnit

import Docker
import Docker.Fetch ( parseChallenge )
import Docker.Fetch.Parsing as Fetch

main :: IO ()
main = runTestTTAndExit tests

tests =
  TestList
  [ TestLabel "parseImageName" test_parseImageName
  , TestLabel "parseImageName localhost" test_parseImageNameLocalhost
  , TestLabel "parseImageName localhost with port" test_parseImageNameLocalhostWithPort
  , TestLabel "parseImageName no tag or digest" test_parseImageNameNoTag
  , TestLabel "parseImageName no registry" test_parseImageNoRegistry
  , TestLabel "parseImageManifestListV2" test_parseManifestListV2
  , TestLabel "parseV2Manifest" test_parseV2Manifest
  , TestLabel "parseImageV1" test_parseImageV1
  , TestLabel "parseChallenge" test_parseChallenge
  ]

test_parseImageName = TestCase $ do
  let actual = fromJust $ parseImageName "mcr.microsoft.com/dotnet/framework/runtime:4.8"

  assertEqual "registry hostname" "mcr.microsoft.com" $ getRegistryHostname actual
  assertEqual "registry port" "" $ getRegistryPort actual
  assertEqual "repository name" "dotnet/framework/runtime" $ getRepositoryName actual
  assertEqual "tag" (Just "4.8") $ getTagOrDigest actual

test_parseImageNameLocalhost = TestCase $ do
  let actual = fromJust $ parseImageName "localhost/some/repo:1.2"

  assertEqual "registry hostname" "localhost" $ getRegistryHostname actual
  assertEqual "registry port" "" $ getRegistryPort actual
  assertEqual "repository name" "some/repo" $ getRepositoryName actual
  assertEqual "tag" (Just "1.2") $ getTagOrDigest actual

test_parseImageNameLocalhostWithPort = TestCase $ do
  let actual = fromJust $ parseImageName "localhost:80/some/repo:1.2"

  assertEqual "registry hostname" "localhost" $ getRegistryHostname actual
  assertEqual "registry port" "80" $ getRegistryPort actual
  assertEqual "repository name" "some/repo" $ getRepositoryName actual
  assertEqual "tag" (Just "1.2") $ getTagOrDigest actual

test_parseImageNameNoTag = TestCase $ do
  let actual = fromJust $ parseImageName "localhost:80/some/repo"

  assertEqual "registry hostname" "localhost" $ getRegistryHostname actual
  assertEqual "registry port" "80" $ getRegistryPort actual
  assertEqual "repository name" "some/repo" $ getRepositoryName actual
  assertEqual "tag" Nothing $ getTagOrDigest actual

test_parseImageNoRegistry = TestCase $ do
  let actual = fromJust $ parseImageName "reponame"

  assertEqual "registry hostname" "registry-1.docker.io" $ getRegistryHostname actual
  assertEqual "registry port" "" $ getRegistryPort actual
  assertEqual "repository name" "reponame" $ getRepositoryName actual
  assertEqual "tag/digest" Nothing $ getTagOrDigest actual


-- Testing registry files

test_parseManifestListV2 = TestCase $ do
  let actual = Fetch.parseManifestListV2 bs

  assertRight "Parsing failed:" actual
  where
    bs = TL.encodeUtf8 . TL.fromStrict $ [trimming|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.docker.distribution.manifest.list.v2+json",
  "manifests": [
    {
       "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
       "size": 1524,
       "digest": "sha256:9582e7a50284e118e6aa139e2a84275e7fee09e995c4e98d17784db2b0d179e4",
       "platform": {
          "architecture": "amd64",
          "os": "windows",
          "os.version": "10.0.14393.4889"
       }
    },
    {
       "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
       "size": 1523,
       "digest": "sha256:7ef94f677d6a5d3c075c8206cdea1452a72e95b4fb9c0bd4f4d51df7a6ba166a",
       "platform": {
          "architecture": "amd64",
          "os": "windows",
          "os.version": "10.0.17763.2458"
       }
    },
    {
       "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
       "size": 1523,
       "digest": "sha256:8e2c11b1fe75bae40f7b9b25e7db1427bac487f4f3d6e30c3987da514ae81065",
       "platform": {
          "architecture": "amd64",
          "os": "windows",
          "os.version": "10.0.19042.1469"
       }
    },
    {
       "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
       "size": 1523,
       "digest": "sha256:5ec4722ef299457d69a7186d3da503a947584aa1a5bfc0bfc021fe115dc2a76b",
       "platform": {
          "architecture": "amd64",
          "os": "windows",
          "os.version": "10.0.20348.473"
       }
    }
  ]
}
|]

test_parseV2Manifest = TestCase $ do
  let actual = Fetch.parseV2Manifest bs

  assertRight "Parsing failed:" actual
  where
    bs = TL.encodeUtf8 . TL.fromStrict $ [trimming|
{
   "schemaVersion": 2,
   "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
   "config": {
      "mediaType": "application/vnd.docker.container.image.v1+json",
      "size": 3446,
      "digest": "sha256:85df02b6f81af17edcf27e6f9e6d771212856d49de6780ef7417d28842670d24"
   },
   "layers": [
      {
         "mediaType": "application/vnd.docker.image.rootfs.foreign.diff.tar.gzip",
         "size": 1251699055,
         "digest": "sha256:8f616e6e9eec767c425fd9346648807d1b658d20ff6097be1d955aac69c26642",
         "urls": [
            "https://mcr.microsoft.com/v2/windows/servercore/blobs/sha256:8f616e6e9eec767c425fd9346648807d1b658d20ff6097be1d955aac69c26642"
         ]
      },
      {
         "mediaType": "application/vnd.docker.image.rootfs.foreign.diff.tar.gzip",
         "size": 955800778,
         "digest": "sha256:0e02c12b1310e6c76c29fcd6f81905400fdb6a01caac9dc825939ad004baffef",
         "urls": [
            "https://mcr.microsoft.com/v2/windows/servercore/blobs/sha256:0e02c12b1310e6c76c29fcd6f81905400fdb6a01caac9dc825939ad004baffef"
         ]
      },
      {
         "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
         "size": 1278,
         "digest": "sha256:f94ef47a84f46e8e28326512d80fdbf10db580baf7720e59529919174cd708d9"
      },
      {
         "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
         "size": 223268696,
         "digest": "sha256:365aab1b67776659055cf1f123d64b106edeaea9d524fe21f58f03799252c1ad"
      }
   ]
}
|]

test_parseImageV1 = TestCase $ do
  let actual = Fetch.parseImageV1 bs

  assertRight "Parsing failed:" actual
  where
    bs = TL.encodeUtf8 . TL.fromStrict $ [trimming|
{
  "architecture":"amd64",
  "config":
    {
      "Hostname":"",
      "Domainname":"",
      "User":"",
      "AttachStdin":false,
      "AttachStdout":false,
      "AttachStderr":false,
      "Tty":false,
      "OpenStdin":false,
      "StdinOnce":false,
      "Env":
        [
          "DOTNET_RUNNING_IN_CONTAINER=true",
          "COMPLUS_RUNNING_IN_CONTAINER=1",
          "COMPLUS_NGenProtectedProcess_FeatureEnabled=0"
        ],
      "Cmd":
        [
          "c:\\windows\\system32\\cmd.exe"
        ],
      "Image":"sha256:dd5606bf3f88dfa87433b70e6171d2bd670b3c025031a59d685d1f278092e2d7",
      "Volumes":null,
      "WorkingDir":"",
      "Entrypoint":null,
      "OnBuild":null,
      "Labels":null
    },
  "container":"b792aa668c5287bdee0358b15e065a1bb68fa4c6818c17eb26b4139365097904",
  "container_config":
    {
      "Hostname":"",
      "Domainname":"",
      "User":"",
      "AttachStdin":false,
      "AttachStdout":false,
      "AttachStderr":false,
      "Tty":false,
      "OpenStdin":false,
      "StdinOnce":false,
      "Env":
        [
          "DOTNET_RUNNING_IN_CONTAINER=true",
          "COMPLUS_RUNNING_IN_CONTAINER=1",
          "COMPLUS_NGenProtectedProcess_FeatureEnabled=0"
        ],
      "Cmd":
        [
          "cmd /S /C curl -fSLo patch.msu https://download.microsoft.com/download/5/a/4/5a4425d3-eb10-47a8-afe0-b07a19b20b1c/Windows10.0-KB9008395-x64-NDP48.msu     \u0026\u0026 mkdir patch     \u0026\u0026 expand patch.msu patch -F:*     \u0026\u0026 del /F /Q patch.msu     \u0026\u0026 dism /Online /Quiet /Add-Package /PackagePath:C:\\patch\\windows10.0-kb9008395-x64-ndp48.cab     \u0026\u0026 rmdir /S /Q patch         \u0026\u0026 %windir%\\Microsoft.NET\\Framework64\\v4.0.30319\\ngen install \"Microsoft.PowerShell.Utility.Activities, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\"     \u0026\u0026 %windir%\\Microsoft.NET\\Framework64\\v4.0.30319\\ngen update     \u0026\u0026 %windir%\\Microsoft.NET\\Framework\\v4.0.30319\\ngen update"
        ],
      "Image":"sha256:dd5606bf3f88dfa87433b70e6171d2bd670b3c025031a59d685d1f278092e2d7",
      "Volumes":null,
      "WorkingDir":"",
      "Entrypoint":null,
      "OnBuild":null,
      "Labels":null
    },
  "created":"2022-01-20T00:16:34.8596475Z",
  "docker_version":"20.10.7",
  "history":
    [
      {
        "created":"2021-05-08T09:40:24.683Z",
        "created_by":"Apply image 2022-RTM-amd64"
      },
      {
        "created":"2022-01-16T05:17:24.6000452Z",
        "created_by":"Install update ltsc2022-amd64"
      },
      {
        "created":"2022-01-20T00:10:21.5924412Z",
        "created_by":"cmd /S /C #(nop)  ENV DOTNET_RUNNING_IN_CONTAINER=true COMPLUS_RUNNING_IN_CONTAINER=1 COMPLUS_NGenProtectedProcess_FeatureEnabled=0"},{"created":"2022-01-20T00:16:34.8596475Z","created_by":"cmd /S /C curl -fSLo patch.msu https://download.microsoft.com/download/5/a/4/5a4425d3-eb10-47a8-afe0-b07a19b20b1c/Windows10.0-KB9008395-x64-NDP48.msu     \u0026\u0026 mkdir patch     \u0026\u0026 expand patch.msu patch -F:*     \u0026\u0026 del /F /Q patch.msu     \u0026\u0026 dism /Online /Quiet /Add-Package /PackagePath:C:\\patch\\windows10.0-kb9008395-x64-ndp48.cab     \u0026\u0026 rmdir /S /Q patch         \u0026\u0026 %windir%\\Microsoft.NET\\Framework64\\v4.0.30319\\ngen install \"Microsoft.PowerShell.Utility.Activities, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\"     \u0026\u0026 %windir%\\Microsoft.NET\\Framework64\\v4.0.30319\\ngen update     \u0026\u0026 %windir%\\Microsoft.NET\\Framework\\v4.0.30319\\ngen update"
      }
    ],
  "os":"windows",
  "os.version":"10.0.20348.473",
  "rootfs":
    {
      "type":"layers",
      "diff_ids":
        [
          "sha256:c18686406f96b263d986714a2d88add06ea9ddbb3e52fb936222711da7d9d395",
          "sha256:ab796afc7ed7e8762e69f652a7b4a524915e8737908d20a080273f8be7258ca7",
          "sha256:049a461e786e684452fbfb9f67950e795f32e4ff8dc406421b27d8aa3c5800b0",
          "sha256:ac90005e5277ac1120a673c1edceff300b792513e1666013f3a2cacf696b992c"
        ]
    }
}
|]

test_parseChallenge = TestCase $ do
  let actual = parseChallenge bs

  assertRight "Parsing failed:" actual

  let Right (realm, service, scope) = actual

  assertEqual "Realm did not match" "https://auth.docker.io/token" realm
  assertEqual "Service did not match" "registry.docker.io" service
  assertEqual "Scope did not match" ["repository:ubuntu:pull"] scope
  where
    bs = "Bearer realm=\"https://auth.docker.io/token\",service=\"registry.docker.io\",scope=\"repository:ubuntu:pull\""

assertRight :: String -> Either String a -> Assertion
assertRight s x = unless (isRight x) $ assertFailure (s ++ " " ++ fromLeft "" x)