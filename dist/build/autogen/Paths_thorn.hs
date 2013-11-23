module Paths_thorn (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\yusuke\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\yusuke\\AppData\\Roaming\\cabal\\thorn-0.1.1\\ghc-7.6.3"
datadir    = "C:\\Users\\yusuke\\AppData\\Roaming\\cabal\\thorn-0.1.1"
libexecdir = "C:\\Users\\yusuke\\AppData\\Roaming\\cabal\\thorn-0.1.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "thorn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "thorn_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "thorn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "thorn_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
