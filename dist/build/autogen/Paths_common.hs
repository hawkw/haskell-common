module Paths_common (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/hawk/.cabal/bin"
libdir     = "/Users/hawk/.cabal/lib/x86_64-osx-ghc-7.10.2/common-0.1.0.0-GRlyKnHcABR55OwM0G5Liq"
datadir    = "/Users/hawk/.cabal/share/x86_64-osx-ghc-7.10.2/common-0.1.0.0"
libexecdir = "/Users/hawk/.cabal/libexec"
sysconfdir = "/Users/hawk/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "common_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "common_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "common_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "common_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "common_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
