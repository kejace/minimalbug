module Paths_minimalbug (
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

bindir     = "/Users/salmaancraig/.cabal/bin"
libdir     = "/Users/salmaancraig/.cabal/lib/x86_64-osx-ghc-7.10.2/minimalbug-0.1.0.0-E0xQJzCPYSD8GCImmvGIKy"
datadir    = "/Users/salmaancraig/.cabal/share/x86_64-osx-ghc-7.10.2/minimalbug-0.1.0.0"
libexecdir = "/Users/salmaancraig/.cabal/libexec"
sysconfdir = "/Users/salmaancraig/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minimalbug_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minimalbug_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "minimalbug_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minimalbug_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minimalbug_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
