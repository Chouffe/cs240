module Paths_memory (
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

bindir     = "/home/chouffe/.cabal/bin"
libdir     = "/home/chouffe/.cabal/lib/x86_64-linux-ghc-7.10.3/memory-0.1.0.0-4IiBq5JPJT4FUnohdEcQiT"
datadir    = "/home/chouffe/.cabal/share/x86_64-linux-ghc-7.10.3/memory-0.1.0.0"
libexecdir = "/home/chouffe/.cabal/libexec"
sysconfdir = "/home/chouffe/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "memory_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "memory_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "memory_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "memory_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "memory_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
