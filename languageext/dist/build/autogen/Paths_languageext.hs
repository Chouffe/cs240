module Paths_languageext (
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
libdir     = "/home/chouffe/.cabal/lib/x86_64-linux-ghc-7.10.3/languageext-0.1.0.0-2reHyqsz5AQ0E8V8tEzx3M"
datadir    = "/home/chouffe/.cabal/share/x86_64-linux-ghc-7.10.3/languageext-0.1.0.0"
libexecdir = "/home/chouffe/.cabal/libexec"
sysconfdir = "/home/chouffe/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "languageext_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "languageext_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "languageext_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "languageext_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "languageext_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
