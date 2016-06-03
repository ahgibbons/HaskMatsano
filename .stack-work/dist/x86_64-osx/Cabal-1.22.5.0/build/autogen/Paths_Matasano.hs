module Paths_Matasano (
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

bindir     = "/Users/Andrew/Dropbox/Haskell/Matasano/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/bin"
libdir     = "/Users/Andrew/Dropbox/Haskell/Matasano/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/Matasano-0.1.0.0-5owQ4W1svNRDGggvsHH52l"
datadir    = "/Users/Andrew/Dropbox/Haskell/Matasano/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/share/x86_64-osx-ghc-7.10.3/Matasano-0.1.0.0"
libexecdir = "/Users/Andrew/Dropbox/Haskell/Matasano/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/libexec"
sysconfdir = "/Users/Andrew/Dropbox/Haskell/Matasano/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Matasano_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Matasano_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Matasano_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Matasano_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Matasano_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
