module Paths_curve (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jan/.cabal/bin"
libdir     = "/home/jan/.cabal/lib/curve-0.1/ghc-7.6.3"
datadir    = "/home/jan/.cabal/share/curve-0.1"
libexecdir = "/home/jan/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "curve_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "curve_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "curve_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "curve_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
