module Paths_fun (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,4], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tom/.cabal/bin"
libdir     = "/home/tom/.cabal/lib/fun-0.0.4/ghc-7.4.1"
datadir    = "/home/tom/.cabal/share/fun-0.0.4"
libexecdir = "/home/tom/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "fun_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fun_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "fun_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fun_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
