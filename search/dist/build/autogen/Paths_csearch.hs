{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_csearch (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Crembo\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Crembo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\csearch-0.1.0.0-33AFrVhsmAj775Xe40LiB9"
datadir    = "C:\\Users\\Crembo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\csearch-0.1.0.0"
libexecdir = "C:\\Users\\Crembo\\AppData\\Roaming\\cabal\\csearch-0.1.0.0-33AFrVhsmAj775Xe40LiB9"
sysconfdir = "C:\\Users\\Crembo\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "csearch_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "csearch_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "csearch_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "csearch_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "csearch_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
