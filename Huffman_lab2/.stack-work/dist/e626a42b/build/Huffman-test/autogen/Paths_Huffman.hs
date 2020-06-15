{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Huffman (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Huffman\\.stack-work\\install\\6aeb83e8\\bin"
libdir     = "D:\\Huffman\\.stack-work\\install\\6aeb83e8\\lib\\x86_64-windows-ghc-8.6.5\\Huffman-0.1.0.0-GvOU6KEUWhj3oCVybPhxng-Huffman-test"
dynlibdir  = "D:\\Huffman\\.stack-work\\install\\6aeb83e8\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "D:\\Huffman\\.stack-work\\install\\6aeb83e8\\share\\x86_64-windows-ghc-8.6.5\\Huffman-0.1.0.0"
libexecdir = "D:\\Huffman\\.stack-work\\install\\6aeb83e8\\libexec\\x86_64-windows-ghc-8.6.5\\Huffman-0.1.0.0"
sysconfdir = "D:\\Huffman\\.stack-work\\install\\6aeb83e8\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Huffman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Huffman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Huffman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Huffman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Huffman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Huffman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
