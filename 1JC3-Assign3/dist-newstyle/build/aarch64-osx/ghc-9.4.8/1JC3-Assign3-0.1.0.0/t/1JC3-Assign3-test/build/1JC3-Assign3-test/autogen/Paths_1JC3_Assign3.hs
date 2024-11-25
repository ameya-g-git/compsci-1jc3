{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_1JC3_Assign3 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/ameyagupta/.cabal/bin"
libdir     = "/Users/ameyagupta/.cabal/lib/aarch64-osx-ghc-9.4.8/1JC3-Assign3-0.1.0.0-inplace-1JC3-Assign3-test"
dynlibdir  = "/Users/ameyagupta/.cabal/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/ameyagupta/.cabal/share/aarch64-osx-ghc-9.4.8/1JC3-Assign3-0.1.0.0"
libexecdir = "/Users/ameyagupta/.cabal/libexec/aarch64-osx-ghc-9.4.8/1JC3-Assign3-0.1.0.0"
sysconfdir = "/Users/ameyagupta/.cabal/etc"

getBinDir     = catchIO (getEnv "1JC3_Assign3_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "1JC3_Assign3_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "1JC3_Assign3_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "1JC3_Assign3_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "1JC3_Assign3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "1JC3_Assign3_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'