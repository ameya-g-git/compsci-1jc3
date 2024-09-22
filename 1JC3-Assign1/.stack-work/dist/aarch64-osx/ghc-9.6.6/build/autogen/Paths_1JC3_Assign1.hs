{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_1JC3_Assign1 (
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
bindir     = "/Users/ameyagupta/Desktop/repos/compsci-1jc3/1JC3-Assign1/.stack-work/install/aarch64-osx/b48f9f8f47818380b092a2a1809561be7a06924a4eef67cc3e0d1b3d581436df/9.6.6/bin"
libdir     = "/Users/ameyagupta/Desktop/repos/compsci-1jc3/1JC3-Assign1/.stack-work/install/aarch64-osx/b48f9f8f47818380b092a2a1809561be7a06924a4eef67cc3e0d1b3d581436df/9.6.6/lib/aarch64-osx-ghc-9.6.6/1JC3-Assign1-0.1.0.0-FAJVOR3NOepDbKEM6dNAB"
dynlibdir  = "/Users/ameyagupta/Desktop/repos/compsci-1jc3/1JC3-Assign1/.stack-work/install/aarch64-osx/b48f9f8f47818380b092a2a1809561be7a06924a4eef67cc3e0d1b3d581436df/9.6.6/lib/aarch64-osx-ghc-9.6.6"
datadir    = "/Users/ameyagupta/Desktop/repos/compsci-1jc3/1JC3-Assign1/.stack-work/install/aarch64-osx/b48f9f8f47818380b092a2a1809561be7a06924a4eef67cc3e0d1b3d581436df/9.6.6/share/aarch64-osx-ghc-9.6.6/1JC3-Assign1-0.1.0.0"
libexecdir = "/Users/ameyagupta/Desktop/repos/compsci-1jc3/1JC3-Assign1/.stack-work/install/aarch64-osx/b48f9f8f47818380b092a2a1809561be7a06924a4eef67cc3e0d1b3d581436df/9.6.6/libexec/aarch64-osx-ghc-9.6.6/1JC3-Assign1-0.1.0.0"
sysconfdir = "/Users/ameyagupta/Desktop/repos/compsci-1jc3/1JC3-Assign1/.stack-work/install/aarch64-osx/b48f9f8f47818380b092a2a1809561be7a06924a4eef67cc3e0d1b3d581436df/9.6.6/etc"

getBinDir     = catchIO (getEnv "1JC3_Assign1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "1JC3_Assign1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "1JC3_Assign1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "1JC3_Assign1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "1JC3_Assign1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "1JC3_Assign1_sysconfdir") (\_ -> return sysconfdir)



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
