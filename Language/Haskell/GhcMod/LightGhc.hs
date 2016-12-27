{-# LANGUAGE CPP #-}

module Language.Haskell.GhcMod.LightGhc where

import Control.Monad
import Control.Monad.Reader (runReaderT)
import Data.IORef

import GHC
import GHC.Paths (libdir)
#ifdef WITH_GHCJS
import Data.Char (isSpace)
import GHCJS
import System.Process (readProcess)
#endif
import StaticFlags
import SysTools
import DynFlags
import HscMain
import HscTypes

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.PathsAndFiles
import qualified Language.Haskell.GhcMod.Gap as Gap

-- We have to be more careful about tearing down 'HscEnv's since GHC 8 added an
-- out of process GHCI server which has to be shutdown.
newLightEnv :: IOish m => FilePath -> (DynFlags -> LightGhc DynFlags) -> m HscEnv
newLightEnv distDir mdf = do
  df <- liftIO $ do
     initStaticOpts
#ifndef WITH_GHCJS
     settings <- initSysTools (Just libdir)
#else
     settings <- initSysTools =<< getLibDir distDir
#endif
     initDynFlags $ defaultDynFlags settings

  hsc_env <- liftIO $ newHscEnv df
  df' <- runLightGhc hsc_env $ mdf df
  return $ hsc_env {
      hsc_dflags = df',
      hsc_IC = (hsc_IC hsc_env) { ic_dflags = df' }
    }

teardownLightEnv :: MonadIO m => HscEnv -> m ()
teardownLightEnv env = runLightGhc env $ do
  Gap.withCleanupSession $ return ()

withLightHscEnv'
    :: IOish m => FilePath -> (DynFlags -> LightGhc DynFlags) -> (HscEnv -> m a) -> m a
withLightHscEnv' distDir mdf action = gbracket (newLightEnv distDir mdf) teardownLightEnv action

withLightHscEnv :: IOish m => FilePath -> [GHCOption] -> (HscEnv -> m a) -> m a
withLightHscEnv distDir opts = withLightHscEnv' distDir (f <=< liftIO . newHscEnv)
 where
   f env = runLightGhc env $ do
         -- HomeModuleGraph and probably all other clients get into all sorts of
         -- trouble if the package state isn't initialized here
         _ <- setSessionDynFlags =<< addCmdOpts opts =<< getSessionDynFlags
         getSessionDynFlags

runLightGhc :: MonadIO m => HscEnv -> LightGhc a -> m a
runLightGhc env action = liftIO $ do
  renv <- newIORef env
  flip runReaderT renv $ unLightGhc action

runLightGhc' :: MonadIO m => IORef HscEnv -> LightGhc a -> m a
runLightGhc' renv action = liftIO $ do
  flip runReaderT renv $ unLightGhc action

#ifdef WITH_GHCJS
getLibDir :: FilePath -> IO (Maybe FilePath)
getLibDir distDir = do
    isGhcjs <- isGhcjsConfig distDir
    let trim = let f = reverse . dropWhile isSpace in f . f
    if isGhcjs
    then Just . trim <$> readProcess "ghcjs" ["--print-libdir"] ""
    else return (Just libdir)
#endif
