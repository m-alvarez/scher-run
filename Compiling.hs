module Compiling where

import Control.Monad.Writer
import System.Process
import System.Exit
import Text.Printf
import Data.List

data GC = JGC | Stub

data CompilerFlags = CF
                   { tdir :: Maybe String
                   , cCompiler :: String
                   , extraCFlags :: [String]
                   , includes :: [String]
                   , haskellCompiler :: String
                   , extraHaskellFlags :: [String]
                   , toC :: Bool
                   , preprocessor :: [(String, Maybe String)]
                   , gc :: GC }

preprocessorDirectives :: [(String, Maybe String)] -> [String]
preprocessorDirectives =
  map $ \pair ->
    case pair of
      (flag, Nothing) -> "-D"++flag
      (flag, Just val) -> "-D"++flag++"="++val

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a

compilerFlagsToHaskell :: FilePath -> Maybe FilePath -> CompilerFlags -> [String]
compilerFlagsToHaskell input output flags = snd $ runWriter $ do
  case gc flags of
    JGC -> return ()
    Stub -> tell ["-fjgc-stub"]
  tell $ extraHaskellFlags flags
  whenJust (tdir flags) (\dir -> tell ["--tdir="++dir])
  tell $ preprocessorDirectives (preprocessor flags)
  when (toC flags) (tell ["-C"])
  whenJust output (\filename -> tell ["-o", filename])
  tell [input]

compilerFlagsToC :: [FilePath] -> Maybe FilePath -> CompilerFlags -> [String]
compilerFlagsToC inputs output flags = snd $ runWriter $ do
  case gc flags of
    JGC -> tell ["-D_JHC_GC=_JHC_GC_JGC"]
    Stub -> tell ["-D_JHC_GC=_JHC_GC_JGC", "-D_JHC_GC_JGC_STUB"]
  tell $ extraCFlags flags
  whenJust output (\filename -> tell ["-o", filename])
  tell inputs
  

runHaskellCompiler :: FilePath -> Maybe FilePath -> CompilerFlags->IO (ExitCode,String,String)
runHaskellCompiler input output flags = do
  printf "Command: %s %s\n" compiler (unwords haskellFlags)
  readProcessWithExitCode compiler haskellFlags ""
    where compiler     = haskellCompiler flags
          haskellFlags = compilerFlagsToHaskell input output flags

runCCompiler :: [FilePath] -> Maybe FilePath -> CompilerFlags -> IO (ExitCode, String, String)
runCCompiler inputs output flags = do
  printf "Command: %s %s\n" compiler (unwords cFlags)
  readProcessWithExitCode compiler cFlags ""
    where compiler = cCompiler flags
          cFlags   = compilerFlagsToC inputs output flags

compileHs :: FilePath -> Maybe FilePath -> CompilerFlags -> IO ExitCode
compileHs input output flags = do
  (exitCode, out, err) <- runHaskellCompiler input output flags
  case exitCode of
    ExitFailure i -> do
      printf "Error compiling file %s\n" input
      printf "Error code %d\n" i
      printf "STDOUT:\n%s\n" out
      printf "STDERR:\n%s\n" err
    ExitSuccess ->
      return ()
  return exitCode

compileC :: [FilePath] -> Maybe FilePath -> CompilerFlags -> IO ExitCode
compileC inputs output flags = do
  (exitCode, out, err) <- runCCompiler inputs output flags
  case exitCode of
    ExitFailure i -> do
      printf "Error compiling inputs %s\n" (show inputs)
      printf "Error code %d\n" i
      printf "STDOUT:\n%s\n" out
      printf "STDERR:\n%s\n" err
    ExitSuccess ->
      return ()
  return exitCode

