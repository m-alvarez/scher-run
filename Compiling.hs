module Compiling where

import Control.Monad.Writer
import System.Process
import System.Exit
import Text.Printf
import Data.List.Split

data GC = JGC | Stub

data TDir = NoTDir | TDirAt FilePath

data CompilerFlags = CF
                   { tdir              :: TDir
                   , entryPoint        :: Maybe String
                   , cCompiler         :: String
                   , extraCFlags       :: [String]
                   , includes          :: [String]
                   , hsCompiler        :: String
                   , ignoreCache       :: Bool
                   , debug             :: Bool
                   , extraHaskellFlags :: [String]
                   , toC               :: Bool
                   , cPreprocessor     :: [(PreprocessorFlag, Maybe String)]
                   , hsPreprocessor    :: Maybe [(PreprocessorFlag, Maybe String)]
                   , gc                :: GC }

type PreprocessorFlag = String

preprocessorFlags :: [(PreprocessorFlag, Maybe String)] -> [String]
preprocessorFlags =
  map $ \pair ->
    case pair of
      (flag, Nothing) -> "-D"++flag
      (flag, Just val) -> "-D"++flag++"="++val

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a

fileToModuleName :: String -> String
fileToModuleName = takeWhile (/= '.')

compilerFlagsToHaskell :: FilePath -> Maybe FilePath -> CompilerFlags -> [String]
compilerFlagsToHaskell input output flags = snd $ runWriter $ do
  case gc flags of
    JGC -> return ()
    Stub -> tell ["-fjgc-stub"]

  whenJust (entryPoint flags) $ \main -> 
    tell ["--main=" ++ fileToModuleName input ++ "." ++ main]

  when (ignoreCache flags) $ tell ["--ignore-cache"]

  whenJust (hsPreprocessor flags) $ \ppFlags -> do
    tell ["-fcpp"]
    tell $ preprocessorFlags ppFlags

  tell $ extraHaskellFlags flags

  case tdir flags of
    NoTDir -> return ()
    TDirAt dir -> tell ["--tdir=" ++ dir]
    --TDirRandom -> return () -- TODO implement this at some point

  when (toC flags) (tell ["-C"])

  whenJust output (\filename -> tell ["--output=" ++ filename])

  tell [input]

includeDirectives :: [String] -> [String]
includeDirectives = map ("-I" ++)

compilerFlagsToC :: [FilePath] -> Maybe FilePath -> CompilerFlags -> [String]
compilerFlagsToC inputs output flags = snd $ runWriter $ do

  case gc flags of
    JGC -> tell ["-D_JHC_GC=_JHC_GC_JGC"]
    Stub -> tell ["-D_JHC_GC=_JHC_GC_JGC", "-D_JHC_GC_JGC_STUB"]

  tell $ includeDirectives $ includes flags

  tell $ extraCFlags flags

  when (debug flags) $ tell ["-g"]

  tell $ preprocessorFlags $ cPreprocessor flags

  whenJust output (\filename -> tell ["-o " ++ filename])

  tell inputs

runHaskellCompiler :: FilePath -> Maybe FilePath -> CompilerFlags -> IO(ExitCode,String,String)
runHaskellCompiler input output flags = do
  printf "Command: %s %s\n" compiler (unwords haskellFlags)
  readProcessWithExitCode compiler haskellFlags ""
    where compiler     = hsCompiler flags
          haskellFlags = compilerFlagsToHaskell input output flags

runCCompiler :: [FilePath] -> Maybe FilePath -> CompilerFlags -> IO (ExitCode, String, String)
runCCompiler inputs output flags = do
  printf "Command: %s %s\n" compiler (unwords cFlags)
  readProcessWithExitCode compiler cFlags ""
    where compiler = cCompiler flags
          cFlags   = compilerFlagsToC inputs output flags

report :: ExitCode -> String -> String -> String -> IO ExitCode
report exitCode input out err = do
  case exitCode of
    ExitFailure i -> do
      printf "Error compiling inputs %s\n" input
      printf "Error code %d\n" i
      printf "STDOUT:\n%s\n" out
      printf "STDERR:\n%s\n" err
    ExitSuccess ->
      return ()
  return exitCode

compileHs :: FilePath -> Maybe FilePath -> CompilerFlags -> IO ExitCode
compileHs input output flags = do
  (exitCode, out, err) <- runHaskellCompiler input output flags
  report exitCode input out err

runLinker :: [FilePath] -> Maybe FilePath -> IO (ExitCode, String, String)
runLinker files output =
  readProcessWithExitCode "llvm-link" (files ++ out) ""
    where out = case output of
                  Nothing -> []
                  Just file -> ["-o=" ++ file]

replaceExtension :: String -> String
replaceExtension path = 
  takeWhile (/= '.') (last $ splitOn "/" path) ++ ".o"

compileC :: [FilePath] -> Maybe FilePath -> CompilerFlags -> IO ExitCode
compileC inputs output flags = do
  (exitCode, out, err) <- runCCompiler inputs Nothing flags
  _ <- report exitCode (show inputs) out err
  case exitCode of
    ExitFailure _ -> return exitCode
    ExitSuccess -> do
      (exitCode', out', err') <- runLinker (map replaceExtension inputs) output
      _ <- report exitCode' (show $ map replaceExtension inputs) out' err'
      _ <- system "rm -rf *.o"
      return exitCode'
