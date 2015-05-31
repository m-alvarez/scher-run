module Klee where

import System.Exit
import System.Process
import Text.Printf
import Control.Monad.Writer

data KleeFlags = KF
               { libc :: Maybe String
               , emitAllErrors :: Bool
               }

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a

toFlags :: FilePath -> KleeFlags -> [String]
toFlags input flags = snd $ runWriter $ do
  whenJust (libc flags) $ \c -> tell ["--libc=" ++ c]
  when (emitAllErrors flags) (tell ["--emit-all-errors"])
  tell [input]

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

runKlee :: KleeFlags -> FilePath -> IO ExitCode
runKlee flags filename = do
  (exitCode, out, err) <- readProcessWithExitCode "klee" (toFlags filename flags) ""
  report exitCode filename out err
