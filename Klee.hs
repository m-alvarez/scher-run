module Klee where

import System.Exit
import System.Process
import Text.Printf

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

runKlee :: FilePath -> IO ExitCode
runKlee filename = do
  (exitCode, out, err) <- readProcessWithExitCode "klee" ["--libc=uclibc", filename] ""
  report exitCode filename out err
