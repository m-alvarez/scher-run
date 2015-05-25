module Main (main) where

import System.Environment
import System.Exit
import Control.Monad
import Data.List
import Data.Functor
import Data.Maybe
import Text.Printf

import Program
import Compiling
import Klee

data Run = Verify String [String]
         | Help

defaultFlags =
  CF { tdir = Just "tdir"
     , cCompiler = "llvm-gcc"
     , haskellCompiler = "jhc"
     , toC = True
     , preprocessor = []
     , includes = ["tdir/cbits", "tdir", "/home/user/klee/include"]
     , extraCFlags = [ "-std=gnu99"
                     , "-falign-functions=4"
                     , "-ffast-math"
                     , "-fno-strict-aliasing"
                     , "-DNDEBUG"
                     , "-c"
                     , "-emit-llvm" ]
     , extraHaskellFlags = [ "-fffi"
                           , "-fglobal-optimize"
                           , "-fcpp" ]
     , gc = Stub }

cFiles gc =
       [ "tdir/main_code.c"
       , "tdir/lib/lib_cbits.c"
       , "tdir/rts/rts_support.c"
       , "tdir/rts/profile.c"
       , "tdir/rts/jhc_rts.c"
       ] ++ case gc of
              Stub -> [ "tdir/rts/gc_jgc_stub.c" ]
              JGC ->  [ "tdir/rts/gc_jgc.c", "tdir/rts/stableptr.c" ]

verify :: String -> [String] -> IO ()
verify moduleName [] = printf "No test functions specified.\n"
verify moduleName tests = forM_ tests $ \testName -> do
  testFile <- writeTestFile moduleName testName 
  ExitSuccess <- compileHs testModuleFileName Nothing defaultFlags
  printf "Running test %s of module %s\n" testName moduleName
  ExitSuccess <- compileC (cFiles $ gc defaultFlags) (Just "bytecode.bc") defaultFlags
  printf "Done compiling!\n"
  ExitSuccess <- runKlee "bytecode.bc"
  printf "Done verifying!\n"

parseFlags [] = []
parseFlags ((flag@('-':_)):rest) =
  (flag, flagArgs) : parseFlags restArgs
  where (flagArgs, restArgs) = break ((== '-') . head) rest

parseArgs args = 
  if "-help" `elem` args || null args
    then Help
    else Verify filename arguments
      where filename = last args
            arguments = [] `fromMaybe` lookup "-test" (parseFlags $ init args)

printHelp = do
  printf "Usage: scher-run [OPTIONS]... MODULE\n"
  printf "Options:\n"
  printf "\t-test NAME\tadds NAME to the list of test functions\n"
  printf "\t-help\t\tprints this message and exits\n"

main :: IO ()
main = do
  invocation <- parseArgs <$> getArgs
  case invocation of
    Help -> printHelp
    Verify moduleName testFunctions -> verify moduleName testFunctions
