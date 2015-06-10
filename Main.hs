module Main (main) where

import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Data.Functor
import Data.Maybe
import Text.Printf

import Program
import Compiling
import Klee
import qualified PrettyPrint as PP

data Run = Verify String [String]
         | Help
         | PrettyPrintReport FilePath

defaultCompilerFlags :: CompilerFlags
defaultCompilerFlags =
  CF { tdir = TDirAt "tdir"
     , entryPoint = Just "main"
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

defaultKleeFlags :: KleeFlags
defaultKleeFlags = KleeFlags
                 { libc = Just "uclibc"
                 , emitAllErrors = True
                 , outputDirectory = Just "klee-output"
                 } 

cFiles :: GC -> [FilePath]
cFiles garbageCollector =
       [ "tdir/main_code.c"
       , "tdir/lib/lib_cbits.c"
       , "tdir/rts/rts_support.c"
       , "tdir/rts/profile.c"
       , "tdir/rts/jhc_rts.c"
       ] ++ case garbageCollector of
              Stub -> [ "tdir/rts/gc_jgc_stub.c" ]
              JGC ->  [ "tdir/rts/gc_jgc.c", "tdir/rts/stableptr.c" ]

showReport :: KleeReport -> IO ()
showReport r = do
  printf "%s\n" $ show r
  when (not $ null $ testCases r) (printTestCases >> loop)
  where loop = do
          printf "Enter test case number: "
          i <- getLine
          case reads i of
            [(n, _)] -> do
              let filename = testCases r !! (n - 1)
              prettyPrintFromFile filename
            _ -> printf "Invalid number\n"
          loop
        printTestCases = do
          printf "Test cases:\n"
          forM_ (zip [1..] $ testCases r) $ \(i, err) -> do
            printf "Report %d: %s\n" (i :: Int) err


verify :: String -> [String] -> IO ()
verify _ [] = printf "No test functions specified.\n"
verify moduleName tests = forM_ tests $ \testName -> do
  testFile <- writeTestFile moduleName testName 
  ExitSuccess <- compileHs testFile Nothing defaultCompilerFlags
  printf "Running test %s of module %s\n" testName moduleName
  ExitSuccess <- compileC (cFiles $ gc defaultCompilerFlags) 
                          (Just "bytecode.bc") 
                          defaultCompilerFlags
  printf "Done compiling!\n"
  Just kleeReport <- runKlee defaultKleeFlags "bytecode.bc"
  printf "Done verifying!\n"
  showReport kleeReport

parseFlags :: [String] -> [(String, [String])]
parseFlags [] = []
parseFlags ((flag@('-':_)):rest) =
  (flag, flagArgs) : parseFlags restArgs
  where (flagArgs, restArgs) = break ((== '-') . head) rest
parseFlags args = error ("Unrecognized flags when parsing " ++ concat args)

parseArgs :: [String] -> Run
parseArgs [] = Help
parseArgs args | "-help" `elem` args = Help
               | "-pp" `elem` args = PrettyPrintReport file
               | otherwise = Verify filename arguments
                   where Just [file] = lookup "-pp" (parseFlags args)
                         filename = last args
                         arguments = [] `fromMaybe` lookup "-test" (parseFlags $ init args)

printHelp :: IO ()
printHelp = do
  printf "Usage: scher-run [OPTIONS]... MODULE\n"
  printf "Options:\n"
  printf "\t-test NAME\tadds NAME to the list of test functions\n"
  printf "\t-help\t\tprints this message and exits\n"
  printf "\t-pp FILE\t\tpretty-prints the contents of the error report in FILE"

prettyPrintFromFile :: FilePath -> IO ()
prettyPrintFromFile filename = do
  Just raw <- runKTestTool filename
  let objects = PP.fromRawLines raw
  let names = PP.names objects
  forM_ names $ \name -> do
    let repr = PP.repr name objects
    printf "%s:\t%s\n" name (show repr)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  invocation <- parseArgs <$> getArgs
  case invocation of
    Help -> printHelp
    Verify moduleName testFunctions -> verify moduleName testFunctions
    PrettyPrintReport filename -> prettyPrintFromFile filename
