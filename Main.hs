{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Data.Functor
import Data.Maybe
import Data.List
import Data.List.Split
import Text.Printf
import System.TimeIt

import Program
import Compiling
import Klee hiding (whenJust)
import qualified PrettyPrint as PP
import ExtraCFiles

data Run = Verify VerificationOptions [String]
         | Help
         | PrettyPrint [FilePath]

data VerificationOptions =
  VO { testModuleName     :: String
     , kleeFlags          :: KleeFlags
     , compilerFlags      :: CompilerFlags
     , benchmarkFile      :: Maybe FilePath
     }

defaultVerificationOptions =
  VO { testModuleName     = "TestModule"
     , kleeFlags          = defaultKleeFlags
     , compilerFlags      = defaultCompilerFlags
     , benchmarkFile      = Nothing
     }

defaultCompilerFlags :: CompilerFlags
defaultCompilerFlags =
  CF { tdir           = TDirAt "tdir"
     , entryPoint     = Just "main"
     , cCompiler      = "llvm-gcc"
     , hsCompiler     = "jhc"
     , toC            = True
     , cPreprocessor  = []
     , hsPreprocessor = Nothing
     , includes       = ["tdir/cbits", "tdir", "/home/user/klee/include"]
     , ignoreCache    = False
     , extraCFlags = [ "-std=gnu99"
                     , "-falign-functions=4"
                     , "-ffast-math"
                     , "-fno-strict-aliasing"
                     , "-DNDEBUG"
                     , "-c"
                     , "-emit-llvm" ]
     , extraHaskellFlags = [ "-fffi"
                           , "-fglobal-optimize"
                           ]
     , gc = Stub
     }

defaultKleeFlags :: KleeFlags
defaultKleeFlags = KleeFlags
                 { libc            = Just "uclibc"
                 , posixRuntime    = True
                 , emitAllErrors   = False
                 , outputDirectory = Just "klee-output"
                 , maxTime         = Nothing
                 , optimize        = False
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

functionModule :: String -> String
functionModule = intercalate "." <$> init <$> splitOn "."

functionName :: String -> String
functionName = last <$> splitOn "."

writeBenchmarks :: FilePath -> Double -> Double -> Double -> IO ()
writeBenchmarks filename hsCompileTime cCompileTime kleeTime =
  withFile filename WriteMode $ \h -> do
    hPrintf h "Haskell compiler time: %lf\n" hsCompileTime
    hPrintf h "C compiler time: %lf\n" cCompileTime
    hPrintf h "Klee run time: %lf\n" kleeTime

verify :: VerificationOptions -> [String] -> IO ()
verify _ [] = printf "No test functions specified.\n"
verify opts tests = forM_ tests $ \test -> do
  let m = functionModule test
  let f = functionName test
  testFile <- writeTestFile (testModuleName opts) (Function m f)
  (hsCompileTime, ExitSuccess) <- timeItT $ compileHs testFile Nothing (compilerFlags opts)
  case tdir defaultCompilerFlags of
    NoTDir      -> return ()
    TDirAt tdir -> writeExtraCFiles tdir
  printf "Running test function %s of module %s\n" f m
  (cCompileTime, ExitSuccess) <- timeItT $ compileC (cFiles $ gc defaultCompilerFlags) 
                                                    (Just "bytecode.bc") 
                                                    (compilerFlags opts)
  printf "Done compiling!\n"
  (kleeTime, Just kleeReport) <- timeItT $ runKlee (kleeFlags opts) "bytecode.bc"
  printf "Done verifying!\n"
  showReport kleeReport
  case benchmarkFile opts of
    Just file -> writeBenchmarks file hsCompileTime cCompileTime kleeTime
    Nothing -> return ()

parseFlags :: [String] -> ([String], ([String], [(String, String)]))
parseFlags [] = ([], ([], []))
parseFlags ((flag@('-':_)):(value@(c:_)):rest)
  | c /= '-'  = id *** id *** ((flag, value):) $ parseFlags rest
  | otherwise = id *** (flag:) *** id $ parseFlags rest
parseFlags (value:rest) = 
  (value:) *** id *** id $ parseFlags rest
  
parseArgs :: [String] -> Run
parseArgs ("verify":rest)  = Verify options functions
  where (functions, (flags, args)) = parseFlags rest
        options = flip execState defaultVerificationOptions $ do

          let hsPreprocessor = Just $ case lookup "-strategy" args of
                Just "eager"-> [("KLEE_IMPURE", Nothing)]
                Nothing     -> [("KLEE_IMPURE", Nothing)]
                Just "lazy" -> [("KLEE_PURE", Nothing)]
                Just strat  -> error $ printf "Invalid strategy %s" strat
          modify $ \o -> o { compilerFlags = (compilerFlags o) { hsPreprocessor } }

          when ("-ignore-cache" `elem` flags) $ do
            modify $ \o -> o { compilerFlags = (compilerFlags o) { ignoreCache = True } }

          when ("-emit-all-errors" `elem` flags) $ do
            modify $ \o -> o { kleeFlags = (kleeFlags o) { emitAllErrors = True } }

          when ("-optimize" `elem` flags) $ do
            modify $ \o -> o { kleeFlags = (kleeFlags o) { optimize = True } }

          whenJust (lookup "-max-time" args) $ \time ->
            modify $ \o -> o { kleeFlags = (kleeFlags o) { maxTime = Just $ read time } }

          whenJust (lookup "-benchmarkFile" args) $ \file ->
            modify $ \o -> o { benchmarkFile = Just file }

parseArgs ("pp":files) = PrettyPrint files
parseArgs ("help":_)   = Help
parseArgs _            = Help

printHelp :: IO ()
printHelp = do
  printf "Usage: scher-run COMMAND\n"
  printf "Where command is one of\n"
  printf "\tverify [FUNCTION]... [FLAGS]...\n"
  printf "\t\tAvailable flags:\n"
  printf "\t\t\t-strategy (eager|lazy)\n"
  printf "\t\t\t-emit-all-errors\n"
  printf "\t\t\t-max-time TIME\n"
  printf "\t\t\t-ignore-cache\n"
  printf "\tpp [FILE]...\n"
  printf "\thelp\n"

prettyPrintFromFile :: FilePath -> IO ()
prettyPrintFromFile filename = do
  Just raw <- runKTestTool filename
  let objects = PP.fromRawLines raw
  let names = PP.names objects
  forM_ names $ \name -> do
    printf "%s:\t%s\n" name (show $ PP.repr name objects)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  invocation <- parseArgs <$> getArgs
  case invocation of
    Help -> printHelp
    Verify options testFunctions -> verify options testFunctions
    PrettyPrint filenames -> forM_ filenames prettyPrintFromFile
