module Main (main) where

import System.Environment
import Data.List
import Data.Functor
import Data.Maybe
import Text.Printf

import Program
import Compile

data Run = Verify String [String]
         | Help

verify :: String -> [String] -> IO ()
verify moduleName [] = printf "No test functions specified.\n"
verify moduleName tests = sequence_ $ do
  test <- tests
  return $ printf "Running test %s of moduleName %s\n" test moduleName

parseFlags [] = []
parseFlags ((flag@('-':_)):rest) =
  (flag, flagArgs) : parseFlags restArgs
  where (flagArgs, restArgs) = break ((== '-') . head) rest

parseArgs args = 
  if "-help" `elem` args
    then Help
    else Verify filename arguments
      where filename = last args
            arguments = [] `fromMaybe` lookup "-test" (parseFlags $ init args)

printHelp = do
  printf "Usage: scher-run [OPTIONS]... MODULE\n"
  printf "Options:\n"
  printf "\t-test NAME\tadds NAME to the list of test functions\n"
  printf "\t-help\t\tprints this message and exits\n"

runProgram Help = printHelp
runProgram (Verify filename testFunctions) = verify filename testFunctions

main :: IO ()
main = do
  invocation <- parseArgs <$> getArgs
  runProgram invocation
  
