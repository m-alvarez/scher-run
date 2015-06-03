{-# LANGUAGE NamedFieldPuns #-}
module Klee where

import System.Exit
import System.Process
import System.Directory
import System.FilePath
import Text.Printf
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List
import Data.List.Split

data KleeFlags = KleeFlags
               { libc            :: Maybe String
               , emitAllErrors   :: Bool
               , outputDirectory :: Maybe FilePath
               }

data KleeReport = KleeReport
                { completedPaths :: Integer
                , generatedPaths :: Integer
                , exploredPaths  :: Integer
                , testCases      :: [ FilePath ]
                , errors         :: [ FilePath ]
                , statistics     :: Maybe KleeStats
                }

exhaustive :: KleeReport -> Bool
exhaustive = (==) <$> completedPaths <*> generatedPaths

instance Show KleeReport where
  show r = snd $ runWriter $ do
    tell $ printf "Number of completed paths: \t%d\n" (completedPaths r)
    tell $ printf "Number of generated paths: \t%d\n" (generatedPaths r)
    tell $ printf "Number of explored paths: \t%d\n" (exploredPaths r)
    tell $ printf "The test is \t%s\n" $ if exhaustive r then "EXHAUSTIVE" else "NONEXHAUSTIVE"
    tell $ printf "\n"
    tell $ printf "Test cases: \t%s\n" (intercalate "," $ testCases r)
    tell $ printf "Errors: \t%s\n" (intercalate "," $ errors r)
    tell $ printf "Support for performance statistics not yet available\n"
    return ()

data KleeStats = KleeStats
               { instructions          :: Integer
               , fullBranches          :: Integer
               , partialBranches       :: Integer
               , numBranches           :: Integer
               , userTime              :: Double
               , numStates             :: Integer
               , mallocUsage           :: Integer
               , numQueries            :: Integer
               , numQueryConstructs    :: Integer
               , numObjects            :: Integer
               , wallTime              :: Double
               , coveredInstructions   :: Integer
               , uncoveredInstructions :: Integer
               , queryTime             :: Double
               , solverTime            :: Double
               , cexCacheTime          :: Double
               , forkTime              :: Double
               , resolveTime           :: Double
               }

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a

toFlags :: FilePath -> KleeFlags -> [String]
toFlags input flags = snd $ runWriter $ do
  whenJust (libc flags) $ \c -> tell ["-libc=" ++ c]
  whenJust (outputDirectory flags) $ \d -> tell ["-output-dir=" ++ d]
  when (emitAllErrors flags) (tell ["-emit-all-errors"])
  tell [input]

report :: ExitCode -> String -> String -> String -> IO ExitCode
report exitCode input out err = do
  case exitCode of
    ExitFailure i -> do
      printf "Error verifying file %s\n" input
      printf "Error code %d\n" i
      printf "STDOUT:\n%s\n" out
      printf "STDERR:\n%s\n" err
    ExitSuccess ->
      return ()
  return exitCode

readKleeInfoFile :: FilePath -> IO (Integer, Integer, Integer)
readKleeInfoFile path = do
  attributes <- map (\ [a, b] -> (a, b))
                <$> map (splitOn " = ")
                <$> drop (length prefix)
                <$> filter ((== prefix) . take (length prefix)) 
                <$> lines 
                <$> readFile (path </> "info")
  return ( read $ fromJust $ lookup "explored paths" attributes
         , read $ fromJust $ lookup "completed paths" attributes
         , read $ fromJust $ lookup "generated paths" attributes
         )
    where prefix = "KLEE: done: "


readKleeResults :: FilePath -> IO KleeReport
readKleeResults path = do
  kleeResults <- getDirectoryContents path
  let testCases = filter ((== ".ktest") . snd . splitExtension) kleeResults
  let errors = filter ((== ".err") . snd . splitExtension) kleeResults
  let statistics = Nothing
  (exploredPaths, completedPaths, generatedPaths) <- readKleeInfoFile path
  return KleeReport { testCases
                    , errors
                    , generatedPaths
                    , completedPaths
                    , exploredPaths
                    , statistics }
  

runKlee :: KleeFlags -> FilePath -> IO (Maybe KleeReport)
runKlee flags filename = do
  printf "Command: %s %s\n" "klee" (unwords kleeFlags)
  (exitCode, out, err) <- readProcessWithExitCode "klee" kleeFlags ""
  _ <- report exitCode filename out err
  case exitCode of
    ExitFailure _ -> return Nothing
    ExitSuccess -> Just <$> readKleeResults ("klee-last" `fromMaybe` outputDirectory flags)
    where kleeFlags = toFlags filename flags
