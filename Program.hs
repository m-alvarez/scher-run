module Program where

import Text.Printf
import System.IO
import System.FilePath
import Data.Functor
import Data.List.Split

data Strategy = Eager | Lazy

instance Show Strategy where
  show Eager = "Eager"
  show Lazy = "Lazy"

testFileContents :: String -> Strategy -> String -> String -> String
testFileContents testModuleName strategy moduleName testName = 
  concat [ printf "module %s(main) where\n" testModuleName
         , printf "import qualified %s as Test\n" moduleName
         , printf "import Test.Scher\n"
         , printf "main = runSym Test.%s %s\n" testName (show strategy)
         ]

moduleToFileName :: String -> FilePath
moduleToFileName = (<.> "hs") <$> foldl (</>) "" <$> splitOn "."

writeTestFile :: String -> Strategy -> String -> String -> IO FilePath
writeTestFile testModuleName strategy moduleName testName = do
  let filename = moduleToFileName testModuleName
  withFile filename WriteMode $ \h -> do
    hPutStr h $ testFileContents testModuleName strategy moduleName testName
    return filename
