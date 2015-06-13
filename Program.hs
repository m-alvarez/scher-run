module Program (writeTestFile, Function(Function)) where

import Text.Printf
import System.IO
import System.FilePath
import Data.Functor
import Data.List.Split

type Module = String
data Function = Function { moduleName :: Module, functionName :: String }

testFileContents :: Module -> Function -> String
testFileContents testModuleName function =
  concat [ printf "module %s(main) where\n" testModuleName
         , printf "import qualified %s as Tested\n" (moduleName function)
         , printf "import Test.Scher\n"
         , printf "main = verify Tested.%s\n" (functionName function)
         ]

moduleToFileName :: Module -> FilePath
moduleToFileName = (<.> "hs") <$> foldl (</>) "" <$> splitOn "."

writeTestFile :: String -> Function -> IO FilePath
writeTestFile testModuleName function = do
  let filename = moduleToFileName testModuleName
  withFile filename WriteMode $ \h -> do
    hPutStr h $ testFileContents testModuleName function
    return filename
