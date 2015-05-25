module Program where

import Text.Printf
import System.IO

testModuleName :: String
testModuleName = "TestModule"

testModuleFileName = (++ ".hs") testModuleName

testFileContents :: String -> String -> String
testFileContents moduleName testName = 
  concat [ printf "module %s(main) where\n" testModuleName
         , printf "import qualified %s as Test\n" moduleName
         , printf "main = Test.%s\n" testName
         ]

writeTestFile :: String -> String -> IO FilePath
writeTestFile moduleName testName =
  withFile testModuleFileName WriteMode $ \h -> do
    hPutStr h $ testFileContents moduleName testName
    return testModuleFileName
