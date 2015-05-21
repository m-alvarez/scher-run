module Program (writeTestFile) where

import Text.Printf
import System.IO

testModuleName :: String
testModuleName = "TestModule"

testModuleFileName = (++ ".hs") testModuleName

testFileContents :: String -> String -> String
testFileContents moduleName testName = 
  concat [ printf "module %s(main) where\n" testModuleName
         , printf "import %s\n" moduleName
         , printf "main = %s\n" testName
         ]

writeTestFile moduleName testName =
  withFile testModuleFileName ReadMode $ \h -> 
    hPutStr h $ testFileContents moduleName testName
