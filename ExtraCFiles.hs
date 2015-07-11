module ExtraCFiles (writeExtraCFiles) where

import System.IO
import System.FilePath

type CFile = (FilePath, String)

extraCFiles :: [CFile]
extraCFiles = [extras]

writeExtraCFiles :: FilePath -> IO ()
writeExtraCFiles root = sequence_ $ (map $ writeCFile root) extraCFiles

extras :: CFile
extras = unlines [
  "#include <stdint.h>",
  "#include \"klee/klee.h\"",
  "intmax_t klee_intmax_t(char* name) {",
  "  intmax_t i;",
  "  klee_make_symbolic(&i, sizeof(intmax_t), name);",
  "  return i;",
  "}"
  ] `at` "extras.h"
  

writeCFile :: FilePath -> CFile -> IO ()
writeCFile root (filePath, contents) = do
  withFile (root </> filePath) WriteMode $ \h -> do
    hPutStr h contents

at :: String -> FilePath -> CFile
at contents path = (path, contents)
