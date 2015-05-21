module Compile (compileFile, CompilerFlags(...)) where

data CompilerFlags = CF {}

compileFile :: FilePath -> CompilerFlags -> Maybe FilePath
compileFile file flags = undefined
