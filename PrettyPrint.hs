module PrettyPrint where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Arrow
import Control.Applicative

type Objects = [(Path, String)]

type Path = [Name]

type Name = String

parseInt :: String -> Int
parseInt ('\\':'x':str) = 
  sum $ zipWith (*) (map read $ splitOn "\\x" str) (map (256 ^ ) [0 ..])
parseInt _ = error "Incorrect format for integer"

parseBool :: String -> Bool
parseBool = (/= 0) <$> parseInt

names :: Objects -> [Name]
names = map (head . fst)

focus :: Name -> Objects -> Objects
focus name list =
  map (first tail) $
  filter (\(path, _) -> head path == name)
  list

isList :: Objects -> Bool
isList = isJust <$> lookup ["IsCons"]

listSegments :: Objects -> [String]
listSegments attributes = 
  case parseBool <$> lookup ["IsCons"] attributes of
    Just True  -> repr "car" attributes : listSegments (focus "cdr" attributes)
    Just False -> []
    Nothing    -> error "Could not find IsCons in list"

listRepr :: Objects -> String
listRepr attributes = "[" ++ intercalate ", " (listSegments attributes) ++ "]"

isTuple :: Objects -> Bool
isTuple = (&&) <$> (isJust <$> lookup ["1"]) <*> (not <$> isConstructor) 

isConstructor :: Objects -> Bool
isConstructor = isJust <$> lookup ["Constructor"]

tupleSegments :: Int -> Objects -> [String]
tupleSegments num attributes = 
  map (focusedRepr . flip focus attributes . show) [1 .. num]

tupleRepr :: Objects -> String
tupleRepr attributes = 
  "(" ++ intercalate ", " (tupleSegments len attributes) ++ ")"
  where len = parseInt $ fromJust $ lookup ["TupleArity"] attributes

repr :: Name -> Objects -> String
repr name objects = focusedRepr $ focus name objects

constructorFields :: Objects -> [String]
constructorFields objects = 
  focusedRepr <$> flip focus objects <$> filter (isDigit . head) (names objects)

constructorRepr :: Objects -> String
constructorRepr objects =
  case names $ focus "Constructor" objects of
    [constructor] -> "(" ++ constructor ++ intercalate ", " (constructorFields objects) ++ ")"
    _ -> error "Could not find constructor name"
  
focusedRepr :: Objects ->  String
focusedRepr [] = ""
focusedRepr [(["IntVal"], str)] = show $ parseInt str
focusedRepr [(["BoolVal"], str)] = show $ parseBool str
focusedRepr attributes | isList attributes = listRepr attributes
                       | isTuple attributes = tupleRepr attributes
                       | otherwise = constructorRepr attributes
