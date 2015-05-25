module PrettyPrint where

import Data.List
import Data.List.Split
import Data.Maybe
import Control.Arrow
import Data.Functor

type Objects = [(Path, String)]

type Path = [Name]

type Name = String

parseInt :: String -> Int
parseInt ('\\':'x':str) = 
  sum $ zipWith (*) (map read $ splitOn "\\x" str) (map (256 ^ ) [0 ..])

parseBool :: String -> Bool
parseBool = (/= 0) <$> parseInt

names :: Objects -> [Name]
names = map (head . fst)

focus :: Name -> Objects -> Objects
focus name list =
  map (first tail) $
  filter (\(path, s) -> head path == name)
  list

isList :: Objects -> Bool
isList = isJust <$> lookup ["IsCons"]

listSegments :: Objects -> [String]
listSegments attributes = 
  case parseBool <$> lookup ["IsCons"] attributes of
    Just True -> repr "car" attributes : listSegments (focus "cdr" attributes)
    Just False -> []

listRepr :: Objects -> String
listRepr attributes = "[" ++ intercalate ", " (listSegments attributes) ++ "]"

isTuple :: Objects -> Bool
isTuple = isJust <$> lookup ["TupleArity"]

tupleSegments :: Int -> Objects -> [String]
tupleSegments num attributes = 
  map (focusedRepr . flip focus attributes . ("Tuple"++) . show) [1 .. num]

tupleRepr :: Objects -> String
tupleRepr attributes = 
  "(" ++ intercalate ", " (tupleSegments length attributes) ++ ")"
  where length = parseInt $ fromJust $ lookup ["TupleArity"] attributes

repr :: Name -> Objects -> String
repr name objects = focusedRepr $ focus name objects

constructorRepr = undefined
  
focusedRepr :: Objects ->  String
focusedRepr [] = ""
focusedRepr [(["IntVal"], str)] = show $ parseInt str
focusedRepr [(["BoolVal"], str)] = show $ parseBool str
focusedRepr attributes | isList attributes = listRepr attributes
                       | isTuple attributes = tupleRepr attributes
                       | otherwise = constructorRepr attributes
