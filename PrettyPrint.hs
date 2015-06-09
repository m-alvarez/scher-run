module PrettyPrint where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Arrow
import Control.Applicative

data Entity = Int Int
            | Bool Bool
            | Maybe (Maybe Entity)
            | Tuple [Entity]
            | List [Entity]
            | Constructor String [Entity]

instance Show Entity where
  show (Int i)           = show i
  show (Bool b)          = show b
  show (Maybe m)         = show m
  show (Tuple t)         = "(" ++ intercalate ", " (map show t) ++ ")"
  show (List l)          = "[" ++ intercalate ", " (map show l) ++ "]"
  show (Constructor n f) = "(" ++ n ++ " " ++ intercalate " " (map show f) ++ ")"

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

listSegments :: Objects -> [Entity]
listSegments attributes = 
  case parseBool <$> lookup ["IsCons"] attributes of
    Just True  -> repr "car" attributes : listSegments (focus "cdr" attributes)
    Just False -> []
    Nothing    -> error "Could not find IsCons in list"

listRepr :: Objects -> Entity
listRepr = List . listSegments

isTuple :: Objects -> Bool
isTuple = (&&) <$> (isJust <$> lookup ["1"]) <*> (not <$> isConstructor) 

isConstructor :: Objects -> Bool
isConstructor = (not . null) <$> names <$> focus "Constructor"

tupleSegments :: Int -> Objects -> [Entity]
tupleSegments num attributes = 
  map (focusedRepr . flip focus attributes . show) [1 .. num]

tupleRepr :: Objects -> Entity
tupleRepr attributes = 
  Tuple $ tupleSegments len attributes
  where len = parseInt $ fromJust $ lookup ["TupleArity"] attributes

repr :: Name -> Objects -> Entity
repr name objects = focusedRepr $ focus name objects

constructorFields :: Objects -> [Entity]
constructorFields objects = 
  focusedRepr <$> flip focus objects <$> filter (isDigit . head) (names objects)

constructorRepr :: Objects -> Entity
constructorRepr objects =
  case names $ focus "Constructor" objects of
    [constructor] -> Constructor constructor $ constructorFields objects
    _ -> error "Could not find constructor name"
  
focusedRepr :: Objects ->  Entity
focusedRepr [] = error "Empty object map"
focusedRepr [(["IntVal"], str)] = Int $ parseInt str
focusedRepr [(["BoolVal"], str)] = Bool $ parseBool str
focusedRepr attributes | isList attributes = listRepr attributes
                       | isTuple attributes = tupleRepr attributes
                       | isConstructor attributes = constructorRepr attributes
                       | otherwise = error "Could not determine the representation"

fromRawLines :: [String] -> Objects
fromRawLines = map fromRawLine <$> chunksOf packet <$> drop header
  where header = 3
        packet = 3

fromRawLine :: [String] -> (Path, String)
fromRawLine [name, _, content] = (path, element)
  where path = splitOn "%" $ quoted name
        element = quoted content
        quoted = takeWhile (/= '\'') <$> tail <$> dropWhile (/= '\'')
fromRawLine _ = error "Fatal error: wrong invocation of internal fromRawLine"
