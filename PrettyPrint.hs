module PrettyPrint where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Printf
import Control.Arrow
import Control.Applicative
import Debug.Trace

data Entity = Int Int
            | Bool Bool
            | Char Char
            | Maybe (Maybe Entity)
            | Tuple [Entity]
            | List [Entity]
            | Constructor String [Entity]

instance Show Entity where
  show (Int i)           = show i
  show (Bool b)          = show b
  show (Maybe m)         = show m
  show (Char c)          = show c
  show (Tuple t)         = "(" ++ intercalate ", " (map show t) ++ ")"
  show (Constructor n f) = "(" ++ n ++ " " ++ intercalate " " (map show f) ++ ")"
  show (List l) | all isChar l = show $ map ofChar l
                | otherwise    = "[" ++ intercalate ", " (map show l) ++ "]"
            where isChar (Char _) = True
                  isChar _        = False
                  ofChar (Char c) = c

type Objects = [(Path, String)]

type Path = [Name]

type Name = String

parseInt :: String -> Int
parseInt ('\\':str) = 
  sum $ zipWith (*) (map (read . ("0"++)) $ splitOn "\\" str) (map (256 ^ ) [0 ..])
parseInt _ = error "Incorrect format for integer"

parseChar :: String -> Char
parseChar = chr <$> parseInt

parseBool :: String -> Bool
parseBool = (== 1) <$> (`rem` 2) <$> parseInt

names :: Objects -> [Name]
names = nub <$> map ((\a -> if a == [] then error "baz" else head a) . fst)

focus :: Name -> Objects -> Objects
focus name list =
  map (first tail) $
  filter (\(path, _) -> (\a -> if a == [] then error ("quux on " ++ name ++ " AND " ++ (show list)) else head a) path == name)
  list

isList :: Objects -> Bool
isList = isJust <$> lookup ["BoolVal"] <$> focus "IsNil"

listSegments :: Objects -> [Entity]
listSegments attributes = 
  case parseBool <$> lookup ["BoolVal"] (focus "IsNil" attributes) of
    Just True  -> []
    Just False -> repr "car" attributes : listSegments (focus "cdr" attributes)
    Nothing    -> error "Could not find IsNil in list"

listRepr :: Objects -> Entity
listRepr = List . listSegments

isTuple :: Objects -> Bool
isTuple = (&&) <$> ((not . null) <$> focus "1") <*> (not <$> isConstructor) 

isConstructor :: Objects -> Bool
isConstructor = (not . null) <$> names <$> focus "Constructor"

tupleSegments :: Int -> Objects -> [Entity]
tupleSegments num attributes = 
  map (focusedRepr . flip focus attributes . show) [1 .. num]

tupleRepr :: Objects -> Entity
tupleRepr attributes = 
  Tuple $ tupleSegments len attributes
  where len = length $ names attributes

repr :: Name -> Objects -> Entity
repr name objects = focusedRepr $ focus name objects

constructorFields :: Objects -> [Entity]
constructorFields objects =
  focusedRepr <$> flip focus objects <$> filter (isDigit . (\a -> if a == [] then error "foobar" else head a)) (names objects)

constructorRepr :: Objects -> Entity
constructorRepr objects =
  case names $ focus "Constructor" objects of
    [constructor] -> Constructor constructor $ constructorFields objects
    _ -> error "Could not find constructor name"
  
focusedRepr :: Objects ->  Entity
focusedRepr [] = error "Empty object map"
focusedRepr [(["IntVal"], str)] = Int $ parseInt str
focusedRepr [(["BoolVal"], str)] = Bool $ parseBool str
focusedRepr [(["CharVal"], str)] = Char $ parseChar str
focusedRepr attributes | isList attributes = listRepr attributes
                       | isTuple attributes = tupleRepr attributes
                       | isConstructor attributes = constructorRepr attributes
                       | otherwise = error $ printf "Could not determine the representation for %s" (show attributes)

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
