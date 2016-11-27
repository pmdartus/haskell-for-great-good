import qualified Data.Map as Map

-- Making Our Own Types and Typeclasses
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-- Algebraic data type
-- ===================

-- Notice point as same name for mcustructor and data type 
-- Adding show to print them in ghci
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Pattern match on the contructor type
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- contructor are just functions so they can be partially applied
-- map (Circle 1 1) [2,3,4]

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseReactangle :: Float -> Float -> Shape
baseReactangle w h = Rectangle (Point 0 0) (Point w h)

-- Avoiding to expose the value constructor allow better decoupling
-- making them more abstract. Avoid pattern matching in the external 
-- modules

-- Record data types
-- =================

-- Creation using {} and = for attr assignment
-- Person {firstName="bob",lastName="truc",age=12,height=12}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Show)

-- Type parameters
-- ===============

-- Value contructors takes values parameters and create a new value
-- Type contructors takes a type and create another one

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- Dervied instances
-- =================

-- Value contructors are nullary, can be used as Enums
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Usages:
-- Read - read "Tuesday" :: Day
-- Show - show Monday
-- Ord and Eq- compare Monday Tuesday
-- Bounded - minBound :: Day
-- Enum - succ Tuesday - [Friday .. Sunday]

-- Type Synonyms
-- =============

-- Those synonyms are not value contructors they allow to bring
-- more clarity in the code when handeling comple types

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

-- Note: Map Int String, [a], (Ord a) => Maybe a are concrete type
-- Whereas Meaybe is a type constructor!

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ "doesn't exists!"
    Just (state, code) -> if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ "is already taken!"

-- Recursive data types
-- ====================

infixr 5 :-:
data Chain a = Empty | a :-: (Chain a) deriving (Show, Read, Eq, Ord)

-- Usages
-- let a = 3 :-: 4:-: Empty

infixr 5 .++
(.++) :: Chain a -> Chain a -> Chain a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Usage
-- a .++ (5 :-: Empty)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x == y  = Node x left right
  | x < y   = Node y (treeInsert x left) right
  | x > y   = Node y left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y left right)
  | x == y  = True
  | x < y   = treeElem x left
  | x > y   = treeElem x right

-- Usage
-- let nums = [1,2,4,1,6,4]
-- foldr treeElem EmptyTree nums

-- Typeclasses
-- ===========

-- Not deriving here but later
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

-- YesNo typeclass

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno []  = False
  yesno _   = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _)  = True
  yesno Nothing   = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _         = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf val t f = if yesno val then t else f

-- Functors
-- ========

-- Functors can be represented as boxes

-- fmap (++ "Just") $ Just "Something"
-- fmap (++ "Just") $ Nothing

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- Usages:
-- fmap (*2) $ foldr treeInsert EmptyTree [1,2,3]
-- fmap (*2) EmptyTree

-- Kind and some type
-- ==================

-- Kind are the "types" of the type. Like labels on types expressing their structure 
-- Find the kind of a type using :k
-- Find the info of a function using :info
-- :k Maybe or :k Maybe Int

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

-- Usage :t Frank {frankField = Just "Hah" }

instance Tofu Frank where
  tofu x = Frank x

-- Usage tofu (Just 'a') :: Frank Char Maybe

data Barry t k p = Barry { yabba :: p, dabba :: t k }

-- p is concrete --> *
-- k also --> *
-- t (* -> *)
-- Finally we can say that the kind of Barry is
-- (* -> *) -> * -> * -> * based on the type definition

-- If we want to make Barry an instance of functor it need to match * -> *
-- Meaning to apply 2 parameters

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

-- Now mapping only on the first field 