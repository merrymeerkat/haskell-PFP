{-# LANGUAGE  InstanceSigs #-}
module MonadReader where

import Control.Applicative as C
import Control.Monad as M
import Data.Char
import Data.Functor 

boop = (*2)
doop = (+10)

-- we can combine these two functions
bip :: Integer -> Integer
bip = boop . doop

-- which can also be done this way
bloop :: Integer -> Integer
bloop = fmap boop doop
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- here, the "functorial context" is a partially applied function
-- fmap boop doop x == (*2) ((+10) x)

-- Differently from bip and bloop, bbop and duwop pass the given argument to boop and doop in parallel, and the results will be added together
bbop :: Integer -> Integer
bbop = (+) <$> boop C.<*> doop
-- Note: (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- In this case, f = ((->) a)
-- (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
--
-- ((+) <$> (*2) <*> (+10)) x =
-- (x*2) + (x+10)
--
-- or, think of 
bbop' = fmap (+) boop C.<*> doop
-- fmap (+) boop :: a -> a -> b
-- doop          :: a -> a
-- output        :: a -> b

-- so we can use f<$>f1<*>f2 when f1 and f2 share the same input and we want to apply f to the result of these two

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- Exercises chapter 22.2
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

-- write a function that returns the results of cap and rev as a tuple
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev C.<*> cap

-- or, using monads
-- note that the monad here is a partially applied function (I think)
tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    c <- cap
    r <- rev
    --return ((,) c r)
    return (c, r)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap M.>>= (\c ->
           rev M.>>= (\r -> 
           return (c, r)))

-- 22.5
newtype Reader r a =
  Reader { runReader :: r -> a }
instance Functor (Reader r) where
  fmap :: (a -> b)
    -> Reader r a
    -> Reader r b
  fmap f (Reader ra) =
    Reader $ \r -> f (ra r)

-- Exercise: Ask
ask :: Reader a a
ask = Reader id

-- 22.6
-- Exercises
-- 1) Write liftA2 yourself
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 abc fa fb = abc <$> fa C.<*> fb

-- 2) Write the following function
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3) Implement the Applicative function for Reader
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a
(<*>) :: Reader r (a -> b)
      -> Reader r a
      -> Reader r b
(Reader rab) <*> (Reader ra) =
  Reader $ \r -> rab r $ ra r
  
-- 22.7
-- Implement the Reader Monad
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
      -> (a -> Reader r b)
      -> (Reader r b)
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb $ ra r) r

-- write getDogRM to use your Reader datatype
newtype HumanName =
  HumanName String
  deriving (Eq, Show)
newtype DogName =
  DogName String
  deriving (Eq, Show)
newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

-- some sample dogs and people
chris :: Person
chris = Person (HumanName "Chris")
                (DogName "Papu")
               (Address "Austin")

papu :: Dog
papu = Dog (DogName "Papu")
           (Address "Austin")

-- why are we capitalising the constructors? I thought only the types were capitalised?
-- answer: DogName takes in a String and returns a DogName. We need to write DogName
-- followed by a string if we wish to get a DogName.  dogsName, on the other hand, is
-- simply the name given to the DogName component of a Dog. We can use it to retrieve a dog's name, but not to create a dog name ourselves (at least that's my understanding)

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- or, with our Monad Reader
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- Write this function with the bind operator
getDogRM' :: Person -> Dog
getDogRM' = dogName >>= (\n ->
            address >>= (\a ->
            return $ Dog n a ))
-- Note:
-- our monadic context here is Person
-- dogName :: Person -> DogName
-- address :: Person -> Address
-- Dog :: DogName -> Address -> Dog
-- return Dog :: Person -> DogName -> Address -> Dog
-- return $ Dog n a :: Person -> Dog (as we wished)
--

-- 22.11 Chapter Exercises
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maye b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- Now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' = flip lookup (zip x z)

-- Have x1 make a tuple of xs and ys, and x2 make a tuple of of ys and zs.
-- Also, write x3 which takes one input and makes a tuple of the results of
-- two applications of z' from above.

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs C.<*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys C.<*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) z z
  where z = z' n

-- Write a function that sums the elements of a tuple together
-- uncurry :: (a -> b -> c) -> (a, b) -> c
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- Write a function that lifts a boolean function over two partially applied functions
bolt :: Integer -> Bool
bolt = (&&) <$> (>3) C.<*> (<8)


fromMaybe :: a -> Maybe a -> a
fromMaybe n Nothing   = n
fromMaybe _ (Just n) = n


main :: IO ()
main = do
  print $
   sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ sequenceA [(>3), (<8), even] 7 -- very cool! f is ((->) a) and t is []
  print $ summed <$> ((,) <$> xs C.<*> ys)
  print $ fmap summed ((,) <$> xs C.<*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ foldl (&&) True (sequA 4)
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' = summed <$> ((,) <$> xs C.<*> ys)


-- sketches
--getDogRM'' :: Person -> Dog
--getDogRM'' p = dogName >>= (\n ->
--               address >>= (\a ->
--               return $ Dog (n p) (a p)))
--getDogRM' :: Reader Person Dog
--getDogRM' = dogName >>= (\n ->
--            address >>= (\a ->
--                Dog $ n a))
--getDogRM' :: Reader Person Dog
--getDogRM' = Reader $ \r -> dogName r >>= (\n ->
--                            address r >>= (\a ->
--                            Dog n a))









