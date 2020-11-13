module Assignment2 (choose , simulate , cut , shuffle , riffles , permute , genTree) where

import Types
import Data.List

standard52 :: Deck
standard52 = [Card {rank = r, suit = s} | r <- [R2 .. RA], s <- [C .. S]]

code :: PickingMonad m => m Char
code = do
  i <- pick 0 3
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalise :: Eq a => Dist a -> Dist a
normalise xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub" removes duplicates from a list

instance Show Rank where
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = "10"
  show RJ = "J"
  show RQ = "Q"
  show RK = "K"
  show RA = "A"

instance Show Suit where
  show C = "\9827"
  show D = "\9830"
  show H = "\9829"
  show S = "\9824"

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"

instance Show Card where
  show (Card r D) = red(show r ++ show D)
  show (Card r H) = red(show r ++ show H)
  show (Card r C) = black(show r ++ show C)
  show (Card r S) = black(show r ++ show S)

choose :: PickingMonad m => [a] -> m a
choose [] = error ("List is Empty!!")
choose (xs) = do
                 b <- (pick 0 (length xs - 1))
                 return (xs !! b)

simulate :: Monad m => m Bool -> Integer -> m Integer
simulate bm 0 = return 0
simulate bm n = do
                  b <- bm
                  c <- simulate bm (n-1)
                  if (b == True) then return (1 + c)
                  else return c

cut :: PickingMonad m => [a] -> m ([a],[a])
cut [] = error ("List is empty!!")
cut xs = do
           b <- (pick 0 (length xs))
           return (splitAt b xs)

deleteN :: Int -> [Int] -> [Int]
deleteN _ [] = []
deleteN n (a:as) | n == 0 = as
                 | n <= length as = a : deleteN (n-1) as
                 | otherwise = error ("")

insertAts :: a -> Int -> [a] -> [a]
insertAts x _ [] = [x]
insertAts x i (a:as) | i <= 0 = x:a:as
                     | otherwise = a : insertAts x (i-1) as

shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle = undefined
--shuffle ([], []) = return []
--shuffle (xs, []) = return xs
--shuffle ([], ys) = return ys
--shuffle (xs, ys) = do
--                     let zs = length xs + length ys
--                     a <- (pick 0 (zs - 1)) -- pick a position at which to extract an element randomly from the list
--                     b <- (pick 0 (zs - 1)) -- pick the random position at which to insert the new element in the list
--                     c <- shuffle (xs, ys)
--                    if (a >= length xs) then return (insertAts (ys !! (zs - a)) b c)
--                     else return insertAts (xs !! a) b c

permute :: PickingMonad m => [a] -> m [a]
permute [] = return []
permute (x:xs) = do
                   a <- pick 0 (length xs)
                   c <- permute xs
                   return (insertAts x a c)
