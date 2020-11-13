module Assignment1 (doubleList, firstDoubled , priceRange , allergyFree , checkSpec , checkSpec' , linearSort , counterexample , fromBin , toBin) where

import Data.List
import Data.Maybe

import Types

doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x:xs) = [x] ++ [x] ++ doubleList xs


firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled (x:xs) = if (xs /= []) then (if(x == head xs) then Just x else (firstDoubled xs)) else Nothing

priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange minPrice maxPrice [] = []
priceRange minPrice maxPrice [(CC p i)] = if(p>=minPrice && p<maxPrice) then [(CC p i)] else []
priceRange minPrice maxPrice ((CC p i):xs) = if(p>=minPrice && p<maxPrice) then ([(CC p i)] ++ priceRange minPrice maxPrice xs) else []

allergyFree :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree [] x = x
allergyFree _ [] = []
allergyFree y ((CC p i):xs) = if (Soy `elem` i && Soy `elem` y || Dairy `elem` i && Dairy `elem` y || Nuts `elem` i && Nuts `elem` y || Gluten `elem` i && Gluten `elem` y) then allergyFree y xs else [(CC p i)] ++ allergyFree y xs

sampletin :: Tin
sampletin = [[Nuts], [Dairy,Gluten], [], [Soy]]

checkSpec :: Spec -> Tin -> Bool
checkSpec (Not a) c = if (checkSpec a c) then False else True
checkSpec (And a b) c = if (checkSpec a c && checkSpec b c) then True else False
checkSpec (Or a b) c = if (checkSpec a c || checkSpec b c) then True else False
checkSpec (HasCup k x) c = if (x `elem` c !! k) then True else False

checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' (Not a) c = if (checkSpec' a c == Nothing) then Nothing else (if (checkSpec a c) then Just False else Just True)
checkSpec' (And a b) c = if (checkSpec' a c == Nothing || checkSpec' b c == Nothing) then Nothing else (if (checkSpec a c && checkSpec b c) then Just True else Just False)
checkSpec' (Or a b) c = if (checkSpec' a c == Nothing || checkSpec' b c == Nothing) then Nothing else (if (checkSpec a c || checkSpec b c) then Just True else Just False)
checkSpec' (HasCup k x) c = if (k >= length c) then Nothing else (if (x `elem` c !! k) then Just True else Just False)
