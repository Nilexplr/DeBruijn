--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Compart
--

module Compart (
    cmpDeBruijn,
    separate,
    details,
    compareReverse
) where

import Parser
import DeBruijn
import Data.List
import Data.Maybe
import Data.Data

separate :: [[Char]] -> String -> Int ->  [String]
separate result x counter
            | length x >= counter = separate (result ++ [z]) y counter
            | otherwise = result
            where
                y = drop counter x
                z = take counter x

details ::  [Char] -> Int -> [[Char]]
details toConvert counter = tail (separate [[]] toConvert counter)

cmpDeBruijn :: [[Char]] -> [[Char]] -> Bool
cmpDeBruijn x y
    | length y < 1 = True
    | (y !! 0 `elem` x) == True = cmpDeBruijn (speClean x (head y)) (tail y)
    | (reverse (y !! 0) `elem` x) == True = cmpDeBruijn (speClean x (reverse (head y))) (tail y)
    | otherwise = False
    where 
        speClean :: [String] -> String -> [String]
        speClean x y = do
            if elemIndex y x /= Nothing 
                then [x !! a | a <- [0..(length x) - 1], a /= fromJust (elemIndex y x)] 
                else x

compareReverse :: [Char] -> [Char] -> Bool
compareReverse x y = x == reverse y