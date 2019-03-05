--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- deBruijn
--

module DeBruijn
    (   generate,
        algoDuval,
        lydon
    ) where

import System.IO
import Parser

algoDuval :: Int -> Int -> [Int]
algoDuval order lng = do
    let x = lydon order lng [[0]]
    let y = [x | x <- x, mod (length x) order == 0 || (length x) == 1]
    concat y

lydon :: Int -> Int -> [[Int]] -> [[Int]]
lydon order counter prevNb = do
    if head (last prevNb) /= counter - 1 || last (last prevNb) /= counter - 1
        then do lydon order counter (prevNb ++ [createlydon (last prevNb) order counter])
        else prevNb

clean :: [Int] -> Int -> [Int]
clean x counter = do
    if last x == counter - 1
        then do clean (init x) counter
        else x

newNum :: [Int] -> Int -> [Int]
newNum x counter = clean x counter

createlydon :: [Int] -> Int -> Int -> [Int]
createlydon prevNb num counter = (init x) ++ [(last x) + 1]
    where   
        x = newNum (take num (cycle prevNb)) counter

generate :: Setup -> String
generate x = do
    let norder = order x
    let alpha = alphabet x
    if length alpha > 1 
        then [alpha !! x | x <- algoDuval norder (length alpha)]
        else take norder (repeat (alpha !! 0))