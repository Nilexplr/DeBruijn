--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Parser
--

module Parser
    (   parse,
        Setup(..),
        Flag(..),
        usage
    ) where

import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Text.Printf
import Text.Read
import Text.ParserCombinators.ReadPrec

data Setup = Setup {
                order :: Int,
                alphabet :: String,
                flag :: [Flag]
            } deriving (Eq,Show)

data Flag
   = Check
   | Unique
   | Clean
   | Help
   deriving (Eq,Ord,Enum,Show,Bounded)

flags = [Option [] ["check"] (NoArg Check) "",
    Option [] ["unique"] (NoArg Unique) "",
    Option [] ["clean"] (NoArg Clean) "",
    Option ['h'] ["help"] (NoArg Help) ""]

analyseAlpha :: String -> Bool
analyseAlpha "" = False
analyseAlpha x
                | x !! 0 `elem` y = False
                | length x <= 1 = True
                | otherwise = analyseAlpha y
                where y = [x !! y | y <- [1..(length x - 1)]]


createSetup :: [String] -> [Flag] -> Setup
createSetup (x:[]) flg = Setup { order=x', alphabet="01", flag = flg}
        where x' = if (readMaybe x :: Maybe Int) /= Nothing then read x :: Int else -1
createSetup (x:y:_) flg = do
    if analyseAlpha y
        then Setup { order=x', alphabet=y, flag = flg}
        else Setup { order=x', alphabet="", flag = flg}
        where x' = if (readMaybe x :: Maybe Int) /= Nothing then read x :: Int else -1

usage = "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\n" ++ 
        "\t--check\tcheck if a sequence is a de Bruijn sequence\n" ++
        "\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n" ++
        "\t--clean\tlist cleaning\n" ++
        "\tn\torder of the sequence\n" ++
        "\ta\talphabet [def: “01”]"


parse :: IO(Setup)
parse = do
    arg <- getArgs
    if length arg == 0 || length arg > 3
        then do hPutStrLn stderr (usage)
                exitWith (ExitFailure 84)
        else do
            case getOpt Permute flags arg of
                (args,fs,[]) -> do
                    let argument = if null fs then ["-"] else fs
                    if Help `elem` args
                        then do hPutStrLn stderr (usage)
                                exitWith ExitSuccess
                        else return (createSetup argument args)
                    
                otherwise -> do
                    hPutStrLn stderr (usage)
                    exitWith (ExitFailure 84)