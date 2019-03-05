--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Main
--

module Main where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Data.List
import Data.Char
import Control.Monad
import DeBruijn
import Parser
import Parameters

main :: IO ()
main = do
    info <- parse
    let alpha = alphabet info
    let norder = order info
    if alpha == "" || norder == -1
        then do  hPutStrLn stderr (usage); exitWith (ExitFailure 84)
        else do
            case flag info of 
                [Check] ->  check info
                [Unique] -> unique info
                [Clean] ->  clean info []
                _ -> if norder > 0 then putStrLn (generate info) else putStrLn ""
            

        
