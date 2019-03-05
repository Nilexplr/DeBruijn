--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Parameters
--

module Parameters (
    check,
    unique,
    clean
) where
import Parser
import DeBruijn
import Compart

check :: Setup -> IO()
check info = do 
    toCheck <- getLine
    if cmpDeBruijn (details toCheck norder) (details counter norder)
        then putStrLn "OK" else putStrLn "KO"
    where
        counter = generate info
        norder = length (alphabet info)

unique :: Setup -> IO()
unique info = do
    toCheck1 <- getLine
    toCheck2 <- getLine
    if cmpDeBruijn (details toCheck1 norder) (details counter norder) && compareReverse toCheck1 toCheck2
        then putStrLn "OK" else putStrLn "KO"
    where
        counter = generate info
        norder = length (alphabet info)

clean :: Setup -> [String] -> IO()
clean info x = do
    line <- getLine
    if (line /= "END")
        then clean info (x ++ [line])
        else do putStr final
        where
            y = [z | z <- x, (length z) == (length counter) && cmpDeBruijn (details (z) norder) (details counter norder)]
            counter = generate info
            norder = length (alphabet info)
            final = concat [a ++ "\n" | a <- y]
