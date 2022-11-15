

module Main (main) where

import Data.List

findStrings :: (Eq a) => [a] -> [a] -> [Int]
findStrings search str = findIndices (isPrefixOf search) (tails str)

findString ::  Char -> String -> Int
findString search str =
        case findStrings [search] str of
        [] -> -1
        (x:_) -> x


reverseString :: [Char] -> [Char]
reverseString [] = []
reverseString (x:xs) = reverseString xs ++ [x]


sumList :: [Int] -> Int -> [Int]
sumList [] _ = []
sumList (x:xs) y = (x + y) : sumList xs y


{---------------Buscar de forma horizontal-------------}
soupHorizontalWord ::  String -> [String] -> Int ->  [[Int]]
soupHorizontalWord search soup indc
        | indc == length soup = []
        | null (findStrings search (soup !! indc)) = [] : soupHorizontalWord search soup (indc+1)
        | otherwise = findStrings search (soup !! indc) : soupHorizontalWord search soup (indc+1)

soupHorizontalWords :: [String] -> [String] -> Bool -> IO()
soupHorizontalWords [] _ _= return ()
soupHorizontalWords (x:xs) soup reverseCorfirmation= do
        if reverseCorfirmation then do
                showIndexMatrixR (soupHorizontalWord (reverseString x) soup 0) x 0 True (-10)
                soupHorizontalWords xs soup reverseCorfirmation
        else do
                showIndexMatrixR (soupHorizontalWord x soup 0) x 0 False (-10)
                soupHorizontalWords xs soup reverseCorfirmation


{---------------Buscar de forma Vertical-------------}

{- from a word search below a soup of letters depending on whether the address if there is a match -}
soupVerticalConfirmation :: String -> [String] ->  Int -> Int ->Bool
soupVerticalConfirmation [] _ _ _ = True {- the word is complete -}
soupVerticalConfirmation _ [] _ _ = False {- get to the final row -}
soupVerticalConfirmation   (fletter:word) (frow:soup) columFound direction
        | columFound == -1 = False
        | columFound + direction >= length frow  || columFound + direction < 0 = False {- get to the final column -}
        | fletter == (frow !! (columFound + direction)) = soupVerticalConfirmation word soup (columFound + direction) direction
        | otherwise = False

{- finds if there are several words in the same row that are diagonally across -}
soupVerticalRowS :: String -> [String] -> Int -> Int->[Int]
soupVerticalRowS search soup columVerified direction =
        let  strFound = findString (head search) (drop columVerified (head soup))
             strFound' =  if strFound == -1 then -1 else strFound + columVerified {- finds if the first letter of the word is found in a row -}
             res = soupVerticalConfirmation (tail search) (tail soup) strFound' direction {- confirm if the rows below match the word trace -}
        in if strFound' == -1 then []
           else if not res then soupVerticalRowS search  soup (strFound' + 1) direction {- found the first letter but searching vertically did not match the word -}
           else strFound' : soupVerticalRowS search  soup (strFound' + 1) direction


{-from a word searches an entire alphabet for all the rows if there is a match -}
soupVerticalSWord :: String -> [String] -> Int ->[[Int]]
soupVerticalSWord _ [] _ = []
soupVerticalSWord search soup direction = soupVerticalRowS search soup 0 direction : soupVerticalSWord search (tail soup) direction

{- Depending on several word entries, search in a matter of direccition if they are coincidences in the alphabet soup. and show their location. -}
soupVerticalSWords :: [String] -> [String] -> Bool -> Int-> IO()
soupVerticalSWords [] _ _ _= return ()
soupVerticalSWords (x:xs) soup reverseCorfirmation direction = do
        if reverseCorfirmation then do
                showIndexMatrixR (soupVerticalSWord (reverseString x) soup direction ) x 0 True direction
                soupVerticalSWords xs soup reverseCorfirmation direction
        else do
                showIndexMatrixR (soupVerticalSWord x soup direction) x 0 False direction
                soupVerticalSWords xs soup reverseCorfirmation direction


{-----------------------mostrar busqueda-------------------}
      
showIndexMatrixR ::  [[Int]] -> String -> Int -> Bool -> Int -> IO()
showIndexMatrixR [] _ _ _ _= return ()
showIndexMatrixR (x:xs) textFound row reverseConf direction= do
        if null x
                then showIndexMatrixR xs textFound (row+1) reverseConf direction
        else do 
                if direction /= -10 then do
                      if reverseConf then do putStrLn ("Found " ++ textFound ++ " at row " ++ show (row + (length textFound -1)) ++ " column " ++ show  (sumList x ((length textFound-1) * direction)))
                      else do putStrLn ("Found " ++ textFound ++ " at row " ++ show row ++ " column " ++ show  x)
                else do 
                      if reverseConf then do putStrLn ("Found " ++ textFound ++ " at row " ++ show row ++ " column " ++ show  (sumList x (length textFound -1)))
                      else do putStrLn ("Found " ++ textFound ++ " at row " ++ show row ++ " column " ++ show x)
                
                showIndexMatrixR xs textFound (row+1) reverseConf direction



{- --------------------mostrar errores------------------ -}

  {- verificar si el tamaño de las listas enviadas son de igual tamaño -}
verifySoup :: [String] -> Bool
verifySoup soup = length (nub (map length soup)) == 1


main :: IO ()
main = do

   putStrLn "================ Main Running: ======================="
   let wordToSearch = ["hola","hello"]


   let soup1 = ["hhhahahaoa",
                "ahooaoalal",
                "ahlllllaoo",
                "aloaoealoh",
                "aaalholaaa",
                "alahallooh"]



   if verifySoup soup1 then do
 

        putStrLn "\n============ Horizontal Search: ============"
        putStrLn "------------ Normal Search: -------------"
        soupHorizontalWords  wordToSearch soup1 False
        putStrLn "------------ Reverse Search: ------------"
        soupHorizontalWords  wordToSearch soup1 True


        putStrLn "\n============ vertical Search: ==============="
        putStrLn "------------ Normal Search: -------------"
        soupVerticalSWords wordToSearch soup1 False 0
        putStrLn "------------ Reverse Search: ------------"
        soupVerticalSWords wordToSearch soup1 True 0


        putStrLn "\n========= Diagonal Right Search: ============="
        putStrLn "------------ Normal Search: -------------"
        soupVerticalSWords wordToSearch soup1 False 1

        putStrLn "------------ Reverse Search: ------------"
        soupVerticalSWords wordToSearch soup1 True 1

        putStrLn "\n========== Diagonal Left Search: ============="
        putStrLn "------------ Normal Search: -------------"
        soupVerticalSWords wordToSearch soup1 False (-1)
        putStrLn "------------ Reverse Search: ------------"
        soupVerticalSWords wordToSearch soup1 True (-1)




   else do
        putStrLn "Error: The soup is not valid"




   return ()

