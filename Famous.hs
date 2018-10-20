-- Authors: Nicklas Botö, Adam Ryden, Carl Wiede

-- Lab 4 B: Famous

module Famous where

import System.IO

------------------------------------------------------

-- data QA can be a (Name, Question) = QorN or a Tree
-- This allows you to use it as a name suggestion
-- or a question with two outcomes (yes or no).
-- Deriving show for storing the QA in a file 
-- and read for conversion String -> QA

data QA = QorN String
        | Tree QA QA QA
  deriving (Show, Read)

------------------------------------------------------

-- The "base case" QA according to the assignment

defaultTree :: QA
defaultTree = Tree (QorN "Is she from Europe? ") 

                   (Tree (QorN "Is she a scientist? ") 
                         (QorN "Marie Curie") 
                         (QorN "Queen Elisabeth II"))

                   (Tree (QorN "Is she an actress? ") 
                         (QorN "Marilyn Monroe") 
                         (QorN "Hillary Clinton"))

------------------------------------------------------

-- question takes an input QorN (Question) and outputs
-- the answer typed by the user

question :: String -> IO String
question q = 
  do
    putStr q
    ans <- getLine
    return ans


------------------------------------------------------

-- yesNoQuestion takes a question

yesNoQuestion :: String -> IO Bool
yesNoQuestion q =
  do
    ans <- question q
    yesNoQuestion' ans
    where
      yesNoQuestion' :: String -> IO Bool
      yesNoQuestion' ans
        | ans == "yes" = return True
        | ans == "no"  = return False
        | otherwise    = yesNoQuestion "Please answer yes or no! "

------------------------------------------------------

play :: QA -> IO QA
play (QorN name) =
  do
    ansbool <- yesNoQuestion ("My guess: Is it " ++ name ++ "? ")
    if ansbool 
    then
      do
        putStrLn "Hurray! I won!"
        playagain <- yesNoQuestion ("Play again? ")
        if playagain 
          then
            do
              qatext <- readFile "famous.qa.txt"
              return $ getQA qatext
          else
            do
              putStrLn "Bye!"
              return $ getQA "(QorN \"exit\")"
    else
      do
        putStrLn "OK - you won this time."
        putStr "Just curious: Who was your famous person? "
        newperson <- getLine
        putStrLn ("Give me a question for which the answer for "
                  ++ newperson ++ " is \"yes\" and the answer for "
                  ++ name ++      " is \"no\".")
        newquestion <- getLine
        return $ getQA "(QorN \"exit\")"


play (Tree (QorN question) qa1 qa2) =
  do
    ansbool <- yesNoQuestion question
    if ansbool then play qa1 else play qa2  
  

------------------------------------------------------

numQ :: QA -> Int
numQ (QorN _)                = 0
numQ (Tree (QorN _) qa1 qa2) = 1 + numQ(qa1) + numQ(qa2)


------------------------------------------------------

getQA :: String -> QA
getQA qatext = read qatext :: QA


------------------------------------------------------

alterFile :: QA -> IO ()
alterFile newtree = undefined



------------------------------------------------------

main :: IO ()
main =
  do
    putStrLn "Think of a famous person! I will ask you questions about her."
    qatext <- readFile "famous.qa.txt"
    newtree <- play $ getQA qatext
    if (numQ newtree == 0) 
      then 
        return ()
      else
        main

-- YOU DON'T KNOW WHERE I'VE BEEN, LOU