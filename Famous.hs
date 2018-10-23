-- Authors: Nicklas Botö, Adam Ryden, Carl Wiede

-- Lab 4 B: Famous

module Famous where

import Prelude hiding (readFile)           -- hiding the Prelude version
import System.IO hiding (readFile)         -- of "readFile" allows the
import System.IO.Strict (readFile)         -- importing of the strict
import System.IO.Error                     -- version, fixing an IO
import Control.Exception                   -- error that was generated.
import Control.Monad
                   
-- (install cabal strict)

-- known bug:
    -- inputting quotation marks when you submit
    -- an additional question/name to the tree
    -- will cause a recurring error parse error.
    -- (USE SINGLE QUOTES=)

------------------------------------------------------

-- data QA can be a (Name, Question) = QorN or a Tree
-- This allows you to use it as a name suggestion
-- or a question with two outcomes (yes or no).
-- Deriving show for storing the QA in a file 
-- and read for conversion String -> QA

data QA = QorN String
        | Tree QA QA QA
  deriving (Read)

------------------------------------------------------

-- Simple instance of Show QA to make it somewhat more bearable

instance Show QA where
  show (QorN n)              = show n
  show (Tree q qa1 qa2)      = "Question: " ++ show q ++ ": (" ++ show qa1 ++ ") (" ++ show qa2 ++ ")"

------------------------------------------------------

-- The "base case" QA according to the assignment

defaultQA :: QA
defaultQA = Tree (QorN "Is she from Europe?") 

                   (Tree (QorN "Is she a scientist?") 
                         (QorN "Marie Curie") 
                         (QorN "Queen Elisabeth II"))

                   (Tree (QorN "Is she an actress?") 
                         (QorN "Marilyn Monroe") 
                         (QorN "Hillary Clinton"))

------------------------------------------------------

-- question takes an input string and outputs
-- the answer typed by the user

question :: String -> IO String
question q = 
  do
    putStr (q ++ " ")
    hFlush stdout
    getLine


------------------------------------------------------

-- yesNoQuestion takes a question and allows the user
-- to answer "yes" or "no" resulting in a True or False
-- IO Bool. If any other string is input, the user
-- is to answer again.

yesNoQuestion :: String -> IO Bool
yesNoQuestion q =
  do
    ans <- question q
    case ans of
        "yes" -> return True
        "no"  -> return False
        _     -> yesNoQuestion "Please answer yes or no!"

------------------------------------------------------

-- When a tree is input to play, it will simply move
-- further down to the QA according to the yes/no
-- answer. When it reaches a name, the computer will
-- either win or not. If not, you can input the new
-- name and the question that distinguishes it with
-- the guess. If yes, you will simply be able to play
-- again or not.

play :: QA -> IO QA
play (QorN name) =
  do
    ansbool <- yesNoQuestion ("My guess: Is it " ++ name ++ "?")
    if ansbool 
    then
      do
        putStrLn "Hurray! I won!"
        playAgain
    else
      do
        putStrLn "OK - you won this time."
        newname <- question "Just curious: Who was your famous person?"
        newquestion <- question ("Give me a question for which the answer for "
                                ++ newname ++ " is \"yes\"\nand the answer for "
                                 ++ name ++     " is \"no\".\n")
        currQA <- getCurrQA
        alterFile $ genNewQA currQA (QorN name) (QorN newname) (QorN newquestion)
        playAgain


play (Tree (QorN question) qa1 qa2) =
  do
    ansbool <- yesNoQuestion question
    if ansbool then play qa1 else play qa2  
  

------------------------------------------------------

-- since the user is asked the question to play again
-- twice, it was simplified to one function and is
-- called at the end of both of the finalizing
-- do blocks in the play function

playAgain :: IO QA
playAgain =
    do
      playagain <- yesNoQuestion "Play again?"
      if playagain 
        then
            return (QorN "yay")
        else
          do
            putStrLn "Bye!"
            return $ QorN "exit"

------------------------------------------------------

-- getCurrQA is used to get the currect version of 
-- the play QA

getCurrQA :: IO QA
getCurrQA =
  do
    tryqatext <- try $ readFile "famous.qa"
    case tryqatext of
      Left e -> error $ "Source file error: " ++ ioeGetErrorString e
      Right s -> return $ stringToQA s


------------------------------------------------------

-- stringToQA takes a (hopefully) correct String version
-- of a QA and turns it into an actual QA

stringToQA :: String -> QA
stringToQA qatext = read qatext :: QA


------------------------------------------------------

-- qaToString properly writes a QA to a String so that
-- the eventually altered file can be read properly

qaToString :: QA -> String
qaToString (QorN s)         = "QorN " ++ "\"" ++ s ++ "\""
qaToString (Tree q qa1 qa2) = "Tree (" ++ qaToString q ++ ") "
                                ++ "(" ++ qaToString qa1 ++ ") "
                                ++ "(" ++ qaToString qa2 ++ ")"

------------------------------------------------------

-- alterFile takes the new QA, turns it into a correct
-- String, then overwrites the old "famous.qa" with
-- the updated one

alterFile :: QA -> IO ()
alterFile newtree = writeFile "famous.qa" (qaToString newtree)

------------------------------------------------------

-- genNewQA is used to take the current QA, the name
-- to be altered (oldN) together with the new name and
-- the question that is going to be used for them

genNewQA :: QA -> QA -> QA -> QA -> QA

genNewQA (QorN name) oldN newN newQ
  | show name == show oldN = Tree newQ newN oldN
  | otherwise              = QorN name

genNewQA (Tree q qa1 qa2) oldN newN newQ = Tree q (genNewQA qa1 oldN newN newQ) 
                                                  (genNewQA qa2 oldN newN newQ)


------------------------------------------------------

-- main function of IO() data type, allowing file to be
-- run with the runhaskell command

main :: IO ()
main =
  do
    putStrLn "Think of a famous person! I will ask you questions about her."
    playQA <- getCurrQA
    playagain <- play playQA
    unless show playagain == "\"exit\"" main

-- YOU DON'T KNOW WHERE I'VE BEEN, LOU