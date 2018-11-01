-- Authors: Nicklas Botö, Adam Ryden, Carl Wiede

-- Lab 4 B: Famous

module Famous where

  import System.IO
  import System.IO.Error              
  import Control.Exception                
                     
  -- (install cabal strict)
  
  -- known bug:
      -- inputting quotation marks when you submit
      -- an additional question/name to the tree
      -- will cause a recurring error parse error.
      -- (USE SINGLE QUOTES=)
  
  ------------------------------------------------------
  
  -- data QA can be a Suggestion of a name or a Tree,
  -- which allows you to use it as a name suggestion
  -- or a question with two outcomes (yes or no).
  -- Deriving show for converting QA -> String
  -- and read for converting String -> QA
  
  data QA = Suggestion Name
          | Tree Question QA QA
    deriving (Read, Show)
  

  type Name     = String
  type Question = String

  ------------------------------------------------------
  
  -- The "base case" QA according to the assignment
  
  defaultQA :: QA
  defaultQA = Tree "Is she from Europe?"
  
                     (Tree "Is she a scientist?" 
                           (Suggestion "Marie Curie") 
                           (Suggestion "Queen Elisabeth II"))
  
                     (Tree "Is she an actress?" 
                           (Suggestion "Marilyn Monroe") 
                           (Suggestion "Hillary Clinton"))
  
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
  -- either win or not. If it wins, the name suggestion
  -- will be returned and the tree will remain unchanged.
  -- If it doesn't, the player will be prompted to submit
  -- the name of the person in mind and a question that
  -- distinguishes her from the suggested one, then the 
  -- tree is changed accordingly.
  
  play :: QA -> IO QA
  play (Suggestion name) =
    do
      ansbool <- yesNoQuestion ("My guess: Is it " ++ name ++ "?")
      if ansbool 
      then 
        do
          putStrLn "Hurray! I won!"
          return (Suggestion name)
      else
        do
          putStrLn "OK - you won this time."
          newname <- question "Just curious: Who was your famous person?"
          newquestion <- question 
                        ("Give me a question for which the answer for "
                        ++ newname ++ " is \"yes\"\nand the answer for "
                        ++ name ++     " is \"no\".\n")
          return (Tree newquestion (Suggestion newname) (Suggestion name))
  
  
  play (Tree q qa1 qa2) =
    do
      ansbool <- yesNoQuestion q
      if ansbool 
        then
          do
            newQa1 <- play qa1
            return (Tree q newQa1 qa2)
        else
          do
            newQa2 <- play qa2
            return (Tree q qa1 newQa2)
    
  
  ------------------------------------------------------
  
  -- getCurrQA is used to get the current version of 
  -- the play QA. If the file does not exist, the
  -- default tree will be used
  
  getCurrQA :: IO QA
  getCurrQA =
    do
      tryqatext <- try $ readFile "famous.qa"
      case tryqatext of
        Left e -> do 
          putStrLn ("Source file not found: " ++ 
                    ioeGetErrorString e ++ ", using default tree...")
          return defaultQA
        Right s -> return $ read s
  
  ------------------------------------------------------
  
  -- main function of IO() data type, allowing file to be
  -- run with the runhaskell command
  
  main :: IO ()
  main =
    do
      putStrLn $ "Think of a famous person! " ++ 
                 "I will ask you questions about her."
      playQA <- getCurrQA
      newQA <- play playQA
      writeFile "famous.qa" (show newQA)
      again <- yesNoQuestion "Play again?"
      if again
        then main
        else putStrLn "Bye!"