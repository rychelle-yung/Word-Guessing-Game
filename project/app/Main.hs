{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import System.Directory (doesFileExist)
import System.Random (randomRIO)
import Control.Applicative 
import Data.Monoid (Sum(..), getSum)
import Control.Monad 
import Data.Char

-- Define the WordData 
data WordData a = WordData 
    { word :: a, 
      difficulty :: String, 
      category :: String 
    }
    deriving (Show)

-- Define the player statistics
data PlayerStats = PlayerStats
    { guessNumber    :: Int,   -- Track the number of guesses 
      correctAnswers :: Int,   -- Track the number of correct answers
      hintsUsed      :: Int    -- Track the number of hints used
    } deriving Show

-- Newtype to keep track of the score
newtype Score = Score { getScore :: Sum Int }
    deriving (Show, Semigroup, Monoid)

-- Instance of Functor for WordData a
instance Functor WordData where
    fmap f (WordData w d c) = WordData (f w) d c

-- Instance of foldable for WordData a
instance Foldable WordData where
    foldMap f (WordData w _ _) = f w

-- Instance of Applicative for WordData a
instance Applicative WordData where
    pure w = WordData w "" ""
    (WordData wf df cf) <*> (WordData w d c) = WordData (wf w) (df <> d) (cf <> c)

-- Instance of Monad for WordData a
instance Monad WordData where
    return = pure
    (WordData w _ _) >>= f = f w

-- Instance of Semigroup for PlayerStats
instance Semigroup PlayerStats where
    (PlayerStats g1 c1 h1) <> (PlayerStats g2 c2 h2) = 
        PlayerStats (g1 + g2) (c1 + c2) (h1 + h2)

-- Instance of Monoid for PlayerStats
instance Monoid PlayerStats where
    mempty = PlayerStats 0 0 0

-- Parse CSV line into WordData
parseWord :: String -> Maybe (WordData String)
parseWord line = case splitOn "," line of
    [w, d, c] -> Just $ WordData w d c
    _         -> Nothing

-- Load words from CSV file
loadWords :: FilePath -> IO (Either String [WordData String])
loadWords filePath = 
    doesFileExist filePath >>= \exists ->
        if not exists
            then return $ Left "File does not exist."
            else lines <$> readFile filePath >>= \content ->
                let wordsList = mapMaybe parseWord content
                in return $ if null wordsList
                            then Left "File is empty or contains invalid data."
                            else Right wordsList

-- Display game instructions
showInstructions :: IO ()
showInstructions = 
    putStrLn (unlines 
        [ "\n=== Welcome to the Word Guessing Game ==="
        , "Instructions:"
        , "1. Start the game by choosing your desired difficulty level."
        , "2. You will have 5 attempts to guess the correct word."
        , "3. Type 'h' to get a hint."
        , "4. Your score will increase for each correct word based on the difficulty level."
        , "   - Easy  : +1 point"
        , "   - Medium: +2 points"
        , "   - Hard  : +3 points"
        , "5. You can play multiple rounds to improve your score."
        , "========================================="
        , "Press Enter to return to the main menu."
        ]) >>
    getLine >> return ()  

-- Start the game
startGame :: Score -> PlayerStats -> [WordData String] -> IO (Score, PlayerStats)
startGame score stats wordsList = 
    putStrLn "\n1. Easy 2. Medium 3. Hard \nChoose difficulty:" >> 
    getLine >>= \level -> 
        let validLevels = ["1", "2", "3"]
            difficultyMultiplier = case level of
                "1" -> 1  -- Easy
                "2" -> 2  -- Medium
                "3" -> 3  -- Hard
                _   -> 1  -- Default to Easy if invalid input
            
            levelNames = ["easy", "medium", "hard"]
            difficulty' = levelNames !! (read level - 1)  
            filteredWords = filter (\wd -> difficulty wd == difficulty') wordsList

        in if level `elem` validLevels
            then handleFilteredWords filteredWords difficultyMultiplier score stats
            else putStrLn "Invalid difficulty level. Please enter 1, 2, or 3." *> 
                 startGame score stats wordsList

-- Select a word randomly
selectWord :: [WordData String] -> Score -> IO (Maybe (WordData String))
selectWord wordsList _ = 
    if null wordsList 
        then return Nothing
        else fmap (\index -> Just (wordsList !! index)) (randomRIO (0, length wordsList - 1))

-- Handle filtered words and word selection
handleFilteredWords :: [WordData String] -> Int -> Score -> PlayerStats -> IO (Score, PlayerStats)
handleFilteredWords [] _ score stats = 
    putStrLn "No words available for this difficulty. Please choose another difficulty." >>
    startGame score stats []  -- Prompt user to select a different difficulty
handleFilteredWords filteredWords difficultyMultiplier score stats = 
    selectWord filteredWords score >>= \wordChoice ->
    case wordChoice of
        Nothing -> putStrLn "Something went wrong with word selection." >> 
            return (score, stats)
        Just selectedWord -> 
            playRound selectedWord stats >>= \(correct, newStats) -> 
                let newScore = if correct 
                               then score <> Score (Sum difficultyMultiplier) 
                               else score
                in putStrLn ("Your current score is " ++ show (getSum $ getScore newScore)) >> 
                   return (newScore, newStats)

-- Define the maximum number of attempts as a constant 
maxAttempts :: Int
maxAttempts = 5

-- Play a single round
playRound :: WordData String -> PlayerStats -> IO (Bool, PlayerStats)
playRound wd stats = gameLoop maxAttempts [hintCategory, hintStartsWith, hintContainsLetter] wd stats

-- Game loop
gameLoop :: Int -> [WordData String -> String] -> WordData String -> PlayerStats -> IO (Bool, PlayerStats)
gameLoop 0 _ wordData playerStats = 
    putStrLn ("Out of attempts! The word was " ++ word wordData) *> 
    return (False, playerStats)

gameLoop attempts hints wordData playerStats =
    putStrLn ("\nAttempts left: " ++ show attempts) *>
    putStrLn "Enter your guess (or type 'h' for a hint): " *>
    getLine >>= \input -> 
        case input of
            "h" -> handleHint hints wordData playerStats attempts
            _   -> handleGuess input wordData playerStats attempts hints

-- Handle guess input
handleGuess :: String -> WordData String -> PlayerStats -> Int -> [WordData String -> String] -> IO (Bool, PlayerStats)
handleGuess input wordData playerStats attempts hints
    | null input = 
        putStrLn "Guess cannot be empty. Please enter a valid guess." *> 
        gameLoop attempts hints wordData playerStats
    | not (all isAlpha input) = 
        putStrLn "Guess must contain only letters. Please enter a valid guess." *> 
        gameLoop attempts hints wordData playerStats
    | length input < 3 =
        putStrLn "Guess must be at least 3 letters long. Please enter a valid guess." *> 
        gameLoop attempts hints wordData playerStats
    | otherwise = 
        let guessCorrect = input == word wordData
            newStats = updateStats playerStats True guessCorrect
        in if guessCorrect
            then putStrLn "\nCongratulations, you've guessed the word!" *> 
                return (True, newStats)
            else putStrLn ("\nIncorrect guess. You have " ++ show (attempts - 1) ++ " attempts left.") *> 
                gameLoop (attempts - 1) hints wordData newStats

-- Handle hint request
handleHint :: [WordData String -> String] -> WordData String -> PlayerStats -> Int -> IO (Bool, PlayerStats)
handleHint [] wordData playerStats attempts = 
    putStrLn "No more hints available. Continue guessing." *> 
    gameLoop attempts [] wordData playerStats  

handleHint (h:hs) wordData playerStats attempts = do
    putStrLn ("Hint: " ++ h wordData)
    let updatedStats = updateStats playerStats False False
    gameLoop attempts hs wordData updatedStats

-- Hint functions
-- Hint for the category of the word
hintCategory :: WordData String -> String
hintCategory wordData = category wordData

-- Hint for the first letter of the word
hintStartsWith :: WordData String -> String
hintStartsWith wordData = "The word starts with " ++ [head $ word wordData]

-- Hint for a letter that is contained in the word
hintContainsLetter :: WordData String -> String
hintContainsLetter wordData = "The word contains letter " ++ [word wordData !! 1]

-- Continue game after a round
contGame :: Score -> PlayerStats -> [WordData String] -> IO ()
contGame score stats wordsList = 
    putStrLn "Press any key to continue playing or 'q' to quit to the main menu: " >>
    getLine >>= \response -> 
        if response == "q"
            then mainMenuLoop score stats wordsList  -- Return to the main menu
            else startGame score stats wordsList >>= \(newScore, newStats) -> contGame newScore newStats wordsList

-- Display the current score
showScore :: Score -> IO ()
showScore score =
    putStrLn "\n=== Score Board ===" *> 
    foldMap (putStrLn . ("Total Score: " ++) . show . getSum . getScore) [score]
    
-- Update player statistics
updateStats :: PlayerStats -> Bool -> Bool -> PlayerStats
updateStats stats guessMade correct = PlayerStats
    { guessNumber    = guessNumber stats + (if guessMade then 1 else 0)    
    , correctAnswers = correctAnswers stats + (if correct then 1 else 0)   
    , hintsUsed      = hintsUsed stats + (if not guessMade then 1 else 0)  
    }

-- Display player statistics 
showPlayerStats :: PlayerStats -> IO ()
showPlayerStats stats =
    putStrLn "\n=== Player Statistics ===" >>
    putStrLn ("Guesses Made   : " ++ show (guessNumber stats)) >>
    putStrLn ("Correct Answers: " ++ show (correctAnswers stats)) >>
    putStrLn ("Hints Used     : " ++ show (hintsUsed stats)) >>
    putStrLn "\nPress Enter to return to the main menu." >>
    getLine >> return ()

-- Main menu
mainMenuLoop :: Score -> PlayerStats -> [WordData String] -> IO ()
mainMenuLoop score stats wordsList =
    putStrLn "\n=== Main Menu ===" >>
    putStrLn "1. View Instructions" >>
    putStrLn (gameOptionText score) >>
    putStrLn "3. Score Board and Stats" >>
    putStrLn "4. Reset Score and Stats" >>
    putStrLn "5. Exit" >>
    putStrLn "\nChoose an option: " >>
    
    getLine >>= \option ->
    menuOption option score stats wordsList

-- Handle different menu options based on user input
menuOption :: String -> Score -> PlayerStats -> [WordData String] -> IO ()
menuOption "1" score stats wordsList = 
    showInstructions *> 
    mainMenuLoop score stats wordsList
menuOption "2" score stats wordsList = 
    let gameAction = if getSum (getScore score) > 0 
                     then putStrLn "Resuming game..."
                     else putStrLn "Starting a new game..."
    in gameAction >> 
       startGame score stats wordsList >>= \(newScore, newStats) -> 
       contGame newScore newStats wordsList
menuOption "3" score stats wordsList = 
    showScore score *> 
    showPlayerStats stats *> 
    mainMenuLoop score stats wordsList
menuOption "4" _ _ wordsList = 
    let resetScore = Score 0
        resetStats = PlayerStats 0 0 0
    in showScore resetScore >>
       showPlayerStats resetStats >>
       mainMenuLoop resetScore resetStats wordsList
menuOption "5" _ _ _ = putStrLn "Goodbye and see you again! \nExiting..." *>
    return ()
menuOption _ score stats wordsList = 
    putStrLn "Invalid option, please try again." >> 
    mainMenuLoop score stats wordsList

-- Display the appropriate game option text 
gameOptionText :: Score -> String
gameOptionText score
    | getSum (getScore score) > 0 = "2. Resume Game"
    | otherwise = "2. Start Game"

-- Main function
main :: IO ()
main = 
    putStrLn "Loading words..." >>
    loadWords "words.csv" >>= \result ->
        case result of
            Left err       -> putStrLn $ "Error: " ++ err
            Right wordsList -> mainMenuLoop (Score (Sum 0)) mempty wordsList


-- ghci -package directory
-- :set -package random
-- :l app/Main.hs
-- main