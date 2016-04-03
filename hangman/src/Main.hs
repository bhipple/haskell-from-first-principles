module Main where
import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

badGuessLimit :: Int
badGuessLimit = 10

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        intersperse ' ' (fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

main :: IO ()
main = do
    word <- gameWords >>= randomWord
    runGame $ freshPuzzle (fmap toLower word)

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in l > minWordLength && l < maxWordLength

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (replicate (length s) Nothing) ""

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character."

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    when (length guessed > badGuessLimit) $
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    when (all isJust filledInSoFar) $
        do putStrLn "You win!"
           exitSuccess

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guess that character."
            return puzzle
        (True, _) -> do
            putStrLn "Correct!  Updated word accordingly."
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "Sorry, try again!"
            return (fillInCharacter puzzle guess)

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wd _ _) c = c `elem` wd

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) c = c `elem` guesses

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle wd soFar s) c = Puzzle wd newSoFar (c : s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newSoFar :: [Maybe Char]
          newSoFar = zipWith (zipper c) wd soFar


-- TESTING
a = freshPuzzle "foobar"
