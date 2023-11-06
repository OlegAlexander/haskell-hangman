{-# LANGUAGE RecordWildCards #-}

module Main where


import           Control.Exception (finally)
import           Data.Char         (toUpper)
import qualified Data.Set          as Set
import           Pics              (pics, showPic)
import           System.IO         (hSetEcho, stdin)


data Outcome = Lose | Win | AlreadyGuessed | GoodGuess | BadGuess deriving Show


data GameState = GameState {
     secretWord  :: String,
     goodGuesses :: Set.Set Char,
     badGuesses  :: Set.Set Char
   } deriving Show


getOutcome :: GameState -> Char -> Outcome
getOutcome GameState {..} guess
    | guess `Set.member` (badGuesses `Set.union` goodGuesses) = AlreadyGuessed
    | Set.insert guess goodGuesses == Set.fromList secretWord = Win
    | not (guess `Set.member` Set.fromList secretWord)
        && Set.size badGuesses + 1 == length pics - 1 = Lose
    | guess `Set.member` Set.fromList secretWord = GoodGuess
    | otherwise = BadGuess


match :: String -> String -> String
match secretWord goodGuesses =
    map (\c -> if c `elem` goodGuesses then c else '-') secretWord


showState :: GameState -> IO ()
showState GameState {..} = do
    putStrLn ""
    putStrLn $ showPic $ Set.size badGuesses
    putStrLn $ match secretWord (Set.toList goodGuesses) ++ "  |  " ++ Set.toList badGuesses


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


getGuess :: IO Char
getGuess = do
    putStrLn "\nGuess a letter: "
    guess <- safeHead . map toUpper <$> getLine
    case guess of
        Nothing -> do
            putStrLn "Invalid input."
            getGuess
        Just c -> return c


play :: GameState -> IO ()
play gameState@GameState {..} = do
    guess <- getGuess
    let outcome = getOutcome gameState guess
    case outcome of
        Lose -> do
            showState gameState {badGuesses = Set.insert guess badGuesses}
            putStrLn "\nYou lose!"
            putStrLn $ "The word was: " ++ secretWord
        Win -> do
            showState gameState {goodGuesses = Set.insert guess goodGuesses}
            putStrLn "\nYou win!"
        AlreadyGuessed -> do
            showState gameState
            putStrLn "\nYou already guessed that letter."
            play gameState
        GoodGuess -> do
            showState gameState {goodGuesses = Set.insert guess goodGuesses}
            putStrLn "\nCorrect."
            play gameState {goodGuesses = Set.insert guess goodGuesses}
        BadGuess -> do
            showState gameState{badGuesses = Set.insert guess badGuesses}
            putStrLn "\nIncorrect."
            play gameState {badGuesses = Set.insert guess badGuesses}


-- Source: https://stackoverflow.com/a/62317066
withoutEcho :: IO a -> IO a
withoutEcho action = finally (hSetEcho stdin False >> action) (hSetEcho stdin True)


main :: IO ()
main = do
    putStrLn "\n===HANGMAN==="
    putStrLn "\nPlayer 1: Enter a word: "
    secretWord <- map toUpper <$> withoutEcho getLine
    putStrLn "\nPlayer 2: Try to guess it!"
    play GameState {secretWord=secretWord, goodGuesses=Set.empty, badGuesses=Set.empty}
