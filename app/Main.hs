{-# LANGUAGE Strict #-}

module Main where


import           Control.Exception (finally)
import           Data.Char         (toUpper)
import qualified Data.Set          as Set
import           System.IO         (hSetEcho, stdin)



pics :: [[String]]
pics =
    [   [ "       ",
          "       ",
          "       ",
          "       ",
          "       ",
          "       ",
          "========="
        ],
        [ "      +",
          "      |",
          "      |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "      |",
          "      |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "      |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          "  |   |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|   |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|\\  |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|\\  |",
          " /    |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|\\  |",
          " / \\  |",
          "      |",
          "========="
        ]
    ]


data Outcome = Lose | Win | AlreadyGuessed | GoodGuess | BadGuess deriving Show

type SecretWord = String

type GoodGuesses = Set.Set Char

type BadGuesses = Set.Set Char

data GameState = GameState SecretWord GoodGuesses BadGuesses deriving Show


getOutcome :: GameState -> Char -> Outcome
getOutcome (GameState secretWord goodGuesses badGuesses) guess
    | guess `Set.member` (badGuesses `Set.union` goodGuesses) = AlreadyGuessed
    | Set.insert guess goodGuesses == Set.fromList secretWord = Win
    | not (guess `Set.member` Set.fromList secretWord)
        && Set.size badGuesses + 1 == length pics - 1 = Lose
    | guess `Set.member` Set.fromList secretWord = GoodGuess
    | otherwise = BadGuess


showPic :: Int -> String
showPic i = unlines $ pics !! i


match :: Foldable t => [Char] -> t Char -> [Char]
match word goodGuesses = map (\c -> if c `elem` goodGuesses then c else '-') word


showState :: GameState -> IO ()
showState (GameState secretWord goodGuesses badGuesses) = do
    putStrLn ""
    putStrLn $ showPic $ Set.size badGuesses
    putStrLn $ match secretWord goodGuesses ++ "  |  " ++ Set.toList badGuesses


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
play (GameState secretWord goodGuesses badGuesses) = do
    guess <- getGuess
    let outcome = getOutcome (GameState secretWord goodGuesses badGuesses) guess
    case outcome of
        Lose -> do
            showState (GameState secretWord goodGuesses (Set.insert guess badGuesses))
            putStrLn "\nYou lose!"
            putStrLn $ "The word was: " ++ secretWord
        Win -> do
            showState (GameState secretWord (Set.insert guess goodGuesses) badGuesses)
            putStrLn "\nYou win!"
        AlreadyGuessed -> do
            showState (GameState secretWord goodGuesses badGuesses)
            putStrLn "\nYou already guessed that letter."
            play (GameState secretWord goodGuesses badGuesses)
        GoodGuess -> do
            showState (GameState secretWord (Set.insert guess goodGuesses) badGuesses)
            putStrLn "\nCorrect."
            play (GameState secretWord (Set.insert guess goodGuesses) badGuesses)
        BadGuess -> do
            showState (GameState secretWord goodGuesses (Set.insert guess badGuesses))
            putStrLn "\nIncorrect."
            play (GameState secretWord goodGuesses (Set.insert guess badGuesses))


-- Source: https://stackoverflow.com/a/62317066
withoutEcho :: IO a -> IO a
withoutEcho action =
  finally (hSetEcho stdin False >> action) (hSetEcho stdin True)


hangman :: IO ()
hangman = do
    putStrLn "\n===HANGMAN==="
    putStrLn "\nPlayer 1: Enter a word: "
    word <- map toUpper <$> withoutEcho getLine
    putStrLn "\nPlayer 2: Try to guess it!"
    play (GameState word Set.empty Set.empty)


main :: IO ()
main = hangman
