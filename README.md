# Haskell Hangman

My very first Haskell program! Constructive feedback is welcome.

## To run

```
git clone https://github.com/OlegAlexander/haskell-hangman.git
cd haskell-hangman
cabal run
```

## Example run

```

===HANGMAN===

Player 1: Enter a word:

Player 2: Try to guess it!

Guess a letter:
e







=========

-E--  |

Correct.

Guess a letter:
s

      +
      |
      |
      |
      |
      |
=========

-E--  |  S

Incorrect.

Guess a letter:
t

  +---+
      |
      |
      |
      |
      |
=========

-E--  |  ST

Incorrect.

Guess a letter:
a

  +---+
      |
      |
      |
      |
      |
=========

-EA-  |  ST

Correct.

Guess a letter:
r

  +---+
      |
      |
      |
      |
      |
=========

-EAR  |  ST

Correct.

Guess a letter:
l

  +---+
  |   |
      |
      |
      |
      |
=========

-EAR  |  LST

Incorrect.

Guess a letter:
s

  +---+
  |   |
      |
      |
      |
      |
=========

-EAR  |  LST

You already guessed that letter.

Guess a letter:
g

  +---+
  |   |
  O   |
      |
      |
      |
=========

-EAR  |  GLST

Incorrect.

Guess a letter:
j

  +---+
  |   |
  O   |
  |   |
      |
      |
=========

-EAR  |  GJLST

Incorrect.

Guess a letter:
k

  +---+
  |   |
  O   |
 /|   |
      |
      |
=========

-EAR  |  GJKLST

Incorrect.

Guess a letter:
i

  +---+
  |   |
  O   |
 /|\  |
      |
      |
=========

-EAR  |  GIJKLST

Incorrect.

Guess a letter:
o

  +---+
  |   |
  O   |
 /|\  |
 /    |
      |
=========

-EAR  |  GIJKLOST

Incorrect.

Guess a letter:
b

  +---+
  |   |
  O   |
 /|\  |
 /    |
      |
=========

BEAR  |  GIJKLOST

You win!
```