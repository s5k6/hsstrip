module Main ( main ) where

import System.IO
import System.Environment
import Data.Char



{- The boolean indicates whether we want to move to the next input
character, or not.  Below, this is captured by the two functions
`next` and `stay`. -}

type Transition = State -> Char -> (Bool, State, String)


{- This describes the action to perform in a certain situation: Move to
next character, or stay.  Argument is the next state to transition to,
and the output string to generate. -}

next, stay :: State -> String -> (Bool, State, String)

next = (,,) True
stay = (,,) False



{- Definition of the DFA: States and transition function. -}

data State
  = Start Bool
  | StringLit | StringLitEsc | StringGap
  | CharLit | CharLitEsc
  | Minus
  | EolComment
  | RangeComment | RangeCommentEnd
  | Brace
  deriving (Eq, Show)


transition :: Transition

transition = \case

  Start primeIsLetter -> \case
    '-' -> next Minus []
    '"' -> next StringLit "\""
    '\'' -> if primeIsLetter
            then next (Start True) "'"
            else next CharLit "'"
    '{' -> next Brace []
    x -> next (Start $ isAlphaNum x) [x]

  Brace -> \case
    '-' -> next RangeComment []
    _ -> stay (Start False) "{"

  Minus -> \case
    '-' -> next EolComment []
    _ -> stay (Start False) "-"

  EolComment -> \case
    '\n' -> next (Start False) "\n"
    _ -> next EolComment ""

  StringLit -> \case
    '"' -> next (Start False) "\""
    '\\' -> next StringLitEsc "\\"
    x -> next StringLit [x]

  StringLitEsc -> \case
    x | isSpace x -> next StringGap [x]
      | otherwise -> next StringLit [x]

  StringGap -> \case
    '\\'          -> next StringLit ['\\']
    x | isSpace x -> next StringGap [x]
      | otherwise ->
          error $ "Unexpected character " <> show x <> " in StringGap"

  CharLit -> \case
    '\'' -> next (Start False) "'"
    '\\' -> next CharLitEsc "\\"
    x -> next CharLit [x]

  CharLitEsc -> \case
    x -> next CharLit [x]

  RangeComment -> \case
    '-' -> next RangeCommentEnd []
    _ -> next RangeComment []

  RangeCommentEnd -> \case
    '}' -> next (Start False) []
    _ -> stay RangeComment []




{- DFA processing function. -}

dfa :: Transition -> State -> String -> String

dfa t = go
  where
    go (Start _) [] = []

    go s [] = error $ "End of input in state " <> show s

    go s (x:xs) = out ++ go s' (if taken then xs else x:xs)
      where
        (taken, s', out) = t s x



main :: IO ()

main = do
  getArgs >>= \case
    [] -> putStrLn
      "\n\
      \Synopsis:\n\
      \\n\
      \    hsstrip FILE...\n\
      \\n\
      \Strips comments, empty lines and trailing spaces from given FILEs,\n\
      \writes to stdout.\n\
      \\n\
      \Example: Print line count and length of longest line in project:\n\
      \\n\
      \    hsstrip src/**/*hs | wc -lL\n\
      \\n\
      \in bash with `shopt globstar` on.\n"

    xs -> mapM_ doOneFile xs



doOneFile :: FilePath -> IO ()

doOneFile fp =
  withFile fp ReadMode $ \fh -> hGetContents fh >>= putStr . strip



strip :: String -> String

strip =
  unlines
  .
  filter (not . null)
  .
  map (reverse . dropWhile isSpace . reverse)
  .
  lines
  .
  dfa transition (Start False)
