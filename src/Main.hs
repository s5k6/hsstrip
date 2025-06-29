module Main ( main ) where

import Data.Text (Text)



newtype Msg = MkMsg { unMsg :: Text }
  deriving Show



main :: IO ()

main = print $ MkMsg "This is hsstrip."
