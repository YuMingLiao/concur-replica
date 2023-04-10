{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Core
import Concur.Replica
import Data.Text
import Prelude hiding (div)

counter :: Int -> Widget HTML a
counter x = do
  click <- div []
    [ Left  <$> div [ onClick ] [ text "-" ]
    , text $ pack $ show x
    , Right <$> div [ onClick ] [ text "+" ]
    ]

  case click of
    Left _  -> counter (x - 1)
    Right _ -> counter (x + 1)

main :: IO ()
main = runDefault 8080 "Counter" (counter 0)
