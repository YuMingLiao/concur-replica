{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Concur.Core
import Concur.Replica hiding (i)
import Data.Text
import Prelude hiding (div)
import "string-interpolate" Data.String.Interpolate
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
main = runDefault 8080 "Script" app

app :: Widget HTML ()
app = do
  let domID = "map" :: Text
      script' = [i|var map = L.map('map').setView([51.505, -0.09], 13);|] :: Text 
  div [] [counter 0, text script', script [] [text script' ]]
