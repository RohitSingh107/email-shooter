{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Database.MongoDB         as MongoDB
import           API
import           Server
import           Network.Wai.Handler.Warp
import           Servant

app :: MongoDB.Pipe -> Application
app pipe = serve emailAPI (emailServer pipe)

main :: IO ()
main = do
  pipe <- MongoDB.connect (MongoDB.host "localhost")
  putStrLn "Running the server on port 8080..."
  run 8080 $ app pipe
