{-# LANGUAGE OverloadedStrings #-}

module Main where

import           API
import qualified Database.MongoDB            as MongoDB
import           Network.Wai.Handler.Warp
import           Servant
import           Server

import           Configuration.Dotenv        (defaultConfig, loadFile)
import           Network.HaskellNet.Auth     (AuthType (LOGIN))
import           Network.HaskellNet.SMTP     (SMTPConnection, authenticate,
                                              closeSMTP)
import           Network.HaskellNet.SMTP.SSL (connectSMTPSSL)
import           System.Environment          (getEnv)
import           System.Exit                 (die)

app :: MongoDB.Pipe -> SMTPConnection -> Application
app pipe conn = serve emailAPI (emailServer pipe conn)

main :: IO ()
main = do
  pipe <- MongoDB.connect (MongoDB.host "localhost")

  loadFile defaultConfig

  password <- getEnv "PASS"
  conn <- connectSMTPSSL "smtp.gmail.com"
  authSucceed <- authenticate LOGIN "rohitsingh.mait@gmail.com" password conn
  if authSucceed
    then putStrLn "Gmail Authenticated"
    else die "Authentication failed."

  putStrLn "Running the server on port 8080..."
  run 8080 $ app pipe conn

  closeSMTP conn
