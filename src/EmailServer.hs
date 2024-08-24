{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module EmailServer where

import           Control.Monad.IO.Class (liftIO)
import           Data.Bson              ((=:))
import qualified Data.Text              as T
import qualified Database.MongoDB       as MongoDB
import           EmailAPI
import           Servant
import Data.Maybe (mapMaybe)

import Serializers (bsonToEmail, emailToBson)
import           Types
-- import Data.Aeson (encode)
-- import qualified Data.ByteString.Lazy        as BL

type EmailM = MongoDB.Action IO

runMongoDB :: MongoDB.Pipe -> EmailM a -> Handler a
runMongoDB pipe action = liftIO $ MongoDB.access pipe MongoDB.master "email_database" action

emailServer :: MongoDB.Pipe -> Server EmailAPI
emailServer pipe = root :<|> listEmails :<|> storeEmail :<|> retrieveEmail
  where

    root :: Handler T.Text
    root = return "API is working"

    listEmails :: Handler [Email]
    listEmails = runMongoDB pipe $ do
      docs <- MongoDB.find (MongoDB.select [] "emails") >>= MongoDB.rest
      return $ mapMaybe bsonToEmail docs

    storeEmail :: Email -> Handler T.Text
    storeEmail email = runMongoDB pipe $ do
      -- liftIO $ BL.putStr $ encode email 
      oid <- MongoDB.insert "emails" (emailToBson email)
      return $ T.pack (show oid)

    retrieveEmail :: T.Text -> Handler Email
    retrieveEmail idText = do
      let oid = read (T.unpack idText) :: MongoDB.ObjectId
      doc <- runMongoDB pipe $ MongoDB.findOne (MongoDB.select ["_id" =: oid] "emails")
      case doc of
        Just d -> case bsonToEmail d of
          Just e -> return e
          Nothing -> throwError (err422 {errBody = "Email is not stored in proper format."})
        Nothing -> throwError (err404 {errBody = "Email isn't there, please leave this server alone."})


