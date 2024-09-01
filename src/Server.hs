{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Server where

import           API
import           Control.Monad.IO.Class  (liftIO)
import           Data.Bson               ((=:))
import           Data.Maybe              (mapMaybe)
import qualified Data.Text               as T
import qualified Database.MongoDB        as MongoDB
import           Servant

import           Data.Aeson              (encode)
import           Network.HaskellNet.SMTP (SMTPConnection)
import           Network.HTTP.Types      (status404)
import           Network.Wai             (responseLBS)
import           Serializers             (bsonToEmail, emailToBson)
-- import qualified Data.ByteString.Lazy        as BL
import           Models

import           Utils                   (getEmailList, sendEMails)

type EmailM = MongoDB.Action IO

runMongoDB :: MongoDB.Pipe -> EmailM a -> Handler a
runMongoDB pipe action = liftIO $ MongoDB.access pipe MongoDB.master "email_database" action

emailServer :: MongoDB.Pipe -> SMTPConnection -> Server EmailAPI
emailServer pipe conn = root :<|> getEmails :<|> createEmail :<|> getEmail :<|> updateEmail :<|> deleteEmail :<|> sendEmails :<|> notFoundHandler
  where

    root :: Handler T.Text
    root = return "API is working"

    getEmails :: Handler [Email]
    getEmails = runMongoDB pipe $ do
      docs <- MongoDB.find (MongoDB.select [] "emails") >>= MongoDB.rest
      return $ mapMaybe bsonToEmail docs

    getEmail :: T.Text -> Handler Email
    getEmail idText = do
      let oid = read (T.unpack idText) :: MongoDB.ObjectId
      doc <- runMongoDB pipe $ MongoDB.findOne (MongoDB.select ["_id" =: oid] "emails")
      case doc of
        Just d -> case bsonToEmail d of
          Just e -> return e
          Nothing -> throwError (err422 {errBody = "Email is not stored in proper format."})
        Nothing -> throwError (err404 {errBody =  encode $ NotFound "Email isn't there, please leave this server alone."})

    createEmail :: Email -> Handler T.Text
    createEmail email = runMongoDB pipe $ do
      -- liftIO $ BL.putStr $ encode email
      oid <- MongoDB.insert "emails" (emailToBson email)
      return $ T.pack (show oid)

    updateEmail :: T.Text -> Email -> Handler Email
    updateEmail idText newEmail = do
        let oid = read (T.unpack idText) :: MongoDB.ObjectId
        maybeDoc <- runMongoDB pipe $ MongoDB.findOne (MongoDB.select ["_id" =: oid] "emails")
        case maybeDoc of
            Nothing -> throwError err404 { errBody =  encode $ NotFound "Email not found." }
            Just _  -> do
                runMongoDB pipe $ MongoDB.replace (MongoDB.select ["_id" =: oid] "emails") (emailToBson newEmail)
                return $ newEmail {_id = Just idText}

    deleteEmail :: T.Text -> Handler NoContent
    deleteEmail idText = do
        let oid = read (T.unpack idText) :: MongoDB.ObjectId
        maybeDoc <- runMongoDB pipe $ MongoDB.findOne (MongoDB.select ["_id" =: oid] "emails")
        case maybeDoc of
            Nothing -> throwError err404 { errBody = encode $ NotFound "Email not found." }
            Just _  -> do
                runMongoDB pipe $ MongoDB.deleteOne (MongoDB.select ["_id" =: oid] "emails")
                return NoContent

    sendEmails :: SendEmailsRequest -> Handler NoContent
    sendEmails (SendEmailsRequest _emailIds _recipients) = do
      emails <- liftIO $ getEmailList pipe _emailIds _recipients
      liftIO $ sendEMails conn emails
      return NoContent

    notFoundHandler :: Server Raw
    notFoundHandler = Tagged $ \_ res ->
        res $ responseLBS
            status404
            [("Content-Type", "application/json")]
            (encode $ NotFound "The requested resource does not exist.")


