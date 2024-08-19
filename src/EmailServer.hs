{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module EmailServer where

import           Control.Monad.IO.Class (liftIO)
import           Data.Bson              (Document, Value (..), (=:))
import qualified Data.Text              as T
import qualified Database.MongoDB       as MongoDB
import           EmailAPI
import           Servant
import Data.Maybe (mapMaybe)
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


-- EmailAddress conversions
emailAddressToBson :: EmailAddress -> Document
emailAddressToBson EmailAddress {..} =
  [ "emailAddressName" =: emailAddressName,
    "emailAddressEmail" =: emailAddressEmail
  ]

bsonToEmailAddress :: Document -> EmailAddress
bsonToEmailAddress doc =
  EmailAddress
    { emailAddressName = MongoDB.lookup "emailAddressName" doc,
      emailAddressEmail = MongoDB.at "emailAddressEmail" doc
    }



-- EmailBody conversions
emailBodyToBson :: EmailBody -> Document
emailBodyToBson EmailBody {..} =
  [ "emailBodyText" =: emailBodyText,
    "emailBodyHtml" =: emailBodyHtml
  ]

bsonToEmailBody :: Document -> EmailBody
bsonToEmailBody doc =
  EmailBody
    { emailBodyText = MongoDB.lookup "emailBodyText" doc,
      emailBodyHtml = MongoDB.lookup "emailBodyHtml" doc
    }

-- AttachmentContent conversions
attachmentContentToBson :: AttachmentContent -> Value
attachmentContentToBson (AttachmentLocalPath path) = Doc ["tag" =: ("AttachmentLocalPath" :: T.Text), "contents" =: path]
attachmentContentToBson (AttachmentRemoreUri uri) = Doc ["tag" =: ("AttachmentRemoreUri" :: T.Text), "contents" =: uri]
attachmentContentToBson (AttachmentBase64 base64) = Doc ["tag" =: ("AttachmentBase64" :: T.Text), "contents" =: base64]

bsonToAttachmentContent :: Value -> Maybe AttachmentContent
bsonToAttachmentContent (Doc doc) = case MongoDB.lookup "tag" doc :: Maybe T.Text of
  Just "AttachmentLocalPath"  -> AttachmentLocalPath <$> MongoDB.lookup "contents" doc
  Just "AttachmentRemoreUri" -> AttachmentRemoreUri <$> MongoDB.lookup "contents" doc
  Just "AttachmentBase64" -> AttachmentBase64 <$> MongoDB.lookup "contents" doc
  _             -> Nothing
bsonToAttachmentContent _ = Nothing

-- Attachment conversions
attachmentToBson :: Attachment -> Document
attachmentToBson Attachment {..} =
  [ "attachmentFilename" =: attachmentFilename,
    "attachmentContent" =: attachmentContentToBson attachmentContent,
    "attachmentContentType" =: attachmentContentType
  ]

bsonToAttachment :: Document -> Maybe Attachment
bsonToAttachment doc = do
  filename <- MongoDB.lookup "attachmentFilename" doc
  content <- MongoDB.lookup "attachmentContent" doc >>= bsonToAttachmentContent
  return
    Attachment
      { attachmentFilename = filename,
        attachmentContent = content,
        attachmentContentType = MongoDB.lookup "attachmentContentType" doc
      }

-- Email conversions
emailToBson :: Email -> Document
emailToBson Email {..} =
  [ "emailFrom" =: emailAddressToBson emailFrom,
    "emailTo" =: map emailAddressToBson emailTo,
    "emailCc" =: map emailAddressToBson emailCc,
    "emailBcc" =: map emailAddressToBson emailBcc,
    "emailSubject" =: emailSubject,
    "emailBody" =: emailBodyToBson emailBody,
    "emailAttachments" =: map attachmentToBson emailAttachments
  ]

bsonToEmail :: Document -> Maybe Email
bsonToEmail doc = do
  from <- bsonToEmailAddress <$> MongoDB.lookup "emailFrom" doc
  to <- (bsonToEmailAddress <$>) <$> MongoDB.lookup "emailTo" doc
  cc <- (bsonToEmailAddress <$>) <$> MongoDB.lookup "emailCc" doc
  bcc <- (bsonToEmailAddress <$>) <$> MongoDB.lookup "emailBcc" doc
  subject <- MongoDB.lookup "emailSubject" doc
  body <- bsonToEmailBody <$> MongoDB.lookup "emailBody" doc
  attachments <- mapM bsonToAttachment =<< MongoDB.lookup "emailAttachments" doc
  return
    Email
      { emailFrom = from,
        emailTo = to,
        emailCc = cc,
        emailBcc = bcc,
        emailSubject = subject,
        emailBody = body,
        emailAttachments = attachments
      }
