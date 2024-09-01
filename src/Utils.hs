{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( sendEmail
  , processEmail
  , formatAddress
  , formatAttachment
  , getEmailList
  , sendEMails
  ) where

-- import           Configuration.Dotenv        (defaultConfig, loadFile)
import           Control.Concurrent.Async (mapConcurrently_)
import           Data.Bson                ((=:))
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe, mapMaybe)
import           Data.MIME.Types          (defaultmtd, guessType)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified Data.Text.Lazy           as LT
import qualified Database.MongoDB         as MongoDB
import           Models
import           Network.HaskellNet.SMTP  (SMTPConnection, sendMail)
import           Network.Mail.Mime        (Address (..), Mail (..),
                                           addAttachments, addAttachmentsBS,
                                           htmlPart, plainPart)
import           Serializers              (bsonToEmail)
import           System.IO                (BufferMode (BlockBuffering),
                                           IOMode (ReadMode), hClose,
                                           hSetBuffering, openFile)

formatAddress :: T.Text -> EmailAddress
formatAddress s = EmailAddress {emailAddressEmail = s, emailAddressName = Nothing}

emailAddressToAddress :: EmailAddress -> Address
emailAddressToAddress (EmailAddress { emailAddressName = n, emailAddressEmail = e }) = Address {addressName=n, addressEmail=e}

formatAttachment :: String -> T.Text -> (T.Text, FilePath)
formatAttachment attDir file = (mimeType, path)
  where
    mimeType = case guessType defaultmtd False (T.unpack file) of
      (Nothing, _) -> "text/plain"
      (Just t, _)  -> T.pack t
    path = attDir <> "/" <> T.unpack file

formatAttachments' :: [Attachment] -> ([(T.Text, FilePath)], [(T.Text, T.Text, BL.ByteString)])
formatAttachments' [] = ([], [])
formatAttachments' ((Attachment fn fc ft):xs) = case fc of
                             AttachmentLocalPath p -> ((fromMaybe "text/plain" ft, T.unpack p) : fst go, snd go)
                             AttachmentRemoreUri u -> undefined
                             AttachmentBase64 b -> undefined
                  where go = formatAttachments' xs

sendEmail :: SMTPConnection -> Email -> IO ()
sendEmail conn email = do
  -- password <- getEnv "PASS"

  let bodyPart = case emailBody email of
                EmailBody (Just textContent) (Just htmlContent) -> [plainPart $ LT.fromStrict textContent, htmlPart $ LT.fromStrict htmlContent]
                EmailBody Nothing (Just htmlContent) -> [htmlPart $ LT.fromStrict htmlContent]
                EmailBody (Just textContent) Nothing -> [plainPart $ LT.fromStrict textContent]
                EmailBody Nothing Nothing -> []
  let mail = Mail
        { mailFrom = emailAddressToAddress $ emailFrom email
        , mailTo = map emailAddressToAddress $ emailTo email
        , mailCc = map emailAddressToAddress $ emailCc email
        , mailBcc = map emailAddressToAddress $ emailBcc email
        , mailHeaders = [("Subject", emailSubject email)]
        , mailParts = [bodyPart]
        }
  let formattedAttachments = formatAttachments' $ emailAttachments email
  mailWithAttachments <- addAttachments (fst formattedAttachments) (addAttachmentsBS (snd formattedAttachments) mail)
  -- conn <- connectSMTPSSL "smtp.gmail.com"
  -- authSucceed <- authenticate LOGIN "rohitsingh.mait@gmail.com" password conn
  -- if authSucceed
  --   then sendMail mailWithAttachments conn
  --   else die "Authentication failed."
  -- closeSMTP conn
  sendMail mailWithAttachments conn

processEmail :: SMTPConnection -> String -> String -> IO ()
processEmail conn emailDir emailName = do
  fh <- openFile (emailDir <> "/" <> emailName <> ".txt") ReadMode
  hSetBuffering fh (BlockBuffering Nothing)

  -- Read the lines from the file using Text.IO functions
  addressLine <- T.words <$> TIO.hGetLine fh
  let addresses = map formatAddress addressLine
  -- attachmentsLine <- T.words <$> TIO.hGetLine fh
  -- let attachments = map (formatAttachment "/home/rohits/mydata/code/git_repos/email-sender/attachments/") attachmentsLine
  subject <- TIO.hGetLine fh
  content <- TIO.hGetContents fh

  let body = EmailBody
        { emailBodyText = Just content
        , emailBodyHtml = Nothing
        }

  let attachments =
        [
          Attachment {
              attachmentFilename = "Resume_H.pdf"
            , attachmentContent = AttachmentLocalPath "/home/rohits/mydata/code/git_repos/email-shooter/attachments/Resume_H.pdf"
            , attachmentContentType = Just "application/pdf"
          }

        ]


  let emailData = Email
        { _id = Nothing
        , emailFrom = EmailAddress Nothing "rohitsingh.mait@gmail.com"
        , emailTo = addresses
        , emailCc = []
        , emailBcc = [EmailAddress Nothing "rohitsingh.mait@gmail.com"]
        , emailSubject = subject
        , emailBody = body
        , emailAttachments = attachments
        }
  -- let jsonString = encode emailData
  -- BL.putStrLn jsonString
  -- BL.putStr jsonString
  sendEmail conn emailData
  hClose fh



idToEmail :: MongoDB.Pipe -> T.Text -> IO (Maybe Email)
idToEmail pipe idText = do
  let oid = read (T.unpack idText) :: MongoDB.ObjectId
  doc <- run $ MongoDB.findOne (MongoDB.select ["_id" =: oid] "emails")
  case doc of
    Just d  -> return $ bsonToEmail d
    Nothing -> return Nothing

  where run = MongoDB.access pipe MongoDB.master "email_database"

getEmailList :: MongoDB.Pipe -> [T.Text] -> [EmailAddress] -> IO [Email]
getEmailList pipe ids rec = do
  results <- mapM (idToEmail pipe) ids  -- Use mapM to handle IO actions
  return (mapMaybe setReceipent results)
  where
    setReceipent (Just e) = if null rec then Just e else Just $ e { emailTo = rec }
    setReceipent Nothing = Nothing


sendEMails :: SMTPConnection -> [Email] -> IO ()
sendEMails conn = mapConcurrently_ $ sendEmail conn



