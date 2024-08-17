{-# LANGUAGE OverloadedStrings #-}

module EmailSender
  ( sendEmail
  , processEmail
  , formatAddress
  , formatAttachment
  ) where

import           Configuration.Dotenv        (loadFile, defaultConfig)
import           Data.MIME.Types             (defaultmtd, guessType)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.IO                as TIO
import           Network.HaskellNet.Auth     (AuthType (LOGIN))
import           Network.HaskellNet.SMTP     (authenticate, closeSMTP, sendMail)
import           Network.HaskellNet.SMTP.SSL (connectSMTPSSL)
import           Network.Mail.Mime           (addAttachments, plainPart, Address(..), Mail(..))
import           System.IO                   (openFile, hSetBuffering, hClose, IOMode (ReadMode), BufferMode (BlockBuffering))
import           System.Environment          (getEnv)
import           System.Exit                 (die)

import           Types

formatAddress :: T.Text -> Address
formatAddress s = Address {addressEmail = s, addressName = Nothing}

formatAttachment :: String -> T.Text -> (T.Text, FilePath)
formatAttachment attDir file = (mimeType, path)
  where
    mimeType = case guessType defaultmtd False (T.unpack file) of
      (Nothing, _) -> "text/plain"
      (Just t, _)  -> T.pack t
    path = attDir <> "/" <> T.unpack file

sendEmail :: Email -> IO ()
sendEmail email = do
  password <- getEnv "PASS"
  let mail = Mail
        { mailFrom = emailFrom email
        , mailTo = emailTo email
        , mailCc = emailCc email
        , mailBcc = emailBcc email
        , mailHeaders = [("Subject", emailSubject email)]
        , mailParts = [[plainPart $ LT.fromStrict $ emailBody email]]
        }
  mailWithAttachments <- addAttachments (emailAttachments email) mail
  conn <- connectSMTPSSL "smtp.gmail.com"
  authSucceed <- authenticate LOGIN "rohitsingh.mait@gmail.com" password conn
  if authSucceed
    then sendMail mailWithAttachments conn
    else die "Authentication failed."
  closeSMTP conn

processEmail :: String -> String -> IO ()
processEmail emailDir email = do
  fh <- openFile (emailDir <> "/" <> email <> ".txt") ReadMode
  hSetBuffering fh (BlockBuffering Nothing)
  
  -- Read the lines from the file using Text.IO functions
  addressLine <- T.words <$> TIO.hGetLine fh
  let addresses = map formatAddress addressLine
  attachmentsLine <- T.words <$> TIO.hGetLine fh
  let attachments = map (formatAttachment "/home/rohits/mydata/code/git_repos/email-sender/attachments/") attachmentsLine
  subject <- TIO.hGetLine fh
  content <- TIO.hGetContents fh

  let email = Email
        { emailFrom = Address Nothing "rohitsingh.mait@gmail.com"
        , emailTo = addresses
        , emailCc = []
        , emailBcc = [Address Nothing "rohitsingh.mait@gmail.com"]
        , emailSubject = subject
        , emailBody = content
        , emailAttachments = attachments
        }
  sendEmail email
  hClose fh
