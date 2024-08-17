{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, object, (.=))
import Network.HTTP.Simple
import Configuration.Dotenv (loadFile, defaultConfig)

import Types (Email(..))
import EmailSender (sendEmail, processEmail)

-- -- Send email using CLI arguments
-- sendEmailFromCli :: FilePath -> IO ()
-- sendEmailFromCli filePath = do
--     emailData <- BL.readFile filePath
--     case decode emailData of
--         Just (EmailRequest to subject body attachments) -> do
--             let email = Email
--                     { emailFrom = Address Nothing "rohitsingh.mait@gmail.com"
--                     , emailTo = map (Address Nothing) to
--                     , emailCc = []
--                     , emailBcc = [Address Nothing "rohitsingh.mait@gmail.com"]
--                     , emailSubject = subject
--                     , emailBody = body
--                     , emailAttachments = map (\fd -> (fileMime fd, BL.toStrict (fileContent fd))) attachments
--                     }
--             sendEmail email
--             putStrLn "Email sent successfully!"
--         Nothing -> putStrLn "Invalid email data."

main :: IO ()
main = do
    loadFile defaultConfig
    args <- getArgs
    case args of
        -- [filePath] -> sendEmailFromCli filePath
        [emailName] -> do
            processEmail "/home/rohits/mydata/code/git_repos/email-shooter/emails" emailName
            putStrLn "Email sent successfully!"
        _ -> putStrLn "Usage: cli-tool <Email name>"

