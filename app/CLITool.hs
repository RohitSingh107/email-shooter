{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs, getEnv )
import Configuration.Dotenv (loadFile, defaultConfig)
import Utils (processEmail)
import           Network.HaskellNet.SMTP     (authenticate, closeSMTP)
import           Network.HaskellNet.SMTP.SSL (connectSMTPSSL)
import           System.Exit                 (die)
import           Network.HaskellNet.Auth     (AuthType (LOGIN))

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
    args <- getArgs
    case args of
        -- [filePath] -> sendEmailFromCli filePath
        [emailName] -> do

            loadFile defaultConfig

            password <- getEnv "PASS"
            conn <- connectSMTPSSL "smtp.gmail.com"
            authSucceed <- authenticate LOGIN "rohitsingh.mait@gmail.com" password conn
            if authSucceed
              then putStrLn "Gmail Authenticated"
              else die "Authentication failed."

            processEmail conn "/home/rohits/mydata/code/git_repos/email-shooter/emails" emailName
            -- processEmail conn "/home/rohits/mydata/code/git_repos/email-shooter/emails" emailName
            -- processEmail conn "/home/rohits/mydata/code/git_repos/email-shooter/emails" emailName
            -- processEmail conn "/home/rohits/mydata/code/git_repos/email-shooter/emails" emailName
            putStrLn "Email sent successfully!"

            closeSMTP conn

        _ -> putStrLn "Usage: cli-tool <Email name>"

