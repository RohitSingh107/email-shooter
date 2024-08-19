{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Types
  ( Email(..)
  , EmailBody(..)
  , Attachment(..)
  , AttachmentContent(..)
  , EmailAddress(..)
  , fromText
  ) where

import           Data.Aeson
import qualified Data.Text              as T
import           Data.Text.Lazy.Builder (fromText)
import           GHC.Generics


data EmailAddress = EmailAddress
  { emailAddressName  :: Maybe T.Text
  , emailAddressEmail :: T.Text
  } deriving (Show, Eq, Generic)

data EmailBody = EmailBody
  { emailBodyText :: Maybe T.Text
  , emailBodyHtml :: Maybe T.Text
  } deriving (Show, Eq, Generic)

data AttachmentContent
  = AttachmentLocalPath T.Text
  | AttachmentRemoreUri T.Text
  | AttachmentBase64 T.Text
  deriving (Show, Eq, Generic)

data Attachment = Attachment
  { attachmentFilename    :: T.Text
  , attachmentContent     :: AttachmentContent
  , attachmentContentType :: Maybe T.Text
  } deriving (Show, Eq, Generic)

data Email = Email
  { emailFrom        :: EmailAddress
  , emailTo          :: [EmailAddress]
  , emailCc          :: [EmailAddress]
  , emailBcc         :: [EmailAddress]
  , emailSubject     :: T.Text
  , emailBody        ::  EmailBody
  , emailAttachments :: [Attachment]
  } deriving (Show, Eq, Generic)

instance ToJSON EmailAddress where
    toJSON EmailAddress{..} = object
        [ "emailAddressName" .= emailAddressName
        , "emailAddressEmail" .= emailAddressEmail
        ]

instance FromJSON EmailAddress where
    parseJSON = withObject "EmailAddress" $ \v -> EmailAddress
        <$> v .:? "emailAddressName"
        <*> v .: "emailAddressEmail"

instance ToJSON EmailBody
instance FromJSON EmailBody
instance ToJSON AttachmentContent
instance FromJSON AttachmentContent
instance ToJSON Attachment
instance FromJSON Attachment
instance ToJSON Email
instance FromJSON Email
