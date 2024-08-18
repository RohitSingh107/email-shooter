module Types
  ( Email(..)
  , EmailBody(..)
  , Attachment(..)
  , AttachmentContent(..)
  , Address(..)
  , fromText
  ) where

import qualified Data.Text              as T
import           Data.Text.Lazy.Builder (fromText)
import           Network.Mail.Mime      (Address (..))

data EmailBody = EmailBody
  { emailBodyText :: Maybe T.Text
  , emailBodyHtml :: Maybe T.Text
  } deriving (Show, Eq)

data AttachmentContent
  = AttachmentLocalPath T.Text
  | AttachmentRemoreUri T.Text
  | AttachmentBase64 T.Text
  deriving (Show, Eq)

data Attachment = Attachment
  { attachmentFilename    :: T.Text
  , attachmentContent     :: AttachmentContent
  , attachmentContentType :: Maybe T.Text
  } deriving (Show, Eq)

data Email = Email
  { emailFrom        :: Address
  , emailTo          :: [Address]
  , emailCc          :: [Address]
  , emailBcc         :: [Address]
  , emailSubject     :: T.Text
  , emailBody        ::  EmailBody
  , emailAttachments :: [Attachment]
  } deriving (Show, Eq)
