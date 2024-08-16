
{-# LANGUAGE OverloadedStrings #-}

module Email
  ( Email(..)
  , Address(..)
  , fromText
  ) where

import qualified Data.Text as T
import           Data.Text.Lazy.Builder (fromText)  -- Import fromText
import           Network.Mail.Mime (Address(..))

data Email = Email
  { emailFrom    :: Address
  , emailTo      :: [Address]
  , emailCc      :: [Address]
  , emailBcc     :: [Address]
  , emailSubject :: T.Text
  , emailBody    :: T.Text
  , emailAttachments :: [(T.Text, FilePath)]
  }
