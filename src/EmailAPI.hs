{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module EmailAPI where

import Data.Text (Text)
import Servant
import Types

type EmailAPI = 
       Get '[JSON] Text
  :<|> "email" :> Get '[JSON] [Email]
  :<|> "email" :> ReqBody '[JSON] Email :> Post '[JSON] Text
  :<|> "email" :> Capture "id" Text :> Get '[JSON] Email

emailAPI :: Proxy EmailAPI
emailAPI = Proxy
