{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Text (Text)
import Servant.API
import Servant
import Models

type EmailAPI = 
       Get '[JSON] Text
  :<|> "email" :> Get '[JSON] [Email] --get all
  :<|> "email" :> ReqBody '[JSON] Email :> Post '[JSON] Text -- create
  :<|> "email" :> Capture "id" Text :> Get '[JSON] Email -- get
  :<|> "email" :> Capture "id" Text :> ReqBody '[JSON] Email :> Put '[JSON] Email --update
  :<|> "email" :> Capture "id" Text :> Delete '[JSON] NoContent -- get
  :<|> Raw --any url

emailAPI :: Proxy EmailAPI
emailAPI = Proxy
