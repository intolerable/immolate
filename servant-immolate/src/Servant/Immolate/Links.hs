module Servant.Immolate.Links
  ( safeHref_
  , safeAbsHref_
  , safeRelHref_
  , linkHref_
  , linkAbsHref_
  , linkRelHref_
  , addBaseUrl
  ) where

import Data.Text (Text)
import Servant.Links
import Immolate
import Data.Proxy
import Web.HttpApiData
import Data.Text qualified as Text

safeHref_ :: (IsElem endpoint api, HasLink endpoint)
          => Text 
          -> Proxy api 
          -> Proxy endpoint 
          -> MkLink endpoint Attributes
safeHref_ = safeLink' . linkHref_

safeAbsHref_ :: (IsElem endpoint api, HasLink endpoint)
             => Proxy api
             -> Proxy endpoint
             -> MkLink endpoint Attributes
safeAbsHref_ = safeHref_ "/"

safeRelHref_ :: (IsElem endpoint api, HasLink endpoint)
             => Proxy api
             -> Proxy endpoint
             -> MkLink endpoint Attributes
safeRelHref_ = safeHref_ ""

linkHref_ :: Text -> Link -> Attributes
linkHref_ baseUrl = href_ . addBaseUrl baseUrl . toUrlPiece

linkAbsHref_ :: Link -> Attributes
linkAbsHref_ = linkHref_ "/"

linkRelHref_ :: Link -> Attributes
linkRelHref_ = href_ . toUrlPiece

addBaseUrl :: Text -> Text -> Text
addBaseUrl "" path = path
addBaseUrl baseUrl path = 
  if Text.last baseUrl == '/'
    then baseUrl <> path 
    else baseUrl <> "/" <> path