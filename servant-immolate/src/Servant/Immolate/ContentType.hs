module Servant.Immolate.ContentType
  ( HTML
  ) where

import Immolate
import Network.HTTP.Media ((//))
import Servant.API.ContentTypes

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance ToHtml a => MimeRender HTML a where
  mimeRender _ a = renderBS (toHtml a)