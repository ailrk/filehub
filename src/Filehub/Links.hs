module Filehub.Links
  ( apiLinks
  )
  where

import Filehub.Routes (Api(..))
import Servant (Link)
import Servant.Links ( AsLink, allFieldLinks )


apiLinks :: Api (AsLink Link)
apiLinks = allFieldLinks @Api
