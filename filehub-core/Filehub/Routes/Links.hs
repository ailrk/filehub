module Filehub.Routes.Links
  ( apiLinks
  )
  where

import Filehub.Prelude
import Filehub.Routes (Api(..))
import Servant (Link)
import Servant.Links ( AsLink, allFieldLinks )


apiLinks :: Api (AsLink Link)
apiLinks = allFieldLinks @Api
