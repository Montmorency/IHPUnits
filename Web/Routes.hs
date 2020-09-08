module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute StaticController

instance AutoRoute UnitPairController
type instance ModelControllerMap WebApplication UnitPair = UnitPairController
