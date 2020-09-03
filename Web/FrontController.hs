module Web.FrontController where
import IHP.RouterPrelude
import IHP.ControllerSupport
import Generated.Types

import Web.Types

-- Controller Imports
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage UnitsAction
        , parseRoute @StaticController
        ]

instance InitControllerContext WebApplication
