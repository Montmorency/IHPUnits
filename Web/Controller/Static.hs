module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.Units
import Web.View.Static.About

-- units 
import Data.Metrology
import Data.Metrology.Parser
import Data.Metrology.Show ()
import qualified Data.Metrology.SI as MSI
-- units-defs We'll bring in Units Modules as they are required. 
import qualified Data.Units.SI as SI
import qualified Data.Units.US as US
--So the lexer for units is a sepa
import Text.Parse.Units (parseUnit)
--Import the quasiquoted strings for parsing purposes
import Application.Helper.Controller (si, us)
import Application.Helper.Systems (testSymbolTable)

import qualified Data.Text as T

paramDouble :: (?requestContext :: RequestContext) => ByteString -> Double
paramDouble = param @Double

vel1_si ::  MSI.Velocity
vel1_si = 5 % [si| m/s |]

--paramString :: (?requestContext :: RequestContext) => ByteString -> String
--paramString = param @String

instance Controller StaticController where
    action UnitsAction = render UnitsView

    action ConvertUnitsAction = do
            let sourceNumber = paramDouble "sourceNumber" 
                sourceUnitStr = paramText "sourceUnit"
                targetUnitStr = paramText "targetUnit"
                x = parseUnit testSymbolTable $ T.unpack sourceUnitStr
            --y <- parseUnit targetUnitStr
            --z <- redim $ sourceNumber % [si| m s/s^2 |]
            --targetNumber = (z # [us|targetUnit|])
            render UnitsView

    action AboutAction = render AboutView
        
