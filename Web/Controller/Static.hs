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

setUnitPair :: Text -> Double -> Text -> UnitPair
setUnitPair x y z = UnitPair {id=def, sourceUnit = x, sourceNumber=y, targetUnit=z, targetNumber=0.0, meta=def}

instance Controller StaticController where
    action UnitsAction = render UnitsView

    action ConvertUnitsAction = do
            let sourceUnitNumber = paramDouble "sourceNumber" 
                sourceUnitStr = paramText "sourceUnit"
                targetUnitStr = paramText "targetUnit"
                x = parseUnit testSymbolTable $ T.unpack sourceUnitStr
                z = setUnitPair sourceUnitStr sourceUnitNumber targetUnitStr 
            redirectTo UnitsAction

    action AboutAction = render AboutView
        
