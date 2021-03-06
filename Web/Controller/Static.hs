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

--import IHP.ViewPrelude (hsx)
data UnitPair = UnitPair {id :: Int, sourceUnit :: Text, sourceNumber :: Double, 
                          targetUnit :: Text, targetNumber :: Double, meta :: MetaBag}
                          deriving (Eq, Show)

paramDouble :: (?requestContext :: RequestContext) => ByteString -> Double
paramDouble = param @Double

setUnitPair :: Text -> Double -> Text -> UnitPair
setUnitPair x y z = UnitPair {id=def, sourceUnit=x, sourceNumber=y, targetUnit=z, targetNumber=0.0, meta=def}

instance Controller StaticController where
    action UnitsAction = do
            let unitpair = UnitPair {id=def, sourceUnit = "m/s", sourceNumber=10.0, targetUnit="m/s", targetNumber=0.0, meta=def}
            render UnitsView { .. }

    action ConvertUnitsAction = do
            let sourceUnitNumber = paramDouble "sourceNumber" 
                sourceUnitStr = paramText "sourceUnit"
                targetUnitStr = paramText "targetUnit"
                symbolTable = paramText "symbolTable"
                x = parseUnit testSymbolTable $ T.unpack sourceUnitStr
                z = setUnitPair sourceUnitStr sourceUnitNumber targetUnitStr 
                unitpair = UnitPair {id=def, sourceUnit = "m/s", sourceNumber=9.8, targetUnit="m/s", targetNumber=0.0, meta=def}
            render UnitsView { .. }

    action AboutAction = render AboutView
        
