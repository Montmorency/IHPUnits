module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.Units

--Units Modules
import Data.Metrology
import Data.Metrology.Show ()
import Data.Metrology.SI

instance Controller StaticController where
    action UnitsAction = render UnitsView

    action ConvertUnitsAction = do
            let inputUnit = paramText "inputUnitQuantity" 
                inputNumeric = paramText "inputNumericQuantity"
                outputUnit = paramText "outputUnitQuantity"
            render UnitsView
--    action ConvertUnitsDropdown = do
--        render UnitsView 
--    action ConvertUnitsParse = do
--        render UnitsView 
--    action ParseandReduceUnits = do
--        render UnitsView 
        
