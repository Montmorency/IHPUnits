{-# LANGUAGE TemplateHaskell #-}
module Application.Helper.Controller (
    si, us
) where
--
-- units 
import Data.Metrology
import Data.Metrology.Parser
import Data.Metrology.Show ()
import qualified Data.Metrology.SI 
-- units-defs We'll bring in Units Modules as they are required. 
import qualified Data.Units.SI as SI

import Data.Units.SI.Prefixes (siPrefixes) 
import Data.Units.SI (siUnits)

import Language.Haskell.TH ( Name )

import Application.Helper.Systems (usUnits)

-- Here you can add functions which are available in all your controllers
--SI already makes available two fxns to export types
--For US system we export them explicitly
$(makeQuasiQuoter "si" siPrefixes siUnits)
$(makeQuasiQuoter "us" siPrefixes usUnits)



