{-# LANGUAGE TemplateHaskell #-}
module Application.Helper.Systems where

import Prelude hiding ( lex, exp )

import Data.Generics

import Data.Metrology.Parser

import qualified Data.Units.SI as SI
import qualified Data.Units.SI.Prefixes as SIPref

import qualified Data.Units.US as US
import qualified Data.Units.US.Liquid as Liq
import qualified Data.Units.US.Avoirdupois as Avdp
import qualified Data.Units.US.Survey as Survey

import Language.Haskell.TH ( Name, nameBase, mkName )

stripModules :: Data a => a -> a
stripModules = everywhere (mkT (mkName . nameBase))

usUnits :: [Name]
usUnits = 
        [ ''US.Angstrom, ''US.Mil, ''US.Point, ''US.Pica, ''US.Inch
        , ''US.Foot, ''US.Yard, ''US.Mile, ''US.NauticalMile
        , ''US.Acre, ''US.Teaspoon  
        , ''Liq.Teaspoon, ''Liq.Tablespoon, ''Liq.FluidOunce
        , ''Liq.Cup, ''Liq.Pint, ''Liq.Quart, ''Liq.Gallon
        , ''Avdp.Ounce, ''Avdp.Pound, ''Avdp.Ton
        , ''US.Atmosphere, ''US.Bar, ''US.FoodCalorie, ''US.Therm
        , ''US.Btu, ''US.Horsepower
        ]


testSymbolTable :: SymbolTable Name Name
Right testSymbolTable =
   mkSymbolTable (stripModules [ ("k", ''SIPref.Kilo)
                               , ("da", ''SIPref.Deca)
                               , ("m", ''SIPref.Milli)
                               , ("d", ''SIPref.Deci) ])
                 (stripModules [ ("m", ''SI.Meter)
                               , ("s", ''SI.Second)
                               , ("min", ''SI.Minute)
                               , ("am", ''SI.Ampere) ])
