module Web.View.Static.Units where
import Web.View.Prelude

import Data.Aeson (ToJSON (..))

import Data.Metrology
import Data.Metrology.Show ()
import Data.Metrology.SI

data UnitsView = UnitsView
instance View UnitsView ViewContext where
    html UnitsView = [hsx|
        <div class = "intro">
            <h1> Type Safe Units </h1>
            <p> 
                This page is a snapshot of  
                <a href="https://github.com/goldfirere"> goldfirere's </a>
                Haskell <a href="https://github.com/goldfirere/units"> units </a> module.
                The <a href="https://github.com/goldfirere/units"> units </a> package is meant to 
                facilitate compile time dimensional analysis for any program
                which describes physically (or otherwise) dimensioned quantities. 
                At the moment this site only exposes the physically dimensioned aspects 
                of the units package. 
            </p>
            <h2> How It Works</h2>
            <p>
                The server side program is written in Haskell and is type safe at compile time. 
                The units parser means you can write your quantities' units free hand:
                e.g. J/s/m^2 or  btu/hour/ft^2. 
                The parsing convention is similar to that of: 
                <a href="https://docs.microsoft.com/en-us/dotnet/fsharp/"> F# </a>.
                You can exponentiate, juxtapose (i.e. place two unit symbols next to each other without
                an operator between them <samp> s s = s^2 </samp>), and finally,
                divide and multiply, with that order of precedence (divide and multiply sharing the same level
                of precedence). So, for example, <samp> m/s^2 </samp> and 
                <samp>(m/s s)</samp> are the same (seconds exponentiated and seconds juxtaposed) unit, 
                but <samp> m/s*s = (m/s)*s = m </samp>. See the units-defs package for 
                systems that are defined. Whereever a unit does not have a standard string representation
                or there is some ambiguity in the prefixes the user may pass them in here as a map; in the form
                ("kJ", Kilo :@ Joule).
            </p>
        </div>
        <div class = "units-convert"> 
            <h2> Convert Units </h2>
                <form action={ConvertUnitsAction} method="POST">
                  <div class="form-row">
                    <div class="form-group col-md-2">
                      <label for="sourceNumber">Source Number</label>  
                      <input type="text" class="form-control" id="sourceNumber" placeholder="e.g. (1.01)">
                    </div>
                    <div class="form-group col-md-2">
                      <label for="sourceUnit">Source Unit</label>  
                      <input type="text" class="form-control" id="sourceUnit" placeholder="pure Unit (e.g. m/s)">
                    </div>
                    <div class="form-group col-md-2">
                      <label for="targetNumber">Target Number</label>  
                      <input type="text" class="form-control" id="targetNumber" placeholder="-.-" readonly="readonly">
                    </div>
                    <div class="form-group col-md-2">
                      <label for="targetUnit"> Target Unit</label>  
                      <input type="text" class="form-control" id="targetUnit" placeholder="pure Unit (e.g. m/s)">
                    </div>
                  </div>
                  <div class="form-group">
                    <label for="inputAddress">Symbol Table</label>
                    <input type="text" class="form-control" id="symbolTable" placeholder="(kg, Kilo :@ Gram)">
                  </div>
                  <button type="submit" class="btn btn-primary"> Convert  </button>
                </form>
        </div>
|]
