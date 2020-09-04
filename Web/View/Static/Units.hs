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
                Haskell <a href="https://github.com/goldfirere/units"> units </a> package.
                The <a href="https://github.com/goldfirere/units"> units </a> package is meant to 
                facilitate compile time dimensional analysis for any program
                which describes physically (or otherwise) dimensioned quantities. 
                At the moment this site
                only exposes the physically dimensioned aspects 
                of the units package. 
            </p>
            <p>
                Let us fix some ideas and terminology. 
                A Quantity is a numerical value <em> and </em> its associated unit of measurement.
                A Quantity describes an underlying Physically Invariant Property which we can associate with some
                pure or composite Dimension; e.g., a pure dimension like the Length of a piece of string, 
                or the composite dimension of irradiance which has dimensions of Energy per Time per Area. 
                Quantities associated with particular Dimensions can be expressed
                in any system of units one pleases e.g., meters and feet for a length dimension, or,
                Joules per Second per Square Meter or British Thermal Units per Week per Square Yard 
                for our irradiance. There is a great deal of freedom in the way we measure dimensions.
            </p>
            <p>   
                Hopefully this package/site will eventually be of use to:
                <ul> 
                    <li> Curious people with spare time. </li>
                    <li> People doing their science homework. </li>
                    <li> Electronic structure people trying to rationalize data expressed 
                         in quite complex combinations of Hartree Atomic Units, SI units, Webers, 
                         and bohr magnetons. 
                    </li>
                </ul>
                It is also hoped that this site will spur some interest and activity to support and extend a 
                <a href="https://github.com/goldfirere/units"> module </a> that provides an important 
                tool for any scientific software; that is, a compile time type-safe consistency 
                check on all physically dimensioned quantities. 
            </p>
            <h2> How It Works</h2>
            <p>
                The server side program is written in Haskell and is type safe at compile time. 
                The units parser means you can write your quantities with complicated units free hand:
                e.g. J/s/m^2 to btu/hour/ft^2. 
                The parsing convention is similar to that of: 
                <a href="https://docs.microsoft.com/en-us/dotnet/fsharp/"> F# </a>.
                You can exponentiate, juxtapose (i.e. place two unit symbols next to each other without
                an operator between them <samp> s s = s^2 </samp>), and finally,
                divide and multiply, with that order of precedence (divide and multiply sharing the same level
                of precedence). So, example, <samp> m/s^2 </samp> and 
                (m/s s) are the same (seconds exponentiated and seconds juxtaposed) unit, 
                but <samp> m/s*s = (m/s)*s = m </samp>. See the units-defs package for the 
                systems that are defined. Whereever a unit does not have a standard string representation
                or there is some ambiguity in the prefixes the user may pass them in as a map in the form
                ("kJ", Kilo :@ Joule).
            </p>
        </div>
        <div class = "units-convert"> 
            <h2> Convert Units </h2>
                <form>
                  <div class="form-row">
                    <div class="form-group col-md-2">
                      <label for="inputNumericQuantity">Input Number</label>  
                      <input type="text" class="form-control" id="inputNumericQuantity" placeholder="e.g. (1.01)">
                    </div>
                    <div class="form-group col-md-2">
                      <label for="inputUnitQuantity">Input Unit</label>  
                      <input type="text" class="form-control" id="inputUnitQuantity" placeholder="pure Unit (e.g. m/s)">
                    </div>
                    <div class="form-group col-md-2">
                      <label for="outputNumericQuantity">Output Number</label>  
                      <input type="text" class="form-control" id="outputNumericQuantity" placeholder="-.-" readonly="readonly">
                    </div>
                    <div class="form-group col-md-2">
                      <label for="outputUnitQuantity"> Output Unit</label>  
                      <input type="text" class="form-control" id="outputUnitQuantity" placeholder="pure Unit (e.g. m/s)">
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
