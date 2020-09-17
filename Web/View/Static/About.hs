module Web.View.Static.About where
import Web.View.Prelude

data AboutView = AboutView
instance View AboutView ViewContext where
    html AboutView = [hsx|
            <h1> About </h1>
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
                    <li> Electronic structure people and their experimental colleagues 
                         trying to rationalize data expressed 
                         in quite complex combinations of Hartree Atomic Units, SI units, and 
                         c.g.s. 
                    </li>
                </ul>
                It is also hoped that this site will spur some interest and activity to support and extend a 
                <a href="https://github.com/goldfirere/units"> module </a> that provides an important 
                tool for any scientific software; that is, a compile time type-safe consistency 
                check on all physically dimensioned quantities. 
            </p>
            <p>
             Future additions to the site would be providing some dimensional analysis capability. 
             For instance, given the relevant physical dimensions for a specific problem return 
             a valid algebraic combination that is dimensionless.  
            </p>
|]
