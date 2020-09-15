module Web.View.Static.Units where
import Web.View.Prelude

import Data.Aeson (ToJSON (..))

import Data.Metrology
import Data.Metrology.Show ()
import Data.Metrology.SI

import qualified Text.Blaze.Html5 as Html5
import IHP.NameSupport

import Application.Helper.View

data UnitsView = UnitsView {unitpair :: UnitPair}

instance View UnitsView ViewContext where
    html UnitsView {..} = [hsx|
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
                You can exponentiate, juxtapose (i.e. place two unit symbols 
                next to each other without
                an operator between them <samp> s s = s^2 </samp>), and finally,
                divide and multiply, with that order of precedence (divide 
                and multiply sharing the same level
                of precedence). So, for example, <samp> m/s^2 </samp> and 
                <samp>(m/s s)</samp> are the same (seconds exponentiated and 
                seconds juxtaposed) unit, 
                but <samp> m/s*s = (m/s)*s = m </samp>. See the units-defs package for 
                systems that are defined. Whereever a unit does not have a 
                standard string representation
                or there is some ambiguity in the prefixes the user may 
                pass them in here as a map; in the form
                ("kJ", Kilo :@ Joule).
            </p>
        </div>
        <div class = "units-convert"> 
            <h2> Convert Units </h2>
                <form action={ConvertUnitsAction} method="POST" class="edit-form">
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

        <div>
            {renderForm unitpair}
        </div>
    |]
        where
            renderForm :: UnitPair -> Html
            renderForm unitpair =  formForRecord unitpair [hsx|
                {textFieldRecord unitpair sourceUnit }
                {textFieldRecord unitpair targetUnit }
                |]

            formForRecord :: forall record viewContext parent id application. (
                               ?viewContext :: viewContext
                              , Eq record
                              , Typeable record
                              , HasField "id" record id
                              , HasField "meta" record MetaBag
                              , application ~ ViewApp viewContext  
                              , Default id
                              , Eq id
                              ) => record -> ((?viewContext :: viewContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
            formForRecord record  = buildFormRecord (createFormRecordContext record)

createFormRecordContext :: forall record viewContext parent id application. (
        ?viewContext :: viewContext
        , Eq record
        , Typeable record
       -- , ModelFormAction application record
        , HasField "id" record id
        , application ~ ViewApp viewContext
        , HasField "meta" record MetaBag
        ) => record -> FormContext record
createFormRecordContext record =
    FormContext
        { model = record
        , renderFormField = renderHorizontalBootstrapFormField
        , renderSubmit = renderHorizontalBootstrapSubmitButton
        , formAction = "" 
        }

textFieldRecord :: forall fieldName model value.
    (?formContext :: FormContext model
    --, HasField fieldName model value
    , HasField "meta" model MetaBag
    --, KnownSymbol fieldName
    --, InputValue value
    --, KnownSymbol (GetModelName model)
    ) => model -> (model -> Text) -> FormField
textFieldRecord model field = FormField
        { fieldType = TextInput
        , fieldName = "" --cs fieldName
        , fieldLabel = "" --fieldNameToFieldLabel (cs fieldName)
        , fieldValue = field model --inputValue ((getField @fieldName model) :: value)
        , fieldInputId = "units"--cs (IHP.NameSupport.lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = Just ""
        , fieldClass = ""
        , labelClass = ""
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , fieldInput = const Html5.input
        , renderFormField = renderBootstrapFormField --getField @"renderFormField" ?formContext
        , helpText = ""
        , placeholder = ""
        , required = False
        }
    where
        --fieldName = symbolVal field
        FormContext { model } = ?formContext
