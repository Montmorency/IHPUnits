module Web.Types where
import IHP.Prelude
import qualified IHP.Controller.Session
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.ModelSupport
import Application.Helper.Controller
import IHP.ViewSupport
import Generated.Types

import GHC.Generics (Generic)
import Database.PostgreSQL.Simple (FromRow)

import Data.UUID (UUID)

data WebApplication = WebApplication deriving (Eq, Show)

--Writing a dataController that doesn't connect to database.
data UnitPair = UnitPair {id :: (Id' "unitpairs"), sourceUnit :: Text, sourceNumber :: Double, 
                          targetUnit :: Text, targetNumber :: Double, meta :: MetaBag}
                deriving (Eq, Show)

instance InputValue UnitPair where inputValue = IHP.ModelSupport.recordToInputValue

type instance GetTableName UnitPair = "unitpairs"
type instance GetModelByTableName "unitpairs" = UnitPair
type instance GetModelName UnitPair = "UnitPair"

type instance PrimaryKey "unitpairs" = UUID

instance Record UnitPair where
    newRecord = UnitPair def def def def def def

instance SetField "id" (UnitPair ) (Id' "unitpairs") where
    setField newValue (UnitPair id sourceUnit sourceNumber targetUnit targetNumber meta) = 
        UnitPair newValue sourceUnit sourceNumber targetUnit targetNumber (meta { touchedFields = "id" : touchedFields meta })

instance SetField "sourceUnit" (UnitPair ) Text  where
    setField newValue (UnitPair id sourceUnit sourceNumber targetUnit targetNumber meta) = 
        UnitPair id newValue sourceNumber targetUnit targetNumber meta

instance SetField "sourceNumber" (UnitPair ) Double  where
    setField newValue (UnitPair id sourceUnit sourceNumber targetUnit targetNumber meta) = 
        UnitPair id sourceUnit newValue targetUnit targetNumber meta

instance SetField "targetUnit" (UnitPair ) Text  where
    setField newValue (UnitPair id sourceUnit sourceNumber targetUnit targetNumber meta) = 
        UnitPair id sourceUnit sourceNumber newValue targetNumber meta

instance SetField "targetNumber" (UnitPair ) Double  where
    setField newValue (UnitPair id sourceUnit sourceNumber targetUnit targetNumber meta) = 
        UnitPair id sourceUnit sourceNumber targetUnit newValue meta

instance SetField "meta" (UnitPair ) MetaBag where
    setField newValue (UnitPair id sourceUnit sourceNumber targetUnit targetNumber meta) = 
        UnitPair id sourceUnit sourceNumber targetUnit targetNumber newValue
--End of Record DataType

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [IHP.Controller.Session.FlashMessage]
    , controllerContext :: ControllerSupport.ControllerContext
    , layout :: Layout
    }

data StaticController
    = UnitsAction | ConvertUnitsAction | AboutAction
    deriving (Eq,Show,Data)

