module Application.Helper.View (
    module IHP.LoginSupport.Helper.View,
    buildFormRecord
    
) where

import IHP.ViewPrelude
import IHP.LoginSupport.Helper.View
import IHP.ModelSupport

import qualified Text.Blaze.Html5 as Html5

buildFormRecord :: forall model viewContext parent id. (?viewContext :: viewContext, HasField "id" model id, Default id, Eq id) => FormContext model -> ((?viewContext :: viewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
buildFormRecord formContext inner =
    let
        theModel = model formContext
        action = formAction formContext
        isNewRecord = IHP.ModelSupport.isNew theModel
        formId = formAction formContext -- if isNewRecord then "" else formAction formContext
        formClass :: Text = "record-form" --Text = if isNewRecord then "new-form" else "edit-form"
        formInner = let ?formContext = formContext in inner
    in
        [hsx|<form method="POST" action={action} id={formId} class={formClass}>{formInner}</form>|]


