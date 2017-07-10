port module Multiselect.Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Multiselect.SelectCss


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "app.css", Css.File.compile [ SelectCss.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
