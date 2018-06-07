module Multiselect.Keycodes
    exposing
        ( return
        , downArrow
        , upArrow
        , backspace
        , tab
        , escape
        , end
        , pageDown
        , home
        , pageUp
        )


type alias KeyCode =
    Int


return : KeyCode
return =
    13


escape : KeyCode
escape =
    27


upArrow : KeyCode
upArrow =
    38


downArrow : KeyCode
downArrow =
    40


pageUp : KeyCode
pageUp =
    33


pageDown : KeyCode
pageDown =
    34


home : KeyCode
home =
    36


end : KeyCode
end =
    35


backspace : KeyCode
backspace =
    8


tab : KeyCode
tab =
    9
