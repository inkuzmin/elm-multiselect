module Multiselect.Utils exposing (fst, invisibleCharacter)


fst : ( a1, a2 ) -> a1
fst =
    Tuple.first


invisibleCharacter : String
invisibleCharacter =
    "\u{200C}\u{200C}"
