module Multiselect.Utils exposing (invisibleCharacter, fst)


fst : ( a1, a2 ) -> a1
fst =
    Tuple.first


invisibleCharacter : String
invisibleCharacter =
    "\x200C\x200C"
