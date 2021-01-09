module Multiselect.Utils exposing (fst)


fst : ( a1, a2 ) -> a1
fst =
    Tuple.first
