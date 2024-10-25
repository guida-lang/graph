module Data.Graph.Internal exposing
    ( Array(..)
    , accumArray
    , array
    , assocs
    , bounds
    , find
    , indices
    )

import Dict exposing (Dict)



-------------------------------------------------------------------------
--                                                                      -
--      Arrays
--                                                                      -
-------------------------------------------------------------------------


type Array i e
    = Array Int Int (Dict Int e)


find : Int -> Array i e -> Maybe e
find i (Array _ _ arr) =
    Dict.get i arr


bounds : Array i e -> ( Int, Int )
bounds (Array l u _) =
    ( l, u )


indices : Array i e -> List Int
indices (Array l u _) =
    List.repeat ((u + 1) - l) ()
        |> List.indexedMap (\i _ -> l + i)


assocs : Array i e -> List ( Int, Maybe e )
assocs arr =
    indices arr
        |> List.map (\i -> ( i, find i arr ))


array : ( Int, Int ) -> List ( Int, e ) -> Array i e
array ( l, u ) =
    List.filter (\( i, _ ) -> i >= l && i <= u + 1)
        >> Dict.fromList
        >> Array l u


accumArray : (e -> a -> e) -> e -> ( Int, Int ) -> List ( Int, a ) -> Array i e
accumArray f initial ( l, u ) ies =
    let
        initialArr : Dict Int e
        initialArr =
            List.repeat ((u + 1) - l) ()
                |> List.indexedMap (\i _ -> ( l + i, initial ))
                |> Dict.fromList
    in
    List.foldl
        (\( i, a ) acc ->
            Dict.update i (Maybe.map (\v -> f v a)) acc
        )
        initialArr
        ies
        |> Dict.toList
        |> array ( l, u )
