module Data.GraphTests exposing (suite)

import Data.Graph as G
import Data.Graph.Internal as I
import Expect
import Test exposing (Test)
import Tree


suite : Test
suite =
    Test.describe "Data.Graph"
        [ Test.describe "vertices"
            [ Test.test "vertices (buildG (0,-1) []) == []" <|
                \_ ->
                    G.vertices (G.buildG ( 0, -1 ) [])
                        |> Expect.equal []
            , Test.test "G.vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]" <|
                \_ ->
                    G.vertices (G.buildG ( 0, 2 ) [ ( 0, 1 ), ( 1, 2 ) ])
                        |> Expect.equal [ 0, 1, 2 ]
            ]
        , Test.describe "edges"
            [ Test.test "edges (buildG (0,-1) []) == []" <|
                \_ ->
                    G.edges (G.buildG ( 0, -1 ) [])
                        |> Expect.equal []
            , Test.test "edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]" <|
                \_ ->
                    G.edges (G.buildG ( 0, 2 ) [ ( 0, 1 ), ( 1, 2 ) ])
                        |> Expect.equal [ ( 0, 1 ), ( 1, 2 ) ]
            ]
        , Test.describe "buildG"
            [ Test.test "buildG (0,-1) [] == array (0,-1) []" <|
                \_ ->
                    G.buildG ( 0, -1 ) []
                        |> Expect.equal (I.array ( 0, -1 ) [])
            , Test.test "buildG (0,2) [(0,1), (1,2)] == array (0,2) [(0,[1]),(1,[2]),(2,[])]" <|
                \_ ->
                    G.buildG ( 0, 2 ) [ ( 0, 1 ), ( 1, 2 ) ]
                        |> Expect.equal (I.array ( 0, 2 ) [ ( 0, [ 1 ] ), ( 1, [ 2 ] ), ( 2, [] ) ])
            , Test.test "buildG (0,2) [(0,1), (0,2), (1,2)] == array (0,2) [(0,[2,1]),(1,[2]),(2,[])]" <|
                \_ ->
                    G.buildG ( 0, 2 ) [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]
                        |> Expect.equal (I.array ( 0, 2 ) [ ( 0, [ 2, 1 ] ), ( 1, [ 2 ] ), ( 2, [] ) ])
            ]
        , Test.describe "transposeG"
            [ Test.test "transposeG (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,[]),(1,[0]),(2,[1])]" <|
                \_ ->
                    G.transposeG (G.buildG ( 0, 2 ) [ ( 0, 1 ), ( 1, 2 ) ])
                        |> Expect.equal (I.array ( 0, 2 ) [ ( 0, [] ), ( 1, [ 0 ] ), ( 2, [ 1 ] ) ])
            ]
        , Test.describe "outdegree"
            [ Test.test "outdegree (buildG (0,-1) []) == array (0,-1) []" <|
                \_ ->
                    G.outdegree (G.buildG ( 0, -1 ) [])
                        |> Expect.equal (I.array ( 0, -1 ) [])
            , Test.test "outdegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,1),(1,1),(2,0)]" <|
                \_ ->
                    G.outdegree (G.buildG ( 0, 2 ) [ ( 0, 1 ), ( 1, 2 ) ])
                        |> Expect.equal (I.array ( 0, 2 ) [ ( 0, 1 ), ( 1, 1 ), ( 2, 0 ) ])
            ]
        , Test.describe "indegree"
            [ Test.test "indegree (buildG (0,-1) []) == array (0,-1) []" <|
                \_ ->
                    G.indegree (G.buildG ( 0, -1 ) [])
                        |> Expect.equal (I.array ( 0, -1 ) [])
            , Test.test "indegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,0),(1,1),(2,1)]" <|
                \_ ->
                    G.indegree (G.buildG ( 0, 2 ) [ ( 0, 1 ), ( 1, 2 ) ])
                        |> Expect.equal (I.array ( 0, 2 ) [ ( 0, 0 ), ( 1, 1 ), ( 2, 1 ) ])
            ]
        , Test.describe "graphFromEdges"
            [ Test.test "An empty graph" <|
                \_ ->
                    let
                        ( graph, _, _ ) =
                            G.graphFromEdges []
                    in
                    graph
                        |> Expect.equal (I.array ( 0, -1 ) [])
            , Test.test "A graph where the out-list references unspecified nodes ('c'), these are ignored." <|
                \_ ->
                    let
                        ( graph, _, _ ) =
                            G.graphFromEdges [ ( "a", 'a', [ 'b' ] ), ( "b", 'b', [ 'c' ] ) ]
                    in
                    graph
                        |> Expect.equal (I.array ( 0, 1 ) [ ( 0, [ 1 ] ), ( 1, [] ) ])
            , Test.test "A graph with 3 vertices: ('a') -> ('b') -> ('c')" <|
                \_ ->
                    let
                        ( graph, nodeFromVertex, vertexFromKey ) =
                            G.graphFromEdges [ ( "a", 'a', [ 'b' ] ), ( "b", 'b', [ 'c' ] ), ( "c", 'c', [] ) ]
                    in
                    ( graph, nodeFromVertex 0, vertexFromKey 'a' )
                        |> Expect.equal
                            ( I.array ( 0, 2 ) [ ( 0, [ 1 ] ), ( 1, [ 2 ] ), ( 2, [] ) ]
                            , Just ( "a", 'a', [ 'b' ] )
                            , Just 0
                            )
            , Test.test "Get the label for a given key" <|
                \_ ->
                    let
                        getNodePart : ( a, b, c ) -> a
                        getNodePart ( n, _, _ ) =
                            n

                        ( _, nodeFromVertex, vertexFromKey ) =
                            G.graphFromEdges [ ( "a", 'a', [ 'b' ] ), ( "b", 'b', [ 'c' ] ), ( "c", 'c', [] ) ]
                    in
                    Maybe.andThen (Maybe.map getNodePart << nodeFromVertex) (vertexFromKey 'a')
                        |> Expect.equal (Just "a")
            ]
        , Test.test "stronglyConnComp" <|
            \_ ->
                G.stronglyConnComp [ ( "a", 0, [ 1 ] ), ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ), ( "d", 3, [ 3 ] ) ]
                    |> Expect.equal
                        [ G.CyclicSCC [ "d" ]
                        , G.CyclicSCC [ "b", "c" ]
                        , G.AcyclicSCC "a"
                        ]
        , Test.test "stronglyConnCompR" <|
            \_ ->
                G.stronglyConnCompR [ ( "a", 0, [ 1 ] ), ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ), ( "d", 3, [ 3 ] ) ]
                    |> Expect.equal
                        [ G.CyclicSCC [ ( "d", 3, [ 3 ] ) ]
                        , G.CyclicSCC [ ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ) ]
                        , G.AcyclicSCC ( "a", 0, [ 1 ] )
                        ]
        , Test.test "scc" <|
            \_ ->
                G.scc (G.buildG ( 0, 3 ) [ ( 3, 1 ), ( 1, 2 ), ( 2, 0 ), ( 0, 1 ) ])
                    |> Expect.equal
                        [ Tree.tree 0 [ Tree.tree 1 [ Tree.tree 2 [] ] ]
                        , Tree.tree 3 []
                        ]
        ]
