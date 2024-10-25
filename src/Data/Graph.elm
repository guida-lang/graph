module Data.Graph exposing
    ( Graph, Bounds, Edge, Vertex, Table
    , graphFromEdges, graphFromEdges_, buildG
    , vertices, edges, outdegree, indegree
    , transposeG
    , dfs, dff, scc
    , SCC(..)
    , stronglyConnComp, stronglyConnCompR
    , flattenSCC, flattenSCCs
    , Array
    )

{-| An Elm graph type implementation, inspired by Haskell's Data.Graph.

Reference: <https://hackage.haskell.org/package/containers-0.7/docs/src/Data.Graph.html>

This implementation attempts to adapt the concepts and structure from the
Haskell Graph into the Elm ecosystem.


# Graphs

@docs Graph, Bounds, Edge, Vertex, Table


# Graph Construction

@docs graphFromEdges, graphFromEdges_, buildG


# Graph Properties

@docs vertices, edges, outdegree, indegree


# Graph Transformations

@docs transposeG


# Graph Algorithms

@docs dfs, dff, scc


# Strongly Connected Components

@docs SCC


## Construction

@docs stronglyConnComp, stronglyConnCompR


## Conversion

@docs flattenSCC, flattenSCCs


## Array

@docs Array

-}

import Basics.Extra exposing (flip)
import Data.Graph.Internal as Internal
import Dict
import Set exposing (Set)
import Tree exposing (Tree)


{-| Array structure
-}
type alias Array i e =
    Internal.Array i e



-------------------------------------------------------------------------
--                                                                      -
--      Strongly Connected Components
--                                                                      -
-------------------------------------------------------------------------


{-| Strongly connected component.
-}
type SCC vertex
    = AcyclicSCC vertex
    | CyclicSCC (List vertex)


{-| The vertices of a list of strongly connected components.
-}
flattenSCCs : List (SCC a) -> List a
flattenSCCs =
    List.concatMap flattenSCC


{-| The vertices of a strongly connected component.
-}
flattenSCC : SCC vertex -> List vertex
flattenSCC component =
    case component of
        AcyclicSCC v ->
            [ v ]

        CyclicSCC vs ->
            vs


{-| The strongly connected components of a directed graph,
reverse topologically sorted.

    stronglyConnComp [ ( "a", 0, [ 1 ] ), ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ), ( "d", 3, [ 3 ] ) ]
        == [ CyclicSCC [ "d" ], CyclicSCC [ "b", "c" ], AcyclicSCC "a" ]

-}
stronglyConnComp : List ( node, comparable, List comparable ) -> List (SCC node)
stronglyConnComp edges0 =
    List.map
        (\edge0 ->
            case edge0 of
                AcyclicSCC ( n, _, _ ) ->
                    AcyclicSCC n

                CyclicSCC triples ->
                    CyclicSCC (List.map (\( n, _, _ ) -> n) triples)
        )
        (stronglyConnCompR edges0)


{-| The strongly connected components of a directed graph,
reverse topologically sorted. The function is the same as
'stronglyConnComp', except that all the information about each node retained.
This interface is used when you expect to apply 'SCC' to
(some of) the result of 'SCC', so you don't want to lose the
dependency information.

    stronglyConnCompR [ ( "a", 0, [ 1 ] ), ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ), ( "d", 3, [ 3 ] ) ]
        == [ CyclicSCC [ ( "d", 3, [ 3 ] ) ], CyclicSCC [ ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ) ], AcyclicSCC ( "a", 0, [ 1 ] ) ]

-}
stronglyConnCompR : List ( node, comparable, List comparable ) -> List (SCC ( node, comparable, List comparable ))
stronglyConnCompR edges0 =
    case edges0 of
        [] ->
            []

        _ ->
            let
                ( graph, vertexFn, _ ) =
                    graphFromEdges edges0

                forest : List (Tree Vertex)
                forest =
                    scc graph

                decode : Tree Vertex -> SCC ( node, comparable, List comparable )
                decode tree =
                    let
                        v : Vertex
                        v =
                            Tree.label tree
                    in
                    case ( Tree.children tree, mentionsItself v, vertexFn v ) of
                        ( [], True, _ ) ->
                            CyclicSCC (List.filterMap identity [ vertexFn v ])

                        ( [], False, Just vertex ) ->
                            AcyclicSCC vertex

                        ( ts, _, _ ) ->
                            CyclicSCC (List.filterMap identity (vertexFn v :: List.foldr dec [] ts))

                dec : Tree Vertex -> List (Maybe ( node, comparable, List comparable )) -> List (Maybe ( node, comparable, List comparable ))
                dec node vs =
                    vertexFn (Tree.label node) :: List.foldr dec vs (Tree.children node)

                mentionsItself : Int -> Bool
                mentionsItself v =
                    List.member v (Maybe.withDefault [] (Internal.find v graph))
            in
            List.map decode forest



-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------


{-| Abstract representation of vertices.
-}
type alias Vertex =
    Int


{-| Table indexed by a contiguous set of vertices.
-}
type alias Table a =
    Array Vertex a


{-| Adjacency list representation of a graph, mapping each vertex to its
list of successors.
-}
type alias Graph =
    Array Vertex (List Vertex)


{-| The bounds of an @Array@.
-}
type alias Bounds =
    ( Vertex, Vertex )


{-| An edge from the first vertex to the second.
-}
type alias Edge =
    ( Vertex, Vertex )


{-| (O(V)). Returns the list of vertices in the graph.

==== **Examples**

> vertices (buildG (0,-1) []) == []

> vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]

-}
vertices : Graph -> List Vertex
vertices =
    Internal.indices


{-| (O(V+E)). Returns the list of edges in the graph.

==== **Examples**

> edges (buildG (0,-1) []) == []

> edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]

-}
edges : Graph -> List Edge
edges g =
    List.concatMap (\v -> List.map (Tuple.pair v) (Maybe.withDefault [] (Internal.find v g))) (vertices g)


{-| (O(V+E)). Build a graph from a list of edges.

Warning: This function will cause a runtime exception if a vertex in the edge
list is not within the given @Bounds@.

==== **Examples**

> buildG (0,-1) [] == array (0,-1) []
> buildG (0,2) [(0,1), (1,2)] == array (0,2) [(0,[1]),(1,[2]),(2,[])]
> buildG (0,2) [(0,1), (0,2), (1,2)] == array (0,2) [(0,[2,1]),(1,[2]),(2,[])]

-}
buildG : Bounds -> List Edge -> Graph
buildG =
    Internal.accumArray (flip (::)) []


{-| (O(V+E)). The graph obtained by reversing all edges.

==== **Examples**

> transposeG (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,[]),(1,[0]),(2,[1])]

-}
transposeG : Graph -> Graph
transposeG g =
    buildG (Internal.bounds g) (reverseE g)


reverseE : Graph -> List Edge
reverseE g =
    List.map (\( v, w ) -> ( w, v )) (edges g)


{-| (O(V+E)). A table of the count of edges from each node.

==== **Examples**

> outdegree (buildG (0,-1) []) == array (0,-1) []

> outdegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,1),(1,1),(2,0)]

-}
outdegree : Graph -> Array Vertex Int
outdegree (Internal.Array l u arr) =
    -- This is bizarrely lazy. We build an array filled with thunks, instead
    -- of actually calculating anything. This is the historical behavior, and I
    -- suppose someone *could* be relying on it, but it might be worth finding
    -- out. Note that we *can't* be so lazy with indegree.
    Internal.Array l u (Dict.map (\_ -> List.length) arr)


{-| (O(V+E)). A table of the count of edges into each node.

==== **Examples**

> indegree (buildG (0,-1) []) == array (0,-1) []

> indegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,0),(1,1),(2,1)]

-}
indegree : Graph -> Array Vertex Int
indegree g =
    Internal.accumArray (+) 0 (Internal.bounds g) <|
        List.concatMap (\( _, outs ) -> List.map (\v -> ( v, 1 )) (Maybe.withDefault [] outs)) (Internal.assocs g)


{-| (O((V+E) \\log V)). Identical to 'graphFromEdges', except that the return
value does not include the function which maps keys to vertices. This
version of 'graphFromEdges' is for backwards compatibility.
-}
graphFromEdges_ : List ( node, comparable, List comparable ) -> ( Graph, Vertex -> Maybe ( node, comparable, List comparable ) )
graphFromEdges_ x =
    let
        ( a, b, _ ) =
            graphFromEdges x
    in
    ( a, b )


{-| (O((V+E) \\log V)). Build a graph from a list of nodes uniquely identified
by keys, with a list of keys of nodes this node should have edges to.

This function takes an adjacency list representing a graph with vertices of
type @key@ labeled by values of type @node@ and produces a @Graph@-based
representation of that list. The @Graph@ result represents the /shape/ of the
graph, and the functions describe a) how to retrieve the label and adjacent
vertices of a given vertex, and b) how to retrieve a vertex given a key.

@(graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList@

  - @graph :: Graph@ is the raw, array based adjacency list for the graph.
  - @nodeFromVertex :: Vertex -> (node, key, [key])@ returns the node
    associated with the given 0-based @Int@ vertex; see /warning/ below. This
    runs in (O(1)) time.
  - @vertexFromKey :: key -> Maybe Vertex@ returns the @Int@ vertex for the
    key if it exists in the graph, @Nothing@ otherwise. This runs in
    (O(\\log V)) time.

To safely use this API you must either extract the list of vertices directly
from the graph or first call @vertexFromKey k@ to check if a vertex
corresponds to the key @k@. Once it is known that a vertex exists you can use
@nodeFromVertex@ to access the labelled node and adjacent vertices. See below
for examples.

Note: The out-list may contain keys that don't correspond to nodes of the
graph; they are ignored.

Warning: The @nodeFromVertex@ function will cause a runtime exception if the
given @Vertex@ does not exist.

==== **Examples**

An empty graph.

> (graph, nodeFromVertex, vertexFromKey) = graphFromEdges []
> graph = array (0,-1) []

A graph where the out-list references unspecified nodes (@'c'@), these are
ignored.

> (graph, _, _) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c'])]
> array (0,1) [(0,[1]),(1,[])]

A graph with 3 vertices: ("a") -> ("b") -> ("c")

> (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
> graph == array (0,2) [(0,[1]),(1,[2]),(2,[])]
> nodeFromVertex 0 == Just ("a",'a',['b'])
> vertexFromKey 'a' == Just 0

Get the label for a given key.

> let getNodePart (n, _, _) = n
> (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
> Maybe.andThen (Maybe.map getNodePart << nodeFromVertex) (vertexFromKey 'a') == Just "A"

-}
graphFromEdges : List ( node, comparable, List comparable ) -> ( Graph, Vertex -> Maybe ( node, comparable, List comparable ), comparable -> Maybe Vertex )
graphFromEdges edges0 =
    let
        maxV : Int
        maxV =
            List.length edges0 - 1

        bounds0 : ( number, Int )
        bounds0 =
            ( 0, maxV )

        sortedEdges : List ( node, comparable, List comparable )
        sortedEdges =
            List.sortWith (\( _, k1, _ ) ( _, k2, _ ) -> compare k1 k2) edges0

        edges1 : List ( Int, ( node, comparable, List comparable ) )
        edges1 =
            List.map2 Tuple.pair
                (List.indexedMap (\i _ -> i) (List.repeat (List.length sortedEdges) ()))
                sortedEdges

        graph : Array i (List Int)
        graph =
            edges1
                |> List.map (\( v, ( _, _, ks ) ) -> ( v, List.filterMap keyVertex ks ))
                |> Internal.array bounds0

        keyMap : Array i comparable
        keyMap =
            edges1
                |> List.map (\( v, ( _, k, _ ) ) -> ( v, k ))
                |> Internal.array bounds0

        vertexMap : Array i ( node, comparable, List comparable )
        vertexMap =
            Internal.array bounds0 edges1

        keyVertex : comparable -> Maybe Int
        keyVertex k =
            let
                findVertex : Int -> Int -> Maybe Int
                findVertex a b =
                    if a > b then
                        Nothing

                    else
                        let
                            mid : Int
                            mid =
                                a + (b - a) // 2
                        in
                        Internal.find mid keyMap
                            |> Maybe.andThen
                                (\v ->
                                    case compare k v of
                                        LT ->
                                            findVertex a (mid - 1)

                                        EQ ->
                                            Just mid

                                        GT ->
                                            findVertex (mid + 1) b
                                )
            in
            findVertex 0 maxV
    in
    ( graph, \v -> Internal.find v vertexMap, keyVertex )



-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------


{-| (O(V+E)). A spanning forest of the graph, obtained from a depth-first
search of the graph starting from each vertex in an unspecified order.
-}
dff : Graph -> List (Tree Vertex)
dff g =
    dfs g (vertices g)


{-| (O(V+E)). A spanning forest of the part of the graph reachable from the
listed vertices, obtained from a depth-first search of the graph starting at
each of the listed vertices in order.

This function deviates from King and Launchbury's implementation by
bundling together the functions generate, prune, and chop for efficiency
reasons.

-}
dfs : Graph -> List Vertex -> List (Tree Vertex)
dfs g vs0 =
    let
        go : List Vertex -> SetM s (List (Tree Vertex))
        go vrtcs =
            case vrtcs of
                [] ->
                    pure []

                v :: vs ->
                    contains v
                        |> bind
                            (\visited ->
                                if visited then
                                    go vs

                                else
                                    include v
                                        |> bind
                                            (\_ ->
                                                go (Maybe.withDefault [] (Internal.find v g))
                                                    |> bind
                                                        (\subForest ->
                                                            go vs
                                                                |> bind
                                                                    (\bs ->
                                                                        pure (Tree.tree v subForest :: bs)
                                                                    )
                                                        )
                                            )
                            )
    in
    run (Internal.bounds g) (go vs0)



-- #else /* !USE_ST_MONAD */


{-| Portable implementation using IntSet.
-}
type alias IntSet =
    Set Int


type SetM s a
    = SetM (IntSet -> ( a, IntSet ))


bind : (a -> SetM s b) -> SetM s a -> SetM s b
bind f (SetM v) =
    SetM
        (\s ->
            let
                ( x, s_ ) =
                    v s
            in
            case f x of
                SetM v_ ->
                    v_ s_
        )


pure : a -> SetM s a
pure x =
    SetM (\s -> ( x, s ))


run : Bounds -> SetM s a -> a
run _ (SetM act) =
    Tuple.first (act Set.empty)


contains : Vertex -> SetM s Bool
contains v =
    SetM (\m -> ( Set.member v m, m ))


include : Vertex -> SetM s ()
include v =
    SetM (\m -> ( (), Set.insert v m ))



-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------
--
------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------


postorder : Tree a -> List a -> List a
postorder node =
    postorderF (Tree.children node) << (::) (Tree.label node)


postorderF : List (Tree a) -> List a -> List a
postorderF ts =
    List.foldr (<<) identity <| List.map postorder ts


postOrd : Graph -> List Vertex
postOrd g =
    postorderF (dff g) []



------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------


{-| The strongly connected components of a graph, in reverse topological order.

    scc (buildG ( 0, 3 ) [ ( 3, 1 ), ( 1, 2 ), ( 2, 0 ), ( 0, 1 ) ])
        == [ Tree.tree 0 [ Tree.tree 1 [ Tree.tree 2 [] ] ]
           , Tree.tree 3 []
           ]

-}
scc : Graph -> List (Tree Vertex)
scc g =
    dfs g (List.reverse (postOrd (transposeG g)))
