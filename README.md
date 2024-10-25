# Elm Graph

A graph data structure library for Elm, inspired by [Haskell Data.Graph](https://hackage.haskell.org/package/containers-0.7/docs/Data-Graph.html). This package implements a finite, directed graph type using an adjacency list representation, where vertices are identified by integers. It also includes an SCC (Strongly Connected Component) type for managing and analyzing strongly-connected components within the graph.

## Getting Started

### Installation

To install the package, run the following:

```bash
elm install guida-lang/graph
```

### Usage

Here’s an example of how to use the `Data.Graph` in Elm:

```elm
import Data.Graph as Graph exposing (Graph)

graphWith3Vertices : Graph
graphWith3Vertices =
    let
        ( graph, nodeFromVertex, vertexFromKey ) =
            Graph.graphFromEdges [ ( "a", 'a', [ 'b' ] ), ( "b", 'b', [ 'c' ] ), ( "c", 'c', [] ) ]
    in
    graph
```

## Documentation

For full API documentation and more examples, please visit the
[Elm package documentation](https://package.elm-lang.org/packages/guida-lang/glsl/1.0.0).

## Contributing

Contributions are welcome! If you have ideas for improvements or find bugs, feel free to open an
issue or submit a pull request.

### To contribute:

1. Fork the repository.
2. Create a new feature branch.
3. Commit your changes.
4. Submit a pull request.
