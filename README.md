# purescript-astar

[![CI](https://github.com/lfarroco/purescript-astar/actions/workflows/blank.yml/badge.svg)](https://github.com/lfarroco/purescript-astar/actions/workflows/blank.yml)

An [A\* search](https://en.wikipedia.org/wiki/A*_search_algorithm) algorithm implementation on PureScript for 2D planes.

```
let
    start = {x: 0, y: 0}

    goal = {x: 3, y: 3}

    world =
        [ [ 0, 0, 1, 0 ]
        , [ 0, 0, 0, 0 ]
        , [ 1, 1, 1, 0 ]
        , [ 0, 1, 0, 0 ]
        ]

    -- tiles with `1` are non-walkable
    blocked = [1]
in
    runAStarNonDiagonal blocked start goal world

-- the result will be an array of tuples containg the path coordinates
-- including the source and destination cells
[ { x: 0, y: 0 }, { x: 0, y: 1 }, { x: 0, y: 2 }, { x: 0, y: 3 }, { x: 0, y: 4 }
, { x: 1, y: 4 }, { x: 2, y: 4 }, { x: 3, y: 4 }, { x: 4, y: 4 }
]

-- which corresponds to the following path
✯ ,   ,   ,   ,
= ,   ,   , # ,
= ,   ,   , # ,
= , # , # , # ,
= , = , = , = , ⚑
```

You can use your own types for the `blocked` parameter, as long as they implement
the Ord class and match the ones used for your `world` 2d array.
You can have

```
data Cell = Wall | Grass
```

and a grid with

```
[ [Grass, Wall, Grass]
, [Grass, Wall, Grass]
, [Grass, Grass, Grass]
]
```

and then use `[Wall]` as the `blocked` parameter.

Check the test module to see more usage examples.

Based on the work of:

- https://www.redblobgames.com/pathfinding/a-star/introduction.html
- https://github.com/weissi/astar
- https://github.com/krisajenkins/purescript-astar
