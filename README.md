# purescript-astar

[![CI](https://github.com/lfarroco/purescript-astar/actions/workflows/blank.yml/badge.svg)](https://github.com/lfarroco/purescript-astar/actions/workflows/blank.yml)

An A* search algorithm implementation on PureScript for 2d planes.

```
purescript

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
    blocked = Set.insert 1 Set.empty 

    useDiagonals = false
in
    runAStar blocked useDiagonals start goal world

-- the result will be an array of tuples containg the path coordinates
-- including the source and destination cells

[{ x: 0, y: 0 },{ x: 0, y: 1 },{ x: 0, y: 2 },{ x: 0, y: 3 },{ x: 0, y: 4 },{ x: 1, y: 4 },{ x: 2, y: 4 },{ x: 3, y: 4 },{ x: 4, y: 4 }]

-- which corresponds to the following path
✯ ,   ,   ,   ,   
= ,   ,   , # ,   
= ,   ,   , # ,   
= , # , # , # ,   
= , = , = , = , ⚑
```

You can use your own types for the `blocked` set, as long as they match the types used for your `world` 2d array. 
You can have

```
data Cell = Wall | Grass
```

and then place `Wall` in the `blocked` set parameter. 

Based on the work of:

- https://www.redblobgames.com/pathfinding/a-star/introduction.html
- https://github.com/weissi/astar
- https://github.com/krisajenkins/purescript-astar
