# purescript-astar

An A\* implementation on PureScript for 2d boards.

```
let
    start = Tuple 0 0

    goal = Tuple 3 3

    world =
        [ [ 0, 0, 1, 0 ]
        , [ 0, 0, 0, 0 ]
        , [ 1, 1, 1, 0 ]
        , [ 0, 1, 0, 0 ]
        ]

    blocked = Set.insert 1 Set.empty

    diagonal = false
in
    runAStar blocked diagonal start goal world

-- the result will be an array of tuples containg the path coordinates

[(Tuple 0 0),(Tuple 0 1),(Tuple 1 1),(Tuple 2 1),(Tuple 3 1),(Tuple 3 2),(Tuple 3 3)]

-- which corresponds to the following path
✯ ,   , w ,
x , x , x , x
w , w , w , x
  , w ,   , ✯
```

For `blocked` you can provide a `Set` with a custom type - its element will be
considered unwalkable. Eg. you can have

```
data Cell = Wall | Grass
```

and then pass as `blocked`:

```
Set.insert 1 Set.empty
```

Note that walkable cells in the `world` parameter should also match the type, so
they will need to be either `Wall` or `Grass`.

Based on the work of

- https://www.redblobgames.com/pathfinding/a-star/introduction.html
- https://github.com/weissi/astar
- https://github.com/krisajenkins/purescript-astar
