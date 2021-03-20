module Test.Main where

import Data.Pathfinding.AStar (runAStarDiagonal, runAStarNonDiagonal, Vector2d, vector2d)
import Prelude (class Eq, class Ord, class Show, Unit, discard, map, show, (#), ($), (==))
import Effect (Effect)
import Test.Assert (assertEqual)
import Effect.Class.Console (log)
import Data.Array (foldl)
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Matrix (Matrix)
import Matrix as Matrix

data Cell
  = Walk
  | Blocked
  | Empty
  | Start
  | Goal

derive instance eqCell :: Eq Cell

derive instance ordCell :: Ord Cell

instance showCell :: Show Cell where
  show Walk = "= "
  show Empty = " "
  show Start = "✯ "
  show Blocked = "# "
  show Goal = "⚑ "


main :: Effect Unit
main =
  let
    diagonalMovement = runTest true

    diagonalExpected =
      [ vector2d 0 0
      , vector2d 0 1
      , vector2d 0 2
      , vector2d 0 3
      , vector2d 1 4
      , vector2d 2 4
      , vector2d 3 4
      , vector2d 4 4
      ]

    nonDiagonalMovement = runTest false

    nonDiagonalExpected =
      [ (vector2d 0 0)
      , (vector2d 0 1)
      , (vector2d 0 2)
      , (vector2d 0 3)
      , (vector2d 0 4)
      , (vector2d 1 4)
      , (vector2d 2 4)
      , (vector2d 3 4)
      , (vector2d 4 4)
      ]
  in
    do
      log $ "-----Diagonal Movement-----"
      log $ show diagonalMovement.matrix
      log $ show diagonalMovement.path
      assertEqual { expected: diagonalExpected, actual: diagonalMovement.path }

      log $ "-----Non-Diagonal Movement-----"
      log $ show nonDiagonalMovement.matrix
      log $ show nonDiagonalMovement.path
      assertEqual { expected: nonDiagonalExpected, actual: nonDiagonalMovement.path }

runTest :: Boolean -> { matrix :: Matrix Cell, path :: Array Vector2d }
runTest useDiagonal =
  let
    start = {x: 0, y: 0}

    goal = {x: 4 , y: 4}

    pathfinder = if useDiagonal then runAStarDiagonal else runAStarNonDiagonal

    path = pathfinder [Blocked] start goal testWorld
  in
    { matrix: showPath start goal path testWorld
    , path
    }

testWorld :: Array (Array Cell)
testWorld =
  [ [ 0, 0, 0, 0, 0 ]
  , [ 0, 0, 0, 1, 0 ]
  , [ 0, 0, 0, 1, 0 ]
  , [ 0, 1, 1, 1, 0 ]
  , [ 0, 0, 0, 0, 0 ]
  ]
    # map
        ( \n ->
            map
              ( \c ->
                  if c == 0 then
                    Empty
                  else
                    Blocked
              )
              n
        )

showPath ::
  Vector2d ->
  Vector2d ->
  Array (Vector2d) ->
  Array (Array Cell) ->
  Matrix Cell
showPath start goal path world =
  path
    # foldl
        ( \xs { x, y} -> case Matrix.set x y Walk xs of
            Just m -> m
            _ -> xs
        )
        (Matrix.fromArray world # fromMaybe Matrix.empty)
    # Matrix.indexedMap
        ( \x y val ->
          if { x ,y }== start then
              Start
            else if { x, y} == goal then
              Goal
            else
              val
        )
