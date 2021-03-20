module Test.Main where

import Prelude (class Eq, class Ord, class Show, Unit, discard, map, show, (#), ($), (==))
import Effect (Effect)
import Test.Assert (assertEqual)
import Effect.Class.Console (log)
import Data.Array (foldl)
import Data.Set as Set
import Data.Graph.AStar (runAStar)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple
import Matrix (Matrix)
import Matrix as Matrix

data Cell
  = Walk
  | Empty
  | Blocked
  | Start
  | Goal

derive instance eqCell :: Eq Cell

derive instance ordCell :: Ord Cell

instance showCell :: Show Cell where
  show Walk = "x "
  show Empty = "  "
  show Start = "✯ "
  show Blocked = "w "
  show Goal = "✯ "

main :: Effect Unit
main =
  let
    diagonalMovement = runTest true

    diagonalExpected =
      [ Tuple 0 0
      , Tuple 0 1
      , Tuple 0 2
      , Tuple 0 3
      , Tuple 1 4
      , Tuple 2 4
      , Tuple 3 4
      , Tuple 4 4
      ]

    nonDiagonalMovement = runTest false

    nonDiagonalExpected =
      [ (Tuple 0 0)
      , (Tuple 0 1)
      , (Tuple 0 2)
      , (Tuple 0 3)
      , (Tuple 0 4)
      , (Tuple 1 4)
      , (Tuple 2 4)
      , (Tuple 3 4)
      , (Tuple 4 4)
      ]
  in
    do
      log $ "-----Diagonal Movement-----"
      log $ show diagonalMovement.matrix
      log $ show diagonalMovement.path
      --assertEqual { expected: diagonalExpected, actual: diagonalMovement.path }
      log $ "-----Non-Diagonal Movement-----"
      log $ show nonDiagonalMovement.matrix
      log $ show nonDiagonalMovement.path

--assertEqual { expected: nonDiagonalExpected, actual: nonDiagonalMovement.path }
runTest :: Boolean -> { matrix :: Matrix Cell, path :: Array (Tuple Int Int) }
runTest diagonal =
  let
    start = Tuple 0 0

    goal = Tuple 3 3

    blocked = Set.empty # Set.insert Blocked

    path = runAStar blocked diagonal start goal testWorld
  in
    { matrix: showPath start goal path testWorld
    , path
    }

testWorld :: Array (Array Cell)
testWorld =
  [ [ 0, 0, 1, 0 ]
  , [ 0, 0, 0, 0 ]
  , [ 1, 1, 1, 0 ]
  , [ 0, 1, 0, 0 ]
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
  Tuple Int Int ->
  Tuple Int Int ->
  Array (Tuple Int Int) ->
  Array (Array Cell) ->
  Matrix Cell
showPath start goal path world =
  path
    # foldl
        ( \xs (Tuple x y) -> case Matrix.set x y Walk xs of
            Just m -> m
            _ -> xs
        )
        (Matrix.fromArray world # fromMaybe Matrix.empty)
    # Matrix.indexedMap
        ( \x y val ->
            if Tuple x y == start then
              Start
            else if Tuple x y == goal then
              Goal
            else
              val
        )
