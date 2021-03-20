module Test.Main where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual)
import Effect.Class.Console (log)
import Data.Array (foldl)
import Data.Set as Set
import Data.Graph.AStar (Point, runAStar)
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
    result = runTest

    expected =
      [ Tuple 0 0
      , Tuple 0 1
      , Tuple 0 2
      , Tuple 0 3
      , Tuple 1 4
      , Tuple 2 4
      , Tuple 3 4
      , Tuple 4 4
      ]
  in
    do
      log $ show result.matrix
      log $ show result.path
      assertEqual { expected, actual: result.path }

runTest :: { matrix :: Matrix Cell, path :: Array Point }
runTest =
  let
    start = Tuple 0 0

    goal = Tuple 4 4

    blocked = Set.empty # Set.insert Blocked

    path = runAStar blocked start goal testWorld
  in
    { matrix: showPath start goal path testWorld
    , path: path
    }

testWorld :: Matrix Cell
testWorld =
  let
    arr =
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

    mat = fromMaybe Matrix.empty $ Matrix.fromArray arr

    setCell x y v matrix = case Matrix.set x y v matrix of
      Just m -> m
      _ -> matrix
  in
    mat

showPath :: Point -> Point -> Array Point -> Matrix Cell -> Matrix Cell
showPath start goal path world =
  path
    # foldl
        ( \xs (Tuple x y) -> case Matrix.set x y Walk xs of
            Just m -> m
            _ -> xs
        )
        world
    # Matrix.indexedMap
        ( \x y val ->
            if Tuple x y == start then
              Start
            else if Tuple x y == goal then
              Goal
            else
              val
        )
