module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Array (foldl)
import Data.Graph.AStar (Cell(..), PathCell(..), Point(..), runAStar)
import Data.Maybe (Maybe(..), fromMaybe)
import Matrix (Matrix)
import Matrix as Matrix

main :: Effect Unit
main =
  let
    res = printTest
  in
    do
      log res.matrix
      log res.path

printTest :: { matrix :: String, path :: String }
printTest =
  let
    start = Point 1 1

    goal = Point 8 8

    path = runAStar start goal testWorld
  in
    { matrix: show $ showPath start goal path testWorld
    , path: show path
    }

testWorld :: Matrix Cell
testWorld =
  let
    arr =
      [ [ 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0 ]
      , [ 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0 ]
      , [ 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0 ]
      , [ 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0 ]
      , [ 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ]
      ]
        # map
            ( \n ->
                map
                  ( \c ->
                      if c == 0 then
                        Grass
                      else
                        Wall
                  )
                  n
            )

    mat = fromMaybe Matrix.empty $ Matrix.fromArray arr

    setCell x y v matrix = case Matrix.set x y v matrix of
      Just m -> m
      _ -> matrix
  in
    mat

showPath :: Point -> Point -> Array Point -> Matrix Cell -> Matrix PathCell
showPath start goal path world =
  let
    mtx = Matrix.repeat (Matrix.width world) (Matrix.height world) Empty
  in
    path
      # foldl
          ( \xs (Point x y) -> case Matrix.set x y Walk xs of
              Just m -> m
              _ -> xs
          )
          mtx
      # Matrix.indexedMap
          ( \x y val ->
              let
                cellType = fromMaybe Grass $ Matrix.get x y world

                next = case cellType of
                  Grass -> Empty
                  Rock -> Blocked
                  Wall -> Blocked
              in
                if Point x y == start then
                  Start
                else if Point x y == goal then
                  Goal
                else if val == Walk then
                  val
                else
                  next
          )
