module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Array (foldl)
import Data.Graph.AStar (Cell(..), Point(..), runAStar)
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
      [ [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ]
      , [ 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1 ]
      , [ 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ]
      , [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ]
      , [ 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1 ]
      , [ 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1 ]
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
          ( \xs (Point x y) -> case Matrix.set x y Walk xs of
              Just m -> m
              _ -> xs
          )
          world
      # Matrix.indexedMap
          ( \x y val ->
                if Point x y == start then
                  Start
                else if Point x y == goal then
                  Goal
                else 
                  val
          )
