module Data.Graph.AStar where

import Prelude
import Control.Apply (lift2)
import Data.Array (filter, foldl, head, mapMaybe, sortWith)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple
import Math (abs, sqrt2)
import Matrix (Matrix)
import Matrix as Matrix

-- can be extended to "Point x y weight"
type Point = Tuple Int Int

data Cell
  = Walk
  | Empty
  | Blocked
  | Start
  | Goal

derive instance eqCell :: Eq Cell

instance showCell :: Show Cell where
  show Walk = "x "
  show Empty = "  "
  show Start = "✯ "
  show Blocked = "w "
  show Goal = "✯ "

type Grid
  = Map Point Cell

getNeighbors :: Point -> Matrix Cell -> Array Point
getNeighbors (Tuple x y) matrix =
  let
    getCell (Tuple tx ty) = case Matrix.get (tx + x) (ty + y) matrix of
      Just cell ->
        if cell == Blocked then
          Nothing
        else
          Just $ Tuple (tx + x) (ty + y)
      Nothing -> Nothing

    ns = [ -1, 0, 1 ]

    directions = lift2 Tuple ns ns # filter (\p -> p /= Tuple 0 0)
  in
    mapMaybe getCell directions

-- Making it more general: can receive an array of walkable tiles, and another of non walkable,
-- with the same type as the matrix
findPath ::
  Map Point Number ->
  Map Point Number ->
  Map Point Point ->
  Point ->
  Matrix Cell -> Array Point
findPath openSet costMap cameFrom target world =
  let
    maybeHead =
      openSet
        # Map.toUnfoldable
        # sortWith snd
        # head
        # map fst
  in
    case maybeHead of
      Nothing -> []
      Just current ->
        let
          openSetWithoutCurrent = openSet # Map.update (\_ -> Nothing) current

          state =
            getNeighbors current world
              # foldl
                  ( \acc next ->
                      let
                        cost = Map.lookup current acc.costMap # fromMaybe 0.0

                        isDiagonal = distance current next == 2.0

                        moveToNextCost =
                          if isDiagonal then
                            cost + sqrt2
                          else
                            cost + 1.0

                        nextCost = Map.lookup next acc.costMap

                        nextIsNew = nextCost == Nothing

                        pathToNextIsBetter = Just moveToNextCost < nextCost

                        heuristic = distance next target

                        totalCost = moveToNextCost + heuristic
                      in
                        if nextIsNew || pathToNextIsBetter then
                          { openSet: Map.insert next totalCost acc.openSet
                          , costMap: Map.insert next moveToNextCost acc.costMap
                          , cameFrom: Map.insert next current acc.cameFrom
                          }
                        else
                          acc
                  )
                  { openSet: openSetWithoutCurrent, costMap, cameFrom }
        in
          if current == target then
            traceParent target state.cameFrom <> [ target ]
          else
            findPath state.openSet state.costMap state.cameFrom target world

traceParent :: Point -> Map Point Point -> Array Point
traceParent point index = case Map.lookup point index of
  Just prev -> traceParent prev index <> [ prev ]
  Nothing -> []

distance :: Point -> Point -> Number
distance p1 p2  =
  let
    mag (Tuple x y ) = x + y
  in
    p2 - p1  # mag # toNumber # abs

runAStar :: Point -> Point -> Matrix Cell -> Array Point
runAStar start goal grid =
  let
    openSet = Map.empty # Map.insert start 0.0

    costMap = Map.empty # Map.insert start 0.0

    cameFrom = Map.empty
  in
    findPath openSet costMap cameFrom goal grid
