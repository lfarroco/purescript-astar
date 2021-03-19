module Data.Graph.AStar where

import Prelude

import Control.Apply (lift2)
import Data.Array (filter, foldl, head, mapMaybe, sortWith)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Math (abs, sqrt2)
import Matrix (Matrix)
import Matrix as Matrix

-- can be extended to "Point x y weight"
data Point
  = Point Int Int

instance showPoint :: Show Point where
  show (Point x y) = "Point " <> show x <> " " <> show y

derive instance eqPoint :: Eq Point

derive instance ordPoint :: Ord Point

data Cell
  = Grass
  | Rock
  | Wall

derive instance eqCell :: Eq Cell

data PathCell
  = Walk
  | Empty
  | Start
  | Blocked
  | Goal

derive instance eqPathCell :: Eq PathCell

instance showPathCell :: Show PathCell where
  show Walk = "x "
  show Empty = "  "
  show Start = "S "
  show Blocked = "█ "
  show Goal = "✯ "

instance showCell :: Show Cell where
  show Grass = " ^ "
  show Rock = " O "
  show Wall = "█ "

type Grid
  = Map Point Cell

getNeighbors :: Point -> Matrix Cell -> Array Point
getNeighbors (Point x y) matrix =
  let
    getCell (Point dx dy) = case Matrix.get (dx + x) (dy + y) matrix of
      Just cell ->
        if cell == Rock || cell == Wall then
          Nothing
        else
          Just $ Point (dx + x) (dy + y)
      Nothing -> Nothing

    ns = [ -1, 0, 1 ]

    directions = lift2 Point ns ns # filter (\p -> p /= Point 0 0)
  in
    mapMaybe getCell directions

findPath ::
  Map Point Number ->
  Map Point Number ->
  Map Point Point ->
  Point ->
  Matrix Cell -> Array Point
findPath openSet costMap cameFrom target world =
  let
    mhead =
      openSet
        # Map.toUnfoldable
        # sortWith (\(Tuple k v) -> v)
        # head
        # map (\(Tuple point priority) -> point)
  in
    case mhead of
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

                        foundABetterPath = Just moveToNextCost < nextCost

                        heuristic = distance next target

                        totalCost = moveToNextCost + heuristic
                      in
                        if nextIsNew || foundABetterPath then
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
distance (Point x y) (Point p1 p2) =
  let
    t = toNumber

    x' = abs (t p1) - (t x)

    y' = abs (t p2) - (t y)
  in
    x' + y'

runAStar :: Point -> Point -> Matrix Cell -> Array Point
runAStar start goal grid =
  let
    openSet = Map.empty # Map.insert start 0.0

    costMap = Map.empty # Map.insert start 0.0

    cameFrom = Map.empty
  in
    findPath openSet costMap cameFrom goal grid
