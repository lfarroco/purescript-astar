module Data.Graph.AStar (runAStar, Vector2d, vector2d) where

import Prelude
import Control.Apply (lift2)
import Data.Array (filter, foldl, head, length, mapMaybe, sortWith)
import Data.Int (pow, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Math (sqrt, sqrt2)
import Matrix (Matrix)
import Matrix as Matrix

-- An alias for a representation of a 2d coordinate
type Vector2d
  = { x :: Int, y :: Int }

-- Creates a Vector2d 
vector2d :: Int -> Int -> Vector2d
vector2d x y = { x, y }

runAStar :: forall a. Ord a => Set a -> Boolean -> Vector2d -> Vector2d -> Array (Array a) -> Array Vector2d
runAStar blockedCells useDiagonal start target grid =
  let
    openSet = Map.empty # Map.insert start 0.0

    knownCosts = Map.empty # Map.insert start 0.0

    cameFrom = Map.empty

    closedSet = Set.empty

    ns = [ -1, 0, 1 ]

    directions =
      lift2 vector2d ns ns
        # filter \p ->
            p /= vector2d 0 0
              && ( if useDiagonal then
                    true
                  else
                    not (isDiagonal p)
                )

    world = Matrix.fromArray grid # fromMaybe Matrix.empty
  in
    step blockedCells directions { openSet, closedSet, knownCosts, cameFrom, target } world

isDiagonal :: Vector2d -> Boolean
isDiagonal { x, y } = pow x 2 + pow y 2 # toNumber # sqrt # (==) sqrt2

getNeighbors ::
  forall a.
  Ord a =>
  Set a ->
  Array Vector2d -> Vector2d -> Set Vector2d -> Matrix a -> Array Vector2d
getNeighbors blockedCells directions sourceVector closedSet matrix =
  let
    getCell coord =
      let
        neighbor = sourceVector + coord
      in
        case Matrix.get neighbor.x neighbor.y matrix of
          Just cell
            | Set.member cell blockedCells -> Nothing
            | Set.member neighbor closedSet -> Nothing
            | otherwise -> Just neighbor
          Nothing -> Nothing
  -- todo move to parent fn
  in
    mapMaybe getCell directions

-- The "frontier" of the search algorithm. Contains path candidates. On each 
-- step, the one with lower cost is chosen to expand the search area.
type OpenSet
  = Map Vector2d Number

-- Cells that were already been visited whose optimal path is already known.
type ClosedSet
  = Set Vector2d

-- Cost for each visited cell. Cells in the OpenSet might change value if we
-- discover another shortest path to it.
type KnownCosts
  = Map Vector2d Number

-- Registers what is the "parent" each cell, allowing us to backtrack the
-- source when the goal is found.
type CameFrom
  = Map Vector2d Vector2d

type State
  = { openSet :: OpenSet
    , closedSet :: ClosedSet
    , knownCosts :: KnownCosts
    , cameFrom :: CameFrom
    , target :: Vector2d
    }

getLowestRank :: OpenSet -> Maybe Vector2d
getLowestRank = Map.toUnfoldable >>> sortWith snd >>> head >>> map fst

step :: forall a. Ord a => Set a -> Array Vector2d -> State -> Matrix a -> Array Vector2d
step blockedCells directions { openSet, closedSet, knownCosts, cameFrom, target } world = case getLowestRank openSet of
  Nothing -> []
  Just current ->
    if current == target then
      traceParent target state.cameFrom <> [ target ]
    else
      step blockedCells directions state world
    where
    closedSetWithoutCurrent = closedSet # Set.insert current

    state =
      getNeighbors blockedCells directions current closedSetWithoutCurrent world
        # foldl
            ( \acc next ->
                let
                  cost = Map.lookup current acc.knownCosts # fromMaybe 0.0

                  usesDiagonal = length directions == 8

                  moveToNextCost
                    | usesDiagonal && distance current next == sqrt2 = cost + sqrt2
                    | otherwise = cost + 1.0

                  nextCost = Map.lookup next acc.knownCosts

                  nextIsNew = nextCost == Nothing

                  pathToNextIsBetter = Just moveToNextCost < nextCost

                  heuristic = distance next target

                  totalCost = moveToNextCost + heuristic
                in
                  if nextIsNew || pathToNextIsBetter then
                    { openSet: Map.insert next totalCost acc.openSet
                    , closedSet: closedSetWithoutCurrent
                    , knownCosts: Map.insert next moveToNextCost acc.knownCosts
                    , cameFrom: Map.insert next current acc.cameFrom
                    , target
                    }
                  else
                    acc
            )
            { openSet: openSet # Map.update (\_ -> Nothing) current
            , knownCosts
            , cameFrom
            , closedSet: closedSetWithoutCurrent
            , target
            }

traceParent :: Vector2d -> Map Vector2d Vector2d -> Array Vector2d
traceParent point index = case Map.lookup point index of
  Just prev -> traceParent prev index <> [ prev ]
  Nothing -> []

distance :: Vector2d -> Vector2d -> Number
distance p1 p2 = p2 - p1 # \{ x, y } -> x * x + y * y # toNumber # sqrt
