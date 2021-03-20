module Data.Graph.AStar (runAStar) where 

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
import Data.Set as Set
import Data.Set (Set)

runAStar :: forall a. Ord a => Set a -> Boolean -> Point -> Point -> Array (Array a) -> Array Point
runAStar blocked diagonal start target grid =
  let
    openSet = Map.empty # Map.insert start 0.0

    knownCosts = Map.empty # Map.insert start 0.0

    cameFrom = Map.empty

    closedSet = Set.empty

    world = Matrix.fromArray grid # fromMaybe Matrix.empty
  in
    step blocked diagonal { openSet, closedSet, knownCosts, cameFrom, target } world

type Point
  = Tuple Int Int

sumTuple (Tuple x y) = x + y # toNumber # abs

getNeighbors :: forall a. Ord a => Set a -> Boolean -> Point -> Set Point -> Matrix a -> Array Point
getNeighbors blocked diagonal (Tuple x y) closedSet matrix =
  let
    getCell (Tuple x' y') =
      let
        x'' = x + x'

        y'' = y + y'

        point = Tuple x'' y''
      in
        case Matrix.get x'' y'' matrix of
          Just cell ->
            if Set.member cell blocked || Set.member point closedSet then
              Nothing
            else
              Just point
          Nothing -> Nothing

    ns = [-1,0,1]

    directions = lift2 Tuple ns ns # filter \p -> sumTuple p <= (if diagonal then 2.0 else 1.0 )
  in
    mapMaybe getCell directions

type State
  = { openSet :: Map Point Number
    , closedSet :: Set Point
    , knownCosts :: Map Point Number
    , cameFrom :: Map Point Point
    , target :: Point
    }

step :: forall a. Ord a => Set a -> Boolean -> State -> Matrix a -> Array Point
step blocked diagonal { openSet, closedSet, knownCosts, cameFrom, target } world =
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
          openSet_ = openSet # Map.update (\_ -> Nothing) current

          closed_ = closedSet # Set.insert current

          state =
            getNeighbors blocked diagonal current closed_ world
              # foldl
                  ( \acc next ->
                      let
                        cost = Map.lookup current acc.knownCosts # fromMaybe 0.0

                        moveToNextCost =
                          if diagonal && distance current next == 2.0 then
                            cost + sqrt2
                          else
                            cost + 1.0

                        nextCost = Map.lookup next acc.knownCosts

                        nextIsNew = nextCost == Nothing

                        pathToNextIsBetter = Just moveToNextCost < nextCost

                        heuristic = distance next target

                        totalCost = moveToNextCost + heuristic
                      in
                        if nextIsNew || pathToNextIsBetter then
                          { openSet: Map.insert next totalCost acc.openSet
                          , closedSet: closed_
                          , knownCosts: Map.insert next moveToNextCost acc.knownCosts
                          , cameFrom: Map.insert next current acc.cameFrom
                          , target
                          }
                        else
                          acc
                  )
                  { openSet: openSet_, knownCosts, cameFrom, closedSet: closed_, target }
        in
          if current == target then
            traceParent target state.cameFrom <> [ target ]
          else
            step blocked diagonal state world

traceParent :: Point -> Map Point Point -> Array Point
traceParent point index = case Map.lookup point index of
  Just prev -> traceParent prev index <> [ prev ]
  Nothing -> []

distance :: Point -> Point -> Number
distance p1 p2 = p2 - p1 # sumTuple
