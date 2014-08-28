-- solver for gameaboutsquares.com
-- by Harald Bögeholz

-- | Main entry point to the application.
module Main where

import Prelude hiding (Left, Right)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, group)
import Control.Arrow ((&&&))

type Position = (Int, Int)
type Rectangle = (Position, Position)
data Color = Red | Green | Blue | Black | Yellow | Orange deriving (Show, Eq, Ord)
data Direction = Up | Down | Left | Right deriving (Show, Eq, Ord)
data Square = Square { sqColor :: Color
                     , sqDirection :: Direction
                     } deriving (Show, Eq, Ord)
type State = M.Map Position Square
data Puzzle = Puzzle { pArrows  :: M.Map Position Direction
                     , pTargets :: M.Map Position Color
                     , pInitial :: State
                     , pBoundingBox :: Rectangle
                     } deriving (Show, Eq)

extendRectangle :: Position -> Rectangle -> Rectangle
extendRectangle (r, c) ((minR, minC), (maxR, maxC)) = ((min minR r, min minC c), (max maxR r, max maxC c))

inR :: Position -> Rectangle -> Bool
inR (r, c) ((minR, minC), (maxR, maxC)) = r >= minR && r <= maxR && c >= minC && c <= maxC

createPuzzle :: [(Position, Direction)] -> [(Position, Color)] -> [(Position, Square)] -> Puzzle
createPuzzle arrows targets initial = Puzzle (M.fromList arrows) (M.fromList targets) (M.fromList initial) boundingBox
    where ((minR, minC), (maxR, maxC)) = foldr extendRectangle (h, h) $ tail positions
          l = length initial
          boundingBox = ((minR-l, minC-l), (maxR+l, maxC+l))
          positions = map fst arrows ++ map fst targets
          h = head positions
          
level0 = createPuzzle [] [((3,1), Red)] [((1,1), Square Red Down)]
                     
level2 = createPuzzle []
                      [((1,2), Blue), ((1,3), Red), ((2,2), Black)]
                      [((1,1), Square Red Right)
                      ,((2,4), Square Black Left)
                      ,((3,2), Square Blue Up)
                      ]

level3 = createPuzzle []
                      [((4,1), Red), ((6,3), Blue)]
                      [((1,3), Square Blue Down), ((3,5), Square Red Left)]

level4 = createPuzzle []
                      [((2,2), Red), ((3,3), Black), ((4,4), Blue)]
                      [((1,2), Square Red Down), ((2,1), Square Blue Right), ((2,3), Square Black Down)]

level19 = createPuzzle [((4,1), Down), ((5,2), Down), ((5,4), Left)
                       ,((6,2), Right), ((6,3), Up), ((7,1), Right)
                       ,((7,4), Up)
                       ]
                       [((1,3), Red), ((3,3), Blue), ((5,3), Black)]
                       [((4,1), Square Red Down)
                       ,((5,2), Square Blue Down)
                       ,((6,3), Square Black Up)
                       ]

level33 = createPuzzle [((2,2), Down), ((4,2), Right), ((4,4), Up), ((2,5), Left)]
                       [((1,1), Blue), ((1,2), Red), ((1,3), Black)]
                       [((2,2), Square Blue Down), ((4,2), Square Black Right), ((4,4), Square Red Up)]

level35 = createPuzzle [((1,2), Down), ((2,5), Left), ((4,1), Right), ((5,4), Up)]
                       [((1,3), Black), ((1,4), Red), ((1,5), Orange)]
                       [((5,1), Square Orange Right), ((5,2), Square Red Right), ((5,3), Square Black Right)]
                      
direction :: Direction -> Position -> Position
direction Up    (r, c) = (r-1, c)
direction Down  (r, c) = (r+1, c)
direction Left  (r, c) = (r, c-1)
direction Right (r, c) = (r, c+1)

move :: Puzzle -> Position -> Direction -> State -> State
move puzzle pos dir state = M.insert newPos newSq $ M.delete pos pushedState
    where sq = state M.! pos
          newPos = direction dir pos
          pushedState = case M.lookup newPos state of
                          Nothing -> state
                          Just _ -> move puzzle newPos dir state
          newSq = case M.lookup newPos (pArrows puzzle) of
                    Nothing -> sq
                    Just newDir -> sq {sqDirection = newDir}

click :: Puzzle -> State -> Position -> (Color, State)
click puzzle state pos = (color, move puzzle pos dir state)
    where Square color dir = state M.! pos

allMoves :: Puzzle -> State -> [(Color, State)]
allMoves puzzle state = map (click puzzle state) $ M.keys state 

finished :: Puzzle -> State -> Bool
finished puzzle state = all correct $ M.assocs $ pTargets puzzle
    where correct (pos, color) = case M.lookup pos state of
                                   Nothing -> False
                                   Just (Square c _) -> c == color

validState :: Puzzle -> State -> Bool
validState puzzle state = all inside $ M.keys state
    where inside pos = pos `inR` pBoundingBox puzzle

solve' :: Puzzle -> [(State, [Color])] -> S.Set State -> [Color]
solve' puzzle ((s,cs):ss) visited = case find solution newStates of
                                 Just (c, _) -> reverse (c:cs)
                                 Nothing -> solve' puzzle (ss++newStatesColors) newVisited
    where nextStates = allMoves puzzle s
          newStates = filter (\(_, s) -> S.notMember s visited) nextStates
          newVisited = foldr S.insert visited $ map snd newStates
          newStates' = filter (\(_, s) -> validState puzzle s) newStates
          newStatesColors = map (\(c, s) -> (s, c:cs)) newStates'
          solution (_, s) = finished puzzle s

solve :: Puzzle -> [(Int, Color)]
solve puzzle = format $ solve' puzzle [(pInitial puzzle,[])] S.empty
    where format = map (length &&& head) . group
         
-- | The main entry point.
main :: IO ()
main = do
    print $ solve level19

