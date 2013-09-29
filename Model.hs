{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

module Model where

import qualified Data.List as DL
import qualified System.Random as R
import Control.Lens

data Position = Position { _x :: Int, _y :: Int } deriving (Show, Eq, Ord)
data Heading = North | East | South | West deriving (Show, Eq, Enum)
succHeading West = North
succHeading h = succ h
predHeading North = West
predHeading h = pred h

type Snake = [Position]
data Status = Playing | Quit | Lost deriving (Eq, Show)
data Star = SimpleStar Position | TimedStar Position Int deriving (Eq, Show)
data Game = Game { _heading :: Heading
                 , _lastTickHeading :: Heading
                 , _snake :: Snake
                 , _stars :: [Star]
                 , _stretching :: Int
                 , _status :: Status
                 , _rows :: Int
                 , _columns :: Int
                 , _inputQueue :: [Heading]
                 , _score :: Int
                 } deriving Show

class HasPosition a where
  getPosition :: a -> Position

inSamePosition :: (HasPosition a, HasPosition b) => a -> b -> Bool
inSamePosition a b = (getPosition a) == (getPosition b)

instance HasPosition Snake where
  getPosition = head

instance HasPosition Star where
  getPosition (SimpleStar p) = p
  getPosition (TimedStar p _) = p

instance HasPosition Position where
  getPosition = id


makeLenses ''Position
makeLenses ''Game

offset :: Heading -> Position -> Position
offset North = y `over` pred
offset South = y `over` succ
offset East = x `over` succ
offset West = x `over` pred

headingBetween :: Position -> Position -> Heading
headingBetween (Position x0 y0) (Position x1 y1)
  | x0 + 1 == x1 = East
  | x0 - 1 == x1 = West
  | y0 + 1 == y1 = South
  | y0 - 1 == y1 = North
  -- Wrapped around
  | y0 == y1 && x0 < x1 = West
  | y0 == y1 && x0 > x1 = East
  | x0 == x1 && y0 < y1 = North
  | x0 == x1 && y0 > y1 = South

wrapAround :: Game -> Game
wrapAround game = game&snake.mapped %~ wrapXY
  where wrapXY = (over x $ wrapOne $ game^.columns) . (over y $ wrapOne $ game^.rows)
        wrapOne limit n
          | n == -1 = limit -1
          | n == limit = 0
          | otherwise = n

moveSnake :: Game -> Game
moveSnake = updateHead . addNeck . decStretching . updateTail . updateLastTickHeading where
      updateHead g = g&snake._head %~ (offset $ g^.heading)
      addNeck g = g&snake._tail %~ (g^.snake.to head :)
      updateTail g = g&snake._tail %~ if g^.stretching > 0 then id else init
      decStretching = stretching %~ (\n -> max 0 (n - 1))
      updateLastTickHeading g = g&lastTickHeading .~ g^.heading

turnSnake :: Game -> Game
turnSnake g
  | null $ g^.inputQueue = g
  | otherwise = g & shiftInputQueue . turnUnlessReverse where
      shiftInputQueue = inputQueue %~ tail
      turnUnlessReverse =
        if new `elem` [succHeading old, predHeading old]
          then heading.~new
          else id
        where old = g^.heading
              new = g^.inputQueue.to head

detectCrash :: Game -> Game
detectCrash game =
  if crashed then game&status.~Lost else game
  where crashed = any ((>1) . length) $ DL.group $ DL.sort $ game^.snake

generateStarPosition :: Game -> IO Position
generateStarPosition g = do
  gen <- R.getStdGen
  x <- R.getStdRandom $ R.randomR (0, g^.columns-1)
  y <- R.getStdRandom $ R.randomR (0, g^.rows-1)
  let p = Position x y
  if p `elem` (g^.snake) || any (inSamePosition p) (g^.stars) then generateStarPosition g else return p

placeStars :: Game -> IO Game
placeStars game = do
  game' <- if not haveSimpleStar then placeSimpleStar game else return game
  wantTimedStar <- generateWantTimedStar
  if wantTimedStar && not haveSimpleStar && not haveTimedStar then placeTimedStar game' else return game'
  where generateWantTimedStar = do
          rand <- R.getStdRandom $ R.randomR (0, 10)
          return $ (rand::Int) == 0
        haveTimedStar = any isTimedStar (_stars game)
        haveSimpleStar = any isSimpleStar (_stars game)

placeSimpleStar :: Game -> IO Game
placeSimpleStar g = do
  pos <- generateStarPosition g
  return $ g&stars %~ (++[SimpleStar pos])

placeTimedStar :: Game -> IO Game
placeTimedStar g = do
  pos <- generateStarPosition g
  return $ g&stars %~ (++[TimedStar pos 7])

tickTimedStars :: Game -> Game
tickTimedStars = (stars.mapped %~ transform) . (stars %~ filter notExpiredStar)
    where transform s@(SimpleStar pos) = s
          transform s@(TimedStar pos i) = TimedStar pos (i-1)
          notExpiredStar s@(SimpleStar pos) = True
          notExpiredStar s@(TimedStar pos i) = i > 0

isSimpleStar (SimpleStar _) = True
isSimpleStar (TimedStar _ _) = False
isTimedStar = not . isSimpleStar

stretchIfStarHit :: Int -> Game -> IO Game
stretchIfStarHit stretchLength g = placeStars $ foldr handleStarHit g hitStars where
  hitStars = filter (inSamePosition $ g^.snake) (g^.stars)
  handleStarHit hit@(SimpleStar pos) game = handleStarHit' hit 1 game
  handleStarHit hit@(TimedStar pos score) game = handleStarHit' hit score game
  handleStarHit' star hitScore = (score +~ hitScore)
                               . (stars %~ filter (/= star))
                               . (stretching +~ stretchLength * (length hitStars))

newGame :: Int -> Int -> IO Game
newGame rows columns = do
  let snake = [Position x 10 | x <- [5..10]]
  let heading = West
  placeStars Game {
    _heading = heading
  , _lastTickHeading = heading
  , _snake = snake
  , _stars = []
  , _stretching = 0
  , _status = Playing
  , _rows = rows
  , _columns = columns
  , _inputQueue = []
  , _score = 0
  }
