module Model where

import qualified Data.List as DL
import qualified System.Random as R

data Position = Position { x :: Int, y :: Int } deriving (Show, Eq, Ord)
data Heading = North | East | South | West deriving (Show, Eq)
data Status = Playing | Quit | Lost deriving Eq
data Star = SimpleStar Position | TimedStar Position Int deriving Eq
data Game = Game { gameHeading :: Heading
                 , gameLastTickHeading :: Heading
                 , gameSnake :: [Position]
                 , gameStars :: [Star]
                 , gameStretching :: Int
                 , gameStatus :: Status
                 , gameRows :: Int
                 , gameColumns :: Int
                 , gameInputQueue :: [Heading]
                 , gameScore :: Int
                 }

offset :: Position -> Heading -> Position
p@(Position x y) `offset` h 
  | h == North = p{y = y - 1}
  | h == East  = p{x = x + 1}
  | h == South = p{y = y + 1}
  | h == West  = p{x = x - 1}

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
wrapAround game@Game{gameSnake=snake, gameRows=rows, gameColumns=columns} = 
  game{gameSnake = map wrapAroundOne snake}
  where wrapAroundOne (Position x y) = Position (wrapX x) (wrapY y)
        wrapX x
          | x == -1 = columns - 1
          | x == columns = 0
          | otherwise = x
        wrapY y
          | y == -1 = rows - 1
          | y == rows = 0
          | otherwise = y

moveSnake :: Game -> Game
moveSnake game@Game{gameSnake=snake, gameHeading=heading, gameStretching=stretching} =
  let newHead = (head snake) `offset` heading
      newTail = if stretching > 0 then snake else init snake
  in game{ gameSnake = newHead : newTail
                    , gameStretching = max 0 (stretching - 1)
                    , gameLastTickHeading = heading
                    }

turnSnake :: Game -> Game
turnSnake game@Game{gameLastTickHeading=old, gameInputQueue=q}
  | null q = game
  | otherwise = let new = head q
                    game' = game { gameInputQueue = tail q }
                    withPoppedFromQueue
                      | new == North && old == South = game'
                      | new == East && old == West = game'
                      | new == South && old == North = game'
                      | new == West && old == East = game'
                      | otherwise = game'{gameHeading=new}
                in withPoppedFromQueue

detectCrash :: Game -> Game
detectCrash game = 
  let snake = gameSnake game 
      crashed = any ((>1) . length) $ DL.group $ DL.sort snake
  in if crashed then game{gameStatus = Lost} else game

stretch :: Int -> Game -> Game
stretch new game@Game{gameStretching=old} = game{gameStretching = old + new}

generateStarPosition :: Game -> IO Position
generateStarPosition game@Game{gameRows=rows, gameColumns=columns, gameSnake=snake, gameStars=stars} = do
  gen <- R.getStdGen 
  x <- R.getStdRandom $ R.randomR (0, columns-1)
  y <- R.getStdRandom $ R.randomR (0, rows-1)
  let p = Position x y
  if p `elem` snake || any (starAt p) stars then generateStarPosition game else return p
  where starAt p (SimpleStar p') = p == p'
        starAt p (TimedStar p' _) = p == p'

placeStars :: Game -> IO Game
placeStars game = do
  game' <- if not haveSimpleStar then placeSimpleStar game else return game
  wantTimedStar <- generateWantTimedStar
  if wantTimedStar && not haveSimpleStar && not haveTimedStar then placeTimedStar game' else return game'
  where generateWantTimedStar = do
          rand <- R.getStdRandom $ R.randomR (0, 10)
          return $ (rand::Int) == 0
        haveTimedStar = any isTimedStar (gameStars game)
        haveSimpleStar = any isSimpleStar (gameStars game)

placeSimpleStar :: Game -> IO Game
placeSimpleStar game = do
  pos <- generateStarPosition game
  return game{gameStars = (gameStars game) ++ [SimpleStar pos]}

placeTimedStar :: Game -> IO Game
placeTimedStar game@Game{gameStars=stars} = do
  pos <- generateStarPosition game
  return game{gameStars = stars ++ [TimedStar pos 7]}

tickTimedStars :: Game -> Game
tickTimedStars game@Game{gameStars=stars} = 
    game{gameStars = filter notExpiredStar $ map transform stars}
    where transform s@(SimpleStar pos) = s
          transform s@(TimedStar pos i) = TimedStar pos (i-1)
          notExpiredStar s@(SimpleStar pos) = True
          notExpiredStar s@(TimedStar pos i) = i > 0

isSimpleStar (SimpleStar _) = True
isSimpleStar (TimedStar _ _) = False
isTimedStar = not . isSimpleStar

stretchIfStarHit :: Int -> Game -> IO Game
stretchIfStarHit stretchLength game@Game{gameSnake=(snakeHead:_), gameStars=stars} = do
  let hitStars = filter starHit stars
  placeStars $ stretch (length hitStars) $ foldr handleStarHit game $ filter starHit stars
  where starHit (SimpleStar pos) = snakeHead == pos
        starHit (TimedStar pos _) = snakeHead == pos
        handleStarHit hit@(SimpleStar pos) game = handleStarHit' hit 1 game
        handleStarHit hit@(TimedStar pos score) game = handleStarHit' hit score game
        handleStarHit' star score game = game { gameScore = score + gameScore game
                                              , gameStars = [s | s <- gameStars game, s /= star] }
        stretch n game = game { gameStretching = n + gameStretching game}

newGame :: Int -> Int -> IO Game
newGame rows columns = do
  let snake = [Position x 10 | x <- [5..10]]
  let heading = West
  placeStars Game {
    gameHeading = heading
  , gameLastTickHeading = heading
  , gameSnake = snake
  , gameStars = []
  , gameStretching = 0
  , gameStatus = Playing
  , gameRows = rows
  , gameColumns = columns
  , gameInputQueue = []
  , gameScore = 0
  }
