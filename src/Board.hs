module Board
  ( fillBoard,
    fillWith,
    dims,
    rows,
    cols,
    objectAt,
    replaceAt,
    matcherFor,
    boardFromM,
    inspectBoard,
    Board (Board, matrix, obstacles, kids, corrals, robots, dirts),
    BoardObject (Empty, Obstacle, Dirt, EmptyCorral, CorralAndKid, CorralAndKidAndRobot, CorralAndRobot, Kid, Robot, RobotAndKid, CorralAndKidAndRobotRelease, RobotAndDirt, RobotAndKidAndDirt),
  )
where

import System.Random (RandomGen, uniformR)

-- Data {{{

data Board = Board
  { matrix :: [[BoardObject]],
    obstacles :: [(Int, Int)],
    kids :: [(Int, Int)],
    corrals :: [(Int, Int)],
    robots :: [(Int, Int)],
    dirts :: [(Int, Int)]
  }

instance Show Board where
  show (Board board _ _ _ _ _) = showBoard board

showBoard :: [[BoardObject]] -> String
showBoard = foldr ((++) . showRow) "\n"

showRow :: [BoardObject] -> String
showRow [] = "\n"
showRow (x : row) = show x ++ concat (replicate (14 - length (show x)) " ") ++ showRow row

boardFromM :: [[BoardObject]] -> Board
boardFromM matrix =
  let corrals =
        inspectBoard (matcherFor EmptyCorral) matrix
          ++ inspectBoard (matcherFor CorralAndKid) matrix
          ++ inspectBoard (matcherFor CorralAndKidAndRobot) matrix
          ++ inspectBoard (matcherFor CorralAndKidAndRobotRelease) matrix
      kids = inspectBoard (matcherFor Kid) matrix
      robots =
        inspectBoard (matcherFor Robot) matrix
          ++ inspectBoard (matcherFor RobotAndKid) matrix
          ++ inspectBoard (matcherFor RobotAndDirt) matrix
          ++ inspectBoard (matcherFor RobotAndKidAndDirt) matrix
          ++ inspectBoard (matcherFor CorralAndRobot) matrix
          ++ inspectBoard (matcherFor CorralAndKidAndRobot) matrix
          ++ inspectBoard (matcherFor CorralAndKidAndRobotRelease) matrix
      obstacles = inspectBoard (matcherFor Obstacle) matrix
      dirts =
        inspectBoard (matcherFor Dirt) matrix
          ++ inspectBoard (matcherFor RobotAndDirt) matrix
          ++ inspectBoard (matcherFor RobotAndKidAndDirt) matrix
   in Board {matrix = matrix, obstacles = obstacles, kids = kids, corrals = corrals, robots = robots, dirts = dirts}

data BoardObject = Empty | Obstacle | Dirt | EmptyCorral | CorralAndKid | CorralAndKidAndRobot | CorralAndRobot | Kid | Robot | RobotAndKid | CorralAndKidAndRobotRelease | RobotAndDirt | RobotAndKidAndDirt deriving (Eq)

instance Show BoardObject where
  show Empty = "Empty"
  show Obstacle = "(Obstacle)"
  show Dirt = "~~Dirt~~"
  show EmptyCorral = "{Corral}"
  show CorralAndKid = "{--C+K--}"
  show CorralAndRobot = "{[C+R]}"
  show CorralAndKidAndRobot = "{[--C+K+R--]}"
  show Kid = "--Kid--"
  show Robot = "[Robot]"
  show CorralAndKidAndRobotRelease = "{[--C+R=>K--]}"
  show RobotAndDirt = "[~~R+D~~]"
  show RobotAndKid = "[--R+K--]"
  show RobotAndKidAndDirt = "[--~~R+K+D--~~]"

-- }}}

-- Utils {{{

dims :: [[a]] -> (Int, Int)
dims matrix = (length matrix, length (head matrix))

rows :: [[a]] -> Int
rows matrix = let (rows, _) = dims matrix in rows

cols :: [[a]] -> Int
cols matrix = let (_, cols) = dims matrix in cols

replaceAt :: (Int, Int) -> a -> [[a]] -> [[a]]
replaceAt (i, j) boardObject board =
  let (rows1, cols1 : colsTale1) = splitAt i board
      (rows2, _ : colsTale2) = splitAt j cols1
   in rows1 ++ (rows2 ++ boardObject : colsTale2) : colsTale1

objectAt :: [[a]] -> (Int, Int) -> Maybe a
objectAt board (i, j) =
  let (rows, cols) = dims board
   in if i >= 0 && i < rows && j >= 0 && j < cols then Just ((board !! i) !! j) else Nothing

-- }}}

-- Generating Board {{{

-- TODO: Check object amounts
fillBoard :: RandomGen g => g -> (Int, Int) -> Int -> Int -> Int -> Int -> (Board, g)
fillBoard gen0 boardDims corralCount robotCount dirtCount obstacleCount =
  let (rows, cols) = boardDims
      initialBoard = replicate rows (replicate cols Empty)

      (withCorrals, gen1) = fillWith gen0 corralCount EmptyCorral initialBoard
      (withKids, gen2) = fillWith gen1 corralCount Kid withCorrals
      (withRobots, gen3) = fillWith gen2 robotCount Robot withKids
      (withObstacles, gen4) = fillWith gen3 obstacleCount Obstacle withRobots
      (withDirts, gen5) = fillWith gen4 dirtCount Dirt withObstacles
   in (boardFromM withDirts, gen5)

fillWith :: RandomGen g => g -> Int -> BoardObject -> [[BoardObject]] -> ([[BoardObject]], g)
fillWith gen 0 _ board = (board, gen)
fillWith gen0 n object board =
  let (i, gen1) = uniformR (0, rows board -1) gen0
      (j, gen2) = uniformR (0, cols board -1) gen1
      boardObject = (board !! i) !! j
   in case boardObject of
        Empty ->
          let newBoard = replaceAt (i, j) object board
           in fillWith gen2 (n - 1) object newBoard
        _ -> fillWith gen2 n object board

-- TOOD: Randomize direction
fillCorralsAround :: (Int, Int) -> Int -> [[BoardObject]] -> ([[BoardObject]], Int)
fillCorralsAround (_, _) 0 board = (board, 0)
fillCorralsAround (i, j) n board =
  let left = (i, j -1)
      right = (i, j + 1)
      up = (i -1, j)
      down = (i + 1, j)

      (boardLeft, nLeft) = fillCorralsAt left n board
      (boardRight, nRight) = fillCorralsAt right n boardLeft
      (boardUp, nUp) = fillCorralsAt up n boardRight
      (boardDown, nDown) = fillCorralsAt down n boardUp

      (boardRecLeft, nRecLeft) = fillCorralsAround left nDown boardDown
      (boardRecRight, nRecRight) = fillCorralsAround right nRecLeft boardRecLeft
      (boardRecUp, nRecUp) = fillCorralsAround up nRecRight boardRecRight
      (boardRecDown, nRecDown) = fillCorralsAround down nRecUp boardRecUp
   in (boardRecDown, nRecDown)

fillCorralsAt :: (Int, Int) -> Int -> [[BoardObject]] -> ([[BoardObject]], Int)
fillCorralsAt (i, j) n board =
  let left = (i, j -1)
      right = (i, j + 1)
      up = (i -1, j)
      down = (i + 1, j)

      leftBoardObject = objectAt board left
      rightBoardObject = objectAt board right
      upBoardObject = objectAt board up
      downBoardObject = objectAt board down
   in case leftBoardObject of
        Just Empty -> (replaceAt left EmptyCorral board, n - 1)
        _ -> (board, n)

inspectBoard :: (BoardObject -> Bool) -> [[BoardObject]] -> [(Int, Int)]
inspectBoard predicate board = inspectRows (rows board - 1, cols board -1) predicate board []

inspectRows :: (Int, Int) -> (BoardObject -> Bool) -> [[BoardObject]] -> [(Int, Int)] -> [(Int, Int)]
inspectRows (-1, _) _ _ matches = matches
inspectRows (i, j) predicate board matches =
  let newMatches = inspectRow (i, j) predicate (board !! i) []
   in inspectRows (i -1, j) predicate board (matches ++ newMatches)

inspectRow :: (Int, Int) -> (BoardObject -> Bool) -> [BoardObject] -> [(Int, Int)] -> [(Int, Int)]
inspectRow (_, -1) _ _ matches = matches
inspectRow (i, j) predicate row matches =
  let object = row !! j
   in if predicate object
        then inspectRow (i, j -1) predicate row ((i, j) : matches)
        else inspectRow (i, j -1) predicate row matches

matcherFor :: BoardObject -> BoardObject -> Bool
matcherFor target object = object == target

-- }}}
