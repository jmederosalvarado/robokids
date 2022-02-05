module Main where

import Board
  ( Board (Board, corrals, dirts, kids, matrix, obstacles, robots),
    BoardObject (Dirt, Obstacle, RobotAndDirt, RobotAndKidAndDirt),
    cols,
    dims,
    fillBoard,
    inspectBoard,
    matcherFor,
    rows,
  )
import Kids (moveKids)
import Robot (proactiveRobot, reactiveRobot)
import System.Random (RandomGen (next), StdGen, mkStdGen, randomIO, randomRIO, uniformR)

main :: IO ()
main = do
  print "--> Insert seed or empty for random"
  seedIO <- randomIO
  seedInput <- getLine
  let seed = if seedInput == "" then seedIO else (read seedInput :: Int)

  print "--> Insert iterations or empty for random"
  itersIO <- randomIO
  itersInput <- getLine
  let iters = if itersInput == "" then itersIO else (read itersInput :: Int)

  print "--> Enter 1 for reactive agent, 2 for proactive agent or empty for random"
  agentIO <- randomRIO (1, 2)
  agentInput <- getLine
  let agent = if agentInput == "" then agentIO else (read agentInput :: Int)
  let robot = if agent == 1 then reactiveRobot else proactiveRobot

  let pureGen = mkStdGen seed
  let (board, gen) = fillBoard pureGen (8, 8) 8 2 3 4

  print "--> Initial board"
  print board
  simulate gen iters 1 (moveRobots robot) board

simulate :: RandomGen g => g -> Int -> Int -> ([(Int, Int)] -> Board -> Board) -> Board -> IO ()
simulate gen0 iters n simRobot simBoard = do
  let boardDims = dims (matrix simBoard)
  let updatedBoard = simRobot (robots simBoard) simBoard

  if n == iters
    then do
      print "--> Moving children"
      let (newBoardM, newKids, gen1) = moveKids gen0 (kids updatedBoard) (matrix updatedBoard) []

      let dirts =
            inspectBoard (matcherFor Dirt) newBoardM
              ++ inspectBoard (matcherFor RobotAndDirt) newBoardM
              ++ inspectBoard (matcherFor RobotAndKidAndDirt) newBoardM

      let obstacles = inspectBoard (matcherFor Obstacle) newBoardM

      let newBoard =
            Board
              { matrix = newBoardM,
                obstacles = obstacles,
                kids = newKids,
                corrals = corrals updatedBoard,
                robots = robots updatedBoard,
                dirts = dirts
              }

      print newBoard

      inp <- getLine
      if inp == "abort"
        then print "Aborting"
        else do
          let contMsg = shouldContinue newBoard
          if contMsg == ""
            then simulate gen1 iters 1 simRobot newBoard
            else print contMsg
    else do
      print updatedBoard

      inp <- getLine
      if inp == "abort"
        then print "Aborting"
        else do
          let contMsg = shouldContinue updatedBoard
          if contMsg == ""
            then simulate gen0 iters (n + 1) simRobot updatedBoard
            else print contMsg

moveRobots :: ((Int, Int) -> Board -> Board) -> [(Int, Int)] -> Board -> Board
moveRobots _ [] board = board
moveRobots agent ((i, j) : rest) board = moveRobots agent rest (agent (i, j) board)

shouldContinue :: Board -> String
shouldContinue board =
  let dirt_condition = fromIntegral (rows (matrix board) * cols (matrix board)) * 60 / 100
   in if dirt_condition <= fromIntegral (length (dirts board))
        then "Dirt reached 60% of board"
        else if null (kids board) && null (dirts board) then "All clean" else ""
