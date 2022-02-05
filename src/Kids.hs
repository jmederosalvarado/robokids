module Kids
  ( moveKids,
  )
where

import Board (BoardObject (Dirt, Empty, Kid, Obstacle), fillWith, objectAt, replaceAt)
import System.Random (RandomGen, uniformR)

moveKids :: RandomGen g => g -> [(Int, Int)] -> [[BoardObject]] -> [(Int, Int)] -> ([[BoardObject]], [(Int, Int)], g)
moveKids gen [] board kids = (board, kids, gen)
moveKids gen0 (kid : rest) board kids =
  let (newBoard, newKid, gen1) = moveKid gen0 kid board
      newKids = newKid : kids
   in moveKids gen1 rest newBoard newKids

moveKid :: RandomGen g => g -> (Int, Int) -> [[BoardObject]] -> ([[BoardObject]], (Int, Int), g)
moveKid gen0 (i, j) board =
  let (deltaRow, gen1) = uniformR (-1, 1) gen0
      (deltaCol, gen2) = uniformR (-1, 1) gen1

      newI = i + deltaRow
      newJ = j + deltaCol
   in case objectAt board (i, j) of
        Just Kid -> tryMoveKid gen2 (i, j) (newI, newJ) board
        _ -> (board, (i, j), gen2)

tryMoveKid :: RandomGen g => g -> (Int, Int) -> (Int, Int) -> [[BoardObject]] -> ([[BoardObject]], (Int, Int), g)
tryMoveKid gen0 (fromI, fromJ) (toI, toJ) board =
  let n = checkAround (fromI, fromJ) board
      withKidToDest = replaceAt (toI, toJ) Kid board
      withEmptyOrig = replaceAt (fromI, fromJ) Empty withKidToDest
      (withDirt, gen1) = kidFillWithDirt gen0 (fromI, fromJ) n withEmptyOrig
   in case objectAt board (toI, toJ) of
        Just Empty -> (withDirt, (toI, toJ), gen1)
        Just Obstacle ->
          let dirI = toI - fromI
              dirJ = toJ - fromJ
              withObstaclePushed = tryPush (toI, toJ) (dirI, dirJ) withDirt
           in if withObstaclePushed == withDirt
                then (withObstaclePushed, (toI, toJ), gen1)
                else (board, (fromI, fromJ), gen0)
        _ -> (board, (fromI, fromJ), gen0)

tryPush :: (Int, Int) -> (Int, Int) -> [[BoardObject]] -> [[BoardObject]]
tryPush (i, j) (dirI, dirJ) board =
  let dest = (i + dirI, j + dirJ)
      push = replaceAt dest Obstacle . replaceAt (i, j) Empty
   in case objectAt board dest of
        Just Empty -> push board
        Just Obstacle ->
          let withPushedObstacles = tryPush dest (dirI, dirJ) board
           in if withPushedObstacles == board
                then board
                else push withPushedObstacles
        _ -> board

checkAround :: (Int, Int) -> [[BoardObject]] -> Int
checkAround (i, j) board =
  let left = (i, j - 1)
      right = (i, j + 1)
      up = (i - 1, j)
      down = (i + 1, j)

      diagUR = (i - 1, j + 1)
      diagDR = (i + 1, j + 1)
      diagDL = (i + 1, j - 1)
      diagUL = (i - 1, j - 1)

      getCount = \pos -> case objectAt board pos of
        Just Kid -> 1
        _ -> 0
   in case sum (map getCount [left, right, up, down, diagUR, diagDR, diagDL, diagUL]) of
        0 -> 1
        1 -> 3
        _ -> 6

kidFillWithDirt :: RandomGen g => g -> (Int, Int) -> Int -> [[BoardObject]] -> ([[BoardObject]], g)
kidFillWithDirt gen _ 0 board = (board, gen)
kidFillWithDirt gen0 (i, j) n board =
  let (deltaI, gen1) = uniformR (-1, 1) gen0
      (deltaJ, gen2) = uniformR (-1, 1) gen1
      newI = i + deltaI
      newJ = j + deltaJ
      boardObject = objectAt board (newI, newJ)
   in case boardObject of
        Just Empty ->
          let newBoard = replaceAt (newI, newJ) Dirt board
           in kidFillWithDirt gen2 (newI, newJ) (n - 1) newBoard
        _ -> (board, gen2)
