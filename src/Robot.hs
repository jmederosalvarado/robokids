module Robot
  ( reactiveRobot,
    proactiveRobot,
  )
where

import Board
  ( Board (Board, corrals, dirts, kids, matrix, obstacles, robots),
    BoardObject (CorralAndKid, CorralAndKidAndRobot, CorralAndKidAndRobotRelease, CorralAndRobot, Dirt, Empty, EmptyCorral, Kid, Obstacle, Robot, RobotAndDirt, RobotAndKid, RobotAndKidAndDirt),
    boardFromM,
    cols,
    dims,
    fillBoard,
    inspectBoard,
    matcherFor,
    objectAt,
    replaceAt,
    rows,
  )

reactiveRobot :: (Int, Int) -> Board -> Board
reactiveRobot (i, j) board =
  let boardM = matrix board
      distances = bfs (i, j) [(0, [], (i, j))] boardM (replicate (rows boardM) (replicate (cols boardM) (-1, [])))
      (toI, toJ) = case objectAt boardM (i, j) of
        Just Robot ->
          let (kidD, kidP) = nearestMatching (matcherFor Kid) (kids board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if kidD == -1 && dirtD == -1
                then (i, j)
                else if dirtD == -1 || (kidD > -1 && kidD < dirtD + 2) then kidP !! 1 else dirtP !! 1
        Just RobotAndKid ->
          let (corralD, corralP) = nearestMatching (matcherFor EmptyCorral) (corrals board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if corralD == -1 && dirtD == -1
                then (i, j)
                else
                  if dirtD == -1 || (corralD > -1 && corralD < dirtD)
                    then if length corralP > 2 then corralP !! 2 else corralP !! 1
                    else if length dirtP > 2 then dirtP !! 2 else dirtP !! 1
        Just RobotAndDirt -> (i, j)
        Just RobotAndKidAndDirt -> (i, j)
        Just CorralAndRobot ->
          let (kidD, kidP) = nearestMatching (matcherFor Kid) (kids board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if kidD == -1 && dirtD == -1
                then (i, j)
                else if dirtD == -1 || (kidD > -1 && kidD < dirtD) then kidP !! 1 else dirtP !! 1
        Just CorralAndKidAndRobot ->
          let (targetN, (targetI, targetJ)) = targetCorral (i, j) (corrals board) boardM (8, (-1, -1))
              (corralD, corralP) = (distances !! targetI) !! targetJ
           in if corralD <= 0
                then (i, j)
                else if length corralP > 2 then corralP !! 2 else corralP !! 1
        Just CorralAndKidAndRobotRelease ->
          let (kidD, kidP) = nearestMatching (matcherFor Kid) (kids board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if kidD == -1 && dirtD == -1
                then (i, j)
                else if dirtD == -1 || (kidD > -1 && kidD < dirtD) then kidP !! 1 else dirtP !! 1
        _ -> (i, j)
      (fromObj, toObj) = boardUpdate (i, j) (toI, toJ) boardM
      boardMOrigRepl = replaceAt (i, j) fromObj boardM
      boardMDestRepl = replaceAt (toI, toJ) toObj boardMOrigRepl
   in boardFromM boardMDestRepl

proactiveRobot :: (Int, Int) -> Board -> Board
proactiveRobot (i, j) board =
  let boardM = matrix board
      distances = bfs (i, j) [(0, [], (i, j))] boardM (replicate (rows boardM) (replicate (cols boardM) (-1, [])))
      (toI, toJ) = case objectAt boardM (i, j) of
        Just Robot ->
          let (kidD, kidP) = nearestMatching (matcherFor Kid) (kids board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if kidD == -1 && dirtD == -1
                then (i, j)
                else if kidD /= -1 then kidP !! 1 else dirtP !! 1
        Just RobotAndKid ->
          let (corralD, corralP) = nearestMatching (matcherFor EmptyCorral) (corrals board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if corralD == (-1) && dirtD == (-1)
                then (i, j)
                else
                  if corralD /= -1
                    then if length corralP > 2 then corralP !! 2 else corralP !! 1
                    else if length dirtP > 2 then dirtP !! 2 else dirtP !! 1
        Just RobotAndDirt -> (i, j)
        Just RobotAndKidAndDirt -> (i, j)
        Just CorralAndRobot ->
          let (kidD, kidP) = nearestMatching (matcherFor Kid) (kids board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if kidD == -1 && dirtD == -1
                then (i, j)
                else if kidD /= -1 then kidP !! 1 else dirtP !! 1
        Just CorralAndKidAndRobot ->
          let (targetN, (targetI, targetJ)) = targetCorral (i, j) (corrals board) boardM (8, (-1, -1))
              (corralD, corralP) = (distances !! targetI) !! targetJ
           in if corralD == -1 || corralD == 0
                then (i, j)
                else if length corralP > 2 then corralP !! 2 else corralP !! 1
        Just CorralAndKidAndRobotRelease ->
          let (kidD, kidP) = nearestMatching (matcherFor Kid) (kids board) distances boardM (-1, [])
              (dirtD, dirtP) = nearestMatching (matcherFor Dirt) (dirts board) distances boardM (-1, [])
           in if kidD == (-1) && dirtD == (-1)
                then (i, j)
                else
                  if kidD /= -1
                    then if length kidP > 2 then kidP !! 2 else kidP !! 1
                    else if length dirtP > 2 then dirtP !! 2 else dirtP !! 1
        _ -> (i, j)
      (fromObj, toObj) = boardUpdate (i, j) (toI, toJ) boardM
      boardMOrigRepl = replaceAt (i, j) fromObj boardM
      boardMDestRepl = replaceAt (toI, toJ) toObj boardMOrigRepl
   in boardFromM boardMDestRepl

boardUpdate :: (Int, Int) -> (Int, Int) -> [[BoardObject]] -> (BoardObject, BoardObject)
boardUpdate (fromI, fromJ) (toI, toJ) board =
  if fromI /= toI || fromJ /= toJ
    then
      let fromObj = (board !! fromI) !! fromJ
          toObj = (board !! toI) !! toJ
       in case (fromObj, toObj) of
            (Robot, Empty) -> (Empty, Robot)
            (Robot, EmptyCorral) -> (Empty, CorralAndRobot)
            (Robot, Dirt) -> (Empty, RobotAndDirt)
            (Robot, Kid) -> (Empty, RobotAndKid)
            (CorralAndRobot, Empty) -> (EmptyCorral, Robot)
            (CorralAndRobot, EmptyCorral) -> (EmptyCorral, CorralAndRobot)
            (CorralAndRobot, Dirt) -> (EmptyCorral, RobotAndDirt)
            (CorralAndRobot, Kid) -> (EmptyCorral, RobotAndKid)
            (RobotAndDirt, Empty) -> (Dirt, Robot)
            (RobotAndDirt, EmptyCorral) -> (Dirt, CorralAndKidAndRobot)
            (RobotAndDirt, Dirt) -> (Dirt, RobotAndDirt)
            (RobotAndDirt, Kid) -> (Dirt, RobotAndKid)
            (RobotAndKid, Empty) -> (Empty, RobotAndKid)
            (RobotAndKid, EmptyCorral) -> (Empty, CorralAndKidAndRobot)
            (RobotAndKid, Dirt) -> (Empty, RobotAndKidAndDirt)
            (RobotAndKidAndDirt, Empty) -> (Dirt, RobotAndKid)
            (RobotAndKidAndDirt, EmptyCorral) -> (Dirt, CorralAndKidAndRobot)
            (RobotAndKidAndDirt, Dirt) -> (Dirt, RobotAndKidAndDirt)
            (CorralAndKidAndRobot, Empty) -> (EmptyCorral, RobotAndKid)
            (CorralAndKidAndRobot, EmptyCorral) -> (EmptyCorral, CorralAndKidAndRobot)
            (CorralAndKidAndRobot, Dirt) -> (EmptyCorral, RobotAndKidAndDirt)
            (CorralAndKidAndRobotRelease, Empty) -> (CorralAndKid, Robot)
            (CorralAndKidAndRobotRelease, EmptyCorral) -> (CorralAndKid, CorralAndRobot)
            (CorralAndKidAndRobotRelease, Dirt) -> (CorralAndKid, RobotAndDirt)
            (CorralAndKidAndRobotRelease, Kid) -> (CorralAndKid, RobotAndKid)
            (_, _) -> (Empty, Empty)
    else case (board !! fromI) !! fromJ of
      Robot -> (Robot, Robot)
      RobotAndDirt -> (Robot, Robot)
      CorralAndRobot -> (CorralAndRobot, CorralAndRobot)
      RobotAndKidAndDirt -> (RobotAndKid, RobotAndKid)
      RobotAndKid -> (RobotAndKid, RobotAndKid)
      CorralAndKidAndRobot -> (CorralAndKidAndRobotRelease, CorralAndKidAndRobotRelease)
      CorralAndKidAndRobotRelease -> (CorralAndKidAndRobotRelease, CorralAndKidAndRobotRelease)
      _ -> (Empty, Empty)

nearestMatching :: (BoardObject -> Bool) -> [(Int, Int)] -> [[(Int, [(Int, Int)])]] -> [[BoardObject]] -> (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
nearestMatching _ [] _ _ result = result
nearestMatching predicate ((i, j) : targets) distances board (n, path) =
  let (targetN, targetP) = (distances !! i) !! j
   in if targetN > -1 && (targetN < n || n == -1) && predicate ((board !! i) !! j)
        then nearestMatching predicate targets distances board (targetN, targetP)
        else nearestMatching predicate targets distances board (n, path)

targetCorral :: (Int, Int) -> [(Int, Int)] -> [[BoardObject]] -> (Int, (Int, Int)) -> (Int, (Int, Int))
targetCorral _ [] _ result = result
targetCorral (currI, currJ) ((i, j) : rest) board (best, result) =
  let left = (i, j - 1)
      right = (i, j + 1)
      up = (i - 1, j)
      down = (i + 1, j)

      diagUR = (i - 1, j + 1)
      diagDR = (i + 1, j + 1)
      diagDL = (i + 1, j - 1)
      diagUL = (i - 1, j - 1)

      getCount = \pos -> case objectAt board pos of
        Just Obstacle -> 0
        Just CorralAndKid -> 0
        _ -> 1

      n = sum (map getCount [left, right, up, down, diagUR, diagDR, diagDL, diagUL])
   in if (board !! i) !! j == EmptyCorral || (i, j) == (currI, currJ) && n <= best
        then targetCorral (currI, currJ) rest board (n, (i, j))
        else targetCorral (currI, currJ) rest board (best, result)

-- bfs :: (Int, Int) -> [(Int, [(Int, Int)], (Int, Int))] -> [[BoardObject]] -> [[(Int, [(Int, Int)])]] -> [[(Int, [(Int, Int)])]]
-- bfs _ [] _ result = result
-- bfs (fromI, fromJ) queue board result =
--   let (n, path, (i, j)) = head queue
--    in case objectAt result (i, j) of
--         -- Just (-1, _) -> bfs (fromI, fromJ) (tail queue) board result
--         Just (dist, _) ->
--           let newPath = path ++ [(i, j)]
--               newResult = replaceAt (i, j) (n, newPath) result
--
--               left = (i - 1, j)
--               right = (i + 1, j)
--               up = (i, j - 1)
--               down = (i, j + 1)
--
--               canMove =
--                 dist /= -1 && case objectAt board (i, j) of
--                   Just Empty -> True
--                   Just EmptyCorral -> True
--                   Just Dirt -> True
--                   _ -> False
--
--               newQueue =
--                 if (fromI == i && fromJ == j) || canMove
--                   then tail queue ++ [(n + 1, newPath, up), (n + 1, newPath, right), (n + 1, newPath, down), (n + 1, newPath, left)]
--                   else tail queue
--            in bfs (fromI, fromJ) newQueue board newResult
--         _ -> bfs (fromI, fromJ) (tail queue) board result

bfs :: (Int, Int) -> [(Int, [(Int, Int)], (Int, Int))] -> [[BoardObject]] -> [[(Int, [(Int, Int)])]] -> [[(Int, [(Int, Int)])]]
bfs _ [] _ result = result
bfs (initial_x, initial_y) stack board result =
  let (n, path, (x, y)) = head stack
   in if x >= 0 && x < length (head board)
        && y >= 0
        && y < length board
        && (fst ((result !! x) !! y) == -1)
        then
          let new_path = path ++ [(x, y)]
              new_result = replaceAt (x, y) (n, new_path) result

              (up_x, up_y) = (x, y -1)
              (ri_x, ri_y) = (x + 1, y)
              (da_x, da_y) = (x, y + 1)
              (le_x, le_y) = (x -1, y)

              new_stack =
                if (initial_x == x && initial_y == y) || case objectAt board (x, y) of
                  Just Empty -> True
                  Just EmptyCorral -> True
                  Just Dirt -> True
                  _ -> False
                  then tail stack ++ [(n + 1, new_path, (up_x, up_y)), (n + 1, new_path, (ri_x, ri_y)), (n + 1, new_path, (da_x, da_y)), (n + 1, new_path, (le_x, le_y))]
                  else tail stack
           in bfs (initial_x, initial_y) new_stack board new_result
        else bfs (initial_x, initial_y) (tail stack) board result
