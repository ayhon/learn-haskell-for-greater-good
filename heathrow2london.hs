getCosts :: IO [Int]
getCosts = do
    contents <- getContents
    let costs = map (\n -> read n) $ words contents
    return costs

data UpOrDown = Up | Down

shortestPath :: [Int] -> UpOrDown -> Int
shortestPath [] _ = 0
shortestPath (up:_:change:rest) Up = choseUp `min` choseDown
    where choseUp = shortestPath rest Up + up
          choseDown = shortestPath rest Down + up + change
shortestPath (_:down:change:rest) Down = choseUp `min` choseDown
    where choseUp = shortestPath rest Up + down + change
          choseDown = shortestPath rest Down + down

findShortestPath :: [Int] -> Int
findShortestPath costs = startUp `min` startDown
    where startUp = shortestPath costs Up
          startDown = shortestPath costs Down

main = do
    costs <- getCosts
    let result = findShortestPath costs
    putStrLn $ "The shortest path is " ++ show result
