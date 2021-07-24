-- Number guessing game
import System.Random
import Control.Monad(when)

game_limits :: (Int, Int)
game_limits = (0,100)
game_def_tries :: Int
game_def_tries = 10

main = do
    gen <- getStdGen
    let (number, _) = randomR game_limits gen
    putStrLn "I've thought of a number."
    putStrLn "Try to guess it"
    guess <- askGuess
    turn game_def_tries number guess

askGuess :: IO Int
askGuess = do
    putStr "Your guess: "
    strGuess <- getLine
    return (read strGuess :: Int)

nextTurn :: Int -> Int -> IO ()
nextTurn 0 _ = putStrLn "Sorry, you have no more tries"
nextTurn tries number = do
    guess <- askGuess
    turn (tries-1) number guess

turn :: Int -> Int -> Int -> IO ()
-- turn tries number guess 
--     = case guess `compare` number of
--     GT -> do
--         putStrLn "Too large"
--         putStrLn $ show tries ++ " tries left\n"
--         nextTurn tries number
--     LT -> do
--         putStrLn "Too small"
--         putStrLn $ show tries ++ " tries left\n"
--         nextTurn tries number
--     EQ -> putStrLn "You won"
turn tries number guess = do
    if number == guess 
    then putStrLn "You won!"
    else do
        putStrLn $ "Too " ++ (case (guess `compare` number) of GT -> "large"
                                                               LT -> "small")
        putStrLn $ show (tries-1)++" left\n"
        nextTurn tries number
