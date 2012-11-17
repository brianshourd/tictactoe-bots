import TicTacBase
import TicTacBots
import BotBuilder
import System.Random (random, getStdGen, newStdGen, StdGen)

makeBot :: IO Bot
makeBot = do
    putStr "Population Size: "
    popSize <- getLine
    putStr "Archive Size: "
    archSize <- getLine
    putStr "Number of Generations: "
    maxGens <- getLine

    putStr "Building Bots..."
    g <- newStdGen
    let bot = getBestBot (read popSize) (read archSize) (read maxGens) g
    putStrLn "Done!"
    putStrLn $ show bot

    return bot

playGame :: Bot -> IO ()
playGame bot = do
    g <- newStdGen
    let n = (fst $ ((random g) :: (Int,StdGen)))
    case n `mod` 2 of
        1 -> takeTurnHuman start 0
        0 -> takeTurnBot start 0
    where
        start = [[0,0,0],[0,0,0],[0,0,0]] :: Board
        takeTurnHuman :: Board -> Int -> IO ()
        takeTurnHuman _ 9 = do
            putStrLn "Tie Game!"
            takeTurnHuman start 0
        takeTurnHuman b turn = do
            putStrLn $ printBoard b
            putStr "Your move: "
            n <- getLine
            case tryMove b (read n) of
                Nothing -> do
                    putStrLn "Not allowed!"
                    takeTurnHuman b turn
                Just b' -> do
                    case hasWon b' of
                        Right _ -> do
                            putStrLn $ printBoard b'
                            putStrLn "You Win!"
                            takeTurnBot start 0
                        Left "Nobody" -> takeTurnBot b' (turn + 1)
                        _ -> putStrLn "Uh...Fail"
        takeTurnBot :: Board -> Int -> IO ()
        takeTurnBot _ 9 = do
            putStrLn "Tie Game!"
            takeTurnBot start 0
        takeTurnBot b turn = do
            let b' = takeBotTurn bot b
            case hasWon b' of
                Right _ -> do
                    putStrLn $ printBoard b'
                    putStrLn "You Lose!"
                    takeTurnHuman start 0
                Left "Nobody" -> takeTurnHuman b' (turn+1)
                _ -> putStrLn "Uh...Fail"

main = do
    bot <- makeBot
    playGame bot
