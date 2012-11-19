import TicTacBase
import TicTacBots
import BotBuilder
import System.Random (random, getStdGen, newStdGen, StdGen)
import Safe (readMay)

makeBot :: IO Bot
makeBot = do
    (popSize, archSize, maxGens) <- getInput

    putStr "Building Bots..."
    g <- newStdGen
    let bot = getBestBot popSize archSize maxGens g
    putStrLn "Done!"
    putStrLn $ show bot

    return bot where
        getInput = do
            putStr "Population Size: "
            popSize <- safeRead
            putStr "Archive Size: "
            archSize <- safeRead
            putStr "Number of Generations: "
            maxGens <- safeRead
            
            return (popSize, archSize, maxGens)
        safeRead :: IO Int
        safeRead = do
            x <- fmap readMay $ getLine
            case x of
                Nothing -> do
                    putStrLn "It needs to be a number!"
                    safeRead
                (Just n) | n > 0 -> return n
                         | n <= 0 -> do
                             putStrLn "No, a positive number!"
                             safeRead

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
            case readMay n >>= tryMove b of
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
