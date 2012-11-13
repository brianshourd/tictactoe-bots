{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import TicTacBots
import Control.Monad.Identity (Identity(..))
import Data.List (sortBy)
import System.Random (mkStdGen, random, getStdGen, randoms)
import Data.Function (on)

import GA 

-- TournamentBot is used for keeping track of the position (2nd piece) and
-- score (3rd piece)
data TournamentBot = TournamentBot {tBot :: Bot, tPos :: Integer, tScore :: Integer}

instance Entity Bot Integer () () IO where
 
    -- generate a random entity, i.e. a random integer value 
    genRandom _ seed = return $ randomBot (mkStdGen seed)

    -- crossover operator: sum, (abs value of) difference or (rounded) mean
    crossover _ _ seed bot1 bot2 = return $ Just $ breedBots bot1 bot2 (mkStdGen seed) 0.5

    -- mutation operator: add or subtract random value (max. 10)
    mutation _ rate seed bot = return $ Just $ mutateBot bot (mkStdGen seed) rate

    -- score: how closely does the given number match the criteria?
    -- NOTE: lower is better
    scorePop _ _ bots = return $ Just $ tournament bots
        where
            tournament :: [Bot] -> [Maybe Integer]
            tournament bots = map (Just . tScore) . sortBy (compare `on` tPos) . goRounds (ceiling . logBase 2 . fromIntegral . length $ bots) $ (zipWith (\b o -> TournamentBot b o 0) bots $ [1..]) 

            goRounds :: Int -> [TournamentBot] -> [TournamentBot]
            goRounds n tbots = (iterate (nextRound . sortBy (compare `on` tScore)) tbots) !! n

            nextRound :: [TournamentBot] -> [TournamentBot]
            nextRound (b1:b2:bs) = (doMatch b1 b2) ++ (nextRound bs)
            nextRound _ = []

            doMatch :: TournamentBot -> TournamentBot -> [TournamentBot]
            doMatch bot1@(TournamentBot b1 o1 s1) bot2@(TournamentBot b2 o2 s2) = case (runGame b1 b2) of
                Nothing -> [bot1,bot2]
                Just bot -> case (bot == b1) of
                    True -> [TournamentBot b1 o1 (s1-2), TournamentBot b2 o2 (s2+2)]
                    False -> [TournamentBot b1 o1 (s1+2), TournamentBot b2 o2 (s2-2)]

main :: IO() 
main = do
        let cfg = GAConfig 
                    40 -- population size
                    10 -- archive size (best entities to keep track of)
                    100 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of replaced words)
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation

        -- g <- getStdGen -- random generator
        let g = mkStdGen 0 -- for repeateability

        -- Do the evolution!
        -- two last parameters (pool for generating new entities and 
        -- extra data to score an entity) are unused in this example
        es <- evolveVerbose g cfg () ()
        let e = snd $ head es :: Bot
        
        putStrLn $ "best entity: " ++ (show e)
