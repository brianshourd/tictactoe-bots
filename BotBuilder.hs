{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import TicTacBots
import Control.Monad.Identity (Identity(..))
import Data.List (foldl')
import System.Random (mkStdGen, random, getStdGen)

import GA (Entity(..), GAConfig(..), evolve)

instance Entity Bot Integer () () Identity where
 
    -- generate a random entity, i.e. a random integer value 
    genRandom _ seed = return $ randomBot (mkStdGen seed)

    -- crossover operator: sum, (abs value of) difference or (rounded) mean
    crossover _ _ seed bot1 bot2 = return $ Just $ breedBots bot1 bot2 (mkStdGen seed) 0.5

    -- mutation operator: add or subtract random value (max. 10)
    mutation _ rate seed bot = return $ Just $ mutateBot bot (mkStdGen seed) rate

    -- score: how closely does the given number match the criteria?
    -- NOTE: lower is better
    scorePop _ _ bots = return $ Just $ map totalGrade bots
        where
            pairs = [(x,y) | x <- bots, y <- bots]
            totalGrade x = Just $ sum $ map (grade x) bots

            -- grades x against y, remember that lower is better
            grade x y = case (runGame x y) of
                Nothing -> 0 -- Points gained in the case of a tie
                Just bot -> case (bot == x) of
                                 True -> -2 -- x won
                                 False -> 1 -- x lost

main :: IO() 
main = do
        let cfg = GAConfig 
                    128 -- population size
                    20 -- archive size (best entities to keep track of)
                    100 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of replaced words)
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation

        g <- getStdGen -- random generator

        -- Do the evolution!
        -- two last parameters (pool for generating new entities and 
        -- extra data to score an entity) are unused in this example
        let (Identity es) = evolve g cfg () ()
            e = snd $ head es :: Bot
        
        putStrLn $ "best entity: " ++ (show e)
