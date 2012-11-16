module BotBuilder (getBestBot) where

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad.Identity (Identity(..))
import Data.List (sortBy)
import System.Random 
import Data.Function (on)

import GA 
import TicTacBots
import TicTacBase

-- TournamentBot is used for keeping track of the position (2nd piece) and
-- score (3rd piece)
data TournamentBot = TournamentBot {tBot :: Bot, tPos :: Integer, tScore :: Integer}

instance Entity Bot Integer () () Identity where
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

-- Builds a GAConfig using the parameters as
-- PopulationSize -> ArchiveSize -> MaxGenerations
getBestBot :: Int -> Int -> Int -> StdGen -> Bot
getBestBot popSize archSize numGens g = snd . head $ es
    where
        (Identity es) = evolve g cfg () ()
        cfg = GAConfig
                popSize
                archSize
                numGens
                0.8
                0.2
                0.0
                0.2
                False
                False

main :: IO() 
main = do
        g <- getStdGen
        putStrLn $ "best entity: " ++ (show $ getBestBot 40 10 100 g)
