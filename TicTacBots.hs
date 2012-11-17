module TicTacBots (Board, Bot, takeBotTurn, breedBots, randomBot, runGame, mutateBot) where

import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Word
import System.Random

import TicTacBase

-- =======================
-- Data Structures
-- =======================
type BoardIndex = Int
newtype Bot = Bot [[Word16]] deriving (Show, Eq, Ord, Read)
type Turn = Int
data BoardInfo = BoardInfo {legalMoves :: [Board], boardNum :: Int, winner :: Int} deriving Show

instance Random Bot where
    --random :: RandomGen g => g -> (Bot,g)
    random g = (\(l,gen) -> (Bot (reverse l), gen)) . foldl buildBot ([],g) $ zip numWords maxSize
        where
            numInWord = map (floor . (/) 16 . logBase 2 . (-) 9) [0..7]
            numWords = [1,1,3,7,18,20,19,6] :: [Int]
            maxSize = zipWith (\x y -> (9-x)^y) [0..7] numInWord :: [Int]
            getSomeRands :: RandomGen g => Int -> Int -> g -> ([Word16],g)
            getSomeRands 0 _ gen = ([],gen)
            getSomeRands howMany size gen = ((fromIntegral r):list,retGen)
                where
                    (r,newGen) = randomR (0,size) gen
                    (list,retGen) = getSomeRands (howMany - 1) size newGen
            buildBot :: RandomGen g => ([[Word16]],g) -> (Int, Int) -> ([[Word16]],g)
            buildBot (list,gen) (n,x) = (nextList:list, newGen)
                where
                    (nextList,newGen) = getSomeRands n x gen

    randomR _ g = random g

-- =======================
-- Exported Functions
-- =======================
-- The Float parameter indicates which (if any) bot should
-- be preferred. 0 returns bot1, 1 returns bot2, .5 equally
-- mixes both.
breedBots :: Bot -> Bot -> StdGen -> Float -> Bot
breedBots (Bot bot1) (Bot bot2) g x = Bot $ zipOver chooser (zipWith (\xs ys -> zip xs ys) bot1 bot2) (randoms g :: [Float])
    where
        chooser :: (a, a) -> Float -> a
        chooser (w1, w2) a = case (x `compare` a) of
            LT -> w1
            GT -> w2
            EQ -> w2
        zipOver :: (a -> b -> c) -> [[a]] -> [b] -> [[c]]
        zipOver f xxs ys = fst $ foldr (\xs (xxs,ys) -> (let (list, rest) = zipOver' f xs ys [] in (list:xxs, rest))) ([],ys) xxs
        zipOver' :: (a -> b -> c) -> [a] -> [b] -> [c] -> ([c], [b])
        zipOver' f (x:[]) (y:ys) acc = (reverse $ (f x y):acc, ys)
        zipOver' f (x:xs) (y:ys) acc = zipOver' f xs ys ((f x y):acc)

-- This mutates by breeding the bot with a
-- random bot. The Float parameter is the amount of
-- mutation you want - 1.0: all random, 0.0: no mutation
mutateBot :: Bot -> StdGen -> Float -> Bot
mutateBot bot g x = let (g1,g2) = split g in breedBots bot (fst $ random g1) g2 x

randomBot :: StdGen -> Bot
randomBot g = fst (random g :: (Bot,StdGen))

runGame :: Bot -> Bot -> Maybe Bot
runGame bot1 bot2 = runGame' bot1 bot2 [[0,0,0],[0,0,0],[0,0,0]] 0
    where
        runGame' _ _ _ 9 = Nothing
        runGame' p1 p2 board turn = case (hasWon board) of
            Left _ -> runGame' p2 p1 (takeBotTurn p1 board) (turn + 1)
            Right 1 -> Just bot1
            Right 2 -> Just bot2

-- Two versions - takeBotTurn' is faster, but requires
-- the turn as an arument. Used for bot tournaments
-- mostly. It also returns the current winner for
-- speed (0 is nobody)
takeBotTurn :: Bot -> Board -> Board
takeBotTurn bot b = fst $ takeBotTurn' bot b (9 - (count b 0))

takeBotTurn' :: Bot -> Board -> Turn -> (Board, Int)
takeBotTurn' _ b 8 = (map (map change0to1) b, 9)
    where
        change0to1 0 = 1
        change0to1 x = x
takeBotTurn' bot b t = (bs !! (bMove), w)
    where
        thisBoardInfo@(BoardInfo bs n _) = (boardMaps !! t) M.! b
        bMove = botMoveLookup bot t n
        newBoard = bs !! bMove
        newBoardInfo@(BoardInfo _ _ w) = (boardMaps !! (t+1)) M.! newBoard

-- =======================
-- Utility Functions (not exported)
-- =======================
allRotations :: Board -> [Board]
allRotations b = nub . map ($b) $ [i . j | i <- rotations, j <- reflections]
    where
        rotations :: [Board -> Board]
        rotations = take 4 . iterate (.r) $ id
        reflections :: [Board -> Board]
        reflections = take 2 . iterate (.m) $ id
        r = transpose . map reverse
        m = map reverse

-- boardMaps is where all the magic happens. Basically, it is a list of
-- maps, indexed by turn. Each map takes as key a Board and returns a
-- BoardInfo. Why? So that all of this is calculated up front, so that
-- when it inevitably called lots of times for lots of bots, lookup is
-- speedy.
boardMaps :: [M.Map Board BoardInfo]
boardMaps = map (M.fromList . concat . map allPerms . getInfo) $ pieces
    where
        pieces = map (map fst) . groupBy ((==) `on` snd) . sortBy (compare `on` snd) . map (\b -> (b, 9 - (count b 0))) $ getAllBoards
        getInfo :: [Board] -> [(Board,BoardInfo)]
        getInfo = map (\((b,w),n) -> (b, BoardInfo (getNextBoards b) n w)) . concat . numbered . groupWin
            where
                groupWin :: [Board] -> [[(Board, Int)]]
                groupWin = groupBy ((==) `on` snd) . sortBy (compare `on` snd) . map (\b -> (b, winner b))
                numbered :: [[(Board, Int)]] -> [[((Board, Int), Int)]]
                numbered xs = [(zip (head xs) [0..])] ++ (numbered' (tail xs))
                    where
                        numbered' xs = map (\ys -> zip ys (repeat 0)) xs
        winner b = case (hasWon b) of
                        Right p -> p
                        Left _ -> 0
        allPerms :: (Board, BoardInfo) -> [(Board, BoardInfo)]
        allPerms (b,BoardInfo bs n w) = map (\f -> (f b, BoardInfo (map f bs) n w)) permList
            where
                permList = nubBy (\f g -> f b == g b) [i . j | i <- [id, r, r . r, r . r . r], j <- [id, m]]
                m = map reverse
                r = transpose . map reverse

-- Get the choice of move for the bot,
-- takes the turn as a parameter
botMoveLookup :: Bot -> Turn -> BoardIndex -> Int
botMoveLookup (Bot bot) t n = (changeBase modulus chunk) !! remainder
    where
        chunk = fromIntegral ((bot !! t) !! quotient)
        quotient = div n numInWord
        remainder = mod n numInWord
        modulus = (9 - t)
        numInWord = floor . (/) 16 . logBase 2 $ fromIntegral modulus

-- Utility function
-- Ex: changeBase 10 2 -> [
changeBase :: (Integral a) => a -> a -> [a]
changeBase _ 0 = repeat 0
changeBase base n = (fst . foldr (\x (qs,p) -> ((div p x):qs, mod p x)) ([],n) $ powers) ++ (repeat 0)
    where
        powers = map (base^) [0..(floor . logBase (fromIntegral base) $ (fromIntegral n))]

getAllBoards :: [Board]
getAllBoards = concat . take 9 . iterate following $ [b0]
    where
        following bs = nubBy (rEqual) . concat . map getNextBoards $ bs
        rEqual b b' = elem b (allRotations b')
        b0 = [[0,0,0],[0,0,0],[0,0,0]] :: Board

getNextBoards :: Board -> [Board]
getNextBoards b = case (hasWon b, turn b) of
                       (Left "Nobody", Right p) -> map (doMove b p) . filter (isLegalMove b) $ [0..8]
                       _ -> []

