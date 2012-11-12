module TicTacBots (Board, Bot, takeTurn, breedBots, randomBot, runGame, mutateBot) where

import Data.Bits
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import System.Random

-- Examples
b0 = [[0,0,0],[0,0,0],[0,0,0]] :: Board
b1 = [[1,0,0],[2,0,0],[0,1,0]] :: Board
b2 = [[1,0,2],[2,0,0],[0,1,0]] :: Board
b3 = [[1,0,1],[2,0,0],[0,1,0]] :: Board
b4 = [[1,1,1],[2,0,0],[0,2,0]] :: Board
b5 = [[2,0,0],[2,1,1],[2,0,1]] :: Board
b6 = [[1,0,2],[2,1,0],[0,0,1]] :: Board

randBots = map (fst . (\x -> random (mkStdGen x) :: (Bot,StdGen))) [1..]

type Board = [[Int]]
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
            buildBot (list,gen) (n,x) = (next:list, newGen)
                where
                    (next,newGen) = getSomeRands n x gen

    randomR _ g = random g

allRotations :: Board -> [Board]
allRotations b = nub . map ($b) $ [i . j | i <- rotations, j <- reflections]
    where
        rotations :: [Board -> Board]
        rotations = take 4 . iterate (.r) $ id
        reflections :: [Board -> Board]
        reflections = take 2 . iterate (.m) $ id
        r = transpose . map reverse
        m = map reverse

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
-- takes the turn as a paramete
botMoveLookup :: Bot -> Turn -> BoardIndex -> Int
botMoveLookup (Bot bot) t n = (changeBase modulus chunk) !! remainder
    where
        chunk = fromIntegral ((bot !! t) !! quotient)
        quotient = div n numInWord
        remainder = mod n numInWord
        modulus = (9 - t)
        numInWord = floor . (/) 16 . logBase 2 $ fromIntegral modulus

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

changeBase :: (Integral a) => a -> a -> [a]
changeBase base 0 = repeat 0
changeBase base n = (fst . foldr (\x (qs,p) -> ((div p x):qs, mod p x)) ([],n) $ powers) ++ (repeat 0)
    where
        powers = map (base^) [0..(floor . logBase (fromIntegral base) $ (fromIntegral n))]

count :: Board -> Int -> Int
count b n = length . filter (==n) . concat $ b

-- make move in spot n on board b with player p
-- return the resulting board, doesn't worry about
-- whether or not such a move is legal.
doMove :: Board -> Int -> Int -> Board
doMove b p n = unflatten . insert p n . flatten $ b
    where
        flatten b = concat b
        unflatten [] = []
        unflatten (x:y:z:xs) = [[x,y,z]] ++ (unflatten xs) 
        insert p n l = let (begin,end) = splitAt n l in
            begin ++ [p] ++ (tail end)

getAllBoards :: [Board]
getAllBoards = concat . take 9 . iterate following $ [b0]
    where
        following bs = nubBy (rEqual) . concat . map getNextBoards $ bs
        rEqual b b' = elem b (allRotations b')

getNextBoards :: Board -> [Board]
getNextBoards b = case (hasWon b, turn b) of
                       (Left "Nobody", Right p) -> map (doMove b p) . filter (isLegalMove b) $ [0..8]
                       other -> []

-- Returns Left "Nobody", Left "Both", or
-- Right p where p won
hasWon :: Board -> Either String Int
hasWon b = case ((didWin 1, didWin 2)) of
                (True, False) -> Right 1
                (False, True) -> Right 2
                (False, False) -> Left "Nobody"
                (True, True) -> Left "Both"
    where
        didWin p = any ($p) [rowWin b, colWin b, diagWin b]
        rowWin b p = or . map (all (==p)) $ b
        colWin b p = rowWin (transpose b) p
        diagWin b p = (all (==p) . map (\n -> b !! n !! n) $ [0,1,2]) || (all (==p) . map (\n -> b !! n !! (2 - n)) $ [0,1,2])

isLegalMove :: Board -> Int -> Bool
isLegalMove b n = ((concat b) !! n) == 0

-- This mutates by breeding the bot with a
-- random bot. The Float parameter is the amount of
-- mutation you want - 1.0: all random, 0.0: no mutation
mutateBot :: Bot -> StdGen -> Float -> Bot
mutateBot bot g x = let (g1,g2) = split g in breedBots bot (fst $ random g1) g2 x

applyToSome :: (Random a) => (a -> a) -> StdGen -> [a] -> Int -> [a]
applyToSome f g list modulus = map apply . zip (randoms g) $ list
    where
        apply (r,x) = case (r `mod` modulus) of
                         0 -> f x
                         other -> x

printBoard :: Board -> String
printBoard b = concat . intersperse "\n" . intersperse "-+-+-" . map (\[x,y,z] -> (show x) ++ "|" ++ (show y) ++ "|" ++ (show z)) $ b

randomBot :: StdGen -> Bot
randomBot g = fst (random g :: (Bot,StdGen))

runGame :: Bot -> Bot -> Maybe Bot
runGame bot1 bot2 = runGame' bot1 bot2 [[0,0,0],[0,0,0],[0,0,0]] 0
    where
        runGame' _ _ _ 9 = Nothing
        runGame' p1 p2 board turn = case (hasWon board) of
            Left _ -> runGame' p2 p1 (takeTurn p1 board) (turn + 1)
            Right 1 -> Just bot1
            Right 2 -> Just bot2

-- Two versions - takeTurn' is faster, but requires
-- the turn as an arument. Used for bot tournaments
-- mostly. It also returns the current winner for
-- speed (0 is nobody)
takeTurn :: Bot -> Board -> Board
takeTurn bot b = fst $ takeTurn' bot b (9 - (count b 0))

takeTurn' :: Bot -> Board -> Turn -> (Board, Int)
takeTurn' bot b t = (bs !! (bMove), w)
    where
        thisBoardInfo@(BoardInfo bs n _) = (boardMaps !! t) M.! b
        bMove = botMoveLookup bot t n
        newBoard = bs !! bMove
        newBoardInfo@(BoardInfo _ _ w) = (boardMaps !! (t+1)) M.! newBoard

turn :: Board -> Either String Int
turn b = case ((count b 1) - (count b 2)) of
              0 -> Right 1
              1 -> Right 2
              other -> Left "Nobody"
