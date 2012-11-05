module TicTacToeBots (Board, BoardInfo, Bot) where

import Data.Bits
import Data.Word
import Data.List
import Data.Function (on)
import Data.Maybe
import qualified Data.Map as M

-- Examples
b0 = [[0,0,0],[0,0,0],[0,0,0]] :: Board
b1 = [[1,0,0],[2,0,0],[0,1,0]] :: Board
b2 = [[1,0,2],[2,0,0],[0,1,0]] :: Board
b3 = [[1,0,1],[2,0,0],[0,1,0]] :: Board
b4 = [[1,1,1],[2,0,0],[0,2,0]] :: Board
b5 = [[2,0,0],[2,1,1],[2,0,1]] :: Board
b6 = [[1,0,2],[2,1,0],[0,0,1]] :: Board

type Board = [[Int]]
data BoardInfo = BoardInfo {legalMoves :: [Board], boardNum :: Int, winner :: Int} deriving Show
type Bot = [[Word16]]
type Turn = Int
type BoardIndex = Int

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

--printBoard :: Board -> String
printBoard b = concat . intersperse "\n" . intersperse "-+-+-" . map (\[x,y,z] -> (show x) ++ "|" ++ (show y) ++ "|" ++ (show z)) $ b

turn :: Board -> Either String Int
turn b = case ((count b 1) - (count b 2)) of
              0 -> Right 1
              1 -> Right 2
              other -> Left "Nobody"

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

isLegalMove :: Board -> Int -> Bool
isLegalMove b n = ((concat b) !! n) == 0

getNextBoards :: Board -> [Board]
getNextBoards b = case (hasWon b, turn b) of
                       (Left "Nobody", Right p) -> map (doMove b p) . filter (isLegalMove b) $ [0..8]
                       other -> []

getAllBoards :: [Board]
getAllBoards = concat . take 9 . iterate following $ [b0]
    where
        following bs = nubBy (rEqual) . concat . map getNextBoards $ bs
        rEqual b b' = elem b (allRotations b')

allRotations :: Board -> [Board]
allRotations b = nub . map ($b) $ [i . j | i <- rotations, j <- reflections]
    where
        rotations :: [Board -> Board]
        rotations = take 4 . iterate (.r) $ id
        reflections :: [Board -> Board]
        reflections = take 2 . iterate (.m) $ id
        r = transpose . map reverse
        m = map reverse

-- Two versions - takeTurn' is faster, but requires
-- the turn as an arument. Used for bot tournaments
-- mostly. It also returns the current winner for
-- speed (0 is nobody)
takeTurn :: Bot -> Board -> Board
takeTurn bot b = fst $ takeTurn' bot b (9 - (count b 0))

takeTurn' :: Bot -> Board -> Turn -> (Board, Int)
takeTurn' bot b t = (bs !! (bMove), w)
    where
        (BoardInfo bs n w) = (boardMaps !! t) M.! b
        bMove = botMoveLookup bot t n

-- Get the choice of move for the bot,
-- takes the turn as a paramete
botMoveLookup :: Bot -> Turn -> BoardIndex -> Int
botMoveLookup bot t n = (changeBase modulus chunk) !! remainder
    where
        chunk = fromIntegral ((bot !! t) !! quotient)
        quotient = div n numInWord
        remainder = mod n numInWord
        modulus = (9 - t)
        numInWord = floor . (/) 16 . logBase 2 $ fromIntegral modulus
        changeBase :: (Integral a) => a -> a -> [a]
        changeBase base 0 = repeat 0
        changeBase base n = (fst . foldr (\x (qs,p) -> ((div p x):qs, mod p x)) ([],n) $ powers) ++ (repeat 0)
            where
                powers = map (base^) [0..(floor . logBase (fromIntegral base) $ (fromIntegral n))]

