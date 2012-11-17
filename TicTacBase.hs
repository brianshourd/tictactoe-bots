module TicTacBase (Board, count, doMove, hasWon, isLegalMove, printBoard, turn, tryMove) where

import Data.List

-- =======================
-- Data Structures
-- =======================
type Board = [[Int]]

-- =======================
-- Exported Functions
-- =======================

-- counts the number of times a given player occurs, used especially to
-- determine what turn it is
count :: Board -> Int -> Int
count b n = length . filter (==n) . concat $ b

-- make move in spot n on board b with player p return the resulting
-- board, doesn't worry about whether or not such a move is legal.
doMove :: Board -> Int -> Int -> Board
doMove b p n = unflatten . insertAt p n . flatten $ b
    where
        flatten = concat
        unflatten [] = []
        unflatten (x:y:z:xs) = [x,y,z] : (unflatten xs) 
        insertAt p n l = let (begin,end) = splitAt n l in
            begin ++ [p] ++ (tail end)

-- Returns Left "Nobody", Left "Both", or Right p where p won
hasWon :: Board -> Either String Int
hasWon b = case (didWin 1, didWin 2) of
                (True, False) -> Right 1
                (False, True) -> Right 2
                (False, False) -> Left "Nobody"
                (True, True) -> Left "Both"
    where
        didWin p = any ($p) [rowWin b, colWin b, diagWin b]
        rowWin b p = any (all (==p)) b
        colWin b = rowWin (transpose b)
        diagWin b p = (all (==p) . map (\n -> b !! n !! n) $ [0,1,2]) || (all (==p) . map (\n -> b !! n !! (2 - n)) $ [0,1,2])

-- Just checks whether or not the location on the board is a 0, and thus
-- a legal move to make
isLegalMove :: Board -> Int -> Bool
isLegalMove b n = ((concat b) !! n) == 0

-- Format the board real nicely
printBoard :: Board -> String
printBoard b = intercalate "\n" . intersperse "-+-+-" . map (\[x,y,z] -> (getLetter x) ++ "|" ++ (getLetter y) ++ "|" ++ (getLetter z)) $ b
    where
        getLetter 0 = " "
        getLetter 1 = "X"
        getLetter 2 = "O"

-- Try to do a move, if you can
tryMove :: Board -> Int -> Maybe Board
tryMove b n = case (turn b, isLegalMove b n) of
    (Right p, True) -> Just (doMove b p n)
    (Left _, _) -> Nothing
    (_, False) -> Nothing

-- Returns the current player whose turn it is (1 or 2), or Left with an
-- error message
turn :: Board -> Either String Int
turn b = case (count b 1) - (count b 2) of
              0 -> Right 1
              1 -> Right 2
              _ -> Left "Nobody"
