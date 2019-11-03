-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author(s): Shapiro, John; Leibovich, Gil
-- Date: 11/2/2019

import System.Environment
import System.IO
import Data.List
import Control.Monad

type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

-- name: toInt
-- description: converts given parameter to integer
-- input: a string
-- output: the string converted to integer
-- example: toInt "123" returns 123
toInt :: [Char] -> Int
toInt s = read s :: Int

-- name: toIntList
-- description: converts given parameter to a sequence of integers (one digit at a time)
-- input: a string
-- output: the string converted into a sequence of integers
-- example: toIntList "123" returns [1, 2, 3]
toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]

-- name: slice
-- description: returns a sequence starting from b and ending at e
-- input: a sequence
-- output: slice of the sequence
slice s b e = (drop b . take e) s

-- name: print solutions
-- description: uses Control.Monad package and prints out each board in a list of solutions
-- input: a list of valid solutions 's'
-- output: print all solved boards with a break in between them
printSolutions s = 
  forM_ s $ \l -> do
    putStrLn "-----Solution------"
    forM_ l print

-- ***** GETTER FUNCTIONS *****

-- name: getBoard
-- description: convert given string to a sudoku board
-- input: a string (the board as read from a sudoku input file)
-- output: a sudoku board
getBoard :: [Char] -> Board
getBoard s = [ toIntList rows | rows <- lines s]

-- name: getNRows
-- description: given a board, return its number of rows
-- input: a board
-- output: number of rows
getNRows :: Board -> Int
getNRows b = length(b)

-- name: getNCols
-- description: given a board, return its number of columns or 0 if rows do not have the same number of columns
-- input: a board
-- output: number of columns
getNCols :: Board -> Int
getNCols b
  | length ( nub l ) == 1 = ( l !! 0 )
  | otherwise = 0
  where l = [ length r | r <- b ]

-- name: getBox
-- description: given a board and box coordinates, return the correspondent box as a sequence
-- input: a board and two integer (box coordinates)
-- output: a sequence
getBox :: Board -> Int -> Int -> Sequence
getBox b d o = concat [ slice l (o*3) (o*3+3) | l <- (slice b (d*3) (d*3+3)) ]
 
-- name: getEmptySpot
-- description: given a board, return the first location that is empty (i.e., it has zero), if one exists; OK to assume that you will only call this function when you know that there is an empty spot
-- input: a board
-- output: a tuple with the coordinates (i, j) of the empty spot found
getEmptySpot :: Board -> (Int, Int)
getEmptySpot b = (x,y)
  where
    i = head (elemIndices 0 (concat b))
    x = quot i 9
    y = i - x * 9

-- ***** PREDICATE FUNCTIONS *****

-- name: isGridValid
-- description: given a board, return True/False depending whether the given board constitutes a valid grid (i.e., #rows = #cols = 9) or not
-- input: a board
-- output: True/False
isGridValid :: Board -> Bool
isGridValid b
  | getNRows b == getNCols b = True
  | otherwise = False

-- name: isSequenceValid
-- description: return True/False depending whether the given sequence is valid or not, according to sudoku rules
-- input: a sequence of digits from 0-9
-- output: True/False
isSequenceValid :: Sequence -> Bool
isSequenceValid s
  | i == 0 && r == 0 = True
  | otherwise = False
  where
    r = length( [ v | v <- [ elemIndices e s | e <- s, e /= 0 ], (length v) > 1 ] )
    i = length [ e | e <- s, not(elem e [0..9]) ]    

-- name: areRowsValid
-- description: return True/False depending whether ALL of the row sequences are valid or not
-- input: a board
-- output: True/False
areRowsValid :: Board -> Bool
areRowsValid b = not (elem False [isSequenceValid e | e <-b ])

-- name: areColsValid
-- description: return True/False depending whether ALL of the col sequences are valid or not
-- input: a board
-- output: True/False
areColsValid :: Board -> Bool
areColsValid b = areRowsValid (transpose b)

-- name: areBoxesValid
-- description: return True/False depending whether ALL of the box sequences are valid or not
-- input: a board
-- output: True/False
areBoxesValid :: Board -> Bool
areBoxesValid b = areRowsValid [ getBox b x y | x <- [0..2], y <- [0..2]]

-- name: isValid
-- description: return True/False depending whether the given board is valid sudoku configuration or not
-- input: a board
-- output: True/False
isValid :: Board -> Bool
isValid b = areColsValid b && areRowsValid b && areBoxesValid b && isGridValid b

-- name: isCompleted
-- description: return True/False depending whether the given board is completed or not; a board is considered completed if there isn't a single empty cell
-- input: a board
-- output: True/False
isCompleted :: Board -> Bool
isCompleted b = not (elem 0 (concat b))

-- name: isSolved
-- description: return True/False depending whether the given board is solved or not; a board is solved if it is completed and still valid
-- input: a board
-- output: True/False
isSolved :: Board -> Bool
isSolved b = isCompleted b && isValid b

-- ***** SETTER FUNCTIONS *****

-- name: setRowAt
-- description: given a sequence, an index, and a value, writes the value at the index location, returning a new sequence, but only if the original value at the specified location is empty; otherwise, return the original sequence unchanged
-- input: a sequence, an index, and a value
-- output: a new sequence
setRowAt :: Sequence -> Int -> Int -> Sequence
setRowAt s i v
  | s !! i == 0 = (concat [take i s, [v], drop (i+1) s])
  | otherwise = s

-- name: setBoardAt
-- description: given a board, two indexes i and j (representing coordinates), and a value, writes the value at the (i, j) coordinate, returning the new board, but only if the original value at the specified location is empty; otherwise, return the original board unchanged
-- input: a board, two indexes (i, j), and a value
-- output: a new board
setBoardAt :: Board -> Int -> Int -> Int -> Board
setBoardAt b i j v = (concat [take i b, [setRowAt (b !! i) j v], drop (i+1) b])

-- name: buildChoices
-- description: given a board and a two indexes i and j (representing coordinates), generate ALL possible boards, replacing the cell at (i, j) with ALL possible digits from 1 to 9; OK to assume that the cell at (i, j) is empty
-- input: a board and two indexes (i, j)
-- output: a list of boards from the original board
buildChoices :: Board -> Int -> Int -> [Board]
buildChoices b i j = [ setBoardAt b i j v | v <- [1..9] ]

-- name: solve
-- description: given a board, finds all possible solutions (note that dead ends or invalid intermediate solutions are listed as empty boards)
-- input: a board
-- output: a list of boards from the original board
solve :: Board -> [Board]
solve board
  | isSolved board = [board]
  | isCompleted board = [[[]]]
  | not (isValid board) = [[[]]]
  | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
    where
      emptySpot = getEmptySpot board
      i = fst emptySpot
      j = snd emptySpot

-- name: validChoices
-- description: calls solve and uses list comprehension to make a list of valid boards
-- input: a board
-- output: a list of solved boards
validChoices b = [ v | v <- solve b, length v > 1]

-- program starts here
main = do
  myArgs <- getArgs
  f <- openFile (myArgs !! 0) ReadMode
  contents <- hGetContents f
  let b = getBoard contents
  let s = validChoices b
  printSolutions s

  print "Done!"