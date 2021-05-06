type Sudoku = [Int]
type Box = [Int]
type Index = Int

{-

Lauch program with command: display [sudoku]
                   example: display [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

Sudoku for testing
[5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

[0,0,0,2,6,0,7,0,1,6,8,0,0,7,0,0,9,0,1,9,0,0,0,4,5,0,0,8,2,0,1,0,0,0,4,0,0,0,4,6,0,2,9,0,0,0,5,0,0,0,3,0,2,8,0,0,9,3,0,0,0,7,4,0,4,0,0,5,0,0,3,6,7,0,3,0,1,8,0,0,0]

[0,2,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,3,0,7,4,0,8,0,0,0,0,0,0,0,0,0,3,0,0,2,0,8,0,0,4,0,0,1,0,6,0,0,5,0,0,0,0,0,0,0,0,0,1,0,7,8,0,5,0,0,0,0,9,0,0,0,0,0,0,0,0,0,0,4,0] -- very hard to solve - take approximately 10 second to solve

-}

-- Main function which run the program and display final result
display :: Sudoku -> IO ()
display sudoku =
    putStrLn ("\t" ++ (show (fst resultTuple)) ++ "\n" ++ str)
    where 
        resultTuple = solve sudoku
        str = table (concatMap show (snd resultTuple)) -- Convers [Int] to [Char] without commas

-- Converts [Char] to string that looks like a sudoku
table :: String -> String
table [] = []
table str = spaced ++ table (drop 9 str) -- recursion - connects changed strings
    where 
        space              = ' '
        newline            = "\n"
        separator          = '|'
        spaced             = (lineEditor space separator (take 9 str)) ++ newline 

-- Takes space and separator. Between first 3 elements puts space and after them separator
lineEditor :: a -> a -> [a] -> [a]
lineEditor _ _ [] = []
lineEditor space _ (x:y:z:[])  = x : space : y : space : z : []
lineEditor space separator (x:y:z:xs)  = x : space : y : space : z : space : separator : space : lineEditor space separator xs

-- If sudoku is solved returns it otherwise call backtracking function
solve :: Sudoku -> (Bool, Sudoku)
solve [] = error "You can't pass empty list"
solve sudoku =
    if length sudoku == 81
        then
            if index == (-1) -- sudoku is solved
                then (True, sudoku)
            else
                backtracking sudoku index 1
    else
        error "Wrong length of sudoku"
    where
        index = findEmpty sudoku

-- Backtracking algorithm
backtracking :: Sudoku -> Index -> Int -> (Bool, Sudoku)
backtracking sudoku index iter =
    if iter > 9
        then (False, sudoku')
    else
        if check sudoku index iter -- check if new number can be put at specific index
            then 
                if fst solveTuple
                    then (True, snd solveTuple)
                else
                    backtracking sudoku index (iter + 1)  -- if failes then try again with iter + 1
        else 
            backtracking sudoku index (iter + 1) -- next step - simulate for cycle
    where 
        sudoku' = (take index sudoku ++ iter : []) ++ drop (index + 1) sudoku -- put new number at specific index into sudoku list
        solveTuple = solve (sudoku') -- move to next index / number

-- Find first zero. If there is no zero return -1
findEmpty :: Sudoku -> Index
findEmpty (x:xs) =
    if not (0 `elem` (x:xs))
        then -1 
    else
        if x /= 0    
            then 1 + findEmpty xs
        else 0

-- Checks if filled number is not already in the row, the column or in the box
check :: Sudoku -> Index -> Int -> Bool 
check sudoku index num
    | num `elem` take numOfColumn rowNum ++ (tail $ drop numOfColumn rowNum) = False -- Row check
    | num `elem` take numOfRow columnNum ++ (tail $ drop numOfRow columnNum) = False -- Column check
    | num `elem` boxNum                                                      = False -- Box check
    | otherwise                                                              = True
    where 
        numOfRow = index `div` 9 -- number of row where index is located
        rowNum = take 9 $ drop (9 * numOfRow) sudoku -- numbers in that row
        numOfColumn = index - (numOfRow * 9) -- number of column where index is located
        columnNum = let columnIndexes = [numOfColumn + (9 * n)| n <- [0..8]] in 
                                        [sudoku !! cI | cI <- columnIndexes] -- numbers in that column
        boxNum = (box sudoku index)

-- Returns list of numbers from the box where is filled number located without filled number
box :: Sudoku -> Index -> Box
box [] _ = error "You can't pass empty list"
box sudoku index 
    | (0 <= index && index <= 2) ||
     (9 <= index && index <= 11) ||
     (18 <= index && index <= 20) = [sudoku !! fb | fb <- firstBox, index /= fb]

    | (3 <= index && index <= 5) ||
     (12 <= index && index <= 14) ||
     (21 <= index && index <= 23) = [sudoku !! seb | seb <- secondBox, index /= seb]

    | (6 <= index && index <= 8) ||
     (15 <= index && index <= 17) ||
     (24 <= index && index <= 26) = [sudoku !! thb | thb <- thirdBox, index /= thb]

    | (27 <= index && index <= 29) ||
     (36 <= index && index <= 38) ||
     (45 <= index && index <= 47) = [sudoku !! fob | fob <- fourthBox, index /= fob]

    | (30 <= index && index <= 32) ||
     (39 <= index && index <= 41) ||
     (48 <= index && index <= 50) = [sudoku !! fib | fib <- fifthBox, index /= fib]

    | (33 <= index && index <= 35) ||
     (42 <= index && index <= 44) ||
     (51 <= index && index <= 53) = [sudoku !! sib | sib <- sixthBox, index /= sib]


    | (54 <= index && index <= 56) ||
     (63 <= index && index <= 65) ||
     (72 <= index && index <= 74) = [sudoku !! seb | seb <- seventhBox, index /= seb]

    | (57 <= index && index <= 59) ||
     (66 <= index && index <= 68) ||
     (75 <= index && index <= 77) = [sudoku !! eib | eib <- eighthBox, index /= eib]

    | (60 <= index && index <= 62) ||
     (69 <= index && index <= 71) ||
     (78 <= index && index <= 80) = [sudoku !! nib | nib <- ninthBox, index /= nib]  
    where 
        -- Indexes of each box
        firstBox = [0,1,2,9,10,11,18,19,20]
        secondBox = [3,4,5,12,13,14,21,22,23]
        thirdBox = [6,7,8,15,16,17,24,25,26]
        fourthBox = [27,28,29,36,37,38,45,46,47]
        fifthBox = [30,31,32,39,40,41,48,49,50]
        sixthBox = [33,34,35,42,43,44,51,52,53]
        seventhBox = [54,55,56,63,64,65,72,73,74]
        eighthBox = [57,58,59,66,67,68,75,76,77]
        ninthBox = [60,61,62,69,70,71,78,79,80]