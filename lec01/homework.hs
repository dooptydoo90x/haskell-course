-- Exercise 1 - 4
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)
    
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ints = reverse(doubleEveryOtherHelper (reverse ints))

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper [x] = [x]
doubleEveryOtherHelper (x:(y:ints)) = x : y*2 : doubleEveryOtherHelper ints

sumDigits :: [Integer] -> Integer
sumDigits = sum

validate :: Integer -> Bool
validate n = mod(sumDigits (doubleEveryOther (toDigits n))) 10 == 0


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
type Disk = Integer
type HanoiGame = ([Disk], [Peg], [Move])

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi ndisks sp mp ep = 
  let
    disks :: [Disk] = [1..ndisks]
  in
  solveGame (disks, [sp, mp, ep], [])

solveGame :: HanoiGame -> [Move]
solveGame gamestate = [("","")]


