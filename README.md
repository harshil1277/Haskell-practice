Team Members:
- Harshil Kaneria (B21CS033)
- Amisha Kumari (B21CS007)

# Haskell Assignment

Simple Haskell functions to be written according to question.

## Question 1
Write a function called add_and_double which adds two numbers together and then doubles the result.

```haskell
add_and_double :: Int -> Int -> Int
add_and_double x y = 2 * (x + y)

```

## Question 2
Write a function called solve_quadratic_equation which takes in three arguments (a, b, and c) which are coefficients to the quadratic equation a x^2 + b x + c = 0. a, b, c, and x should have type Double. The output should be a tuple containing the two roots. Don't worry about complex roots; if you apply the sqrt function to a negative number you will get NaN (Not A Number). Use a let or a where expression to define the square root of the discriminant (sqrt(b ** 2 - 4 * a * c)).

```haskell
solve_quadratic_equation :: Double -> Double -> Double -> (Double, Double)
solve_quadratic_equation a b c =
  let discriminant = b * b - 4 * a * c
  in if discriminant >= 0
         then ( (-b + sqrt discriminant) / (2 * a), (-b - sqrt discriminant) / (2 * a) )
         else error "NaN"

```

## Question 3
Define a function isPerfect :: Integer -> Bool that checks if the given input (a positive integer) is a perfect number. A positive integer is perfect if it is the sum of all its proper divisors.

```haskell
isPerfect :: Int -> Bool
isPerfect n = sum [x | x <- [1..n-1], n `mod` x == 0] == n

```

## Question 4
Define a function nextPerfect :: Integer -> Integer such that for each positive integer n, nextPerfect n returns the least perfect number m > n.

```haskell
nextPerfect :: Int -> Maybe Int
nextPerfect n = find (\x -> isPerfect x && x > n) [n+1..]

```


## Question 5
Define a function partitioned :: [Int] -> Bool that returns True if there is an element n of the list such that:
- for each element m occurring before n in the list, m ≤ n, and
- for each element m occurring after n in the list, m > n.

Sample cases:
- partitioned [] = False
- partitioned [22] = True
- partitioned [19,17,18,7] = False
- partitioned [7,18,17,19] = True
- partitioned [19,13,16,15,19,25,22] = True
- partitioned [19,13,16,15,25,19,22] = False

```haskell
isNonDecreasingLeft :: Int -> Int -> [Int] -> Bool
isNonDecreasingLeft j i arr
    | j < 0           = True
    | arr !! j > arr !! i = False
    | otherwise       = isNonDecreasingLeft (j - 1) i arr

isStrictlyIncreasingRight :: Int -> Int -> [Int] -> Bool
isStrictlyIncreasingRight j i arr
    | j == length arr = True
    | arr !! j <= arr !! i = False
    | otherwise        = isStrictlyIncreasingRight (j + 1) i arr

isPartitionedAtIndex :: Int -> [Int] -> Bool
isPartitionedAtIndex i arr
    | i == length arr = False
    | isNonDecreasingLeft (i - 1) i arr && isStrictlyIncreasingRight (i + 1) i arr = True
    | otherwise       = isPartitionedAtIndex (i + 1) arr

partitioned :: [Int] -> Bool
partitioned arr = isPartitionedAtIndex 0 arr

```

## Question 6
Define a function connected :: [String] -> Bool that checks whether the input list of strings is connected. A list of strings is connected if:
- each string in the list (other than the first) is obtained from the previous one by changing the character in exactly one position, and
- no string occurs twice in the list.

Sample cases:
- connected [] = True
- connected [”aa”, ”ab”, ”ba”] = False
- connected [”aa”,”ab”,”bb”,”ba”] = True
- connected [”aa”,”ab”,”bb”,”ba”,”aa”] = False

```haskell

difference :: String -> String -> Int
difference a b = sum $ zipWith (\x y -> if x /= y then 1 else 0) a b

connected :: [String] -> Bool
connected strArr = length (nub strArr) == length strArr && checkConnected strArr

checkConnected :: [String] -> Bool
checkConnected [] = True
checkConnected [_] = True
checkConnected (x:y:rest) = difference x y == 1 && checkConnected (y:rest)

```

## Main Function

Run the main function combined with other functions for output.

```haskell
import Data.List
import System.IO

-- Main function
main :: IO ()
main = do
    putStrLn "Question 1: add_and_double"
    putStrLn $ "Result: " ++ show (add_and_double 3 4)  -- Expected: 14

    putStrLn "\nQuestion 2: solve_quadratic_equation"
    putStrLn $ "Result: " ++ show (solve_quadratic_equation 1 (-3) 2)  -- Expected: (2.0, 1.0)

    putStrLn "\nQuestion 3: isPerfect"
    putStrLn $ "Result: " ++ show (isPerfect 28)  -- Expected: True

    putStrLn "\nQuestion 4: nextPerfect"
    putStrLn $ "Result: " ++ show (nextPerfect 26)  -- Expected: 28

    putStrLn "\nQuestion 5: partitioned"
    putStrLn $ "Result: " ++ show (partitioned [])  -- Expected: False
    putStrLn $ "Result: " ++ show (partitioned [22])  -- Expected: True
    putStrLn $ "Result: " ++ show (partitioned [19,17,18,7])  -- Expected: False
    putStrLn $ "Result: " ++ show (partitioned [7,18,17,19])  -- Expected: True
    putStrLn $ "Result: " ++ show (partitioned [19,13,16,15,19,25,22])  -- Expected: True
    putStrLn $ "Result: " ++ show (partitioned [19,13,16,15,25,19,22])  -- Expected: False

    putStrLn "\nQuestion 6: connected"
    putStrLn $ "Result: " ++ show (connected [])  -- Expected: True
    putStrLn $ "Result: " ++ show (connected ["aa","ab","ba"])  -- Expected: False
    putStrLn $ "Result: " ++ show (connected ["aa","ab","bb","ba"])  -- Expected: True
    putStrLn $ "Result: " ++ show (connected ["aa","ab","bb","ba","aa"])  -- Expected: False

    putStrLn "\nDone."

```

## Complete Code
```haskell
import Data.List
import System.IO

-- Question 1
add_and_double :: Int -> Int -> Int
add_and_double x y = 2 * (x + y)

-- Question 2
solve_quadratic_equation :: Double -> Double -> Double -> (Double, Double)
solve_quadratic_equation a b c =
  let discriminant = b * b - 4 * a * c
  in if discriminant >= 0
         then ( (-b + sqrt discriminant) / (2 * a), (-b - sqrt discriminant) / (2 * a) )
         else error "NaN"

-- Question 3
isPerfect :: Int -> Bool
isPerfect n = sum [x | x <- [1..n-1], n `mod` x == 0] == n

-- Question 4
nextPerfect :: Int -> Maybe Int
nextPerfect n = find (\x -> isPerfect x && x > n) [n+1..]

-- Question 5
isNonDecreasingLeft :: Int -> Int -> [Int] -> Bool
isNonDecreasingLeft j i arr
    | j < 0           = True
    | arr !! j > arr !! i = False
    | otherwise       = isNonDecreasingLeft (j - 1) i arr

isStrictlyIncreasingRight :: Int -> Int -> [Int] -> Bool
isStrictlyIncreasingRight j i arr
    | j == length arr = True
    | arr !! j <= arr !! i = False
    | otherwise        = isStrictlyIncreasingRight (j + 1) i arr

isPartitionedAtIndex :: Int -> [Int] -> Bool
isPartitionedAtIndex i arr
    | i == length arr = False
    | isNonDecreasingLeft (i - 1) i arr && isStrictlyIncreasingRight (i + 1) i arr = True
    | otherwise       = isPartitionedAtIndex (i + 1) arr

partitioned :: [Int] -> Bool
partitioned arr = isPartitionedAtIndex 0 arr

-- Question 6

difference :: String -> String -> Int
difference a b = sum $ zipWith (\x y -> if x /= y then 1 else 0) a b

connected :: [String] -> Bool
connected strArr = length (nub strArr) == length strArr && checkConnected strArr

checkConnected :: [String] -> Bool
checkConnected [] = True
checkConnected [_] = True
checkConnected (x:y:rest) = difference x y == 1 && checkConnected (y:rest)


-- Main function
main :: IO ()
main = do
    putStrLn "Question 1: add_and_double"
    putStrLn $ "Result: " ++ show (add_and_double 3 4)  -- Expected: 14

    putStrLn "\nQuestion 2: solve_quadratic_equation"
    putStrLn $ "Result: " ++ show (solve_quadratic_equation 1 (-3) 2)  -- Expected: (2.0, 1.0)

    putStrLn "\nQuestion 3: isPerfect"
    putStrLn $ "Result: " ++ show (isPerfect 28)  -- Expected: True

    putStrLn "\nQuestion 4: nextPerfect"
    putStrLn $ "Result: " ++ show (nextPerfect 26)  -- Expected: 28

    putStrLn "\nQuestion 5: partitioned"
    putStrLn $ "Result: " ++ show (partitioned [])  -- Expected: False
    putStrLn $ "Result: " ++ show (partitioned [22])  -- Expected: True
    putStrLn $ "Result: " ++ show (partitioned [19,17,18,7])  -- Expected: False
    putStrLn $ "Result: " ++ show (partitioned [7,18,17,19])  -- Expected: True
    putStrLn $ "Result: " ++ show (partitioned [19,13,16,15,19,25,22])  -- Expected: True
    putStrLn $ "Result: " ++ show (partitioned [19,13,16,15,25,19,22])  -- Expected: False

    putStrLn "\nQuestion 6: connected"
    putStrLn $ "Result: " ++ show (connected [])  -- Expected: True
    putStrLn $ "Result: " ++ show (connected ["aa","ab","ba"])  -- Expected: False
    putStrLn $ "Result: " ++ show (connected ["aa","ab","bb","ba"])  -- Expected: True
    putStrLn $ "Result: " ++ show (connected ["aa","ab","bb","ba","aa"])  -- Expected: False

    putStrLn "\nDone."

```

## Output

![Output](/haskell.png)

Alter the inputs in code of desired function to get desired output answer.
