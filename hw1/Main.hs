--HW1 (Dongni Wang, Wei Hu; 2017/1/23)

{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs #-}

{-# OPTIONS -fdefer-type-errors  -Werror #-}

module Main where
import Prelude hiding (takeWhile, all, zip, reverse, concat)
import Test.HUnit
import qualified Data.List as List
import qualified Data.Char as Char

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testWeather,
                               testSoccer ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzap ]

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)
 
 
tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True,
                          abc True False False ~?= False,
                          abc False True True ~?= False]

arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =(b*f - c*e, c*d - a*f, a*e - b*d)
 

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

reverse :: [a] -> [a]
reverse l  = reverse_aux l [] where
  reverse_aux l acc =
    case l of
      [] -> acc
      x:xs -> reverse_aux xs (x:acc)


treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]
zap :: [a -> b] -> [a] -> [b]
zap funs args =
  case (funs, args) of
    ([], _) -> []
    (_, []) -> []
    (f:fs, a:as) -> f a : zap fs as  

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ]
                   ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ] [ [], "a" ] ~?= [True, True],
             zap [] "a" ~?=  "",
             zap [not] [] ~?= []]

--------------------------------------------------------------------------------

testLists :: Test
testLists = "testLists" ~: TestList
  [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip,
   tmapMaybe, ttranspose, tconcat, tcountSub, tsplitBy]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
--
-- intersperse is defined in Data.List, and you can test your solution against
-- that one.

intersperse :: a -> [a] -> [a]
intersperse ele l =
  case l of
    [] -> []
    [x] -> [x]
    x:xs -> x:ele:intersperse ele xs 

tintersperse :: Test
tintersperse = "intersperse" ~:
  TestList [ intersperse 0 [1..5] ~?= [1,0,2,0,3,0,4,0,5],
             intersperse ',' "abcde" ~?= "a,b,c,d,e",
             intersperse (0, 0) [(1,2), (3,3), (1,100)]
                ~?= [(1,2),(0,0),(3,3),(0,0),(1,100)]]
 

-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []
 
--   note, you need to add a type annotation to test invert with []
--

invert :: [(a,b)] -> [(b,a)]
invert lst =
  case lst of
    [] -> []
    ((a0, a1):xs) -> (a1, a0): invert xs 


tinvert :: Test
tinvert = "invert" ~:
  TestList [ invert [("a",1),("a",2)] ~?= [(1,"a"),(2,"a")],
             invert ([] :: [(Int,Char)]) ~?= [],
             invert [(1,2),(2,3),(3,4)] ~?= [(2,1),(3,2),(4,3)]]
 

-- takeWhile, applied to a predicate p and a list xs,
-- returns the longest prefix (possibly empty) of xs of elements
-- that satisfy p:
-- For example,
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []
--
-- takeWhile is a Prelude function

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred lst =
  case lst of
    [] -> []
    (x:xs) -> if pred x then x : takeWhile pred xs else []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~:
  TestList [ takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2],
             takeWhile (< 9) [1,2,3] ~?= [1,2,3],
             takeWhile (< 0) [1,2,3] ~?= []]
 

-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3
--
-- find is defined in Data.List

find ::(a -> Bool) -> [a] -> Maybe a
find pred lst =
  case lst of
    [] -> Nothing
    (x:xs) -> if pred x then Just x else find pred xs

tfind :: Test
tfind = "find" ~:
  TestList [ find odd [0,2,3,4] ~?= Just 3,
             find odd [0,2,4,6] ~?= Nothing,
             find (>5) [] ~?= Nothing]
 

-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
--
-- all is a prelude function

all :: (a -> Bool) -> [a] -> Bool
all pred lst =
  case lst of
    [] -> True
    (x:xs) -> pred x && all pred xs

tall :: Test
tall = "all" ~:
  TestList [ all odd [1,2,3] ~?= False,
             all (>3) [4,5,6] ~?= True,
             all (<3) [5] ~?= False,
             all even [] ~?= True]
 

-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the Prelude

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 op args0 args1 =
  case (args0, args1) of
    ([], _) -> []
    (_, []) -> []
    (x:xs, y:ys) -> op x y:map2 op xs ys

tmap2 :: Test
tmap2 = "map2" ~:
  TestList [ map2 (+) [] [1,23] ~?= [],
             map2 (+) [1,2,3] [] ~?= [],
             map2 (+) [1,2,3] [3,2,1] ~?= [4,4,4],
             map2 (-) [4,2,0,-2,-4] [3,3,3] ~?= [1,-1,-3]]

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:
--    zip [1,2] [True] returns [(1,True)]
--
-- zip is defined in the prelude

zip :: [a] -> [b] -> [(a,b)]
zip lst0 lst1 =
  case (lst0, lst1) of
    ([], _) -> []
    (_, []) -> []
    (x:xs, y:ys) -> (x,y) : zip xs ys

tzip :: Test
tzip = "zip" ~:
  TestList [ zip [1,2] [True] ~?= [(1,True)],
             zip ([]::[Int]) ['a'] ~?= [],
             zip ['a', 'b', 'c'] [1..5] ~?= [('a',1), ('b',2), ('c',3)]]

-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is not the same behavior as the library version
-- of transpose.
 
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
--
-- transpose is defined in Data.List

containsEmpty :: [[a]] -> Bool
containsEmpty lst =
  case lst of
    [] -> False
    x:xs -> case x of
      [] -> True
      _ -> containsEmpty xs

transpose :: [[a]] -> [[a]]
transpose matrix 
  | containsEmpty matrix = []
  | otherwise = case matrix of
      [] -> []
      xs -> [h | (h:_) <- xs] : transpose [t | (_:t) <- xs] 

ttranspose :: Test
ttranspose = "transpose" ~:
  TestList [ transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]],
             transpose  [[1,2],[3,4,5]] ~?= [[1,3], [2,4]],
             transpose [[1,4],[2,5],[3,6]] ~?= [[1,2,3],[4,5,6]],
             transpose [[1,2],[]] ~?= []
           ]
  
-- concat
 
-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.

concat :: [[a]] -> [a]
concat lst =
  case lst of
    [] -> []
    []:xs -> concat xs 
    (x:xs):xss -> x: concat (xs:xss)
 
tconcat :: Test
tconcat = "concat" ~:
  TestList [ concat [[1,2,3],[4,5,6],[7,8,9]] ~?= [1,2,3,4,5,6,7,8,9],
             concat [[],[2,4],[6],[8,10]] ~?= [2,4,6,8,10],
             concat [[]::[Int]] ~?= [],
             concat ([]::[[Int]]) ~?= []
           ]

-- mapMaybe
 
-- Map a partial function over all the elements of the list
-- for example:
--    mapMaybe root [0.0, -1.0, 4.0] == [0.0,2.0]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

mapMaybe f lst =
  case lst of
    [] -> []
    x:xs -> case f x of
      Nothing -> mapMaybe f xs
      Just a -> a : mapMaybe f xs
          
tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~:
  TestList [ mapMaybe root [0.0, -1.0, 4.0] ~?= [0.0,2.0],
             mapMaybe root [] ~?= [],
             mapMaybe root [0.0, 1.0, 4.0, -8.0] ~?= [0.0, 1.0, 2.0]
           ]

-- countSub sub str
 
-- Return the number of (potentially overlapping) occurrences of substring sub
-- found in the string str.
-- for example:
--      countSub "aa" "aaa" returns 2

countSub :: String -> String -> Int
countSub sub s =
  case (sub,s) of
    ("",_) -> 0
    (_,"") -> 0
    (_,s@(x:xs)) -> if countSubAux sub s then 1 + subCount else subCount
      where subCount = countSub sub xs  

-- | countSub_aux: return whether the string starts with the substring

countSubAux :: String -> String -> Bool
countSubAux sub s =
  case (sub,s) of
    ("",_) -> True
    (_,"") -> False
    (x:xs,y:ys) -> x == y && countSubAux xs ys 

tcountSub :: Test
tcountSub = "countSub" ~:
  TestList [ countSub "aa" "aaa" ~?= 2,
             countSub "  " "   " ~?= 2,
             countSub "" "aaa" ~?= 0,
             countSub "d" "" ~?= 0,
             countSub "" "" ~?= 0,
             countSub "bcd" "abcdefg" ~?= 1,
             countSub "aa" "aaaababaabaaa" ~?= 6
           ]


-- splitBy pred lst
--
-- Divide the list into sections delimited by the given predicate, which do not
-- appear in the output below.
--    for example,
--      splitBy isSpace "four score and seven years" returns
--            ["four","score","and","seven","years"]
--      splitBy isSpace "" returns []

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f lst =
  case lst of
    [] -> []
    _ -> res_h : splitBy f res_t
      where
        (res_h, res_t) = splitByAux f lst  

splitByAux :: (a -> Bool) -> [a] -> ([a], [a])
splitByAux f lst =
  case lst of
    [] -> ([],[])
    x:xs -> if f x then ([],xs) else (x:res_x, res_xs)
      where
        (res_x, res_xs) = splitByAux f xs

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace  _  = False

tsplitBy :: Test
tsplitBy = "splitBy" ~:
  TestList [ splitBy isSpace "four score and seven years" ~?=
             ["four","score","and","seven","years"],
             splitBy isSpace "" ~?= [],
             splitBy isSpace " four " ~?= ["","four"],
             splitBy isSpace " four" ~?= ["","four"],
             splitBy isSpace "  four star  ?" ~?= ["","","four", "star","","?"]
           ]

--------------------------------------------------------------------------------

-- Part One: Weather Data

weather :: String -> String
weather str = head (words (rows !! row_i)) 
  where
    rows = drop 2 (lines str)
    diff = absDiff (extractCols 1 rows) (extractCols 2 rows)
    row_i = r_i where Just r_i = List.elemIndex (minimum diff) diff

weatherProgram :: IO ()
weatherProgram = do
  str <- readFile "weather.dat"
  putStrLn (weather str)

-- Get String that could be read as Int
-- ex: "0*" -> "0", "1.234" -> "1"

getIntStr :: String -> String
getIntStr l =
  case l of
    [] -> []
    x:xs -> if Char.isNumber x then x : getIntStr xs else []

readInt :: String -> Int
readInt = read 

-- Extract target columns 

extractCols :: Int -> [String] ->[String]
extractCols col_i l =
  case l of
    [] -> []
    x:xs -> if length (words x) < col_i then skipped else (words x !! col_i) : skipped
      where skipped = extractCols col_i xs

-- Get the absolute difference between two list of same-length string that can be read
-- as integers 

absDiff :: [String] -> [String] -> [Int]
absDiff [] _ = []::[Int]
absDiff _ [] = []::[Int]
absDiff (x:xs) (y:ys) = abs (readInt (getIntStr y) - readInt (getIntStr x)) : absDiff xs ys

testWeather :: Test
testWeather = "weather" ~: do str <- readFile "weather.dat"
                              weather str @?= "14"

--------

-- Part Two: Soccer League Table

soccer :: String -> String
soccer str = words (rows !! row_i) !! 1
  where
    rows = drop 1 (lines str)
    diff = absDiff (extractCols 6 rows) (extractCols 8 rows)
    row_i = r_i where Just r_i = List.elemIndex (minimum diff) diff
 
soccerProgram :: IO ()
soccerProgram = do
  str <- readFile "football.dat"
  putStrLn (soccer str)

testSoccer :: Test
testSoccer = "soccer" ~: do
  str <- readFile "football.dat"
  soccer str @?= "Aston_Villa"

-- Part Three: DRY Fusion
-- Common Function: get the target col by min of the difference between two specified cols

getTargetByMin :: String -> Int -> (Int, Int) -> Int -> String
getTargetByMin str drop_n (col1_i,col2_i) target_i = words (rows !! row_i) !! target_i
  where
    rows = drop drop_n (lines str)
    diff = absDiff (extractCols col1_i rows) (extractCols col2_i rows)
    row_i = r_i where Just r_i = List.elemIndex (minimum diff) diff

weather2 :: String -> String
weather2 str = getTargetByMin str 2 (1,2) 0
 
soccer2 :: String -> String
soccer2 str = getTargetByMin str 1 (6,8) 1

-- Kata Questions

-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?
 
shortAnswer1 :: String
shortAnswer1 = "It made the refactoring much easier. I only changed a few lines that\
\are in charge of different parameters across the two functions."

-- Was the way you wrote the second program influenced by writing the first?
 
shortAnswer2 :: String
shortAnswer2 = "Yes, largely."

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?
 
shortAnswer3 :: String
shortAnswer3 = "It may not be always good. Some data may look different\
\and require different processing. Putting all ways of handling cases together\
\may work fine but harm the readability of the programs. Once one case changes,\
\we need to be extra careful when refactoring the common code as it may also\
\impact other functions that use the code and cause problems. Therefore it could\
\harm the maintainbility of the codes."



