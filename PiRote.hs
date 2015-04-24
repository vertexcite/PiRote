module Main where

import Control.Monad
import Data.Char

import Reflex
import Reflex.Dom

compareForPi :: String -> String
compareForPi t = zipWith (\x y -> if x == y then x else 'X') t piString


main = mainWidget $ el "div" $ do
  t <- textInput
  text "3."
  --  theText = fmap show $ _testInput_value t
  compareResultDyn <- mapDyn (groupString . compareForPi) (_textInput_value t)
  dynText compareResultDyn

piString :: String
piString = "14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196442881097566593344612847564823378678316527120190914564856692346034861045432664821339360726024914127372458700660631558817488152092096282925409171536436789259036001133053054882046652138414695194151160943305727036575959195309218611738193261179310511854807446237996274956735188575272489122793818301194912983367336244065664308602139494639522473719070217986094370277053921717629317675238467481846766940513200056812714526356082778577134275778960917363717872146844090122495343014654958537105079227968925892354201995611212902196086403441815981362977477130996051870721134999999837297804995105973173281609631859502445945534690830264252230825334468503526193118817101000313783875288658753320838142061717766914730359825349042875546873115956286388235378759375195778185778053217122680661300192787661119590921642019"

groupSize, offset :: Int
groupSize = 5
offset = 2

groupString :: String -> String
groupString [] = []
groupString xs =
  let (next, rest) = splitAt groupSize xs 
  in next ++ " " ++ groupString rest

piOutput :: [String]
piOutput = map makeOutput piWithPos
  where
    makeOutput (digit, pos) =
      if ((pos - offset) `mod` groupSize) == 0
      then [' ', digit]
      else [digit]
    piWithPos = zip piString [0..]


processEachKey :: (Char, Char, String) -> IO Bool
processEachKey (try, actual, output) = do
  let
    correct = try == actual
    echo
      | correct = output
      | length output == 1 = "X"
      | otherwise = " X"

  putStr echo
  return correct
 
{-
mainForTerm :: IO ()
mainForTerm = do
  args <- getArgs
  if not (null args) then do
    let skip = read (head args) :: Int
    mainHelper skip
  else do
    putStrLn "Pi digits rote drill."
    putStr "Number of digits to jump ahead:"
    skipStr <- getLine
    let skip = read skipStr :: Int
    mainHelper skip
-}

mainHelper :: Int -> IO ()
mainHelper skip = do
  putStr $ concat . take skip $ piOutput

--  hSetBuffering stdin NoBuffering
--  hSetEcho stdin False
  checks <- do
    input <- getContents
    let triples = zip3 input (drop skip piString) (drop skip piOutput)
    mapM processEachKey $ takeWhile ((/= 27) . ord . first) triples
  let 
    n = length checks
    correct = length . filter id $ checks
  putStrLn ""
  putStrLn $ "Started at:" ++ show skip
  putStrLn $ "Entered:   " ++ show n
  putStrLn $ "Correct:   " ++ show correct
  putStrLn $ "Errors:    " ++ show (n - correct)
  putStrLn $ "Total:     " ++ show (n + skip)
  

first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,x,_) = x
third :: (a,b,c) -> c
third (_,_,x) = x
