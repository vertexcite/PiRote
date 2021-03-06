{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module PiRote.PiRote where

import Data.FileEmbed
import Data.Monoid ((<>))

import Control.Monad
import Data.Char

import Reflex
import Reflex.Dom

import qualified Data.Map as Map
import Safe (readMay)

import Data.Maybe (fromMaybe)

main =
  mainWidgetWithCss $(embedFile "style.css") $
    elAttr "div" ("class" =: "todomvc-wrapper" <> "visibility" =: "hidden") $ 
      elAttr "section" ("class" =: "todoapp") $ do
        el "h1" $ text "Pi Rote"      
        elAttr "div" ("class" =: "view") $ do 
          text "How many digits headstart:"
          headstartInputDyn <- el "div" numberInput
          headstartDyn <- mapDyn (fromMaybe 0) headstartInputDyn

          t <- el "div" $ el "ul" $ do
            el "li" $ text "Enter memorised digits here (but look lower for assessment): "
            el "li" $ piInput headstartDyn

          compareResultDyn <- combineDyn compareForPi headstartDyn t
          elAttr "div" ("class" =: "main") $ el "ul" $ do
            el "li" $ text "Pi digits (a glimpse of the tail, X's for incorrect digits): "
            el "li" $ dynText compareResultDyn
          inputLengthDyn <- mapDyn length t
          countTotalDyn <- combineDyn (+) headstartDyn inputLengthDyn
          peekAheadInputDyn <- el "li" $ do
            text "Peek ahead:"
            el "div" numberInput
          peekAheadDyn <- mapDyn (fromMaybe 0) peekAheadInputDyn
          peekAheadResultDyn <- combineDyn peekAhead peekAheadDyn countTotalDyn
          el "li" $ dynText peekAheadResultDyn
          inputLengthTextualDyn <- mapDyn show inputLengthDyn
          countCorrectDyn <- combineDyn countCorrect headstartDyn t
          countCorrectTextualDyn <- mapDyn show countCorrectDyn
          countErrorsCurrentDyn <- combineDyn (-) inputLengthDyn countCorrectDyn
          countErrorsCurrentTextualDyn <- mapDyn show countErrorsCurrentDyn
          countErrorsCumulativeDyn <- foldDyn incrementIfNewError (0,0) (updated countErrorsCurrentDyn)
          countErrorsCumulativeTextualDyn <- mapDyn (show . snd) countErrorsCumulativeDyn
          countTotalTextualDyn <- mapDyn show countTotalDyn
          text "Stats:"
          el "div" $ el "ul" $ do
            el "li" $ do
              text "Digits entered so far: "
              dynText inputLengthTextualDyn
            el "li" $ do
              text "Correct: "
              dynText countCorrectTextualDyn
            el "li" $ do
              text "Errors (current): "
              dynText countErrorsCurrentTextualDyn
            el "li" $ do
              text "Errors (cumulative): "
              dynText countErrorsCumulativeTextualDyn
            el "li" $ do
              text "Total: "
              dynText countTotalTextualDyn
          
numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Int))
numberInput = do
  let errorState = Map.singleton "style" "border-color: red"
      validState = Map.singleton "style" "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                       & textInputConfig_initialValue .~ "0"
                       & textInputConfig_attributes .~ attrs
      result <- mapDyn readMay $ _textInput_value n
      attrs <- mapDyn (\r -> case r of
                                  Just _ -> validState
                                  Nothing -> errorState) result
  return result

piInput :: (MonadWidget t m) => Dynamic t Int -> m (Dynamic t String)
piInput headstartDyn = do
  let errorState = Map.singleton "style" "border-color: red"
      validState = Map.singleton "style" "border-color: green"
  rec n <- textInput $ def & textInputConfig_attributes .~ attrs
      countCorrectDyn <- combineDyn countCorrect headstartDyn (_textInput_value n)
      inputLengthDyn <- mapDyn length (_textInput_value n)
      countErrorsCurrentDyn <- combineDyn (-) inputLengthDyn countCorrectDyn
      result <- mapDyn id $ _textInput_value n
      attrs <- mapDyn (\r -> if r == 0 then validState else errorState) countErrorsCurrentDyn
  return result


piString :: String
piString = "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196442881097566593344612847564823378678316527120190914564856692346034861045432664821339360726024914127372458700660631558817488152092096282925409171536436789259036001133053054882046652138414695194151160943305727036575959195309218611738193261179310511854807446237996274956735188575272489122793818301194912983367336244065664308602139494639522473719070217986094370277053921717629317675238467481846766940513200056812714526356082778577134275778960917363717872146844090122495343014654958537105079227968925892354201995611212902196086403441815981362977477130996051870721134999999837297804995105973173281609631859502445945534690830264252230825334468503526193118817101000313783875288658753320838142061717766914730359825349042875546873115956286388235378759375195778185778053217122680661300192787661119590921642019"

groupSize, offset :: Int
groupSize = 5
offset = 2

groupString :: Int -> String -> String
groupString offset xs = take offset xs ++ " " ++ groupString' (drop offset xs)

groupString' :: String -> String
groupString' [] = []
groupString' xs =
  let (next, rest) = splitAt groupSize xs 
  in next ++ " " ++ groupString' rest

displayLength :: Int
displayLength = 30

compareForPi :: Int -> String -> String
compareForPi headstart input = drop (length full - displayLength) full
  where
    full = groupString offset $ take headstart piString ++ zipWith (\x y -> if x == y then x else 'X') input (drop headstart piString)

peekAhead :: Int -> Int -> String
peekAhead peekCount currentCount = groupString ((offset - currentCount) `mod` groupSize) . take peekCount . drop currentCount $ piString

countCorrect :: Int -> String -> Int
countCorrect headstart input = length $ filter id $ zipWith (==) input (drop headstart piString)

incrementIfNewError currentErrorCount (previousErrorCount, previousTotal) =
  if currentErrorCount > previousErrorCount then (currentErrorCount, previousTotal + currentErrorCount - previousErrorCount) 
  else (currentErrorCount, previousTotal)
