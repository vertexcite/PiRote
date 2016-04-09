{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module PiRote.PiRote where

import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
  clickHistory :: Event Spider String <- keypad
  enteredDigits :: Dynamic Spider String <- foldDyn (flip (++)) "" clickHistory
  dynText enteredDigits

keypadRow :: MonadWidget Spider m => [Integer] -> m (Event Spider String)
keypadRow digits = do
  let
    bs :: MonadWidget Spider m => [m (Event Spider String)]
    bs = map (ble . show) digits
  bs' :: [(Event Spider String)] <- mapM id bs
  let
    bsm :: Event Spider String
    bsm = mergeWith (++) bs'
  return bsm

keypad :: MonadWidget Spider m => m (Event Spider String)
keypad = do
  kr1 :: Event Spider String <- keypadRow [7..9]
  kr2 :: Event Spider String <- keypadRow [4..6]
  kr3 :: Event Spider String <- keypadRow [1..3]
  kr4 :: Event Spider String <- keypadRow [0]
  let
    bsm :: Event Spider String
    bsm = mergeWith (++) [kr1, kr2, kr3, kr4]
  return bsm

ble :: MonadWidget Spider m => String -> m (Event Spider String)
ble label = button label >>= (return . fmap (const label) )
