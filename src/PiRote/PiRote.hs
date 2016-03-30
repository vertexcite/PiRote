{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module PiRote.PiRote where

import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
  t <- textInput def
  let bs = map (ble . show) [0..9]
  bs' <- mapM id bs
--  bs1 :: Event Spider String <- ble "1"
--  bs2 <- ble "2"
--  let bsm = mergeWith (++) [bs1, bs2]
  let bsm = mergeWith (++) bs'
  clickHistory <- foldDyn (++) "" bsm
  dynText clickHistory
  dynText $ _textInput_value t


ble :: MonadWidget Spider m => String -> m (Event Spider String)
ble label = do
  b :: Event Spider () <- button label
  let x :: Event Spider String = fmap (const label) b
  return x
