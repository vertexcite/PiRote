{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module PiRote.PiRote where

import Reflex.Dom
digits :: [Integer]
digits = [0..9]

main :: IO ()
main = mainWidget $ el "div" $ do
  t <- textInput def
  let bs = map (ble . show) digits
  bs' <- mapM id bs
  let bsm = mergeWith (++) bs'
  clickHistory <- foldDyn (++) "" bsm
  dynText clickHistory
  dynText $ _textInput_value t


ble :: MonadWidget Spider m => String -> m (Event Spider String)
ble label = do
  b :: Event Spider () <- button label
  let x :: Event Spider String = fmap (const label) b
  return x
