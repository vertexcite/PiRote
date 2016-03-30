{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module PiRote.PiRote where

import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
  t <- textInput def
  b1 <- button "1"
  let bs1 = fmap (const "1") b1
  clickHistory <- foldDyn (++) "" bs1
  dynText clickHistory
  dynText $ _textInput_value t
