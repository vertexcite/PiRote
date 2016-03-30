{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module PiRote.PiRote where

import Reflex.Dom
digits :: [Integer]
digits = [0..9]

main :: IO ()
main = mainWidget $ el "div" $ do
  let bs = map (ble . show) digits
  bs' <- mapM id bs
  let bsm = mergeWith (++) bs'
  clickHistory <- foldDyn (flip (++)) "" bsm
  dynText clickHistory


ble :: MonadWidget Spider m => String -> m (Event Spider String)
ble label = button label >>= (return . fmap (const label) )
