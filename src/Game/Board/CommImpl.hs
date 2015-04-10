{-# LANGUAGE TemplateHaskell #-}

module Game.Board.CommImpl
  (PromptRead(..),
   genPR)
where

import Language.Haskell.TH

class PromptRead pr where
  prReads :: pr re -> ReadS re

genPR :: Name -> DecsQ
genPR t = fmap pure $ instanceD (cxt []) ihead [genPRR t]
  where ihead = (appT (conT ''PromptRead) (conT t))

patFor :: Con -> PatQ
patFor (NormalC   n f) = conP n (wildP <$ f)
patFor (RecC      n f) = conP n (wildP <$ f)
patFor (InfixC  _ n _) = infixP wildP n wildP
patFor (ForallC _ _ c) = patFor c

genPRR :: Name -> DecQ
genPRR t = do
  TyConI (DataD _ _ _ cs _) <- reify t
  let prClause c = clause [patFor c] (normalB [| reads |]) []
  funD 'prReads (map prClause cs)

