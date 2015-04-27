{-# LANGUAGE TemplateHaskell #-}

module Game.Board.CommImpl
  (PromptRead(..),
   genPR)
where

import Control.Applicative
import Language.Haskell.TH

class PromptRead pr where
  prReads :: pr re -> ReadS re

genPR :: Name -> DecsQ
genPR t = pure <$> instanceD (cxt []) ihead [genBody t]
  where ihead = appT (conT ''PromptRead) (conT t)

anyWith :: Con -> PatQ
anyWith (NormalC   n f) = conP n (wildP <$ f)
anyWith (RecC      n f) = conP n (wildP <$ f)
anyWith (InfixC  _ n _) = infixP wildP n wildP
anyWith (ForallC _ _ c) = anyWith c

genBody :: Name -> DecQ
genBody t = do
  TyConI (DataD _ _ _ cs _) <- reify t
  let prClause c = clause [anyWith c] (normalB [| reads |]) []
  funD 'prReads (map prClause cs)

