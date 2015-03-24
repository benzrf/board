{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Board.Internal.Types where

import Data.Map
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

type Player  = String
data Players = Players {players :: [Player], current :: Player}
data Query pr a = forall re. Query (pr re, re -> a)

data CommFuncs sh pl pr =
  CommFuncs {pushShared :: sh -> IO (),
             pushPlayer :: Player -> pl -> IO (),
             promptI    ::
               forall a m. MonadIO m =>
               [(Player, Query pr (m a))] -> m [(Player, Maybe a)]}
data GameState sh pl =
  GameState {sharedF :: sh, playersF :: Map Player pl}

newtype Comm sh pl pr a =
  Comm {runComm :: ReaderT (CommFuncs sh pl pr) (StateT (GameState sh pl) IO) a}

deriving instance Functor (Comm sh pl pr)
deriving instance Applicative (Comm sh pl pr)
deriving instance Monad (Comm sh pl pr)

