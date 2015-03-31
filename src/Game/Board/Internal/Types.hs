{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Board.Internal.Types where

import Data.Map
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random

type Player  = String
data Players = Players {players :: [Player], current :: Player}
data Query pr a = forall re. Query (pr re) (re -> a)

data CommFuncs sh pl pr =
  CommFuncs {pushShared :: sh -> IO (),
             pushPlayer :: Player -> pl -> IO (),
             promptI    ::
               forall a m. MonadIO m =>
               [(Player, Query pr (m a))] -> m [(Player, Maybe a)]}
data GameState sh pl =
  GameState {sharedF :: sh, playersF :: Map Player pl}

newtype Comm sh pl pr a =
  Comm {runComm :: StateT (GameState sh pl) (ReaderT (CommFuncs sh pl pr) IO) a}

deriving instance Functor (Comm sh pl pr)
deriving instance Applicative (Comm sh pl pr)
deriving instance Monad (Comm sh pl pr)
deriving instance MonadIO (Comm sh pl pr)

type Game s sh pl pr = ReaderT Players (StateT s (RandT StdGen (Comm sh pl pr)))
type Setup sh pl pr  = IO ([Player], CommFuncs sh pl pr)
type Initial s sh pl = Rand StdGen (s, sh, Rand StdGen pl)
type Victory pr      = Player -> pr ()

