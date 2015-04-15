{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Board.Internal.Types where

import Data.Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random

-- | A player identifier.
type Player  = String
-- | Used as an evironment indicating who's playing
-- and whose turn it is in the 'Game' monad stack.
data PlayerList = PlayerList {getPlayers :: [Player], getCurrent :: Player}
-- | Existential hack to put heterogeneous strongly-typed
-- callbacks in the same container.
data Query pr a = forall re. Query (pr re) (re -> a)

-- | An implementation of the communication primitives
-- necessary to run a game.
data CommFuncs sh pl pr =
  CommFuncs {
             -- | Update each player's copy of the shared state.
             pushShared :: sh -> IO (),
             -- | Update one player's copy of their individual state.
             pushPlayer :: Player -> pl -> IO (),
             -- | Send requests and asynchronously handle responses.
             doQueries  ::
               forall a m. MonadIO m =>
               [(Player, Query pr (m a))] -> m [(Player, Maybe a)]}
-- | The state of a game in progress.
data GameState sh pl =
  GameState {getShared :: sh, getPlayersMap :: Map Player pl}

-- | An abstraction over player interaction and state updates.
newtype Comm sh pl pr a =
  Comm {unwrapComm :: StateT (GameState sh pl)
                     (ReaderT (CommFuncs sh pl pr)
                      IO) a}

deriving instance Functor (Comm sh pl pr)
deriving instance Applicative (Comm sh pl pr)
deriving instance Monad (Comm sh pl pr)
deriving instance MonadIO (Comm sh pl pr)

-- | The main 'Monad' that game implementations run in.
type GameM s sh pl pr   = ReaderT PlayerList
                         (StateT s
                         (RandT StdGen
                         (Comm sh pl pr)))
-- | An action that initiates player connections.
type Connector sh pl pr = IO ([Player], CommFuncs sh pl pr)
-- | An action that generates the initial game state.
type Dealer s sh pl     = Rand StdGen (s, sh, Rand StdGen pl)

