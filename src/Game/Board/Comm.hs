module Game.Board.Comm
  (Player, Comm, Game, Query(..),
   shared', player',
   shared, player, prompt)
where

import Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Game.Board.Internal.Types

shared' :: State sh a -> Comm sh pl pr a
shared' s = Comm . StateT $ \gs -> do
  let (r, sh) = runState s (sharedF gs)
  asks pushShared >>= liftIO . ($sh)
  return (r, gs{sharedF = sh})

shared :: State sh a -> Game s sh pl pr a
shared = lift . lift . lift . shared'

player' :: Player -> State pl a -> Comm sh pl pr (Maybe a)
player' p s = Comm . StateT $ \gs ->
  case M.lookup p (playersF gs) of
    Nothing -> return (Nothing, gs)
    Just pl -> do
      let (r, pl') = runState s pl
      asks pushPlayer >>= liftIO . \f -> f p pl'
      return (Just r, gs{playersF = insert p pl' (playersF gs)})

player :: Player -> State pl a -> Game s sh pl pr (Maybe a)
player p = lift . lift . lift . player' p

prompt :: [(Player, Query pr (Game s sh pl pr a))] -> Game s sh pl pr [(Player, Maybe a)]
prompt r = (lift . lift . lift . Comm $ asks promptI) >>= ($r)

