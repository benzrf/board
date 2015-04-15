module Game.Board.Comm
  (Player, PlayerList(..), Query(..), Comm, GameM,
   shared', player',
   shared, player, prompt,
   curPlayer,
   prompt1, promptAll)
where

import Data.Maybe
import Control.Applicative
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Game.Board.Internal.Types

shared' :: Eq sh => State sh a -> Comm sh pl pr a
shared' s = Comm . StateT $ \(GameState sh pl) -> do
  let (r, sh') = runState s sh
  when (sh' /= sh) $ asks pushShared >>= liftIO . ($sh')
  return (r, GameState sh' pl)

shared :: Eq sh => State sh a -> GameM s sh pl pr a
shared = lift . lift . lift . shared'

player' :: Eq pl => Player -> State pl a -> Comm sh pl pr (Maybe a)
player' p s = Comm . StateT $ \gs@(GameState sh pls) ->
  case M.lookup p pls of
    Nothing -> return (Nothing, gs)
    Just pl -> do
      let (r, pl') = runState s pl
      when (pl' /= pl) $ asks pushPlayer >>= liftIO . \f -> f p pl'
      return (Just r, GameState sh (M.insert p pl' pls))

player :: Eq pl => Player -> State pl a -> GameM s sh pl pr (Maybe a)
player p = lift . lift . lift . player' p

curPlayer :: Eq pl => State pl a -> GameM s sh pl pr (Maybe a)
curPlayer s = asks getCurrent >>= flip player s

prompt :: [(Player, Query pr (GameM s sh pl pr a))] ->
          GameM s sh pl pr [(Player, Maybe a)]
prompt r = (lift . lift . lift . Comm $ asks doQueries) >>= ($r)

prompt1 :: Player -> pr re -> GameM s sh pl pr (Maybe re)
prompt1 p pr = snd . head <$> prompt [(p, Query pr return)]

promptAll :: pr re -> GameM s sh pl pr (M.Map Player re)
promptAll pr = M.fromList . mapMaybe sequenceA <$> do
  ps <- liftA2 (:) (asks getCurrent) (asks getPlayers)
  prompt (map (\p -> (p, Query pr return)) ps)

