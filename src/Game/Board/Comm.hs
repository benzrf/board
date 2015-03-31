module Game.Board.Comm
  (Player, Players(..), Query(..), Comm, Game,
   shared', player',
   shared, player, prompt,
   curPlayer,
   prompt1, promptAll)
where

import Data.Maybe
import Data.Traversable
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

shared :: Eq sh => State sh a -> Game s sh pl pr a
shared = lift . lift . lift . shared'

player' :: Eq pl => Player -> State pl a -> Comm sh pl pr (Maybe a)
player' p s = Comm . StateT $ \gs ->
  case M.lookup p (playersF gs) of
    Nothing -> return (Nothing, gs)
    Just pl -> do
      let (r, pl') = runState s pl
      when (pl' /= pl) $ asks pushPlayer >>= liftIO . \f -> f p pl'
      return (Just r, gs{playersF = M.insert p pl' (playersF gs)})

player :: Eq pl => Player -> State pl a -> Game s sh pl pr (Maybe a)
player p = lift . lift . lift . player' p

curPlayer :: Eq pl => State pl a -> Game s sh pl pr (Maybe a)
curPlayer s = asks current >>= flip player s

prompt :: [(Player, Query pr (Game s sh pl pr a))] -> Game s sh pl pr [(Player, Maybe a)]
prompt r = (lift . lift . lift . Comm $ asks promptI) >>= ($r)

prompt1 :: Player -> pr re -> Game s sh pl pr (Maybe re)
prompt1 p pr = snd . head <$> prompt [(p, Query pr return)]

promptAll :: pr re -> Game s sh pl pr (M.Map Player re)
promptAll pr = M.fromList . mapMaybe sequenceA <$> do
  ps <- liftA2 (:) (asks current) (asks players)
  prompt (map (\p -> (p, Query pr return)) ps)

