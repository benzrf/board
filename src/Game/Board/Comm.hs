module Game.Board.Comm
  (Player, PlayerList(..), Query(..), Comm, GameM,
   shared', player',
   shared, player, query,
   curPlayer,
   query1, queryAll)
where

import Data.Maybe
import Control.Applicative
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Game.Board.Internal.Types

shared' :: Eq sh => State sh a -> Comm sh pl pr a
shared' st = Comm . StateT $ \(GameState sh plMap) -> do
  let (a, sh') = runState st sh
  when (sh' /= sh) $ asks pushShared >>= \ps -> liftIO (ps sh')
  return (a, GameState sh' plMap)

shared :: Eq sh => State sh a -> GameM s sh pl pr a
shared = lift . lift . lift . shared'

player' :: Eq pl => Player -> State pl a -> Comm sh pl pr (Maybe a)
player' name st = Comm . StateT $ \gs@(GameState sh plMap) ->
  case M.lookup name plMap of
    Nothing -> return (Nothing, gs)
    Just pl -> do
      let (a, pl') = runState st pl
      when (pl' /= pl) $ asks pushPlayer >>= \pp -> liftIO (pp name pl')
      return (Just a, GameState sh (M.insert name pl' plMap))

player :: Eq pl => Player -> State pl a -> GameM s sh pl pr (Maybe a)
player name st = lift . lift . lift $ player' name st

curPlayer :: Eq pl => State pl a -> GameM s sh pl pr (Maybe a)
curPlayer st = asks getCurrent >>= \name -> player name st

query :: [(Player, Query pr (GameM s sh pl pr a))] ->
          GameM s sh pl pr [(Player, Maybe a)]
query queries =
  (lift . lift . lift . Comm $ asks doQueries) >>= \dq -> dq queries

query1 :: Player -> pr re -> GameM s sh pl pr (Maybe re)
query1 p pr = snag <$> query [(p, Query pr return)]
  where snag [(name, rep)] = rep
        snag _ = error "No response from comm func?!"

queryAll :: pr re -> GameM s sh pl pr (M.Map Player re)
queryAll prompt = do
  names <- liftA2 (:) (asks getCurrent) (asks getPlayers)
  reps <- query (map (\name -> (name, Query prompt return)) names)
  return . M.fromList . mapMaybe sequenceA $ reps

