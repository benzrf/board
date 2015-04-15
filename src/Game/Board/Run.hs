module Game.Board.Run
  (CommFuncs(..), Connector, Dealer, GameM,
   runGame)
where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random
import Game.Board.Internal.Types

-- ugly =(
-- this is very bad
runGame ::
  Connector sh pl pr ->
  Dealer s sh pl ->
  GameM s sh pl pr Bool ->
  (Player -> pr ()) ->
  IO ()
runGame setup initial turn vic = do
  gen <- newStdGen
  (ps, cf) <- setup
  let ((s, sh, plg), gen') = runRand initial gen
      pls = sequence (plg <$ ps)
      (pl, gen'') = flip runRand gen' $ M.fromList . zip ps <$> pls
      game = takeTurns turn
      comm =
        flip evalRandT gen'' .
        flip evalStateT s .
        flip runReaderT (PlayerList (tail ps) (head ps)) $ -- TODO give partiality descriptive error instead
        game
      io =
        flip runReaderT cf .
        flip evalStateT (GameState sh pl) .
        unwrapComm $
        comm
  PlayerList ps' w <- io
  _ <- doQueries cf (map (\p -> (p, Query (vic w) return)) (w:ps'))
  return ()

takeTurns :: GameM s sh pl pr Bool -> GameM s sh pl pr PlayerList
takeTurns g = ReaderT go
  where go pls@(PlayerList (p:ps) c) = do
          w <- runReaderT g pls
          if w then return pls
               else go (PlayerList (ps ++ [c]) p)
        go _ = error "No players?!"

