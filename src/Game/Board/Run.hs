module Game.Board.Run
  (CommFuncs(..), Setup, Initial, Game, Victory,
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
  Setup sh pl pr ->
  Initial s sh pl ->
  Game s sh pl pr Bool ->
  Victory pr ->
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
        flip runReaderT (Players (tail ps) (head ps)) $ -- TODO give partiality descriptive error instead
        game
      io =
        flip runReaderT cf .
        flip evalStateT (GameState sh pl) .
        runComm $
        comm
  Players ps' w <- io
  _ <- promptI cf (map (\p -> (p, Query (vic w) return)) (w:ps'))
  return ()

takeTurns :: Game s sh pl pr Bool -> Game s sh pl pr Players
takeTurns g = ReaderT go
  where go pls@(Players (p:ps) c) = do
          w <- runReaderT g pls
          if w then return pls
               else go (Players (ps ++ [c]) p)
        go _ = error "No players?!"

