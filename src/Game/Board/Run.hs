module Game.Board.Run where

import Data.Map as M
import Control.Applicative
import Control.Monad.Random
import Game.Board.Internal.Types

runGame ::
  Setup sh pl pr ->
  Initial s sh pl ->
  Game s sh pl pr Bool ->
  IO ()
runGame setup init game = do
  (players, comm) <- setup
  (s, sh, plg) <- evalRandIO init
  pl <- evalRandIO $ M.fromList . zip players <$> sequence (plg <$ players)
  undefined

