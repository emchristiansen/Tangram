module PipeUtil where

import Pipes
import qualified Pipes.Prelude as P
import Data.Either

takeLeftPipe :: Pipe (Either a b) a IO ()
takeLeftPipe = P.map (\x -> lefts [x]) >-> P.concat

takeRightPipe :: Pipe (Either a b) b IO ()
takeRightPipe = P.map (\x -> rights [x]) >-> P.concat