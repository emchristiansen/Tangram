--import Data.Array.Repa.IO.DevIL
--import Control.Monad
--import Data.Maybe
--import Pipes
--import Pipes.Concurrent
--import Control.Concurrent (threadDelay)
--import Data.Monoid
--import Control.Concurrent.Async
--import Data.List.Utils
--import Data.List
--import Codec.Picture
--import ImageIO
--import PipeUtil
import TangramMisc
--import qualified System.Random as R
--import qualified Data.Set as Set
--import Text.Printf
--import Data.Tuple
--import qualified Data.Map as M
--import qualified Data.MultiMap as MM
--import Control.Applicative

main :: IO ()
main = do  
  let imageProducer = directoryImageProducer "/home/eric/Bitcasa/data/fractals" []
  --let imagePool = cat
  let imagePool = myImagePool [] [] []
  let tangramMaker = debugTangramMaker
  let displaySetter = debugDisplaySetter "/home/eric/Downloads/tangram.png"

  runSystem imageProducer imagePool tangramMaker displaySetter

  putStrLn "Done"