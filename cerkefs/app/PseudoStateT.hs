module PseudoStateT
(PseudoStateT
,pseudoStateT
,runPseudoStateT
)
where
import Data.IORef
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

type PseudoStateT s = ReaderT (IORef s) IO

pseudoStateT :: (a -> IO (b, a)) -> PseudoStateT a b
pseudoStateT f = do
 ioref <- ask
 fb <- lift $ readIORef ioref
 (dats, new_fb) <- lift $ f fb
 lift $ writeIORef ioref new_fb
 return dats

runPseudoStateT :: PseudoStateT a b -> a -> IO (b, a)
runPseudoStateT (ReaderT f) fb = do
 ioref <- newIORef fb
 b <- f ioref
 new_fb <- readIORef ioref
 return (b, new_fb)