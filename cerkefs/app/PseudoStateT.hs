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

pseudoStateT :: (s -> IO (b, s)) -> PseudoStateT s b
pseudoStateT f = do
 ioref <- ask
 s <- lift $ readIORef ioref
 (b, new_s) <- lift $ f s
 lift $ writeIORef ioref new_s
 return b

runPseudoStateT :: PseudoStateT s b -> s -> IO (b, s)
runPseudoStateT (ReaderT f) s = do
 ioref <- newIORef s
 b <- f ioref
 new_s <- readIORef ioref
 return (b, new_s)
