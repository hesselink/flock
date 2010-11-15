
module System.Lock.FLock
      (withLock, lock, unlock,
       SharedExclusive(Shared, Exclusive), Block(Block, NoBlock), Lock) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Bits ((.|.))
import Foreign.C.Types (CInt)
import System.Posix.Error (throwErrnoPathIfMinus1_)
import System.Posix.IO (openFd, defaultFileFlags, closeFd,
                        OpenMode(ReadOnly, WriteOnly))
import System.Posix.Types (Fd(Fd))

#include <sys/file.h>

c_LOCK_SH, c_LOCK_EX, c_LOCK_UN, c_LOCK_NB :: CInt
c_LOCK_SH = #const LOCK_SH
c_LOCK_EX = #const LOCK_EX
c_LOCK_UN = #const LOCK_UN
c_LOCK_NB = #const LOCK_NB

foreign import ccall safe "sys/file.h flock" flock :: CInt -> CInt -> IO CInt

data SharedExclusive = Shared | Exclusive

data Block = Block | NoBlock

newtype Lock = Lock CInt

-- We should really use something like bracket, but being in MonadIO makes
-- that tricky
withLock :: MonadIO m => FilePath -> SharedExclusive -> Block -> m a -> m a
withLock fp se b x = do l <- lock fp se b
                        r <- x
                        unlock l
                        return r

lock :: MonadIO m => FilePath -> SharedExclusive -> Block -> m Lock
lock fp se b = liftIO
             $ do Fd fd <- openFd fp om Nothing defaultFileFlags
                  throwErrnoPathIfMinus1_ "flock" fp $ flock fd op'
                  return (Lock fd)
    where (om, op) = case se of
                         Shared -> (ReadOnly, c_LOCK_SH)
                         Exclusive -> (WriteOnly, c_LOCK_EX)
          op' = case b of
                    Block -> op
                    NoBlock -> op .|. c_LOCK_NB

unlock :: MonadIO m => Lock -> m ()
unlock (Lock fd) = liftIO $ do _ <- flock fd c_LOCK_UN
                               closeFd (Fd fd)

