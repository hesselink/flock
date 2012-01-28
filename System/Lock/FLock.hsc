{-# LANGUAGE ForeignFunctionInterface, CPP, FlexibleContexts #-}
module System.Lock.FLock
      (withLock, withFdLock, lock, lockFd, unlock,
       SharedExclusive(Shared, Exclusive), Block(Block, NoBlock), Lock) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits ((.|.))
#if __GLASGOW_HASKELL__ > 702
import Foreign.C.Types (CInt(..))
#else
import Foreign.C.Types (CInt)
#endif
import System.Posix.Error (throwErrnoPathIfMinus1_)
import Foreign.C.Error (throwErrnoIfMinus1_)
import System.Posix.IO (openFd, defaultFileFlags, closeFd,
                        OpenMode(ReadOnly, WriteOnly), dup)
import System.Posix.Types (Fd(Fd))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (bracket)

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

withLock :: (MonadIO m, MonadBaseControl IO m) => FilePath -> SharedExclusive -> Block -> m a -> m a
withLock fp se b x =
  bracket
    (lock fp se b)
    unlock
    (const x)

withFdLock :: (MonadIO m, MonadBaseControl IO m) => Fd -> SharedExclusive -> Block -> m a -> m a
withFdLock fd se b x =
  bracket
    (lockFd fd se b)
    unlock
    (const x)

operation :: SharedExclusive -> Block -> CInt
operation se b = case b of
                     Block -> op
                     NoBlock -> op .|. c_LOCK_NB
    where op = case se of
                   Shared -> c_LOCK_SH
                   Exclusive -> c_LOCK_EX

lock :: MonadIO m => FilePath -> SharedExclusive -> Block -> m Lock
lock fp se b = liftIO
             $ do Fd fd <- openFd fp om Nothing defaultFileFlags
                  throwErrnoPathIfMinus1_ "flock" fp $ flock fd (operation se b)
                  return (Lock fd)
    where om = case se of
                   Shared -> ReadOnly
                   Exclusive -> WriteOnly

lockFd :: MonadIO m => Fd -> SharedExclusive -> Block -> m Lock
lockFd fd se b = liftIO
               $ do (Fd fd') <- dup fd
                    throwErrnoIfMinus1_ "flock" $ flock fd' (operation se b)
                    return (Lock fd')

unlock :: MonadIO m => Lock -> m ()
unlock (Lock fd) = liftIO $ do _ <- flock fd c_LOCK_UN
                               closeFd (Fd fd)

