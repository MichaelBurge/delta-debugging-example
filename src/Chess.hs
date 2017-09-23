module Chess where

import qualified Data.ByteString as BS

import Control.Monad
import Data.Int
import Data.List.Split (splitOneOf)
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import GHC.IO.Handle
import System.IO.Unsafe
import System.Process

newtype Gamestate = Gamestate BS.ByteString
newtype Iterator = Iterator BS.ByteString
newtype Move = Move BS.ByteString

pokeBs :: Ptr a -> BS.ByteString -> IO ()
pokeBs ptr bs = BS.useAsCStringLen bs $ \(src, len) ->
  copyBytes (castPtr ptr) src len

instance Storable Gamestate where
  sizeOf _ = 80
  alignment _ = 8
  peek ptr = Gamestate <$> BS.packCStringLen (castPtr ptr, sizeOf (undefined :: Gamestate))
  poke ptr (Gamestate bs) = pokeBs ptr bs

instance Storable Move where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = Move <$> BS.packCStringLen (castPtr ptr, sizeOf (undefined :: Move))
  poke ptr (Move bs) = pokeBs ptr bs

instance Storable Iterator where
  sizeOf _ = 80
  alignment _ = 8
  peek ptr = Iterator <$> BS.packCStringLen (castPtr ptr, sizeOf (undefined :: Iterator))
  poke ptr (Iterator bs) = pokeBs ptr bs

newGame :: Gamestate
newGame = unsafePerformIO $
  alloca $ \ptr -> do
    newGame_w ptr
    peek ptr

printFen :: Gamestate -> String
printFen g = unsafePerformIO $
  alloca $ \g_ptr ->
  allocaBytes 100 $ \buffer -> do
    poke g_ptr g
    printFen_w g_ptr buffer
    peekCString buffer

mkIterator :: Gamestate -> Iterator
mkIterator g = unsafePerformIO $
  alloca $ \g_ptr ->
  alloca $ \i_ptr -> do
    poke g_ptr g
    mkIterator_w g_ptr i_ptr
    peek i_ptr

advanceIterator :: Gamestate -> Iterator -> Iterator
advanceIterator g i = unsafePerformIO $
  alloca $ \g_ptr ->
  alloca $ \i_ptr ->
  alloca $ \i'_ptr -> do
    poke g_ptr g
    poke i_ptr i
    advanceIterator_w g_ptr i_ptr i'_ptr
    peek i'_ptr

perft :: Gamestate -> Int32 -> Word64
perft g depth = unsafePerformIO $
  alloca $ \g_ptr -> do
    poke g_ptr g
    perft_w g_ptr depth

applyMove :: Gamestate -> Move -> Gamestate
applyMove g m = unsafePerformIO $
  alloca $ \g_ptr ->
  alloca $ \m_ptr ->
  alloca $ \g'_ptr -> do
    poke g_ptr g
    poke m_ptr m
    applyMove_w g_ptr m_ptr g'_ptr
    peek g'_ptr

isIteratorFinished  :: Iterator -> Bool
isIteratorFinished i = unsafePerformIO $
  alloca $ \i_ptr -> do
    poke i_ptr i
    isIteratorFinished_w i_ptr

dereferenceIterator :: Iterator -> Move
dereferenceIterator i = unsafePerformIO $
  alloca $ \i_ptr -> do
  alloca $ \m_ptr -> do
    poke i_ptr i
    dereferenceIterator_w i_ptr m_ptr
    peek m_ptr

{- Standard output from the "roce38" process looks like:
Roce version: 0.0380 - Roman's Own Chess Engine
Copyright (C) 2003-2007 Roman Hartmann, Switzerland. All rights reserved.
warning: couldn't open Roce.cfg

roce: 
roce: 
Perft (3): 8902, Time: 0.001 s
roce: 
-}

reference_perft_w :: Gamestate -> Int32 -> IO Word64
reference_perft_w g d =
  let commands = [
        "setboard " ++ printFen g,
        "perft " ++ show d,
        "quit"
        ]
  in do
    (Just hin, Just hout, _, ph) <- createProcess $ CreateProcess {
      cmdspec = RawCommand "./roce38" [],
      cwd = Nothing, env = Nothing,
      std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit,
      close_fds = False, create_group = False,
      delegate_ctlc = False, detach_console = False,
      create_new_console = False, new_session = False,
      child_group = Nothing, child_user = Nothing
      }
    forM_ commands $ \command -> do
      hPutStr hin command
      hPutChar hin '\n'
    waitForProcess ph
    output <- hGetContents hout
    let perft_line = lines output !! 6
        perft_word = splitOneOf " ," perft_line !! 2
        perft = read perft_word
    return perft

reference_perft g d = unsafePerformIO $ reference_perft_w g d

foreign import ccall unsafe "new_game_w"
    newGame_w :: Ptr Gamestate -> IO ()

foreign import ccall unsafe "print_fen_w"
    printFen_w :: Ptr Gamestate -> Ptr CChar -> IO ()

foreign import ccall unsafe "mkIterator_w"
    mkIterator_w :: Ptr Gamestate -> Ptr Iterator -> IO ()

foreign import ccall unsafe "advance_iterator_w"
    advanceIterator_w :: Ptr Gamestate -> Ptr Iterator -> Ptr Iterator -> IO ()

foreign import ccall unsafe "perft_w"
    perft_w :: Ptr Gamestate -> Int32 -> IO Word64

foreign import ccall unsafe "apply_move_w"
    applyMove_w :: Ptr Gamestate -> Ptr Move -> Ptr Gamestate -> IO ()

foreign import ccall unsafe "is_iterator_finished_w"
    isIteratorFinished_w :: Ptr Iterator -> IO Bool

foreign import ccall unsafe "dereference_iterator_w"
    dereferenceIterator_w :: Ptr Iterator -> Ptr Move -> IO ()
