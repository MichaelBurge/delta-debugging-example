module Chess where

import qualified Data.ByteString as BS

import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import System.IO.Unsafe

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
  alignment _ = 89
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
