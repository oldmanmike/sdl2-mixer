{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "SDL2/SDL_mixer.h"
module SDL.Raw.Mixer where

import Control.Monad.IO.Class
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Word

import SDL.Raw

type MixMusic = Ptr ()

data MixChunk = MixChunk
  { chunkAllocated   :: !CInt
  , chunkAbuf        :: !Word8
  , chunkAlen        :: !Word32
  , chunkVolume      :: !Word8
  } deriving (Show,Eq)

instance Storable MixChunk where
  sizeOf _ = (#size Mix_Chunk)
  alignment = sizeOf
  peek ptr = do
    allocated <- (#peek Mix_Chunk, allocated) ptr
    abuf <- (#peek Mix_Chunk, abuf) ptr
    alen <- (#peek Mix_Chunk, alen) ptr
    volume <- (#peek Mix_Chunk, volume) ptr
    return $! MixChunk allocated abuf alen volume
  poke ptr (MixChunk allocated abuf alen volume) = do
    (#poke Mix_Chunk, allocated) ptr allocated
    (#poke Mix_Chunk, abuf) ptr abuf
    (#poke Mix_Chunk, alen) ptr alen
    (#poke Mix_Chunk, volume) ptr volume


data MusicType = MusicNone
               | MusicCmd
               | MusicWav
               | MusicMod
               | MusicMid
               | MusicOGG
               | MusicMP3
               | MusicMP3Mad
               | MusicFlac
               deriving (Show,Eq)

type MixInitFlag = Word32

pattern MixInitFLAC = (#const MIX_INIT_FLAC) :: MixInitFlag
pattern MixInitMOD = (#const MIX_INIT_MOD) :: MixInitFlag
pattern MixInitMP3 = (#const MIX_INIT_MP3) :: MixInitFlag
pattern MixInitOGG = (#const MIX_INIT_OGG) :: MixInitFlag

foreign import ccall unsafe "Mix_Init" initFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_Quit" quitFFI :: IO ()
foreign import ccall unsafe "Mix_OpenAudio" openAudioFFI :: CInt -> Word16 -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_CloseAudio" closeAudioFFI :: IO ()
foreign import ccall unsafe "Mix_QuerySpec" querySpecFFI :: Ptr CInt -> Ptr Word16 -> Ptr CInt -> IO CInt
foreign import ccall unsafe "Mix_GetNumChunkDecoders" getNumChunkDecodersFFI :: IO CInt
foreign import ccall unsafe "Mix_GetChunkDecoder" getChunkDecoderFFI :: CInt -> IO CString
foreign import ccall unsafe "Mix_LoadWAV_RW" loadWAV_RWFFI :: Ptr RWops -> CInt -> IO (Ptr MixChunk)
foreign import ccall unsafe "Mix_QuickLoad_WAV" quickLoadWAVFFI :: Ptr Word8 -> IO (Ptr MixChunk)
foreign import ccall unsafe "Mix_QuickLoad_RAW" quickLoadRAWFFI :: Ptr Word8 -> IO (Ptr MixChunk)
foreign import ccall unsafe "Mix_VolumeChunk" volumeChunkFFI :: Ptr MixChunk -> CInt -> IO CInt
foreign import ccall unsafe "Mix_FreeChunk" freeChunkFFI :: Ptr MixChunk -> IO ()

init :: MonadIO m => CInt -> m CInt
init v1 = liftIO $ initFFI v1
{-# INLINE init #-}

quit :: MonadIO m => m ()
quit = liftIO quitFFI
{-# INLINE quit #-}

openAudio :: MonadIO m => CInt -> Word16 -> CInt -> CInt -> m CInt
openAudio v1 v2 v3 v4 = liftIO $ openAudioFFI v1 v2 v3 v4
{-# INLINE openAudio #-}

closeAudio :: MonadIO m => m ()
closeAudio = liftIO closeAudioFFI
{-# INLINE closeAudio #-}

querySpec :: MonadIO m => Ptr CInt -> Ptr Word16 -> Ptr CInt -> m CInt
querySpec v1 v2 v3 = liftIO $ querySpecFFI v1 v2 v3
{-# INLINE querySpec #-}

getNumChunkDecoders :: MonadIO m => m CInt
getNumChunkDecoders = liftIO getNumChunkDecodersFFI
{-# INLINE getNumChunkDecoders #-}

getChunkDecoder :: MonadIO m => CInt -> m CString
getChunkDecoder v1 = liftIO $ getChunkDecoderFFI v1
{-# INLINE getChunkDecoder #-}

loadWAV_RW :: MonadIO m => Ptr RWops -> CInt -> m (Ptr MixChunk)
loadWAV_RW v1 v2 = liftIO $ loadWAV_RWFFI v1 v2
{-# INLINE loadWAV_RW #-}

quickLoadWAV :: MonadIO m => Ptr Word8 -> m (Ptr MixChunk)
quickLoadWAV v1 = liftIO $ quickLoadWAVFFI v1
{-# INLINE quickLoadWAV #-}

quickLoadRAW :: MonadIO m => Ptr Word8 -> m (Ptr MixChunk)
quickLoadRAW v1 = liftIO $ quickLoadRAWFFI v1
{-# INLINE quickLoadRAW #-}

volumeChunk :: MonadIO m => Ptr MixChunk -> CInt -> m CInt
volumeChunk v1 v2 = liftIO $ volumeChunkFFI v1 v2
{-# INLINE volumeChunk #-}

freeChunk :: MonadIO m => Ptr MixChunk -> m ()
freeChunk v1 = liftIO $ freeChunkFFI v1
{-# INLINE freeChunk #-}
