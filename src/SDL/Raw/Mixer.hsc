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


data MixFading = MixNoFading
               | MixFadingOut
               | MixFadingIn
               deriving (Show,Eq,Ord,Enum,Bounded)


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
foreign import ccall unsafe "Mix_AllocateChannels" allocateChannelsFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_Volume" volumeFFI :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_PlayChannelTimed" playChannelTimedFFI :: CInt -> Ptr MixChunk -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_FadeInChannelTimed" fadeInChannelTimedFFI :: CInt -> Ptr MixChunk -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_Pause" pauseFFI :: CInt -> IO ()
foreign import ccall unsafe "Mix_Resume" resumeFFI :: CInt -> IO ()
foreign import ccall unsafe "Mix_HaltChannel" haltChannelFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_ExpireChannel" expireChannelFFI :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_FadeOutChannel" fadeOutChannelFFI :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_ChannelFinished" channelFinishedFFI :: FunPtr (CInt -> IO ()) -> IO ()
foreign import ccall unsafe "Mix_Paused" pausedFFI :: CInt -> IO CInt
--foreign import ccall unsafe "Mix_FadingChannel" fadingChannelFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_GetChunk" getChunkFFI :: CInt -> IO (Ptr MixChunk)

foreign import ccall unsafe "Mix_ReserveChannels" reserveChannelsFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_GroupChannel" groupChannelFFI :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_GroupChannels" groupChannelsFFI :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_GroupCount" groupCountFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_GroupAvailable" groupAvailableFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_GroupOldest" groupOldestFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_GroupNewer" groupNewerFFI :: CInt -> IO CInt
foreign import ccall unsafe "Mix_FadeOutGroup" fadeOutGroupFFI :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "Mix_HaltGroup" haltGroupFFI :: CInt -> IO CInt


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


allocateChannels :: MonadIO m => CInt -> m CInt
allocateChannels v1 = liftIO $ allocateChannelsFFI v1
{-# INLINE allocateChannels #-}


volume :: MonadIO m => CInt -> CInt -> m CInt
volume v1 v2 = liftIO $ volumeFFI v1 v2
{-# INLINE volume #-}


playChannel :: MonadIO m => CInt -> Ptr MixChunk -> CInt -> m CInt
playChannel v1 v2 v3= liftIO $ playChannelTimedFFI v1 v2 v3 (-1)
{-# INLINE playChannel #-}


playChannelTimed :: MonadIO m => CInt -> Ptr MixChunk -> CInt -> CInt -> m CInt
playChannelTimed v1 v2 v3 v4 = liftIO $ playChannelTimedFFI v1 v2 v3 v4
{-# INLINE playChannelTimed #-}


fadeInChannel :: MonadIO m => CInt -> Ptr MixChunk -> CInt -> CInt -> m CInt
fadeInChannel v1 v2 v3 v4 = liftIO $ fadeInChannelTimedFFI v1 v2 v3 v4 (-1)
{-# INLINE fadeInChannel #-}


fadeInChannelTimed :: MonadIO m => CInt -> Ptr MixChunk -> CInt -> CInt -> CInt -> m CInt
fadeInChannelTimed v1 v2 v3 v4 v5 = liftIO $ fadeInChannelTimedFFI v1 v2 v3 v4 v5
{-# INLINE fadeInChannelTimed #-}


pause :: MonadIO m => CInt -> m ()
pause v1 = liftIO $ pauseFFI v1
{-# INLINE pause #-}


resume :: MonadIO m => CInt -> m ()
resume v1 = liftIO $ resumeFFI v1
{-# INLINE resume #-}


haltChannel :: MonadIO m => CInt -> m CInt
haltChannel v1 = liftIO $ haltChannelFFI v1
{-# INLINE haltChannel #-}


expireChannel :: MonadIO m => CInt -> CInt -> m CInt
expireChannel v1 v2 = liftIO $ expireChannelFFI v1 v2
{-# INLINE expireChannel #-}


fadeOutChannel :: MonadIO m => CInt -> CInt -> m CInt
fadeOutChannel v1 v2 = liftIO $ fadeOutChannelFFI v1 v2
{-# INLINE fadeOutChannel #-}


channelFinished :: MonadIO m => FunPtr (CInt -> IO ()) -> m ()
channelFinished v1 = liftIO $ channelFinishedFFI v1
{-# INLINE channelFinished #-}


paused :: MonadIO m => CInt -> m CInt
paused v1 = liftIO $ pausedFFI v1
{-# INLINE paused #-}


{-
--foreign import ccall unsafe "Mix_FadingChannel" fadingChannelFFI :: CInt -> IO CInt
fadingChannel :: MonadIO m => CInt -> m CInt
fadingChannel v1 = liftIO $ (toEnum . fadingChannelFFI) $ v1
{-# INLINE fadingChannel #-}
-}


getChunk :: MonadIO m => CInt -> m (Ptr MixChunk)
getChunk v1 = liftIO $ getChunkFFI v1
{-# INLINE getChunk #-}


reserveChannels :: MonadIO m => CInt -> m CInt
reserveChannels v1 = liftIO $ reserveChannelsFFI v1
{-# INLINE reserveChannels #-}


groupChannel :: MonadIO m => CInt -> CInt -> m CInt
groupChannel v1 v2 = liftIO $ groupChannelFFI v1 v2
{-# INLINE groupChannel #-}


groupChannels :: MonadIO m => CInt -> CInt -> CInt -> m CInt
groupChannels v1 v2 v3 = liftIO $ groupChannelsFFI v1 v2 v3
{-# INLINE groupChannels #-}


groupCount :: MonadIO m => CInt -> m CInt
groupCount v1 = liftIO $ groupCountFFI v1
{-# INLINE groupCount #-}


groupAvailable :: MonadIO m => CInt -> m CInt
groupAvailable v1 = liftIO $ groupAvailableFFI v1
{-# INLINE groupAvailable #-}


groupOldest :: MonadIO m => CInt -> m CInt
groupOldest v1 = liftIO $ groupOldestFFI v1
{-# INLINE groupOldest #-}

groupNewer :: MonadIO m => CInt -> m CInt
groupNewer v1 = liftIO $ groupNewerFFI v1
{-# INLINE groupNewer #-}

fadeOutGroup :: MonadIO m => CInt -> CInt -> m CInt
fadeOutGroup v1 v2 = liftIO $ fadeOutGroupFFI v1 v2
{-# INLINE fadeOutGroup #-}

haltGroup :: MonadIO m => CInt -> m CInt
haltGroup v1 = liftIO $ haltGroupFFI v1
{-# INLINE haltGroup #-}
