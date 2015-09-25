{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "SDL2/SDL_mixer.h"
module SDL.Raw.Mixer where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

import SDL.Raw

foreign import ccall unsafe "Mix_Init"
  init :: CInt -> IO CInt

foreign import ccall unsafe "Mix_Quit"
  quit :: IO ()
