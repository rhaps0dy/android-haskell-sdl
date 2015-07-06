{-# LANGUAGE ForeignFunctionInterface #-}
module Android (
    LogPriority(..),
    log
    ) where

import Foreign
import Foreign.C
import Prelude hiding (log)

#include <android/log.h>

{#enum android_LogPriority as LogPriority {underscoreToCase} deriving (Show, Eq) #}

{#fun __android_log_write as log { `LogPriority', `String', `String' } -> `()' #}