{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellActivity where

import Prelude hiding (log)
import Android (log, LogPriority(..))
import qualified Draw

foreign export ccall "hsMain" hsMain :: IO ()
hsMain = Draw.init
