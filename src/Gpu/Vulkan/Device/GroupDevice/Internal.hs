{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.GroupDevice.Internal (

	CreateInfo(..),

	Mask(..), pattern AllDevice, Index, mkMask

	) where

import Data.Bits
import Data.Word
import Data.Default
import Gpu.Vulkan.Device.GroupDevice.Middle

newtype Mask = Mask Word32 deriving (Show, Eq, Bits)

instance Default Mask where def = AllDevice

pattern AllDevice :: Mask
pattern AllDevice = Mask 0

type Index = Word32

mkMask :: [Index] -> Mask
mkMask = foldl ((. fromIntegral) . setBit) zeroBits
