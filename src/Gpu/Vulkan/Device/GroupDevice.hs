{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.GroupDevice (

	CreateInfo(..),

	Mask, pattern AllDevice, Index, mkMask

) where

import Gpu.Vulkan.Device.GroupDevice.Internal
