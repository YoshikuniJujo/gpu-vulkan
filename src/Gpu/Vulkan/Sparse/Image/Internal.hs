{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Image.Internal where

import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Image.Internal qualified as Image
import Gpu.Vulkan.Sparse.Internal

import Gpu.Vulkan.Sparse.Image.Middle qualified as M

data OpaqueMemoryBindInfo si inm fmt sais = OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage :: Image.I si inm fmt,
	opaqueMemoryBindInfoBinds :: HPList.PL (U3 MemoryBind) sais }

opaqueMemoryBindInfoToMiddle :: MemoryBindsToMiddle sais =>
	Device.D sd -> OpaqueMemoryBindInfo si inm fmt sais ->
	IO M.OpaqueMemoryBindInfo
opaqueMemoryBindInfoToMiddle dv OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage = Image.I i,
	opaqueMemoryBindInfoBinds = bs } = do
	mbs <- memoryBindsToMiddle dv bs
	pure M.OpaqueMemoryBindInfo {
		M.opaqueMemoryBindInfoImage = i,
		M.opaqueMemoryBindInfoBinds = mbs }
