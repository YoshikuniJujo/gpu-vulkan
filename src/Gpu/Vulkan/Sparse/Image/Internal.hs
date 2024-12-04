{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Image.Internal where

import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan.Internal
import Gpu.Vulkan.Device.Internal qualified as Device
import Gpu.Vulkan.Memory.Type qualified as Memory
import Gpu.Vulkan.Image.Internal qualified as Image
import Gpu.Vulkan.Sparse.Internal qualified as S

import Gpu.Vulkan.Sparse.Enum qualified as S
import Gpu.Vulkan.Sparse.Image.Middle qualified as M

data OpaqueMemoryBindInfo si inm fmt sais = OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage :: Image.I si inm fmt,
	opaqueMemoryBindInfoBinds :: HPList.PL (U3 S.MemoryBind) sais }

opaqueMemoryBindInfoToMiddle :: S.MemoryBindsToMiddle sais =>
	Device.D sd -> OpaqueMemoryBindInfo si inm fmt sais ->
	IO M.OpaqueMemoryBindInfo
opaqueMemoryBindInfoToMiddle dv OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage = Image.I i,
	opaqueMemoryBindInfoBinds = bs } = do
	mbs <- S.memoryBindsToMiddle dv bs
	pure M.OpaqueMemoryBindInfo {
		M.opaqueMemoryBindInfoImage = i,
		M.opaqueMemoryBindInfoBinds = mbs }

data MemoryBind sm ibargs i = MemoryBind {
	memoryBindSubresource :: Image.Subresource,
	memoryBindOffset :: Offset3d,
	memoryBindExtent :: Extent3d,
	memoryBindMemory :: Memory.M sm ibargs,
	memoryBindMemoryOffset :: Memory.RawOffset i,
	memoryBindFlags :: S.MemoryBindFlags }

memoryBindToMiddle :: Memory.RawOffsetToOffset ibargs i =>
	Device.D sd -> MemoryBind sm ibargs i -> IO M.MemoryBind
memoryBindToMiddle dv MemoryBind {
	memoryBindSubresource = sr,
	memoryBindOffset = ost,
	memoryBindExtent = ex,
	memoryBindMemory = m@(Memory.M _ mm),
	memoryBindMemoryOffset = most,
	memoryBindFlags = fs
	} = do
	mmo <- Memory.rawOffset dv m most
	pure M.MemoryBind {
		M.memoryBindSubresource = sr,
		M.memoryBindOffset = ost,
		M.memoryBindExtent = ex,
		M.memoryBindMemory = mm,
		M.memoryBindMemoryOffset = mmo,
		M.memoryBindFlags = fs }
