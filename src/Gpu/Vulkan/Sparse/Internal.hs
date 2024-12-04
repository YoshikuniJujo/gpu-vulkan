{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Internal where

import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Memory.Type qualified as Memory
import Gpu.Vulkan.Sparse.Enum

import Gpu.Vulkan.Sparse.Middle qualified as M

data MemoryBind sm ibargs i = MemoryBind {
	memoryBindResourceOffset :: Device.Size,
	memoryBindSize :: Device.Size,
	memoryBindMemory :: Memory.M sm ibargs,
	memoryBindMemoryOffset :: Memory.RawOffset i,
	memoryBindFlags :: MemoryBindFlags }

memoryBindToMiddle :: Memory.RawOffsetToOffset ibargs i =>
	Device.D sd -> MemoryBind sm ibargs i -> IO M.MemoryBind
memoryBindToMiddle dv MemoryBind {
	memoryBindResourceOffset = rsco,
	memoryBindSize = sz,
	memoryBindMemory = m@(Memory.M _ mm),
	memoryBindMemoryOffset = rwo,
	memoryBindFlags = fs } = do
	mo <- Memory.rawOffset dv m rwo
	pure M.MemoryBind {
		M.memoryBindResourceOffset = rsco,
		M.memoryBindSize = sz,
		M.memoryBindMemory = mm,
		M.memoryBindMemoryOffset = mo,
		M.memoryBindFlags = fs }

class MemoryBindsToMiddle sais where
	memoryBindsToMiddle ::
		Device.D sd -> HPList.PL (U3 MemoryBind) sais -> IO [M.MemoryBind]

instance MemoryBindsToMiddle '[] where
	memoryBindsToMiddle _ HPList.Nil = pure []

instance (Memory.RawOffsetToOffset ibargs i, MemoryBindsToMiddle sais) =>
	MemoryBindsToMiddle ('(sm, ibargs, i) ': sais) where
	memoryBindsToMiddle dv (U3 mb :** mbs) = (:)
		<$> memoryBindToMiddle dv mb
		<*> memoryBindsToMiddle dv mbs
