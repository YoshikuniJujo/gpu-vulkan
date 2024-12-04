{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGe PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Buffer.Internal where

import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Memory.Type qualified as Memory
import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.Sparse.Internal

import Gpu.Vulkan.Sparse.Middle qualified as M
import Gpu.Vulkan.Sparse.Buffer.Middle qualified as M

data MemoryBindInfo sb bnm objs sais = MemoryBindInfo {
	memoryBindInfoBuffer :: Buffer.B sb bnm objs,
	memoryBindInfoBinds :: HPList.PL (U3 MemoryBind) sais }

memoryBindInfoToMiddle :: MemoryBindsToMiddle sais =>
	Device.D sd -> MemoryBindInfo sb bnm objs sais -> IO M.MemoryBindInfo
memoryBindInfoToMiddle dv MemoryBindInfo {
	memoryBindInfoBuffer = Buffer.B _ b,
	memoryBindInfoBinds = bs
	} = do
	mbs <- memoryBindsToMiddle dv bs
	pure M.MemoryBindInfo {
		M.memoryBindInfoBuffer = b,
		M.memoryBindInfoBinds = mbs }

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
