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
import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.Sparse.Internal

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

class MemoryBindInfosToMiddle mbias where
	memoryBindInfosToMiddle ::
		Device.D sd ->
		HPList.PL (U4 MemoryBindInfo) mbias -> IO [M.MemoryBindInfo]

instance MemoryBindInfosToMiddle '[] where
	memoryBindInfosToMiddle _ HPList.Nil = pure []

instance (MemoryBindsToMiddle sais, MemoryBindInfosToMiddle mbias) =>
	MemoryBindInfosToMiddle ('(sb, bnm, objs, sais) ': mbias) where
	memoryBindInfosToMiddle dv (U4 mbi :** mbis) = (:)
		<$> memoryBindInfoToMiddle dv mbi
		<*> memoryBindInfosToMiddle dv mbis

{-
memoryBindInfosToMiddle ::
	HPList.ToListWithCM' MemoryBindsToMiddle I3_4 mbias =>
	Device.D sd -> HPList.PL (U4 MemoryBindInfo) mbias -> IO [M.MemoryBindInfo]
memoryBindInfosToMiddle dv = toListWithCM' (\U4 mbi -> memoryBindInfoToMiddle dv mbi)
-}
