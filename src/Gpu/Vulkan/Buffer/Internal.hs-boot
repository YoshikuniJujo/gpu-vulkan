{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Internal where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Buffer.Middle qualified as M

type role MemoryBarrier2 nominal phantom phantom phantom nominal
data MemoryBarrier2
	(mn :: Maybe Type) sm sb (nm :: Symbol) (obj :: VObj.O)

class MemoryBarrier2ListToMiddle nsmsbnmobjs where
	memoryBarrier2ListToMiddle ::
		HPList.PL (U5 MemoryBarrier2) nsmsbnmobjs ->
		HPList.PL M.MemoryBarrier2 (TMapIndex.M0_5 nsmsbnmobjs)
