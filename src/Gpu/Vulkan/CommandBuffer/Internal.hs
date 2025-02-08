{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Internal (

	-- * ALLOCATE

	C, GBinded, CBinded,

	-- ** Type Level List

	allocateCs, AllocateInfo(..),

	-- ** Value Level List

	allocateList, AllocateInfoList(..),

	-- * BEGIN AND RESET

	begin, reset, M.BeginInfo(..), M.InheritanceInfo(..),

	-- * SUBMIT INFO

	SubmitInfo(..), SubmitInfoListToMiddle(..)

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TList
import Data.TypeLevel.List qualified as TLength
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import Gpu.Vulkan.Device.GroupDevice.Internal qualified as GDevice
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Middle as M

allocateCs :: (
	WithPoked (TMaybe.M mn), TLength.Length c, HeteroParList.FromList c ) =>
	Device.D sd -> AllocateInfo mn scp c ->
	(forall scb . HeteroParList.LL (C scb) c -> IO a) -> IO a
allocateCs (Device.D dvc) ai f = bracket
	(M.allocateCs dvc $ allocateInfoToMiddle ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPool ai)
	(f . HeteroParList.fromList (HeteroParList.Dummy . C))

allocateList :: WithPoked (TMaybe.M mn) =>
	Device.D sd -> AllocateInfoList mn scp ->
	(forall scb . [C scb] -> IO a) -> IO a
allocateList (Device.D dvc) ai f = bracket
	(M.allocateCs dvc $ allocateInfoToMiddleList ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPoolList ai)
	(f . (C <$>))

data AllocateInfo mn scp (c :: [()]) = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoCommandPool :: CommandPool.C scp,
	allocateInfoLevel :: Level }

data AllocateInfoList mn scp = AllocateInfoList {
	allocateInfoNextList :: TMaybe.M mn,
	allocateInfoCommandPoolList :: CommandPool.C scp,
	allocateInfoLevelList :: Level,
	allocateInfoCommandBufferCountList :: Word32 }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn s c)

allocateInfoToMiddle :: forall n s c . TLength.Length c =>
	AllocateInfo n s c -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = lvl } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount = TLength.length @_ @c }

allocateInfoToMiddleList :: AllocateInfoList n s -> M.AllocateInfo n
allocateInfoToMiddleList AllocateInfoList {
	allocateInfoNextList = mnxt,
	allocateInfoCommandPoolList = CommandPool.C cp,
	allocateInfoLevelList = lvl,
	allocateInfoCommandBufferCountList = c } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount = c }

begin :: (WithPoked (TMaybe.M mn), WithPoked (TMaybe.M ii)) =>
	C s -> M.BeginInfo mn ii -> IO a -> IO a
begin (C cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: C sc -> ResetFlags -> IO ()
reset (C cb) rfs = M.reset cb rfs

data SubmitInfo mn sc = SubmitInfo {
	submitInfoNext :: TMaybe.M mn,
	submitInfoCommandBuffer :: C sc,
	submitInfoDeviceMask :: GDevice.Mask }

class (	TList.Length (TMapIndex.M0_2 mnscs),
	HPList.ToListWithCCpsM' WithPoked TMaybe.M (TMapIndex.M0_2 mnscs)
	) =>
	SubmitInfoListToMiddle mnscs where
	submitInfoListToMiddle ::
		HPList.PL (U2 SubmitInfo) mnscs ->
		HPList.PL M.SubmitInfo (TMapIndex.M0_2 mnscs)

instance SubmitInfoListToMiddle '[] where
	submitInfoListToMiddle HPList.Nil = HPList.Nil

instance (
	TList.Length (TMapIndex.M0_2 (mnsc : mnscs)),
	HPList.ToListWithCCpsM'
		WithPoked TMaybe.M (TMapIndex.M0_2 (mnsc : mnscs)),
	SubmitInfoListToMiddle mnscs ) =>
	SubmitInfoListToMiddle (mnsc ': mnscs) where
	submitInfoListToMiddle (U2 si :** sis) =
		submitInfoToMiddle si :** submitInfoListToMiddle sis

submitInfoToMiddle :: SubmitInfo mn sc -> M.SubmitInfo mn
submitInfoToMiddle SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoCommandBuffer = C c,
	submitInfoDeviceMask = GDevice.Mask dm } = M.SubmitInfo {
	M.submitInfoNext = mnxt,
	M.submitInfoCommandBuffer = c, M.submitInfoDeviceMask = dm }
