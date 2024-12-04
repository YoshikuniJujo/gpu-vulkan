{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue (

	-- * SUBMIT AND WAIT IDLE

	submit, M.waitIdle, M.Q,

	-- * TYPE SYNONYM

	Index,

	-- * SPARSE RESOURCES

	BindSparseInfo(..),

	-- * ENUM

	module Gpu.Vulkan.Queue.Enum

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.Internal

import qualified Gpu.Vulkan.Fence.Type as Fence

import Gpu.Vulkan.Queue.Middle qualified as M
import Gpu.Vulkan.Queue.Enum

import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Semaphore.Internal qualified as Semaphore
import Gpu.Vulkan.Sparse.Buffer.Internal qualified as Sparse.Buffer
import Gpu.Vulkan.Sparse.Image.Internal qualified as Sparse.Image

submit :: SubmitInfoListToMiddle sias => M.Q ->
	HeteroParList.PL (U4 SubmitInfo) sias -> Maybe (Fence.F sf) -> IO ()
submit q sis mf =
	M.submit q (submitInfoListToMiddle sis) $ (\(Fence.F f) -> f) <$> mf

type Index = Word32

data BindSparseInfo mn swss bbs iobs ibs ssss = BindSparseInfo {
	bindSparseInfoNext :: TMaybe.M mn,
	bindSparseInfoWaitSemaphores :: HPList.PL Semaphore.S swss,
	bindSparseInfoBufferBinds ::
		HPList.PL (U4 Sparse.Buffer.MemoryBindInfo) bbs,
	bindSparseInfoImageOpaqueBinds ::
		HPList.PL (U4 Sparse.Image.OpaqueMemoryBindInfo) iobs,
	bindSparseInfoImageBinds ::
		HPList.PL (U4 Sparse.Image.MemoryBindInfo) ibs,
	bindSparseInfoSignalSemaphores :: HPList.PL Semaphore.S ssss }

bindSparseInfoToMiddle :: (
	Sparse.Buffer.MemoryBindInfosToMiddle bbs,
	Sparse.Image.OpaqueMemoryBindInfosToMiddle iobs,
	Sparse.Image.MemoryBindInfosToMiddle ibs ) =>
	Device.D sd ->
	BindSparseInfo mn swss bbs iobs ibs ssss -> IO (M.BindSparseInfo mn)
bindSparseInfoToMiddle dv BindSparseInfo {
	bindSparseInfoNext = mn,
	bindSparseInfoWaitSemaphores =
		HPList.toList (\(Semaphore.S s) -> s) -> wss,
	bindSparseInfoBufferBinds = bbs,
	bindSparseInfoImageOpaqueBinds = iobs,
	bindSparseInfoImageBinds = ibs,
	bindSparseInfoSignalSemaphores =
		HPList.toList (\(Semaphore.S s) -> s) -> sss
	} = do
	mbbs <- Sparse.Buffer.memoryBindInfosToMiddle dv bbs
	miobs <- Sparse.Image.opaqueMemoryBindInfosToMiddle dv iobs
	mibs <- Sparse.Image.memoryBindInfosToMiddle dv ibs
	pure M.BindSparseInfo {
		M.bindSparseInfoNext = mn,
		M.bindSparseInfoWaitSemaphores = wss,
		M.bindSparseInfoBufferBinds = mbbs,
		M.bindSparseInfoImageOpaqueBinds = miobs,
		M.bindSparseInfoImageBinds = mibs,
		M.bindSparseInfoSignalSemaphores = sss }
