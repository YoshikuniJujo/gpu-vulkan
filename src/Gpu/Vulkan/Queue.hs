{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue (

	-- * SUBMIT AND WAIT IDLE

	submit, submit2, M.waitIdle, M.Q,

	-- * TYPE SYNONYM

	Index,

	-- * SPARSE RESOURCES

	bindSparse, BindSparseInfo(..), BindSparseInfosToMiddle,

	-- * ENUM

	module Gpu.Vulkan.Queue.Enum

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.Internal

import qualified Gpu.Vulkan.Fence.Type as Fence

import Gpu.Vulkan.Queue.Middle qualified as M
import Gpu.Vulkan.Queue.Enum

import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList

import Foreign.Storable.PeekPoke
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Semaphore.Internal qualified as Semaphore
import Gpu.Vulkan.Sparse.Buffer.Internal qualified as Sparse.Buffer
import Gpu.Vulkan.Sparse.Image.Internal qualified as Sparse.Image

import Control.Monad
import Debug

submit :: SubmitInfoListToMiddle sias => M.Q ->
	HeteroParList.PL (U4 SubmitInfo) sias -> Maybe (Fence.F sf) -> IO ()
submit q sis mf =
	M.submit q (submitInfoListToMiddle sis) $ (\(Fence.F f) -> f) <$> mf

submit2 :: SubmitInfo2ListToMiddle sias => M.Q ->
	HeteroParList.PL (U4 SubmitInfo2) sias -> Maybe (Fence.F sf) -> IO ()
submit2 q sis mf =
	M.submit2 q (submitInfo2ListToMiddle sis) $ (\(Fence.F f) -> f) <$> mf

type Index = Word32

bindSparse :: (
	BindSparseInfosToMiddle ias,
	HPList.ToListWithCCpsM' WithPoked TMaybe.M (M0_6 ias) ) =>
	Device.D sd -> M.Q ->
	HPList.PL (U6 BindSparseInfo) ias -> Maybe (Fence.F sf) -> IO ()
bindSparse dv q is mf = do
	let	mmf = case mf of
			Just (Fence.F f) -> Just f
			Nothing -> Nothing
	when debug $ putStrLn
		"Gpu.Vulkan.Queue.bindSparse: before bindSparseInfoToMiddle"
	mis <- bindSparseInfosToMiddle dv is
	when debug $ putStrLn
		"Gpu.Vulkan.Queue.bindSparse: before M.bindSparse"
	M.bindSparse q mis mmf

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

class BindSparseInfosToMiddle bsias where
	bindSparseInfosToMiddle :: Device.D sd ->
		HPList.PL (U6 BindSparseInfo) bsias ->
		IO (HPList.PL M.BindSparseInfo (M0_6 bsias))

instance BindSparseInfosToMiddle '[] where
	bindSparseInfosToMiddle _ HPList.Nil = pure HPList.Nil

instance (
	Sparse.Buffer.MemoryBindInfosToMiddle bbs,
	Sparse.Image.OpaqueMemoryBindInfosToMiddle iobs,
	Sparse.Image.MemoryBindInfosToMiddle ibs,
	BindSparseInfosToMiddle bsias ) =>
	BindSparseInfosToMiddle
		('(mn, swss, bbs, iobs, ibs, ssss) ': bsias) where
	bindSparseInfosToMiddle dv (U6 i :** is) = (:**)
		<$> bindSparseInfoToMiddle dv i
		<*> bindSparseInfosToMiddle dv is

-- instance BindSparse
