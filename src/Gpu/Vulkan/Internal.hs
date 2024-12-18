{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Internal (

	-- * INFO

	-- ** ApplicationINfo

	M.ApplicationInfo(..),
	M.ApiVersion, M.makeApiVersion,
	M.apiVersion_1_0, M.apiVersion_1_1, M.apiVersion_1_2, M.apiVersion_1_3,

	-- ** SubmitInfo

	SubmitInfo(..),
	SubmitInfoListToMiddle(..), SemaphorePipelineStageFlags(..),

	-- * PROPERTIES

	LayerProperties(..), layerPropertiesFromMiddle,
	M.FormatProperties(..),

	-- * NAME

	LayerName(..), layerKhronosValidation,

	-- * PIPELINE VALUES

	-- ** ViewPort

	M.Viewport, pattern M.Viewport,
	M.viewportX, M.viewportY, M.viewportWidth, M.viewportHeight,
	M.viewportMinDepth, M.viewportMaxDepth,

	-- ** StencilOpState

	M.StencilOpState(..),

	-- ** ClearValue

	M.ClearValue(..), M.ClearValueListToCore,

	-- *** ClearType

	M.ClearType(..), M.ClearColorType(..),

	-- *** ClearColorValue

	-- *** ClearDepthStencilValue

	M.ClearDepthStencilValue, pattern M.ClearDepthStencilValue,
	M.clearDepthStencilValueDepth, M.clearDepthStencilValueStencil,

	-- * RECT, OFFSET AND EXTENT

	-- ** Rect

	M.Rect2d, pattern M.Rect2d, M.rect2dExtent, M.rect2dOffset,

	-- ** Offset

	M.Offset2d, pattern M.Offset2d, M.offset2dX, M.offset2dY,
	M.Offset3d, pattern M.Offset3d, M.offset3dX, M.offset3dY, M.offset3dZ,

	-- ** Extent

	M.Extent2d, pattern M.Extent2d,
	M.extent2dWidth, M.extent2dHeight,

	M.Extent3d, pattern M.Extent3d,
	M.extent3dWidth, M.extent3dHeight, M.extent3dDepth,

	-- * OTHERS

	M.Size(..),

	DependencyInfo(..), dependencyInfoToMiddle

	) where

import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:**))
import Data.Text qualified as T

import qualified Gpu.Vulkan.Middle as M
import Gpu.Vulkan.Enum
import qualified Gpu.Vulkan.Semaphore.Type as Semaphore
import qualified Gpu.Vulkan.Semaphore.Middle as Semaphore.M
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline

import Gpu.Vulkan.Memory.Middle qualified as Memory.M
import {-# SOURCE #-} Gpu.Vulkan.Buffer.Internal qualified as Buffer
import Gpu.Vulkan.Image.Internal qualified as Image

data SubmitInfo n sss ss ssss = SubmitInfo {
	submitInfoNext :: TMaybe.M n,
	submitInfoWaitSemaphoreDstStageMasks ::
		HeteroParList.PL SemaphorePipelineStageFlags sss,
	submitInfoCommandBuffers :: HeteroParList.PL CommandBuffer.C ss,
	submitInfoSignalSemaphores ::
		HeteroParList.PL Semaphore.S ssss }

class M.SubmitInfoListToCore (MiddleNextList ns3s2s4) => SubmitInfoListToMiddle
	(ns3s2s4 :: [(Maybe Type, [Type], [Type], [Type])]) where
	type MiddleNextList ns3s2s4 :: [Maybe Type]
	submitInfoListToMiddle ::
		HeteroParList.PL (U4 SubmitInfo) ns3s2s4 ->
		HeteroParList.PL M.SubmitInfo (MiddleNextList ns3s2s4)

instance SubmitInfoListToMiddle '[] where
	type MiddleNextList '[] = '[]
	submitInfoListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M n),
	SubmitInfoListToMiddle nssvsss ) =>
	SubmitInfoListToMiddle ('(n, sss, svss, ssss) ': nssvsss) where
	type MiddleNextList ('(n, sss, svss, ssss) ': nssvsss) =
		n ': MiddleNextList nssvsss
	submitInfoListToMiddle (U4 si :** sis) =
		submitInfoToMiddle si :** submitInfoListToMiddle sis

submitInfoToMiddle ::
	SubmitInfo n sss svss ssss -> M.SubmitInfo n
submitInfoToMiddle SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks =
		semaphorePipelineStageFlagsToMiddle -> wsdsms,
	submitInfoCommandBuffers = HeteroParList.toList (\x -> CommandBuffer.unC x) -> cbs,
	submitInfoSignalSemaphores =
		HeteroParList.toList (\(Semaphore.S s) -> s) -> ssmprs
	} = M.SubmitInfo {
	M.submitInfoNext = mnxt,
	M.submitInfoWaitSemaphoreDstStageMasks = wsdsms,
	M.submitInfoCommandBuffers = cbs,
	M.submitInfoSignalSemaphores = ssmprs }

data SemaphorePipelineStageFlags ss =
	SemaphorePipelineStageFlags (Semaphore.S ss) Pipeline.StageFlags
	deriving Show

-- deriving instance (Show n, Show (HeteroParList SemaphorePipelineStageFlags sss)) =>
--	Show (SubmitInfo n sss s vs)

-- deriving instance Show (HeteroParList Semaphore.S ss)

semaphorePipelineStageFlagsToMiddle ::
	HeteroParList.PL SemaphorePipelineStageFlags sss ->
	[(Semaphore.M.S, Pipeline.StageFlags)]
semaphorePipelineStageFlagsToMiddle = HeteroParList.toList
	\(SemaphorePipelineStageFlags (Semaphore.S s) psfs) -> (s, psfs)

class SemaphorePipelineStageFlagsFromMiddle sss where
	semaphorePipelineStageFlagsFromMiddle ::
		[(Semaphore.M.S, Pipeline.StageFlags)] ->
		HeteroParList.PL SemaphorePipelineStageFlags sss

instance SemaphorePipelineStageFlagsFromMiddle '[] where
	semaphorePipelineStageFlagsFromMiddle = \case
		[] -> HeteroParList.Nil
		_ -> error $
			"semaphorePipelineStageFlagsFromMiddle @'[] xs: " ++
			"xs should be null"

instance SemaphorePipelineStageFlagsFromMiddle sss =>
	SemaphorePipelineStageFlagsFromMiddle (ss ': sss) where
	semaphorePipelineStageFlagsFromMiddle = \case
		(s, psfs) : spsfss ->
			SemaphorePipelineStageFlags (Semaphore.S s) psfs :**
			semaphorePipelineStageFlagsFromMiddle spsfss
		[] -> error $
			"semaphorePipelineStageFlagsFromMiddle " ++
			"@(ss ': sss) xs: xs should not be null"

data LayerProperties = LayerProperties {
	layerPropertiesLayerName :: LayerName,
	layerPropertiesSpecVersion :: M.ApiVersion,
	layerPropertiesImplementationVersion :: M.ApiVersion,
	layerPropertiesDescription :: T.Text }
	deriving Show

layerPropertiesFromMiddle :: M.LayerProperties -> LayerProperties
layerPropertiesFromMiddle M.LayerProperties {
	M.layerPropertiesLayerName = ln,
	M.layerPropertiesSpecVersion = sv,
	M.layerPropertiesImplementationVersion = iv,
	M.layerPropertiesDescription = dsc } = LayerProperties {
	layerPropertiesLayerName = LayerName ln,
	layerPropertiesSpecVersion = sv,
	layerPropertiesImplementationVersion = iv,
	layerPropertiesDescription = dsc }

newtype LayerName = LayerName { unLayerName :: T.Text } deriving (Show, Eq)

layerKhronosValidation :: LayerName
layerKhronosValidation = LayerName "VK_LAYER_KHRONOS_validation"

data DependencyInfo mn mbas bmbas imbas = DependencyInfo {
	dependencyInfoNext :: TMaybe.M mn,
	dependencyInfoDependencyFlags :: DependencyFlags,
	dependencyInfoMemoryBarriers :: HPList.PL Memory.M.Barrier2 mbas,
	dependencyInfoBufferMemoryBarriers ::
		HPList.PL (U5 Buffer.MemoryBarrier2) bmbas,
	dependencyInfoImageMemoryBarriers ::
		HPList.PL (U5 Image.MemoryBarrier2) imbas }

dependencyInfoToMiddle :: (
	Buffer.MemoryBarrier2ListToMiddle bmbas,
	Image.MemoryBarrier2ListToMiddle imbas ) =>
	DependencyInfo mn mbas bmbas imbas ->
	M.DependencyInfo mn mbas (TMapIndex.M0_5 bmbas) (TMapIndex.M0_5 imbas)
dependencyInfoToMiddle DependencyInfo {
	dependencyInfoNext = mnxt,
	dependencyInfoDependencyFlags = fs,
	dependencyInfoMemoryBarriers = mbs,
	dependencyInfoBufferMemoryBarriers = bmbs,
	dependencyInfoImageMemoryBarriers = imbs
	} = M.DependencyInfo {
	M.dependencyInfoNext = mnxt,
	M.dependencyInfoDependencyFlags = fs,
	M.dependencyInfoMemoryBarriers = mbs,
	M.dependencyInfoBufferMemoryBarriers =
		Buffer.memoryBarrier2ListToMiddle bmbs,
	M.dependencyInfoImageMemoryBarriers =
		Image.memoryBarrier2ListToMiddle imbs }
