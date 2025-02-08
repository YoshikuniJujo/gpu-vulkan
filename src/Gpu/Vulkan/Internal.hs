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

	-- ** ApplicationInfo

	M.ApplicationInfo(..),
	M.ApiVersion, M.makeApiVersion, M.fromApiVersion,
	M.Variant, M.Major, M.Minor, M.Patch,
	M.apiVersion_1_0, M.apiVersion_1_1, M.apiVersion_1_2, M.apiVersion_1_3,

	-- ** SubmitInfo

	SubmitInfo(..),
	SubmitInfoListToMiddle(..), SemaphorePipelineStageFlags(..),

	SubmitInfo2(..), SubmitInfo2ListToMiddle(..),

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

	M.ClearColorValueToCore,

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

	DependencyInfo(..), dependencyInfoToMiddle,
	BlitImageInfo2(..), blitImageInfo2ToMiddle,

	Sec(..), pattern NanoSec, pattern MicroSec, pattern MilliSec,

	M.remainingMipLevels, M.remainingArrayLayers

	) where

import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TList
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Fixed
import Data.Fixed.Generic qualified as FixedG
import Data.Text qualified as T

import qualified Gpu.Vulkan.Middle as M
import Gpu.Vulkan.Enum
import qualified Gpu.Vulkan.Semaphore.Type as Semaphore
import qualified Gpu.Vulkan.Semaphore.Internal as Semaphore
import qualified Gpu.Vulkan.Semaphore.Middle as Semaphore.M
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline

import Gpu.Vulkan.Memory.Middle qualified as Memory.M
import {-# SOURCE #-} Gpu.Vulkan.Buffer.Internal qualified as Buffer
import Gpu.Vulkan.Image.Internal qualified as Image
import Gpu.Vulkan.Image.Enum qualified as Image

import Gpu.Vulkan.CommandBuffer.Internal qualified as CommandBuffer

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

data SubmitInfo2 mn wsas cbas ssas = SubmitInfo2 {
	submitInfo2Next :: TMaybe.M mn,
	submitInfo2Flags :: SubmitFlags,
	submitInfo2WaitSemaphoreInfos ::
		HPList.PL (U2 Semaphore.SubmitInfo) wsas,
	submitInfo2CommandBufferInfos ::
		HPList.PL (U2 CommandBuffer.SubmitInfo) cbas,
	submitInfo2SignalSemaphoreInfos ::
		HPList.PL (U2 Semaphore.SubmitInfo) ssas }

class M.SubmitInfo2ListToCore (SubmitInfo2ArgsListToMiddle sias) => SubmitInfo2ListToMiddle sias where
	type family SubmitInfo2ArgsListToMiddle sias ::
		[(Maybe Type, [Maybe Type], [Maybe Type], [Maybe Type])]
	submitInfo2ListToMiddle ::
		HPList.PL (U4 SubmitInfo2) sias ->
		HPList.PL (U4 M.SubmitInfo2) (SubmitInfo2ArgsListToMiddle sias)

instance SubmitInfo2ListToMiddle '[] where
	type SubmitInfo2ArgsListToMiddle '[] = '[]
	submitInfo2ListToMiddle HPList.Nil = HPList.Nil

instance (
	WithPoked (TMaybe.M mn),
	TList.Length (TMapIndex.M0_2 cbas),
	HPList.ToListWithCCpsM' WithPoked TMaybe.M (TMapIndex.M0_2 cbas),
	Semaphore.SubmitInfoListToMiddle wsas,
	CommandBuffer.SubmitInfoListToMiddle cbas,
	Semaphore.SubmitInfoListToMiddle ssas,
	SubmitInfo2ListToMiddle sias ) =>
	SubmitInfo2ListToMiddle ('(mn, wsas, cbas, ssas) ': sias) where
	type SubmitInfo2ArgsListToMiddle ('(mn, wsas, cbas, ssas) ': sias) =
		'(mn,	TMapIndex.M0_2 wsas,
			TMapIndex.M0_2 cbas,
			TMapIndex.M0_2 ssas) ': SubmitInfo2ArgsListToMiddle sias
	submitInfo2ListToMiddle (U4 si :** sis) =
		U4 (submitInfo2ToMiddle si) :** submitInfo2ListToMiddle sis

{-
type family SubmitInfo2ArgsListToMiddle sias where
	SubmitInfo2ArgsListToMiddle '[] = '[]
	SubmitInfo2ArgsListToMiddle ('(mn, wsas, cbas, ssas) ': sias) =
		'(mn,	TMapIndex.M0_2 wsas,
			TMapIndex.M0_2 cbas,
			TMapIndex.M0_2 ssas) ': SubmitInfo2ArgsListToMiddle sias
			-}

submitInfo2ToMiddle :: (
	Semaphore.SubmitInfoListToMiddle wsas,
	CommandBuffer.SubmitInfoListToMiddle cbas,
	Semaphore.SubmitInfoListToMiddle ssas ) =>
	SubmitInfo2 mn wsas cbas ssas ->
	M.SubmitInfo2 mn (TMapIndex.M0_2 wsas)
		(TMapIndex.M0_2 cbas) (TMapIndex.M0_2 ssas)
submitInfo2ToMiddle SubmitInfo2 {
	submitInfo2Next = mnxt,
	submitInfo2Flags = fs,
	submitInfo2WaitSemaphoreInfos = wss,
	submitInfo2CommandBufferInfos = cbs,
	submitInfo2SignalSemaphoreInfos = sss
	} = M.SubmitInfo2 {
	M.submitInfo2Next = mnxt,
	M.submitInfo2Flags = fs,
	M.submitInfo2WaitSemaphoreInfos =
		Semaphore.submitInfoListToMiddle wss,
	M.submitInfo2CommandBufferInfos =
		CommandBuffer.submitInfoListToMiddle cbs,
	M.submitInfo2SignalSemaphoreInfos =
		Semaphore.submitInfoListToMiddle sss }

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

data BlitImageInfo2 mn sms sis nms fmts smd sid nmd fmtd ras = BlitImageInfo2 {
	blitImageInfo2Next :: TMaybe.M mn,
	blitImageInfo2SrcImage :: Image.Binded sms sis nms fmts,
	blitImageInfo2SrcImageLayout :: Image.Layout,
	blitImageInfo2DstImage :: Image.Binded smd sid nmd fmtd,
	blitImageInfo2DstImageLayout :: Image.Layout,
	blitImageInfo2Regions :: HPList.PL Image.Blit2 ras,
	blitImageInfo2Filter :: Filter }

blitImageInfo2ToMiddle ::
	BlitImageInfo2 mn sms sis nms fmts smd sid nmd fmtd ras ->
	M.BlitImageInfo2 mn ras
blitImageInfo2ToMiddle BlitImageInfo2 {
	blitImageInfo2Next = mnxt,
	blitImageInfo2SrcImage = Image.Binded si,
	blitImageInfo2SrcImageLayout = sil,
	blitImageInfo2DstImage = Image.Binded di,
	blitImageInfo2DstImageLayout = dil,
	blitImageInfo2Regions = rs,
	blitImageInfo2Filter = flt } = M.BlitImageInfo2 {
	M.blitImageInfo2Next = mnxt,
	M.blitImageInfo2SrcImage = si, M.blitImageInfo2SrcImageLayout = sil,
	M.blitImageInfo2DstImage = di, M.blitImageInfo2DstImageLayout = dil,
	M.blitImageInfo2Regions = rs, M.blitImageInfo2Filter = flt }

newtype Sec = Sec (FixedG.F E9 Word64)

pattern NanoSec :: Word64 -> Sec
pattern NanoSec ns = Sec (FixedG.MkF ns)

pattern MicroSec :: FixedG.F E3 Word64 -> Sec
pattern MicroSec ms <- Sec (FixedG.changeUnit -> ms) where
	MicroSec ms = Sec $ FixedG.changeUnit ms

pattern MilliSec :: FixedG.F E6 Word64 -> Sec
pattern MilliSec ms <- Sec (FixedG.changeUnit -> ms) where
	MilliSec ms = Sec $ FixedG.changeUnit ms
