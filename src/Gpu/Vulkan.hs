{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan (

	-- * INFO

	-- ** ApplicationINfo

	ApplicationInfo(..),
	ApiVersion,
	makeApiVersion, fromApiVersion, Variant, Major, Minor, Patch,
	apiVersion_1_0, apiVersion_1_1, apiVersion_1_2, apiVersion_1_3,

	-- ** SubmitInfo

	SubmitInfo(..),
	SubmitInfoListToMiddle, SemaphorePipelineStageFlags(..),

	SubmitInfo2(..), SubmitInfo2ListToMiddle,

	-- * PROPERTIES

	LayerProperties(..),
	FormatProperties(..),

	-- * NAMES

	LayerName(..), layerKhronosValidation,

	-- * PIPELINE VALUES

	-- ** ViewPort

	Viewport, pattern Viewport,
	viewportX, viewportY, viewportWidth, viewportHeight,
	viewportMinDepth, viewportMaxDepth,

	-- ** StencilOpState

	StencilOpState(..),

	-- ** ClearValue

	ClearValue(..), ClearValueListToCore,

	-- *** ClearType

	ClearType(..), ClearColorType(..),

	-- *** ClearColorValue

	ClearColorValueToCore,

	-- *** ClearDepthStencilValue

	ClearDepthStencilValue, pattern ClearDepthStencilValue,
	clearDepthStencilValueDepth, clearDepthStencilValueStencil,

	-- * RECT, OFFSET AND EXTENT

	-- ** Rect

	Rect2d, pattern Rect2d, rect2dExtent, rect2dOffset,

	-- ** Offset

	Offset2d, pattern Offset2d, offset2dX, offset2dY,
	Offset3d, pattern Offset3d, offset3dX, offset3dY, offset3dZ,

	-- ** Extent

	Extent2d, pattern Extent2d,
	extent2dWidth, extent2dHeight,

	Extent3d, pattern Extent3d,
	extent3dWidth, extent3dHeight, extent3dDepth,

	-- * OTHERS

	Size(..),

	DependencyInfo(..), BlitImageInfo2(..),

	Sec(..), pattern NanoSec, pattern MicroSec, pattern MilliSec,

	remainingMipLevels, remainingArrayLayers,

	-- * ENUM

	module Gpu.Vulkan.Enum,

	) where

import Gpu.Vulkan.Internal
import Gpu.Vulkan.Enum
