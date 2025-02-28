{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Rendering (
	Info(..),
	AttachmentInfo(..), attachmentInfoZero,
	AttachmentInfoMaybeToMiddle, AttachmentInfoListToMiddle, MaybeI0'7_8
	) where

import Gpu.Vulkan.Rendering.Internal
