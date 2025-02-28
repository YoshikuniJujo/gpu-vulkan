{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Rendering.Internal where

import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Image qualified as Img
import Gpu.Vulkan.ImageView qualified as ImgVw
import Gpu.Vulkan.ImageView.Type qualified as ImgVw
import Gpu.Vulkan.Attachment qualified as Att

import Gpu.Vulkan.Rendering.Middle qualified as M

data AttachmentInfo mn vnm vfmt sv rvnm rvfmt srv ct = AttachmentInfo {
	attachmentInfoNext :: TMaybe.M mn,
	attachmentInfoImageView :: ImgVw.I vnm vfmt sv,
	attachmentInfoImageLayout :: Img.Layout,
	attachmentInfoResolveMode :: Vk.ResolveModeFlagBits,
	attachmentInfoResolveImageView :: ImgVw.I rvnm rvfmt srv,
	attachmentInfoResolveImageLayout :: Img.Layout,
	attachmentInfoLoadOp :: Att.LoadOp,
	attachmentInfoStoreOp :: Att.StoreOp,
	attachmentInfoClearValue :: Vk.ClearValue ct }

deriving instance Show (TMaybe.M mn) =>
	Show (AttachmentInfo mn vnm vfmt sv rvnm rvfmt srv ct)

attachmentInfoToMiddle :: AttachmentInfo mn vnm vfmt sv rvnm rvfmt srv ct ->
	M.AttachmentInfo mn ct
attachmentInfoToMiddle AttachmentInfo {
	attachmentInfoNext = mnxt,
	attachmentInfoImageView = ImgVw.I iv,
	attachmentInfoImageLayout = il,
	attachmentInfoResolveMode = rm,
	attachmentInfoResolveImageView = ImgVw.I riv,
	attachmentInfoResolveImageLayout = ril,
	attachmentInfoLoadOp = lo,
	attachmentInfoStoreOp = so,
	attachmentInfoClearValue = cv } = M.AttachmentInfo {
	M.attachmentInfoNext = mnxt,
	M.attachmentInfoImageView = iv,
	M.attachmentInfoImageLayout = il,
	M.attachmentInfoResolveMode = rm,
	M.attachmentInfoResolveImageView = riv,
	M.attachmentInfoResolveImageLayout = ril,
	M.attachmentInfoLoadOp = lo,
	M.attachmentInfoStoreOp = so,
	M.attachmentInfoClearValue = cv }
