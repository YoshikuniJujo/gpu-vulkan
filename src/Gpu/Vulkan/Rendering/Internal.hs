{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Rendering.Internal where

import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index
import Data.TypeLevel.Tuple.MapIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Default
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.Word

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Image qualified as Img
import Gpu.Vulkan.ImageView qualified as ImgVw
import Gpu.Vulkan.ImageView.Type qualified as ImgVw
import Gpu.Vulkan.Attachment qualified as Att

import Gpu.Vulkan.Rendering.Middle qualified as M

data Info mn cas das sas = Info {
	infoNext :: TMaybe.M mn, infoFlags :: Vk.RenderingFlags,
	infoRenderArea :: Vk.Rect2d,
	infoLayerCount :: Word32, infoViewMask :: Word32,
	infoColorAttachments :: HPList.PL (U8 AttachmentInfo) cas,
	infoDepthAttachment :: TPMaybe.M (U8 AttachmentInfo) das,
	infoStencilAttachment :: TPMaybe.M (U8 AttachmentInfo) sas }

deriving instance (
	Show (TMaybe.M mn),
	Show (HPList.PL (U8 AttachmentInfo) cas),
	Show (TPMaybe.M (U8 AttachmentInfo) das),
	Show (TPMaybe.M (U8 AttachmentInfo) sas) ) => Show (Info mn cas das sas)

infoToMiddle :: (
	AttachmentInfoListToMiddle cas,
	AttachmentInfoMaybeToMiddle das, AttachmentInfoMaybeToMiddle sas ) =>
	Info mn	cas das sas ->
	M.Info mn (M0'7_8 cas) (MaybeI0'7_8 das) (MaybeI0'7_8 sas)
infoToMiddle Info {
	infoNext = mnxt, infoFlags = flgs, infoRenderArea = ra,
	infoLayerCount = lc, infoViewMask = vm,
	infoColorAttachments = cas,
	infoDepthAttachment = da, infoStencilAttachment = sa
		} = M.Info {
	M.infoNext = mnxt, M.infoFlags = flgs, M.infoRenderArea = ra,
	M.infoLayerCount = lc, M.infoViewMask = vm,
	M.infoColorAttachments = attachmentInfoListToMiddle cas,
	M.infoDepthAttachment = attachmentInfoMaybeToMiddle da,
	M.infoStencilAttachment = attachmentInfoMaybeToMiddle sa }

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

attachmentInfoZero :: Default (Vk.ClearValue ct) =>
	TMaybe.M mn -> IO (AttachmentInfo mn vnm vfmt sv rvnm rvfmt srv ct)
attachmentInfoZero mnxt = do
	(niv, nriv) <- (,) <$> ImgVw.null <*> ImgVw.null
	pure AttachmentInfo {
		attachmentInfoNext = mnxt,
		attachmentInfoImageView = niv,
		attachmentInfoImageLayout = Img.Layout 0,
		attachmentInfoResolveMode = Vk.ResolveModeFlagBits 0,
		attachmentInfoResolveImageView = nriv,
		attachmentInfoResolveImageLayout = Img.Layout 0,
		attachmentInfoLoadOp = Att.LoadOp 0,
		attachmentInfoStoreOp = Att.StoreOp 0,
		attachmentInfoClearValue = def }

deriving instance Show (TMaybe.M mn) =>
	Show (AttachmentInfo mn vnm vfmt sv rvnm rvfmt srv ct)

class AttachmentInfoListToMiddle cas where
	attachmentInfoListToMiddle ::
		HPList.PL (U8 AttachmentInfo) cas ->
		HPList.PL (U2 M.AttachmentInfo) (M0'7_8 cas)

instance AttachmentInfoListToMiddle '[] where
	attachmentInfoListToMiddle HPList.Nil = HPList.Nil

instance AttachmentInfoListToMiddle cass =>
	AttachmentInfoListToMiddle (cas ': cass) where
	attachmentInfoListToMiddle (U8 ca :** cas) =
		U2 (attachmentInfoToMiddle ca) :**
		attachmentInfoListToMiddle cas

class AttachmentInfoMaybeToMiddle as where
	type MaybeI0'7_8 as :: Maybe (Maybe Type, Vk.ClearType)
	attachmentInfoMaybeToMiddle ::
		TPMaybe.M (U8 AttachmentInfo) as ->
		TPMaybe.M (U2 M.AttachmentInfo) (MaybeI0'7_8 as)

instance AttachmentInfoMaybeToMiddle 'Nothing where
	type MaybeI0'7_8 'Nothing = 'Nothing
	attachmentInfoMaybeToMiddle TPMaybe.N = TPMaybe.N

instance AttachmentInfoMaybeToMiddle ('Just as) where
	type MaybeI0'7_8 ('Just as) = 'Just (I0'7_8 as)
	attachmentInfoMaybeToMiddle (TPMaybe.J (U8 a)) =
		TPMaybe.J . U2 $ attachmentInfoToMiddle a

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
