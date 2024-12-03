{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.ImageBuffer (

	-- * IMAGE BUFFER

	ImageBuffer(..), ImageBufferBinded(..), ImageBufferArg(..),

	-- * GET REQUIREMENTS LIST

	getRequirementsList, getRequirementsListBinded,

	-- * ADJUST OFFSET AND GET SIZE

	adjustOffsetSize, adjustOffsetSizeBinded,

	-- * FOR BIND

	Alignments(..),

	-- * FOR READ AND WRITE

	ObjectLength(..),

	-- * OTHERS

	AlgnSize, adjust,

	-- * RAW OFFSET

	RawOffset(..), RawOffsetToOffset(..)

	) where

import Prelude hiding (map, read)
import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Buffer.Type as Buffer

import qualified Gpu.Vulkan.TypeEnum as T

import qualified Gpu.Vulkan.Device as Device
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M

import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

import qualified Data.HeteroParList as HPList

-- IMAGE BUFFER

data ImageBuffer s (ibarg :: ImageBufferArg) where
	Image :: Image.I si nm fmt -> ImageBuffer si ('ImageArg nm fmt)
	Buffer :: Buffer.B sb nm objs -> ImageBuffer sb ('BufferArg nm objs)
	Raw :: Device.M.Size -> Device.M.Size -> ImageBuffer s RawArg

deriving instance Show (HeteroParList.PL VObj.Length objs) =>
	Show (ImageBuffer sib ('BufferArg nm objs))

data ImageBufferBinded sm sib (ibarg :: ImageBufferArg) where
	ImageBinded :: Image.Binded sm si nm fmt ->
		ImageBufferBinded sm si ('ImageArg nm fmt)
	BufferBinded :: Buffer.Binded sm sb nm objs ->
		ImageBufferBinded sm sb ('BufferArg nm objs)
	RawBinded :: Device.M.Size -> Device.M.Size ->
		ImageBufferBinded sm sb RawArg

deriving instance Show (HeteroParList.PL VObj.Length objs) =>
	Show (ImageBufferBinded sm sib ('BufferArg nm objs))

data ImageBufferArg =
	ImageArg Symbol T.Format | BufferArg Symbol [VObj.O] | RawArg

-- GET REQUIREMENTS LIST

type AlgnSize = (Device.M.Size, Device.M.Size)

getRequirementsList ::
	Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
	IO [Either AlgnSize Memory.M.Requirements]
getRequirementsList dv =
	HeteroParList.toListM \(U2 bi) -> getMemoryRequirements dv bi

getRequirementsListBinded ::
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	IO [Either AlgnSize Memory.M.Requirements]
getRequirementsListBinded dv =
	HeteroParList.toListM \(U2 bi) -> getMemoryRequirementsBinded dv bi

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO (Either AlgnSize Memory.M.Requirements)
getMemoryRequirements (Device.D dv) = \case
	Buffer (Buffer.B _ b) -> Right <$> Buffer.M.getMemoryRequirements dv b
	Image (Image.I i) -> Right <$> Image.M.getMemoryRequirements dv i
	Raw algn sz -> pure $ Left (algn, sz)

getMemoryRequirementsBinded ::
	Device.D sd -> ImageBufferBinded sm sib fos -> IO (Either AlgnSize Memory.M.Requirements)
getMemoryRequirementsBinded (Device.D dv) = \case
	BufferBinded (Buffer.Binded _ b) -> Right <$> Buffer.M.getMemoryRequirements dv b
	ImageBinded (Image.Binded i) -> Right <$> Image.M.getMemoryRequirements dv i
	RawBinded algn sz -> pure $ Left (algn, sz)

-- ADJUST OFFSET AND GET SIZE

adjustOffsetSize :: Device.D sd -> ImageBuffer sib ibarg -> Device.M.Size ->
	IO (Device.M.Size, Device.M.Size)
adjustOffsetSize dv ib ost = (<$> getMemoryRequirements dv ib) \case
	Right rs -> (
		adjust (Memory.M.requirementsAlignment rs) ost,
		Memory.M.requirementsSize rs )
	Left (algn, sz) -> (adjust algn ost, sz)

adjustOffsetSizeBinded :: Device.D sd -> ImageBufferBinded sm sib ibarg ->
	Device.M.Size -> IO (Device.M.Size, Device.M.Size)
adjustOffsetSizeBinded dv ib ost = (<$> getMemoryRequirementsBinded dv ib) \case
	Right rs -> (
		adjust (Memory.M.requirementsAlignment rs) ost,
		Memory.M.requirementsSize rs )
	Left (algn, sz) -> (adjust algn ost, sz)

adjust :: Device.M.Size -> Device.M.Size -> Device.M.Size
adjust algn ost = ((ost - 1) `div` algn + 1) * algn

-- ALIGNMENTS

class Alignments (ibs :: [(Type, ImageBufferArg)]) where
	alignments :: [Maybe Device.M.Size]

instance Alignments '[] where alignments = []

instance Alignments ibs => Alignments ('(_s, 'ImageArg _nm _fmt) ': ibs) where
	alignments = Nothing : alignments @ibs

instance (VObj.WholeAlign objs, Alignments ibs) =>
	Alignments ('(_s, 'BufferArg _nm objs) ': ibs) where
	alignments = Just (VObj.wholeAlign @objs) : alignments @ibs

instance Alignments ibs => Alignments ('(_s, 'RawArg) ': ibs) where
	alignments = Nothing : alignments @ibs

-- OBJECT LENGTH

class ObjectLength (nm :: Symbol) (obj :: VObj.O) ibargs where
	objectLength' :: HeteroParList.PL (U2 ImageBuffer) ibargs ->
		VObj.Length obj

instance VObj.LengthOf obj objs =>
	ObjectLength nm obj ('(sib, 'BufferArg nm objs) ': ibargs) where
	objectLength' (U2 (Buffer (Buffer.B lns _)) :** _) =
		VObj.lengthOf @obj lns

instance {-# OVERLAPPABLE #-} ObjectLength nm obj ibargs =>
	ObjectLength nm obj (ibarg ': ibargs) where
	objectLength' (_ :** lns) = objectLength' @nm @obj lns

-- RAW OFFSET

class RawOffsetToOffset ibargs (n :: Nat) where
	rawOffsetToOffset :: Device.Size -> Device.D sd ->
		HPList.PL (U2 ImageBuffer) ibargs -> RawOffset n ->
		IO Device.Size

instance RawOffsetToOffset ('(sr, RawArg) ': ibargs) 0 where
	rawOffsetToOffset ost0 _ _ (RawOffset o) = pure $ ost0 + o

instance {-# OVERLAPPABLE #-} RawOffsetToOffset ibargs (n - 1) =>
	RawOffsetToOffset ('(sr, RawArg) ': ibargs) n where
	rawOffsetToOffset ost0 dv (U2 ib :** ibs) (RawOffset o) = do
		(ost', sz) <- adjustOffsetSize dv ib ost0
		rawOffsetToOffset (ost' + sz) dv ibs (RawOffset @(n - 1) o)

instance {-# OVERLAPPABLE #-} RawOffsetToOffset ibargs n =>
	RawOffsetToOffset (ibarg ': ibargs) n where
	rawOffsetToOffset ost0 dv (U2 ib :** ibs) ro = do
		(ost', sz) <- adjustOffsetSize dv ib ost0
		rawOffsetToOffset (ost' + sz) dv ibs ro

newtype RawOffset (n :: Nat) = RawOffset Device.Size deriving (Show, Num)
