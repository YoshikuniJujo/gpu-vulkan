{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer (

	-- * CREATE

	create, B, CreateInfo(..),

	-- ** Buffer Group

	Group, group, create', unsafeDestroy, lookup,

	-- * BINDED

	getMemoryRequirements, Binded, lengthBinded, IndexedForList(..),

	-- * COPY

	MakeCopies, ImageCopy(..), ImageCopyListToMiddle,

	-- * MEMORY BARRIER

	MemoryBarrier(..), MemoryBarrierListToMiddle,
	MemoryBarrier2(..), MemoryBarrier2ListToMiddle,

	-- * UNSAFE

	unsafeToBinded,

	-- * ENUM

	module Gpu.Vulkan.Buffer.Enum
	
	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.Buffer.Internal
import Gpu.Vulkan.Buffer.Enum
