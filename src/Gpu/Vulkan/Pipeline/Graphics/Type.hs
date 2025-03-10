{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Type (G(..)) where

import GHC.TypeNats
import Data.Kind
import Text.Show.ToolsYj

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as DscStLyt
import Gpu.Vulkan.Pipeline.Graphics.Middle qualified as M

import Gpu.Vulkan.VertexInput qualified as VertexInput

newtype G s (vibs :: [(Type, VertexInput.Rate)]) (vias :: [(Nat, Type)])
	(lyta :: (Type, [(Type, [DscStLyt.BindingType])], [Type])) = G M.G

instance ShowIO (G s vibs vias lyta) where
	showIO (G g) = do
		sg <- showIO g
		pure $ "(G " ++ sg ++ ")"
