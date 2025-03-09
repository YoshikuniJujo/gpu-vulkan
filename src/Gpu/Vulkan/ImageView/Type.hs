{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Type (I(..)) where

import GHC.TypeLits
import Text.Show.ToolsYj

import Gpu.Vulkan.TypeEnum as T
import Gpu.Vulkan.ImageView.Middle qualified as M

newtype I (nm :: Symbol) (fmt :: T.Format) si = I M.I deriving Show

instance ShowIO (I nm fmt si) where
	showIO (I i) = do
		si <- showIO i
		pure $ "(I " ++ si ++ ")"
