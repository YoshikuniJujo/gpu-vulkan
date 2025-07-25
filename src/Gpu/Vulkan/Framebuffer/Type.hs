{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Type where

import Text.Show.ToolsYj

import qualified Gpu.Vulkan.Framebuffer.Middle as M

newtype F s = F M.F

instance ShowIO (F s) where
	showIO (F f) = do
		sf <- showIO f
		pure $ "( F " ++ sf ++ ")"
