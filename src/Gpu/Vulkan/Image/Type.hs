{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Type where

import GHC.TypeLits
import Text.Show.ToolsYj

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Image.Middle as M

newtype I s (nm :: Symbol) (fmt :: T.Format) = I M.I

instance ShowIO (I s nm fmt) where
	showIO (I i) = do
		si <- showIO i
		pure $ "(I " ++ show si ++ ")"

newtype Binded sm si (nm :: Symbol) (fmt :: T.Format) = Binded M.I

instance ShowIO (Binded sm si nm fmt) where
	showIO (Binded i) = do
		si <- showIO i
		pure $ "(Binded " ++ show si ++ ")"

unsafeToBinded :: I si nm fmt -> Binded sm si hm fmt
unsafeToBinded (I i) = (Binded i)
