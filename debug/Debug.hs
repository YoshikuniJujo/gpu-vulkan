{-# LANGUAGE TemplateHaskell #-}

module Debug where

import Language.Haskell.TH

runIO $ putStrLn "gpu-vulkan Debug: ************* DEBUG DEBUG DEBUG **************" >> pure []

debug = True
