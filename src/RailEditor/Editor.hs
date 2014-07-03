{- |
Module      :  Editor.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

This is the main-module (and entrypoint) for the Editor.
-}
module Main (
               main -- main-function calling the editor
                    -- to be completed with missing required functions
              )
  where
    
    -- imports --
    import qualified MainWindow as MW
    
    -- functions --
    
    -- main-function calling the editor
    main :: IO()
    main = MW.createMainWindow -- TODO: implement this functions
