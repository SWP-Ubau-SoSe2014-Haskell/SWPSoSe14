{- |
Module      :  TextAreaContent.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

This TextAreaContent-module stores two data structures that saves all entries for the editor.
The first data structur saves the text-entries, the second data structure saves the color-entries.
-}
module TextAreaContent (
                        init,   -- initializes both data structures
                        serialize,   -- serializes both data structures
                        deserialize,   -- deserializes both data structures
                        putValue,   -- sets the text-entry for a certian point in the data structure
                        putColor,   -- sets the color for a certian point in the data structure
                        getCell   -- reads the data form both datastructures (text and color)
                       )
  where
    
    -- imports --
    -- no imports yet

    -- functions --

    -- initializes both data structures
    --init :: ?  
    init = undefined   -- TODO: implement this functions

    -- serializes both data structures
    --serialize :: ?  
    serialize = undefined   -- TODO: implement this functions

    -- deserializes both data structures
    --deserialize :: ?  
    deserialize = undefined   -- TODO: implement this functions

    -- doubles the size of both used data strutures
    --resize :: ?  
    resize = undefined   -- TODO: implement this functions

    -- sets the text-entry for a certian point in the data structure
    --putValue :: ?  
    putValue = undefined   -- TODO: implement this functions

    -- gets the text-entry for a certian point in the data structure
    --getValue :: ?  
    getValue = undefined   -- TODO: implement this functions

    -- gets the color for a certian point in the data structure
    --putColor :: ?  
    putColor = undefined   -- TODO: implement this functions

    -- sets the color for a certian point in the data structure
    --getColor :: ?  
    getColor = undefined   -- TODO: implement this functions

    -- reads the data form both data structures (text & color)
    --getCell :: ?  
    getCell = undefined   -- TODO: implement this functions
