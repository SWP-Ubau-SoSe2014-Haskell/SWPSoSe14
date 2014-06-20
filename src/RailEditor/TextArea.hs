{- |
Module      :  TextArea.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The TextArea-module depicts the view on the data structure stored in the TextAreaContent-module for the editor.
-}
module TextArea (
                 loadContent   -- loads content from the TeaxtAreaContent-data structure
                )
  where
    
    -- imports --
    import qualified TextAreaContent as TAC
    import qualified KeyHandler as KH

    -- functions --
    --loadContent :: ?
    loadContent = undefined   -- TODO: implement this functions
