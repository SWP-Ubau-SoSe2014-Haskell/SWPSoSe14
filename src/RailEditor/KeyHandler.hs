{- |
Module      :  KeyHandler.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The KeyHandler-module allows to react on keypress-events in the editor.
-}
module KeyHandler (
                   handleKey,   -- handles keypress-events
                   OutstandingAction(UpdatePosition,ExtendDrawingAreaH,ExtendDrawingAreaV)
                  )
  where

import Graphics.UI.Gtk
import Control.Monad
import TextAreaContent
import TextAreaContentUtils

data OutstandingAction =  UpdatePosition Position | ExtendDrawingAreaH Double | ExtendDrawingAreaV Double deriving (Show)
handleKey textAreaContent pos key hAdjustment vAdjustment = do
  cAKList <- captureArrowKeys pos key hAdjustment vAdjustment
  cNPKList <- captureNonPrintableKeys textAreaContent pos key hAdjustment
  return $ cAKList ++ cNPKList

captureArrowKeys :: Position -> String -> Adjustment -> Adjustment-> IO ([OutstandingAction])
captureArrowKeys (x,y) key hAdj vAdj= do
  case key of
    "Left" 	-> do
      newX  <- updateDirection x bef hAdj
      return $ [(UpdatePosition (newX,y))]
    "Right" -> do 
      return $ (ExtendDrawingAreaH x):(UpdatePosition (x+bef,y)):[]
    "Up" 	-> do 
      newY  <- updateDirection y hef vAdj
      return $ [(UpdatePosition (x,newY))]
    "Down" 	-> do
      return $ (ExtendDrawingAreaV y):(UpdatePosition (x,y+hef)):[]
    _ -> return []

--captureNonPrintableKeys :: TextArea -> String -> IO ([OutstandingAction])
captureNonPrintableKeys areaContent (x,y) key hAdj= do
  let outStActions = []
  case key of 
    "Return" ->  do
      hRList <- handleReturn areaContent (x,y)
      return $ hRList ++ [UpdatePosition (x,y+hef)]
    "BackSpace" -> do
      hBSList <- handleBackSpace areaContent (x,y) hAdj
      return $ hBSList
    _ -> return []

handleReturn :: TextAreaContent -> Position -> IO ([OutstandingAction])
handleReturn areaContent (x,y)  = do
  content <- captureReturn areaContent (x,y)
  let newY = if Prelude.null content then y else Prelude.foldl (\y1 ((_,y2),_) -> max y1 y2) 0.0 content
  return $ [ExtendDrawingAreaV newY]

handleBackSpace areaContent (x,y) hAdj
  | (x == 0) && (y >= hef) = do
    (map1,pos,map2) <- backSpaceLine y areaContent hAdj
    return $ [UpdatePosition pos]
  | (y >= 0) && (x >= bef) = do
    newX <- updateDirection x bef hAdj
    map1 <- shiftLeftLine areaContent (newX,y) hAdj
    return $ [(UpdatePosition (newX,y))]
  | otherwise = return $ []  --statt return Update

updateDirection :: Double -> Double -> Adjustment -> IO (Double)
updateDirection direction size adjustment = do
  if (direction >= size) then do
    value <- adjustmentGetValue adjustment
    when (direction - size < value) $ do adjustmentSetValue adjustment $ direction - size
    return $ direction - size
  else return direction
