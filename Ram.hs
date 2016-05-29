module Ram (createRam) where

import Environment
import Helpers

import "gtk3" Graphics.UI.Gtk
import Data.Array
import Data.IORef

createRam :: Array Int (Maybe String, String) -> [(String, String)] -> IO Frame
createRam cs registers = do
    frame <- createFrame $ Just "Ram and Registers"
    hbox  <- hBoxNew False 10
    registers <- createRegisters registers
    table <- createRamTable cs
    containerAdd hbox table
    --set hbox [boxChildPacking table := PackRepel]
    containerAdd hbox registers
    --set hbox [boxChildPacking registers := PackRepel]
    containerAdd frame hbox 
    return frame

createRamTable :: Array Int (Maybe String, String) -> IO Table
createRamTable cs = do
    table <- (tableNew (length cs) 2 True)
    attachCellsToTable table (createRow <$> cs) 0


attachCellsToTable :: Table -> Array Int (IO (Frame, Frame)) -> Int -> IO Table
attachCellsToTable table cells row
    | row >= (length cells) = return table
    | otherwise             = do
        (label, content) <- cells ! row
        tableAttach table label 0 1 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table content 1 2 row (row + 1) [Fill] [Fill] 0 0
        attachCellsToTable table cells (row + 1)

createRow :: (Maybe String, String) -> IO (Frame, Frame)
createRow (label, content) = do
    labelFrame <- createFrame Nothing
    contentFrame <- createFrame Nothing
    labelAlignment <- alignmentNew 0 0 1 1
    contentAlignment <- alignmentNew 0 0 1 1
    alignmentSetPadding labelAlignment 5 5 5 5
    alignmentSetPadding contentAlignment 5 5 5 5
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    label' <- labelNew (label)
    cell <- labelNew (Just content)
    containerAdd labelAlignment label'
    containerAdd labelFrame labelAlignment
    containerAdd contentAlignment cell 
    containerAdd eventBox contentAlignment
    containerAdd contentFrame eventBox
    return (labelFrame, contentFrame)

createRegisters :: [(String, String)] -> IO VBox
createRegisters registers = do
    vbox  <- vBoxNew True 10
    addRegisters vbox (createRegister <$> registers)


createRegister :: (String, String) -> IO HBox
createRegister (register, content) = do
    hbox     <- hBoxNew False 10
    label    <- labelNew (Just register)
    frame    <- createFrame Nothing
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    value    <- labelNew (Just content)
    containerAdd eventBox value
    containerAdd frame eventBox
    containerAdd hbox label
    containerAdd hbox frame
    return hbox

addRegisters :: VBox -> [IO HBox] -> IO VBox
addRegisters vbox registers = case registers of
    []     -> return vbox
    r : rs -> do
        register <- r
        containerAdd vbox register
        set vbox [boxChildPacking register := PackRepel]
        addRegisters vbox rs
