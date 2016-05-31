module Ram (getRamFromEnvironment) where

import Environment
import Helpers
import Presentation

import "gtk3" Graphics.UI.Gtk
import Data.Array
import Data.Array.MArray
import Data.IORef

getRamFromEnvironment :: Environment -> IO Frame
getRamFromEnvironment env = do
    case (eRAM env) of
        Left ram  -> (freeze ram) >>= (getRamFromArray env)
        Right ram -> getRamFromArray env ram

getRamFromArray :: Environment -> (Array Int Cell) -> IO Frame
getRamFromArray env ram = (extractRegisters env) >>= (createRam (extractCell <$> ram))

extractRegisters :: Environment -> IO [(String, String)]
extractRegisters env = do
    return [("A", showCVal (eA env)), ("SP", show (eSP env)), ("PC", show (ePC env)) ]

extractCell :: Cell -> (Maybe String, String)
extractCell c = case (cLabel c, cVal c) of
    (l, Int c')  -> (Just l, (show c'))
    (l, Inst c') -> (Just l, (showInstruction c'))
    _            -> (Nothing, "")

createRam :: Array Int (Maybe String, String) -> [(String, String)] -> IO Frame
createRam cs registers = do
    frame <- createFrame $ Just "Ram and Registers"
    hbox  <- hBoxNew False 10
    (createRamTable cs) >>= (containerAdd hbox)
    (createRegisters registers) >>= (containerAdd hbox)
    containerAdd frame hbox 
    return frame

createRamTable :: Array Int (Maybe String, String) -> IO Table
createRamTable cs = do
    table <- (tableNew (length cs) 3 False)
    attachCellsToTable table (createRow <$> cs) 0


attachCellsToTable :: Table -> Array Int (IO (Frame, Frame)) -> Int -> IO Table
attachCellsToTable table cells row
    | row >= (length cells) = return table
    | otherwise             = do
        (label, content) <- cells ! row
        rowFrame <- createFrame Nothing
        (labelNew (Just (show row))) >>= (containerAdd rowFrame)
        tableAttach table rowFrame 0 1 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table label 1 2 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table content 2 3 row (row + 1) [Fill] [Fill] 0 0
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
    frame    <- createFrame Nothing
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    (labelNew (Just content)) >>= (containerAdd eventBox)
    containerAdd frame eventBox
    (labelNew (Just register)) >>= (containerAdd hbox)
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
