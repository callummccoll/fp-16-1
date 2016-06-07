module Ram (createRamAndRegisters) where

import Environment
import Helpers
import Presentation

import "gtk3" Graphics.UI.Gtk
import Data.Array
import Data.Array.MArray
import Data.IORef

createRamAndRegisters:: Environment -> IO Frame
createRamAndRegisters env = do
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
    hbox  <- hBoxNew False 10
    (createRamTable cs) >>|> hbox <<|<< (createRegisters registers)
    hbox >|>> (createFrame "Ram and Registers")

createRamTable :: Array Int (Maybe String, String) -> IO Table
createRamTable cs = do
    table <- (tableNew (length cs) 3 False)
    attachCellsToTable table (createRow <$> cs) 0


attachCellsToTable :: Table -> Array Int (IO (Frame, Frame)) -> Int -> IO Table
attachCellsToTable table cells row
    | row >= (length cells) = return table
    | otherwise             = do
        (label, content) <- cells ! row
        rowCell <- createRowCell row
        tableAttach table rowCell 0 1 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table label 1 2 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table content 2 3 row (row + 1) [Fill] [Fill] 0 0
        attachCellsToTable table cells (row + 1)

createRowCell :: Int -> IO Frame
createRowCell row = (labelNew (Just (show row)))
    >>|>> (padWithAlignment (0, 0, 2, 2) (0.5, 0, 1, 1))
    >>|>> (frameNew)

createRow :: (Maybe String, String) -> IO (Frame, Frame)
createRow (label, content) = do
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    label <- (labelNew (label))
        >>|>> (padWithAlignment (5, 5, 5, 5) (1, 0, 1, 1))
        >>|>> (frameNew)
    content <- (labelNew (Just content))
        >>|>> (pad (5, 5, 5, 5))
        >>|> eventBox
        >>|>> (frameNew)
    return (label, content)

createRegisters :: [(String, String)] -> IO VBox
createRegisters registers = do
    vbox  <- vBoxNew True 10
    addRegisters vbox (createRegister <$> registers)


createRegister :: (String, String) -> IO HBox
createRegister (register, content) = do
    hbox     <- hBoxNew False 10
    frame    <- frameNew
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    (labelNew (Just content)) >>|> eventBox >>|> frame
    (labelNew (Just register)) >>|> hbox
    frame >|> hbox
    return hbox

addRegisters :: VBox -> [IO HBox] -> IO VBox
addRegisters vbox registers = case registers of
    []     -> return vbox
    r : rs -> do
        register <- r
        register >|> vbox
        vbox >:= [boxChildPacking register := PackRepel]
        addRegisters vbox rs
