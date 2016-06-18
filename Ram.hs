module Ram (createRamAndRegisters) where

import Environment
import Helpers
import Operators
import Presentation

import "gtk3" Graphics.UI.Gtk
import Data.Array
import Data.Array.MArray
import Data.IORef

type RowData = (Int, Maybe String, String)
type Row = (Int, Frame, Frame, Frame)
type RowFactory = RowData -> IO Row

createRamAndRegisters:: Environment -> Bool -> IO Frame
createRamAndRegisters env running = do
    case (eRAM env) of
        Left ram  -> (freeze ram) >>= (getRamFromArray env running)
        Right ram -> getRamFromArray env running ram

getRamFromArray :: Environment -> Bool -> (Array Int Cell) -> IO Frame
getRamFromArray env running ram = do
    pcColor <- case running of
        True -> return (Color 65535 64250 55769)
        False -> return (Color 65535 65535 65535)
    spColor <- case running of
        True -> return (Color 55769 65535 64250)
        False -> return (Color 65535 65535 65535)
    registers <- extractRegisters env pcColor spColor
    factory <- return (createRowWithColouredRegister
            (ePC env, pcColor)
            (eSP env, spColor)
        )
    createRam (extractRowData (elems ram) 0) factory registers

extractRegisters :: Environment -> Color -> Color -> IO [(String, String, Color)]
extractRegisters env pcColor spColor = do
    return [
            ("A", showCVal (eA env), Color 65535 65535 65535),
            ("SP", show (eSP env), spColor),
            ("PC", show (ePC env), pcColor)
        ]

extractRowData :: [Cell] -> Int -> [RowData]
extractRowData cs pos = case cs of
    [] -> []
    c : cs' -> case (cLabel c, cVal c) of
        (l, Int c')  -> (pos, Just l, (show c')) : extractRowData cs' (pos + 1)
        (l, Inst c') -> (pos, Just l, (showInstruction c')) : extractRowData cs' (pos + 1)
        _            -> (pos, Nothing, "") : extractRowData cs' (pos + 1)

createRowWithColouredRegister :: (Int, Color) -> (Int, Color) -> RowData -> IO Row
createRowWithColouredRegister (pc, pcColor) (sp, spColor) (row, label, content)
    | row == pc = createRow (row, label, content) pcColor
    | row == sp = createRow (row, label, content) spColor
    | otherwise = createRow (row, label, content) (Color 65535 65535 65535)

createRam :: [RowData] -> RowFactory -> [(String, String, Color)] -> IO Frame
createRam rs rowFactory registers = do
    hbox  <- hBoxNew False 10
    (createRamTable rs rowFactory) >>|> hbox <<|<< (createRegisters registers)
    hbox >|>> (pad (5,5,5,5)) >>|>> (createFrame "Ram and Registers")

createRamTable :: [RowData] -> RowFactory -> IO Table
createRamTable rs rowFactory = do
    table <- (tableNew (length rs) 3 False)
    rows <- return (rowFactory <$> rs)
    attachCellsToTable table rows
    return table

attachCellsToTable :: Table -> [IO Row] -> IO ()
attachCellsToTable table rs = case rs of
    []      -> return ()
    r : rs' -> do
        (row, rowCell, labelFrame, contentFrame) <- r
        tableAttach table rowCell 0 1 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table labelFrame 1 2 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table contentFrame 2 3 row (row + 1) [Fill] [Fill] 0 0
        attachCellsToTable table rs'

createRowCell :: Int -> IO Frame
createRowCell row = do
    frame <- frameNew
    (labelNew (Just (show row)))
        >>|>> (padWithAlignment (0, 0, 2, 2) (0.5, 0, 1, 1))
        >>|> frame

createRow :: RowData -> Color -> IO Row
createRow (row, label, content) color = do
    rowCell <- createRowCell row
    labelFrame <- frameNew
    widgetSetSizeRequest labelFrame 50 (-1)
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal color
    widgetSetSizeRequest eventBox 80 (-1)
    label <- (labelNew (label))
        >>|>> (padWithAlignment (5, 5, 5, 5) (1, 0, 1, 1))
        >>|> labelFrame
    content <- (labelNew (Just content))
        >>|>> (pad (5, 5, 5, 5))
        >>|> eventBox
        >>|>> (frameNew)
    return (row, rowCell, label, content)

createRegisters :: [(String, String, Color)] -> IO VBox
createRegisters registers = do
    vbox  <- vBoxNew False 10
    addRegisters vbox (createRegister <$> registers)


createRegister :: (String, String, Color) -> IO HBox
createRegister (register, content, color) = do
    hbox     <- hBoxNew False 10
    frame    <- frameNew
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal color
    (labelNew (Just content)) >>|> eventBox >>|> frame
    (labelNew (Just register)) >>|> hbox
    frame >|> hbox

addRegisters :: VBox -> [IO HBox] -> IO VBox
addRegisters vbox registers = case registers of
    []     -> return vbox
    r : rs -> do
        register <- r
        boxPackStart vbox register PackNatural 0
        addRegisters vbox rs
