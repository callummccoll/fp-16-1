This module is responsible for creating the RAM table and the registers.

Therefore this module only supplies one public function which is
createRamAndRegisters, this function simply creates the ram table and the
registers from the environment and puts them inside a Frame.

\begin{code}
module Ram (createRamAndRegisters) where

import Environment
import GTKHelpers
import Helpers
import Operators
import Presentation

import "gtk3" Graphics.UI.Gtk
import Data.Array
import Data.Array.MArray
import Data.IORef
\end{code}

A RegisterData contains:
    - The registers name
    - The value of the register
    - A Color that is used when highlighting the row that the register on.

\begin{code}
type RegisterData = (String, String, Color)
\end{code}

A RowData contains:
    - the row number
    - an optional label
    - the contents of the cell

\begin{code}
type RowData = (Int, Maybe String, String)
\end{code}

A Row contains:
    - The row number
    - A Frame containing the row number
    - A Frame containing the label
    - A Frame containing the content.

\begin{code}
type Row = (Int, Frame, Frame, Frame)
\end{code}

A RowFactory is simply a function that takes a RowData and creates a Row from
it.

\begin{code}
type RowFactory = RowData -> IO Row
\end{code}

Given the current Environment and whether we are running or not, build the RAM
table and the registers.  This function returns the resulting Frame containing
these widgets.

\begin{code}
createRamAndRegisters:: Environment -> Bool -> IO Frame
createRamAndRegisters env running = case eRAM env of
    Left ram  -> freeze ram >>= getRamFromArray env running
    Right ram -> getRamFromArray env running ram
\end{code}

Given the RAM that is within the array, create the RAM table and the registers,
and place them in a Frame.

\begin{code}
getRamFromArray :: Environment -> Bool -> Array Int Cell -> IO Frame
getRamFromArray env running ram = do
    pcColor <-
        if running then
            return (Color 65535 64250 55769)
        else
            return (Color 65535 65535 65535)
    spColor <- 
        if running then
            return (Color 55769 65535 64250)
        else
            return (Color 65535 65535 65535)
    registers <- extractRegisters env pcColor spColor
    let factory = createRowWithColouredRegister (ePC env, pcColor) (eSP env, spColor)
    createRam (extractRowData (elems ram) 0) factory registers
\end{code}

Extract the registers data from the given environment.

This functions takes the Environment and two Colors.  The first represents the
colour that the PC should highlight as, and the second represents the colour
that the SP should highlight as and returns a list of RegisterData.

The A register has a white highlight colour which means that the background of
the cell that corresponds to the value of the A register does not change.

\begin{code}
extractRegisters :: Environment -> Color -> Color -> IO [RegisterData]
extractRegisters env pcColor spColor = return [
        ("A", showCVal (eA env), Color 65535 65535 65535),
        ("SP", show (eSP env), spColor),
        ("PC", show (ePC env), pcColor)
    ]
\end{code}

Given a list of Cells and the starting row position, extract a RowData for each
cell.

\begin{code}
extractRowData :: [Cell] -> Int -> [RowData]
extractRowData cs pos = case cs of
    [] -> []
    c : cs' -> case (cLabel c, cVal c) of
        (l, Int c')  -> (pos, Just l, show c') : extractRowData cs' (pos + 1)
        (l, Inst c') -> (pos, Just l, showInstruction c') : extractRowData cs' (pos + 1)
        _            -> (pos, Nothing, "") : extractRowData cs' (pos + 1)
\end{code}

This function acts like a a RowFactory.

This function takes two tuples which contain:
    - An int representing the value of a register
    - A Color which is used to highlight the row at the value of the register.

Given the two tuples, this function then takes a RowData and returns the Row,
therefore it can be passed around as a RowFactory.

This function creates a Row from a RowData, but ensures that if the row needs
to be highlighted because it represents either the PC or SP then it will be.

\begin{code}
createRowWithColouredRegister :: (Int, Color) -> (Int, Color) -> RowData -> IO Row
createRowWithColouredRegister (pc, pcColor) (sp, spColor) (row, label, content)
    | row == pc = createRow (row, label, content) pcColor
    | row == sp = createRow (row, label, content) spColor
    | otherwise = createRow (row, label, content) (Color 65535 65535 65535)
\end{code}

Given a list of RowData, a RowFactory and a list of RegisterDatas, create the
RAM table and the registers and place them in a Frame.

\begin{code}
createRam :: [RowData] -> RowFactory -> [RegisterData] -> IO Frame
createRam rs rowFactory registers = do
    hbox  <- hBoxNew False 10
    createRamTable rs rowFactory >>|> hbox <<|<< createRegisters registers
    hbox >|>> pad (5,5,5,5) >>|>> createFrame "Ram and Registers"
\end{code}

Given the RowData and a RowFactory, generate the RAM table.

\begin{code}
createRamTable :: [RowData] -> RowFactory -> IO Table
createRamTable rs rowFactory = do
    table <- tableNew (length rs) 3 False
    let rows = rowFactory <$> rs
    attachCellsToTable table rows
    return table
\end{code}

Given a table and a list of Rows, attach each row to the table.

\begin{code}
attachCellsToTable :: Table -> [IO Row] -> IO ()
attachCellsToTable table rs = case rs of
    []      -> return ()
    r : rs' -> do
        (row, rowCell, labelFrame, contentFrame) <- r
        tableAttach table rowCell 0 1 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table labelFrame 1 2 row (row + 1) [Fill] [Fill] 0 0
        tableAttach table contentFrame 2 3 row (row + 1) [Fill] [Fill] 0 0
        attachCellsToTable table rs'
\end{code}

Create the cell that contains the rows number.

\begin{code}
createRowCell :: Int -> IO Frame
createRowCell row = do
    frame <- frameNew
    labelNew (Just (show row))
        >>|>> padWithAlignment (0, 0, 2, 2) (0.5, 0, 1, 1)
        >>|> frame
\end{code}

Create an entire row in the RAM table.

This functions also takes a background colour which is used to highlight the 
content cell.

\begin{code}
createRow :: RowData -> Color -> IO Row
createRow (row, label, content) color = do
    rowCell <- createRowCell row
    labelFrame <- frameNew
    widgetSetSizeRequest labelFrame 50 (-1)
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal color
    widgetSetSizeRequest eventBox 80 (-1)
    label <- labelNew label
        >>|>> padWithAlignment (5, 5, 5, 5) (1, 0, 1, 1)
        >>|> labelFrame
    content <- labelNew (Just content)
        >>|>> pad (5, 5, 5, 5)
        >>|> eventBox
        >>|>> frameNew
    return (row, rowCell, label, content)
\end{code}

Create registers from a list of RegisterData and add them to a VBox.

\begin{code}
createRegisters :: [RegisterData] -> IO VBox
createRegisters registers = do
    vbox  <- vBoxNew False 10
    addRegisters vbox (createRegister <$> registers)
\end{code}

Create a Register from a RegisterData.

\begin{code}
createRegister :: RegisterData -> IO HBox
createRegister (register, content, color) = do
    hbox     <- hBoxNew False 10
    frame    <- frameNew
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal color
    labelNew (Just content) >>|> eventBox >>|> frame
    labelNew (Just register) >>|> hbox
    frame >|> hbox
\end{code}

Add registers to a vbox.

\begin{code}
addRegisters :: VBox -> [IO HBox] -> IO VBox
addRegisters vbox registers = case registers of
    []     -> return vbox
    r : rs -> do
        register <- r
        boxPackStart vbox register PackNatural 0
        addRegisters vbox rs
\end{code}
