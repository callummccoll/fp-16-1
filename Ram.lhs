The \highlighttt{Ram} module is responsible for creating the RAM table and the registers.  Therefore this module only supplies one public function which is \highlighttt{createRamAndRegisters}, this function simply creates the ram table and the registers from the \highlighttt{Environment} and puts them inside a \highlighttt{Frame}.

\begin{code}
module Ram (createRamAndRegisters) where

import Containers
import Environment
import Helpers
import Operators
import Presentation

import "gtk3" Graphics.UI.Gtk
import Data.Array
import Data.Array.MArray
import Data.IORef
\end{code}

\noindent \textbf{Type Synonyms\newline}

\noindent A \highlighttt{RegisterData} contains:
\begin{enumerate}
\item The registers name
\item The value of the register
\item The \highlighttt{Color} that is used when highlighting the row that the registers value is pointing to.
\end{enumerate}

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

\noindent A \highlighttt{Row} contains:
\begin{enumerate}
\item The row number
\item A \highlighttt{Frame} containing the row number
\item A \highlighttt{Frame} containing the label
\item A \highlighttt{Frame} containing the content
\end{enumerate}

\begin{code}
type Row = (Int, Frame, Frame, Frame)
\end{code}

\noindent A \highlighttt{RowFactory} is a function that takes a \highlighttt{RowData} and creates a \highlighttt{Row}.

\begin{code}
type RowFactory = RowData -> IO Row
\end{code}

\noindent \textbf{Functions\newline}

\noindent \highlighttt{createRamAndRegisters env running} builds the RAM table and the registers.  This function returns the resulting \highlighttt{Frame} containing these widgets.

\begin{code}
createRamAndRegisters:: Environment -> Bool -> IO Frame
createRamAndRegisters env running = case eRAM env of
    Left ram  -> freeze ram >>= getRamFromArray env running
    Right ram -> getRamFromArray env running ram
\end{code}

\noindent \highlighttt{getRamFromArray env running ram} creates the ram and registers from the \highlighttt{ram} \highlighttt{Array}.  This function is also responsible for deciding what the highlight colours for the registers are, and uses them to create a \highlighttt{RowFactory}, before calling \highlighttt{createRam}.

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

\noindent \highlighttt{extractRegiseters env pcColor spColor} extracts the \highlighttt{RegisterData} from the given environment for the SP, PC and A registers. The A register has a white highlight colour which means that the background of the cell that corresponds to the value of the A register does not change.

\begin{code}
extractRegisters :: Environment -> Color -> Color -> IO [RegisterData]
extractRegisters env pcColor spColor = return [
        ("A", showCVal (eA env), Color 65535 65535 65535),
        ("SP", show (eSP env), spColor),
        ("PC", show (ePC env), pcColor)
    ]
\end{code}

\noindent \highlighttt{extractRowData cs pos} extracts the \highlighttt{RowData} from a list of \highlighttt{Cells}, starting at a row position.

\begin{code}
extractRowData :: [Cell] -> Int -> [RowData]
extractRowData cs pos = case cs of
    [] -> []
    c : cs' -> case (cLabel c, cVal c) of
        (l, Int c')  -> (pos, Just l, show c') : extractRowData cs' (pos + 1)
        (l, Inst c') -> (pos, Just l, showInstruction c') : extractRowData cs' (pos + 1)
        _            -> (pos, Nothing, "") : extractRowData cs' (pos + 1)
\end{code}

\noindent \highlighttt{createRowWithColouredRegister (pc, pcColor) (sp, spColor)} acts like a \highlighttt{RowFactory}.  This is because the function takes two tuples which contain:

\begin{enumerate}
\item An int representing the value of a register
\item A \highlighttt{Color} which is used to highlight the row at the value of the register.
\end{enumerate}

\noindent Given the two tuples, this function then takes a \highlighttt{RowData} and returns the \highlighttt{Row}, therefore it can be passed around as a RowFactory.  This function creates a Row from a RowData, but ensures that if the row needs to be highlighted because it represents either the PC or SP then it will be.

\begin{code}
createRowWithColouredRegister
    :: (Int, Color)
    -> (Int, Color)
    -> RowData
    -> IO Row
createRowWithColouredRegister
    (pc, pcColor)
    (sp, spColor)
    (row, label, content)
        | row == pc = createRow (row, label, content) pcColor
        | row == sp = createRow (row, label, content) spColor
        | otherwise = createRow (row, label, content) (Color 65535 65535 65535)
\end{code}

\noindent \highlighttt{createRam rs rowFactory registers} creates the RAM table and the registers and places them in a \highlighttt{Frame}.

\begin{code}
createRam :: [RowData] -> RowFactory -> [RegisterData] -> IO Frame
createRam rs rowFactory registers = do
    hbox  <- hBoxNew False 10
    createRamTable rs rowFactory >>|> hbox <<|<< createRegisters registers
    hbox >|>> pad (5,5,5,5) >>|>> createFrame "Ram and Registers"
\end{code}

\noindent \highlighttt{createRamTable rs rowFactory} generates the RAM table from a list of \highlighttt{RowData} and a \highlighttt{RowFactory}.

\begin{code}
createRamTable :: [RowData] -> RowFactory -> IO Table
createRamTable rs rowFactory = do
    table <- tableNew (length rs) 3 False
    let rows = rowFactory <$> rs
    attachCellsToTable table rows
    return table
\end{code}

\noindent \highlighttt{attachCellsToTable table rs} attaches each row in a list of \highlighttt{Rows} to a \highlighttt{Table}.

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

\noindent \highlighttt{createRowCell row} creates a \highlighttt{Frame} containing the row number.

\begin{code}
createRowCell :: Int -> IO Frame
createRowCell row = labelNew (Just (show row))
    >>|>> padWithAlignment (0, 0, 2, 2) (0.5, 0, 1, 1)
    >>|>> frameNew
\end{code}

\noindent \highlighttt{createRow (row, label, content) color} creates an entire row in the RAM table.  This function changes the background colour of the content cell to the \highlighttt{color}.

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

\noindent \highlighttt{createRegisters registers} creates registers from a list of \highlighttt{RegisterData} and adds them to a \highlighttt{VBox}.

\begin{code}
createRegisters :: [RegisterData] -> IO VBox
createRegisters registers = do
    vbox  <- vBoxNew False 10
    addRegisters vbox (createRegister <$> registers)
\end{code}

\noindent \highlighttt{createRegister (register, content, color)} creates a \highlighttt{Register} from a \highlighttt{RegisterData}.

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

\noindent \highlighttt{addRegisters vbox registers} adds a list of registers to a \highlighttt{VBox}.

\begin{code}
addRegisters :: VBox -> [IO HBox] -> IO VBox
addRegisters vbox registers = case registers of
    []     -> return vbox
    r : rs -> do
        register <- r
        boxPackStart vbox register PackNatural 0
        addRegisters vbox rs
\end{code}
