module Main (main) where

import Graphics.UI.Gtk

import Control.Applicative
import Control.Monad.Trans
import Data.IORef

type Point2d = (Double, Double)
type Time = Double
type Behavior a = Time -> a

main :: IO ()
main = do
    initGUI
    -- A counter which is incremented when the button is pressed.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 900 600
    -- Draw the Window
    redraw window counter
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ do
        liftIO $ mainQuit
    mainGUI

changeWithLimits :: (Integral a) => a -> a -> (a -> a) -> a -> a
changeWithLimits min max f x
    | x' >= max = max
    | x' <= min = min
    | otherwise = x'
        where x' = f x

redraw :: Window -> IORef Int -> IO ()
redraw window num = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num
    widgetShowAll window

createDrawing :: Window -> IORef Int -> IO ()
createDrawing window x = do
    hbox   <- hBoxNew True 10
    c      <- createFrame $ Just "C"
    ass    <- createFrame $ Just "Assembly"
    ram    <- createRAM [(Just "l1", "c1"), (Just "l2", "c2")] [("PC", "2"), ("SP", "5")]
    vbox   <- vBoxNew True 10
    stdin  <- createTextAreaFrame (Just "Stdin") Nothing False
    stdout <- createTextAreaFrame (Just "Stdout") Nothing False
    containerAdd hbox c
    containerAdd hbox ass
    containerAdd hbox ram
    containerAdd vbox stdin
    containerAdd vbox stdout
    containerAdd hbox vbox
    containerAdd window hbox
    return ()

createRAM :: [(Maybe String, String)] -> [(String, String)] -> IO Frame
createRAM cs registers = do
    frame <- createFrame $ Just "Ram and Registers"
    hbox  <- hBoxNew False 10
    registers <- createRegisters registers
    table <- createRamTable cs
    containerAdd hbox table
    containerAdd hbox registers
    containerAdd frame hbox 
    return frame

createRamTable :: [(Maybe String, String)] -> IO Table
createRamTable cs = do
    table <- (tableNew (length cs) 1 True)
    attachCellsToTable table (createRAMCell <$> cs) 0


attachCellsToTable :: Table -> [IO HBox] -> Int -> IO Table
attachCellsToTable table cells row = case cells of
    []     -> return table
    c : cs -> do
        hBox <- c
        tableAttachDefaults table hBox 0 1 row (row + 1)
        attachCellsToTable table cs (row + 1)

createRAMCell :: (Maybe String, String) -> IO HBox
createRAMCell (label, content) = do
    hbox <- hBoxNew False 10
    frame <- createFrame Nothing
    set frame [ widgetHeightRequest := 5 ]
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    cell <- labelNew (Just content)
    label <- labelNew label
    containerAdd hbox label
    containerAdd eventBox cell
    containerAdd frame eventBox
    containerAdd hbox frame
    return hbox

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


createTextAreaFrame :: Maybe String -> Maybe String -> Bool -> IO Frame
createTextAreaFrame title content editable = do
    frame <- createFrame title
    area <- createTextArea content editable
    containerAdd frame area
    return frame

createTextArea :: Maybe String -> Bool -> IO TextView
createTextArea content editable = case content of
    Nothing -> createEmptyTextArea editable
    Just s  -> do
        buffer <- textBufferNew Nothing
        textBufferSetText buffer s
        area <- textViewNewWithBuffer buffer
        set area [textViewEditable := editable]
        return area

createEmptyTextArea :: Bool -> IO TextView
createEmptyTextArea editable = do
    area <- textViewNew
    set area [textViewEditable := editable]
    return area

createFrame :: Maybe String -> IO Frame
createFrame s = case s of
    Nothing -> do 
        frame <- frameNew
        return frame
    Just s' -> do
        frame <- frameNew
        frameSetLabel frame s'
        frameSetLabelAlign frame 0.5 0.5
        return frame

createButton :: Window -> IORef Int -> (Int -> Int) -> IO Button
createButton window counter f = do
    button <- (readIORef counter) >>= (\num -> buttonNewWithLabel ("test " ++ (show num)))
    -- Increment the counter when the button is pressed.
    button `on` buttonActivated $ do
        modifyIORef' counter f
        redraw window counter
    return button
