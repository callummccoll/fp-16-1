module Main (main) where

import Emulation
import Environment
import Helpers
import Ram

import "gtk3" Graphics.UI.Gtk

import Control.Applicative
import Control.Monad.Trans
import Data.IORef

type Point2d = (Double, Double)
type Time = Double
type Behavior a = Time -> a

main :: IO ()
main = do
    mainLoop
    env <- environmentFromFile "test.ass"
    initGUI
    -- A counter which is incremented when the button is pressed.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 1200 800
    -- Draw the Window.
    redraw window counter
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ do
        liftIO $ mainQuit
    -- Start the application.
    mainGUI

environmentFromFile :: String -> IO Environment
environmentFromFile filename = (readFile filename) >>= makeEnvFromAss

redraw :: Window -> IORef Int -> IO ()
redraw window num = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num

createDrawing :: Window -> IORef Int -> IO ()
createDrawing window x = do
    hbox   <- hBoxNew True 10
    c      <- createFrame $ Just "C"
    ass    <- createFrame $ Just "Assembly"
    ram    <- createRam [(Just "l1", "c123"), (Just "l2", "c2")] [("PC", "2"), ("SP", "5")]
    io     <- createIO
    containerAdd hbox c
    containerAdd hbox ass
    containerAdd hbox ram
    containerAdd hbox io
    containerAdd window hbox
    widgetShowAll window

createIO :: IO VBox
createIO = do
    vbox   <- vBoxNew True 10
    stdin  <- createTextAreaFrame (Just "Stdin") Nothing False
    stdout <- createTextAreaFrame (Just "Stdout") Nothing False
    containerAdd vbox stdin
    containerAdd vbox stdout
    return vbox

createButton :: Window -> IORef Int -> (Int -> Int) -> IO Button
createButton window counter f = do
    button <- (readIORef counter) >>= (\num -> buttonNewWithLabel ("test " ++ (show num)))
    -- Increment the counter when the button is pressed.
    button `on` buttonActivated $ do
        modifyIORef' counter f
        redraw window counter
    return button
