module Main (main) where

import Emulation
import Environment
import Helpers
import Ram

import "gtk3" Graphics.UI.Gtk

import Control.Applicative
import Control.Monad.Trans
import Data.IORef
import Data.Array.MArray
import Data.Array

type Point2d = (Double, Double)
type Time = Double
type Behavior a = Time -> a

main :: IO ()
main = do
    mainLoop
    initGUI
    -- A counter which is incremented when the button is pressed.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 1200 800
    -- Draw the Window.
    redraw window counter (environmentFromFile "test.ass")
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ do
        liftIO $ mainQuit
    -- Start the application.
    mainGUI

environmentFromFile :: String -> IO Environment
environmentFromFile filename = (readFile filename) >>= makeEnvFromAss

redraw :: Window -> IORef Int -> IO Environment -> IO ()
redraw window num env = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num env

createDrawing :: Window -> IORef Int -> IO Environment -> IO ()
createDrawing window x env = do
    hbox   <- hBoxNew True 10
    c      <- createFrame $ Just "C"
    ass    <- createFrame $ Just "Assembly"
    ram    <- getRamFromEnvironment env
    io     <- createIO
    containerAdd hbox c
    containerAdd hbox ass
    containerAdd hbox ram
    containerAdd hbox io
    containerAdd window hbox
    widgetShowAll window

getRamFromEnvironment :: IO Environment -> IO Frame
getRamFromEnvironment env = do
    env' <- env 
    case (eRAM env') of
        Left ram  -> (freeze ram) >>= (getRamFromArray env)
        Right ram -> getRamFromArray env ram

getRamFromArray :: IO Environment -> (Array Int Cell) -> IO Frame
getRamFromArray env ram = do
    cells     <- return (extractCell <$> ram)
    registers <- extractRegisters env
    createRam cells registers

extractCell :: Cell -> (Maybe String, String)
extractCell c = case (cLabel c, cVal c) of
    (l, Int c')  -> (Just l, (show c'))
    (l, Inst c') -> (Just l, (show c'))
    _            -> (Nothing, "")

extractRegisters :: IO Environment -> IO [(String, String)]
extractRegisters env = do
    env' <- env
    return [("A", show (eA env')), ("SP", show (eSP env')), ("PC", show (ePC env')) ]

createIO :: IO VBox
createIO = do
    vbox   <- vBoxNew True 10
    stdin  <- createTextAreaFrame (Just "Stdin") Nothing False
    stdout <- createTextAreaFrame (Just "Stdout") Nothing False
    containerAdd vbox stdin
    containerAdd vbox stdout
    return vbox

createButton :: Window -> IORef Int -> IO Environment -> (Int -> Int) -> IO Button
createButton window counter env f = do
    button <- (readIORef counter) >>= (\num -> buttonNewWithLabel ("test " ++ (show num)))
    -- Increment the counter when the button is pressed.
    button `on` buttonActivated $ do
        modifyIORef' counter f
        redraw window counter env
    return button
