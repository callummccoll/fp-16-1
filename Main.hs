module Main (main) where

import Assembly
import Emulation
import Environment
import Helpers
import Presentation
import Ram

import "gtk3" Graphics.UI.Gtk

import Control.Applicative
import Control.Monad.Trans
import Data.IORef
import Data.Array.MArray
import Data.Array

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
    env <- (environmentFromFile "test.ass")
    redraw window counter (return env) (return env)
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ do
        liftIO $ mainQuit
    -- Start the application.
    mainGUI

environmentFromFile :: String -> IO Environment
environmentFromFile filename = (readFile filename) >>= makeEnvFromAss

redraw :: Window -> IORef Int -> IO Environment -> IO Environment -> IO ()
redraw window num startEnv env = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num startEnv env

createDrawing :: Window -> IORef Int -> IO Environment -> IO Environment -> IO ()
createDrawing window x startEnv env = do
    hbox <- hBoxNew True 10
    (createButtons window x startEnv) >>= (containerAdd hbox) 
    (createFrame $ Just "C") >>= (containerAdd hbox)
    (createFrame $ Just "Assembly") >>= (containerAdd hbox)
    (getRamFromEnvironment env) >>= (containerAdd hbox)
    createIO >>= (containerAdd hbox)
    containerAdd window hbox
    widgetShowAll window

createButtons :: Window -> IORef Int -> IO Environment -> IO VBox
createButtons window x startEnv = do
    vbox <- vBoxNew True 10
    (createButton window x startEnv "next" (changeWithPredicate (<= 11) (+ 1))) >>= (containerAdd vbox)
    (createButton window x startEnv "previous" (changeWithPredicate (>= 0) (flip (-) 1))) >>= (containerAdd vbox)
    return vbox

createIO :: IO VBox
createIO = do
    vbox   <- vBoxNew True 10
    stdin  <- createTextAreaFrame (Just "Stdin") Nothing False
    stdout <- createTextAreaFrame (Just "Stdout") Nothing False
    containerAdd vbox stdin
    containerAdd vbox stdout
    return vbox

createButton :: Window -> IORef Int -> IO Environment -> String -> (Int -> Int) -> IO Button
createButton window counter env name f = do
    button <- (readIORef counter) >>= (\num -> buttonNewWithLabel (name ++ " " ++ (show num)))
    -- Increment the counter when the button is pressed.
    button `on` buttonActivated $ do
        modifyIORef' counter f
        env' <- env
        newEnv <- (readIORef counter) >>= (\num -> getExeStep env' num)
        redraw window counter env (return newEnv)
    return button
