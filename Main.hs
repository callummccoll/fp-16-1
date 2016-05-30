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
import Data.List

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
    env <- environmentFromFile "test.ass"
    redraw window counter env env
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ do
        liftIO $ mainQuit
    -- Start the application.
    mainGUI

environmentFromFile :: String -> IO Environment
environmentFromFile filename = (readFile filename) >>= makeEnvFromAss

redraw :: Window -> IORef Int -> Environment -> Environment -> IO ()
redraw window num startEnv env = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num startEnv env

createDrawing :: Window -> IORef Int -> Environment -> Environment -> IO ()
createDrawing window x startEnv env = do
    hbox <- hBoxNew True 10
    (createButtons window x startEnv) >>= (containerAdd hbox) 
    (createFrame $ Just "C") >>= (containerAdd hbox)
    (createFrame $ Just "Assembly") >>= (containerAdd hbox)
    (getRamFromEnvironment env) >>= (containerAdd hbox)
    (createIO env) >>= (containerAdd hbox)
    containerAdd window hbox
    widgetShowAll window

createButtons :: Window -> IORef Int -> Environment -> IO VBox
createButtons window x startEnv = do
    vbox <- vBoxNew True 10
    (createButton (createButtonFactory "next" x) (createButtonAction window x startEnv (changeWithPredicate (<= 11) (+ 1)))) >>= (containerAdd vbox)
    (createButton (createButtonFactory "previous" x) (createButtonAction window x startEnv (changeWithPredicate (>= 0) (flip (-) 1)))) >>= (containerAdd vbox)
    return vbox

createButtonFactory :: String -> IORef Int -> (() -> IO Button)
createButtonFactory name counter = (\_ -> do
    (readIORef counter) >>= (\num -> buttonNewWithLabel (name ++ " " ++ (show num)))
    )

createButtonAction :: Window -> IORef Int -> Environment -> (Int -> Int) -> (() -> IO ())
createButtonAction window counter env f = (\_ -> do
    modifyIORef' counter f
    (readIORef counter) >>= (getExeStep env) >>= (redraw window counter env)
    )

createIO :: Environment -> IO VBox
createIO env = do
    vbox   <- vBoxNew True 10
    (createTextAreaFrame (Just "Stdin") (Just (toLines $ eStdIn env)) False) >>= (containerAdd vbox)
    (createTextAreaFrame (Just "Stdout") (Just (toLines $ eStdOut env)) False) >>= (containerAdd vbox)
    return vbox
