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
import Data.Array.IO
import Data.List
import System.Environment

main :: IO ()
main = do
    initGUI
    -- A counter which is incremented when the button is pressed.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 1200 800

    file <- fileFromArgs 
    case file of
        Nothing    -> error "No File Specified"
        Just file' -> do
	        -- Command for getting all environments for a prog
	        -- envs is an IOArray for quick lookup.
            envs <- (environmentFromFile file') >>= getFullProgEnv 
            -- Draw the Window.
            redraw window counter envs
            -- Stop the application when the window is closed.
            -- Stop the application when the window is closed.
            window `on` deleteEvent $ tryEvent $ do
                liftIO $ mainQuit
            -- Start the application.
            mainGUI

fileFromArgs :: IO (Maybe String)
fileFromArgs = do
    args <- getArgs
    case args of
        [] -> return Nothing
        file : _ -> return $ Just file

environmentFromFile :: String -> IO Environment
environmentFromFile filename = do
   ass <- readFile filename
   makeEnvFromAss ass [5] --this is the stdin that needs to be linked in some way?

redraw :: Window -> IORef Int -> Array Int Environment -> IO ()
redraw window num envs = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num envs

createDrawing :: Window -> IORef Int -> Array Int Environment -> IO ()
createDrawing window x envs = do
    hbox <- hBoxNew True 10
    env  <- currentEnvironment x envs
    (createButtons window x envs) >>= (containerAdd hbox) 
    (createFrame $ Just "C") >>= (containerAdd hbox)
    (createFrame $ Just "Assembly") >>= (containerAdd hbox)
    (createRamAndRegisters env) >>= (containerAdd hbox)
    (createIO env) >>= (containerAdd hbox)
    containerAdd window hbox
    widgetShowAll window

currentEnvironment :: IORef Int -> Array Int Environment -> IO Environment
currentEnvironment x envs = do
    i <- readIORef x
    return (envs ! i)

createButtons :: Window -> IORef Int -> Array Int Environment -> IO VBox
createButtons window x envs = do
    vbox <- vBoxNew True 10
    (createButton (createButtonFactory "next" x)
        (createButtonAction window x envs
        (changeWithPredicate (< (length envs)) (+ 1))))
        >>= (containerAdd vbox)
    (createButton (createButtonFactory "previous" x)
        (createButtonAction window x envs
        (changeWithPredicate (>= 0) (flip (-) 1))))
        >>= (containerAdd vbox)
    return vbox

createButtonFactory :: String -> IORef Int -> (() -> IO Button)
createButtonFactory name counter = (\_ -> do
    (readIORef counter) >>= (\num -> buttonNewWithLabel (name ++ " " ++ (show num)))
    )

createButtonAction :: Window -> IORef Int -> Array Int Environment -> (Int -> Int) -> (() -> IO ())
createButtonAction window counter envs f = (\_ -> do
    modifyIORef' counter f
    redraw window counter envs
    )

createIO :: Environment -> IO VBox
createIO env = do
    vbox   <- vBoxNew True 10
    (createTextAreaFrame (Just "Stdin") (Just (toLines $ eStdIn env)) False) >>= (containerAdd vbox)
    (createTextAreaFrame (Just "Stdout") (Just (toLines $ eStdOut env)) False) >>= (containerAdd vbox)
    return vbox
