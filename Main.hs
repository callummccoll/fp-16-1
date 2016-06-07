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
    (file, stdin) <- fileFromArgs 
    case file of
        Nothing    -> error "No File Specified"
        Just file' -> do
	        -- Command for getting all environments for a prog
	        -- envs is an IOArray for quick lookup.
            (assembly, env) <- (environmentFromFile file' stdin)
            envs <- env >>= getFullProgEnv 
            -- Draw the Window.
            redraw window counter assembly envs
            -- Stop the application when the window is closed.
            -- Stop the application when the window is closed.
            window `on` deleteEvent $ tryEvent $ do
                liftIO $ mainQuit
            -- Start the application.
            mainGUI

fileFromArgs :: IO (Maybe String, [Int])
fileFromArgs = do
    args <- getArgs
    case args of
        []            -> return (Nothing, [])
        file : stdins -> return (Just file, (\s -> read s :: Int) <$> (stdins >>= lines))

environmentFromFile :: String -> [Int] -> IO (String, IO Environment)
environmentFromFile filename stdin = do
   ass <- readFile filename
   return (ass, makeEnvFromAss ass stdin)

redraw :: Window -> IORef Int -> String -> Array Int Environment -> IO ()
redraw window num assembly envs = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num assembly envs

createDrawing :: Window -> IORef Int -> String -> Array Int Environment -> IO ()
createDrawing window x assembly envs = do
    hbox <- hBoxNew True 10
    env  <- currentEnvironment x envs
    (createButtons window x assembly envs) >>= (containerAdd hbox) 
    --(createFrame $ Just "C") >>= (containerAdd hbox)
    (createTextAreaFrame (Just "Assembly") (Just (assembly)) False) >>= (containerAdd hbox)
    (createRamAndRegisters env) >>= (containerAdd hbox)
    (createIO env) >>= (containerAdd hbox)
    containerAdd window hbox
    widgetShowAll window

currentEnvironment :: IORef Int -> Array Int Environment -> IO Environment
currentEnvironment x envs = do
    i <- readIORef x
    return (envs ! i)

createButtons :: Window -> IORef Int -> String -> Array Int Environment -> IO VBox
createButtons window x assembly envs = do
    vbox <- vBoxNew True 10
    (createButton (createButtonFactory "next" x)
        (createButtonAction window x assembly envs
        (changeWithPredicate (< (length envs)) (+ 1))))
        >>= (containerAdd vbox)
    (createButton (createButtonFactory "previous" x)
        (createButtonAction window x assembly envs
        (changeWithPredicate (>= 0) (flip (-) 1))))
        >>= (containerAdd vbox)
    return vbox

createButtonFactory :: String -> IORef Int -> (() -> IO Button)
createButtonFactory name counter = (\_ -> do
    (readIORef counter) >>= (\num -> buttonNewWithLabel (name ++ " " ++ (show num)))
    )

createButtonAction :: Window -> IORef Int -> String -> Array Int Environment -> (Int -> Int) -> (() -> IO ())
createButtonAction window counter assembly envs f = (\_ -> do
    modifyIORef' counter f
    redraw window counter assembly envs
    )

createIO :: Environment -> IO VBox
createIO env = do
    vbox   <- vBoxNew True 10
    (createTextAreaFrame (Just "Stdin") (Just (toLines $ eStdIn env)) False) >>= (containerAdd vbox)
    (createTextAreaFrame (Just "Stdout") (Just (toLines $ eStdOut env)) False) >>= (containerAdd vbox)
    return vbox
