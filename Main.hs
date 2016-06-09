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
            vbox <- vBoxNew False 10
            content <- vBoxNew False 0
            menubar <- createMenu window content counter
            boxPackStart vbox menubar PackNatural 5
            content >|> vbox >>|> window
            -- Draw the Window.
            redraw window content counter assembly envs
            -- Stop the application when the window is closed.
            -- Stop the application when the window is closed.
            window `on` deleteEvent $ tryEvent $ do
                liftIO $ mainQuit
            widgetShowAll window
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

redraw :: (ContainerClass c) => Window -> c -> IORef Int -> String -> Array Int Environment -> IO ()
redraw window container num assembly envs = do
    containerForeach container (\w -> containerRemove container w)
    createDrawing window container num assembly envs

createDrawing :: (ContainerClass c) => Window -> c -> IORef Int -> String -> Array Int Environment -> IO ()
createDrawing window container counter assembly envs = do
    hbox <- hBoxNew True 10
    env  <- currentEnvironment counter envs
    (createButtons window container counter assembly envs) >>|> hbox 
    --(createFrame $ "C") >>= (containerAdd hbox)
    (createTextAreaFrame (Just "Assembly") (Just (assembly)) False) >>|> hbox
    (createRamAndRegisters env) >>|> hbox
    (createIO env) >>|> hbox
    hbox >|> container
    widgetShowAll container

currentEnvironment :: IORef Int -> Array Int Environment -> IO Environment
currentEnvironment x envs = do
    i <- readIORef x
    return (envs ! i)

createMenu :: (ContainerClass c) => Window -> c -> IORef Int -> IO MenuBar
createMenu window container counter  = do
    menubar <- menuBarNew
    fileMenu <- menuNew
    file <- menuItemNewWithLabel "File"
    open <- menuItemNewWithLabel "Open..."
    separator <- separatorMenuItemNew
    exit <- menuItemNewWithLabel "Exit"
    menuItemSetSubmenu file fileMenu
    menuShellAppend fileMenu open
    menuShellAppend fileMenu separator
    menuShellAppend fileMenu exit
    menuShellAppend menubar file
    open `on` menuItemActivate $ do
        dialog <- fileChooserDialogNew
            (Just "Choose Assmebly File")
            (Just window)
            FileChooserActionOpen
            [("Open", ResponseAccept), ("Cancel", ResponseCancel)]
        widgetShow dialog
        response <- dialogRun dialog
        case response of
            ResponseAccept -> do
                Just fileName <- fileChooserGetFilename dialog
                widgetDestroy dialog
                (assembly, env) <- (environmentFromFile fileName [])
                envs <- env >>= getFullProgEnv 
                modifyIORef' counter (\_ -> 0)
                -- Draw the Window.
                redraw window container counter assembly envs
            _ -> widgetDestroy dialog
        return ()
    exit `on` menuItemActivate $ do
        liftIO $ mainQuit
    return menubar

createButtons :: (ContainerClass c) => Window -> c -> IORef Int -> String -> Array Int Environment -> IO VBox
createButtons window container x assembly envs = do
    vbox <- vBoxNew True 10
    (createButton (createButtonFactory "next" x)
        (createButtonAction window container x assembly envs
        (changeWithPredicate (< (length envs)) (+ 1))))
        >>|> vbox
    (createButton (createButtonFactory "previous" x)
        (createButtonAction window container x assembly envs
        (changeWithPredicate (>= 0) (flip (-) 1))))
        >>|> vbox
    return vbox

createButtonFactory :: String -> IORef Int -> (() -> IO Button)
createButtonFactory name counter = (\_ -> do
    (readIORef counter) >>= (\num -> buttonNewWithLabel (name ++ " " ++ (show num)))
    )

createButtonAction :: (ContainerClass c) => Window -> c -> IORef Int -> String -> Array Int Environment -> (Int -> Int) -> (() -> IO ())
createButtonAction window container counter assembly envs f = (\_ -> do
    modifyIORef' counter f
    redraw window container counter assembly envs
    )

createIO :: Environment -> IO VBox
createIO env = do
    vbox   <- vBoxNew True 10
    (createTextAreaFrame (Just "Stdin") (Just (toLines $ eStdIn env)) False) >>|> vbox
    (createTextAreaFrame (Just "Stdout") (Just (toLines $ eStdOut env)) False) >>|> vbox
    return vbox
