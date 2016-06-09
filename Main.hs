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
    (assembly, envs) <- case file of
        Nothing    -> return ("", listArray (0, 0) [initEnvF]) 
        Just file' -> do
            (assembly, env) <- (environmentFromFile file' stdin)
            envs <- env >>= getFullProgEnv 
            return (assembly, envs)
    -- Create the container and layout of the menu.
    vbox <- vBoxNew False 10
    content <- vBoxNew False 0
    menubar <- createMenu window content counter False
    boxPackStart vbox menubar PackNatural 5
    content >|> vbox >>|> window
    -- Draw the Window.
    redraw content counter assembly envs False
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ do
        liftIO $ mainQuit
    -- Show the window
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

redraw :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> IO ()
redraw container num assembly envs running = do
    containerForeach container (\w -> containerRemove container w)
    createDrawing container num assembly envs running

createDrawing :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> IO ()
createDrawing container counter assembly envs running = do
    hbox <- hBoxNew True 10
    env  <- currentEnvironment counter envs
    (createButtons container counter assembly envs running) >>|> hbox 
    --(createFrame $ "C") >>= (containerAdd hbox)
    (createTextAreaFrame (Just "Assembly") (Just (assembly)) False) >>|> hbox
    (createRamAndRegisters env) >>|> hbox
    (createIO env running) >>|> hbox
    hbox >|> container
    widgetShowAll container

currentEnvironment :: IORef Int -> Array Int Environment -> IO Environment
currentEnvironment x envs = do
    i <- readIORef x
    return (envs ! i)

createMenu :: (ContainerClass c) => Window -> c -> IORef Int -> Bool -> IO MenuBar
createMenu window container counter running = do
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
                redraw container counter assembly envs running
            _ -> widgetDestroy dialog
        return ()
    exit `on` menuItemActivate $ do
        liftIO $ mainQuit
    return menubar

createButtons :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> IO VBox
createButtons container x assembly envs running = do
    vbox <- vBoxNew True 10
    (createButton (createButtonFactory "next" x)
        (createButtonAction container x assembly envs running
        (changeWithPredicate (< (length envs)) (+ 1))))
        >>|> vbox
    (createButton (createButtonFactory "previous" x)
        (createButtonAction container x assembly envs running
        (changeWithPredicate (>= 0) (flip (-) 1))))
        >>|> vbox
    return vbox

createButtonFactory :: String -> IORef Int -> (() -> IO Button)
createButtonFactory name counter = (\_ -> do
    (readIORef counter) >>= (\num -> buttonNewWithLabel (name ++ " " ++ (show num)))
    )

createButtonAction :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> (Int -> Int) -> (() -> IO ())
createButtonAction container counter assembly envs running f = (\_ -> do
    modifyIORef' counter f
    redraw container counter assembly envs running
    )

createIO :: Environment -> Bool -> IO VBox
createIO env running = do
    vbox   <- vBoxNew True 10
    (createTextAreaFrame (Just "Stdin") (Just (toLines $ eStdIn env)) (running == False)) >>|> vbox
    (createTextAreaFrame (Just "Stdout") (Just (toLines $ eStdOut env)) (running == False)) >>|> vbox
    return vbox
