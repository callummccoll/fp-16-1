module Main (main) where

import Assembly
import Emulation
import Environment
import Helpers
import Icons
import Menus
import Operators
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
    preloadIcons
    -- A counter which is incremented when the button is pressed.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 1200 800
    (file, stdin) <- fileFromArgs 
    (assembly, envs) <- case file of
        Nothing    -> return ("", listArray (0, 0) [initEnvF]) 
        Just file' -> do
            (assembly, env) <- environmentFromFile file' stdin
            envs <- env >>= getFullProgEnv 
            return (assembly, envs)
    -- Create the container and layout of the menu.
    vbox <- vBoxNew False 10
    content <- vBoxNew False 0
    menubar <- createMenu window (redrawFromFile content counter)
    boxPackStart vbox menubar PackNatural 5
    content >|> vbox >>|> window
    -- Draw the Window.
    redraw content counter assembly envs False
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ liftIO mainQuit
    -- Show the window
    widgetShowAll window
    -- Start the application.
    mainGUI

redrawFromFile :: (ContainerClass c) => c -> IORef Int -> String -> IO ()
redrawFromFile container counter fileName = do
    (assembly, env) <- environmentFromFile fileName []
    envs <- env >>= getFullProgEnv 
    -- Reset the counter to 0
    modifyIORef' counter (const 0)
    -- Draw the Window.
    redraw container counter assembly envs False

fileFromArgs :: IO (Maybe String, [Int])
fileFromArgs = do
    args <- getArgs
    case args of
        []            -> return (Nothing, [])
        file : stdins -> return (Just file, (\s -> read s :: Int) <$> (stdins >>= lines))

strToInts :: String -> IO [Int]
strToInts str = return ((\s -> read s :: Int) <$> lines str)

environmentFromFile :: String -> [Int] -> IO (String, IO Environment)
environmentFromFile filename stdin = do
   ass <- readFile filename
   return (ass, makeEnvFromAss ass stdin)

redraw :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> IO ()
redraw container num assembly envs running = do
    containerForeach container (containerRemove container)
    createDrawing container num assembly envs running

createDrawing :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> IO ()
createDrawing container counter assembly envs running = do
    vbox <- vBoxNew False 0
    hbox <- hBoxNew False 10
    env  <- currentEnvironment counter envs
    assemblyTextView <- createTextArea (Just assembly) (not running)
    assemblyTextView >|>> createFrame "Assembly" >>|> hbox
    createRamAndRegisters env running >>|> hbox
    (ioVbox, stdinTextGetter) <- createIO env running
    ioVbox >|> hbox
    stdin <- stdinTextGetter False >>= strToInts
    bar <- createToolbarMenu container counter assembly envs running (getTextViewsText assemblyTextView) stdinTextGetter
    boxPackStart vbox bar PackNatural 0
    hbox >|> vbox >>|> container
    widgetShowAll container

currentEnvironment :: IORef Int -> Array Int Environment -> IO Environment
currentEnvironment x envs = do
    i <- readIORef x
    return (envs ! i)

resetCounter :: IORef Int -> IO ()
resetCounter counter = modifyIORef' counter (const 0)

createIO :: Environment -> Bool -> IO (VBox, Bool -> IO String)
createIO env running = do
    vbox   <- vBoxNew True 10
    stdin <- createTextArea (Just (toLines $ eStdIn env)) (not running)
    stdin >|>> createFrame "Stdin" >>|> vbox
    createTextAreaFrame (Just "Stdout") (Just (toLines $ eStdOut env)) False >>|> vbox
    return (vbox, getTextViewsText stdin)


createToolbarMenu :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> (Bool -> IO String) -> (Bool -> IO String) -> IO Toolbar
createToolbarMenu container counter assembly envs running assemblySource stdinSource = do
    playBtn <- createPlayBtn running (\_ -> do
            resetCounter counter
            stdin <- stdinSource False >>= strToInts
            assembly' <- assemblySource False
            env <- makeEnvFromAss assembly' stdin
            envs' <- getFullProgEnv env
            redraw container counter assembly' envs' True
        )
    stopBtn <- createStopBtn running (\_ -> do
            resetCounter counter
            redraw container counter assembly envs False
        )
    nextBtn <- createNextBtn running (\_ -> do
            modifyIORef' counter (changeWithPredicate (< length envs) (+ 1))
            redraw container counter assembly envs True
        )
    previousBtn <- createPreviousBtn running (\_ -> do
            modifyIORef' counter (changeWithPredicate (>= 0) (flip (-) 1))
            redraw container counter assembly envs True
        )
    counterBtn <- createCounterBtn counter
    createToolbar (toToolItem <$> [playBtn, stopBtn, previousBtn, counterBtn, nextBtn])

createPlayBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createPlayBtn = createToolbarButtonFromStock playButton playButtonDisabled

createStopBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createStopBtn running = createToolbarButtonFromStock stopButton stopButtonDisabled (not running)

createNextBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createNextBtn running = createToolbarButtonFromStock nextButton nextButtonDisabled (not running)

createPreviousBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createPreviousBtn running = createToolbarButtonFromStock previousButton previousButtonDisabled (not running)

createCounterBtn :: IORef Int -> IO ToolButton
createCounterBtn counter = do
    counter' <- show <$> readIORef counter
    counterEntry <- entryNew
    entrySetText counterEntry counter'
    entrySetWidthChars counterEntry 4
    btn <- toolButtonNew (Just counterEntry) (Nothing :: Maybe String)
    widgetSetSensitive btn False
    return btn

createToolbarButtonFromStock :: StockId -> StockId-> Bool -> (() -> IO ()) -> IO ToolButton
createToolbarButtonFromStock iconActive iconDisabled disabled f
    | disabled  = createToolButtonFromStock iconDisabled True Nothing
    | otherwise = createToolButtonFromStock iconActive False (Just f)

getTextViewsText :: (TextViewClass self) => self -> Bool -> IO String
getTextViewsText textView includeHidden = do
    buffer <- textViewGetBuffer textView
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    textBufferGetText buffer start end includeHidden
