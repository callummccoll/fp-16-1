This module provides the main entry point of the application.

This module is therefore responsible for setting up the gui and drawing
performing the actual logic of the program.

This modules only contains application specific code and is not reusable at all.

\begin{code}
module Main (main) where

import Assembly
import Containers
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
\end{code}

Setup the GUI and run gtks main event loop.

\begin{code}
main :: IO ()
main = do
    initGUI
    preloadIcons
    -- A counter which is incremented when the button is pressed.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 1200 800
    (assembly, envs) <- environmentFromArgs
    -- Create the container and layout of the menu.
    vbox <- vBoxNew False 10
    container <- vBoxNew False 0
    menubar <- createTheMenu window container counter 
    boxPackStart vbox menubar PackNatural 5
    container >|> vbox >>|> window
    -- Draw the Window.
    redraw container counter assembly envs False
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ liftIO mainQuit
    -- Show the window
    widgetShowAll window
    -- Start the application.
    mainGUI
\end{code}

Fetch the assembly source code and the environments for that source code from
loading the file specified in the command line arguments.

\begin{code}
environmentFromArgs :: IO (String, Array Int Environment)
environmentFromArgs = do
    (file, stdin) <- fileFromArgs
    case file of
        Nothing -> return ("", listArray (0, 0) [initEnvF])
        Just file' -> do
            (assembly, env) <- environmentFromFile file' stdin
            envs <- env >>= getFullProgEnv
            return (assembly, envs)
\end{code}

Redraw everything in the container by extracting the assembly and evironments
from the given file.

This function also resets the counter to 0 and stops the emulation from running.

\begin{code}
redrawFromFile :: (ContainerClass c) => c -> IORef Int -> String -> IO ()
redrawFromFile container counter fileName = do
    (assembly, env) <- environmentFromFile fileName []
    envs <- env >>= getFullProgEnv 
    -- Reset the counter to 0
    modifyIORef' counter (const 0)
    -- Draw the Window.
    redraw container counter assembly envs False
\end{code}

Load the assembly source code and stdin from the command line arguments.

\begin{code}
fileFromArgs :: IO (Maybe String, [Int])
fileFromArgs = do
    args <- getArgs
    case args of
        []            -> return (Nothing, [])
        file : stdins -> return (Just file, (\s -> read s :: Int) <$> (stdins >>= lines))
\end{code}

Read the contents of a file and create the initial environment from the
contents.

Returns the source code and the initial environment in a tuple.

\begin{code}
environmentFromFile :: String -> [Int] -> IO (String, IO Environment)
environmentFromFile filename stdin = do
   ass <- readFile filename
   return (ass, makeEnvFromAss ass stdin)
\end{code}

Redraw everything in the container.

This involves removing everything in the container then adding it all back in.

This function delegates the actual drawing to createDrawing and simply removes
everything from the container before delgating.

\begin{code}
redraw :: (ContainerClass c) => c -> IORef Int -> String -> Array Int Environment -> Bool -> IO ()
redraw container num assembly envs running = do
    containerForeach container (containerRemove container)
    createDrawing container num assembly envs running
\end{code}

Create everything in the container.

This includes creating the assembly source code area, the ram and registers and
the IO section.

All of this is added to the given container.

\begin{code}
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
\end{code}

Retrieve the current environment from the environments array.

\begin{code}
currentEnvironment :: IORef Int -> Array Int Environment -> IO Environment
currentEnvironment x envs = do
    i <- readIORef x
    return (envs ! i)
\end{code}

Reset the counter back to zero.

\begin{code}
resetCounter :: IORef Int -> IO ()
resetCounter counter = modifyIORef' counter (const 0)
\end{code}

Create the Stdin and Stdout frames and add them to a VBox.

This function returns the getter for the stdin text which is needed when
attempting to retrieve the values for the stdin and converting them to ints.

\begin{code}
createIO :: Environment -> Bool -> IO (VBox, Bool -> IO String)
createIO env running = do
    vbox   <- vBoxNew True 10
    stdin <- createTextArea (Just (toLines $ eStdIn env)) (not running)
    stdin >|>> createFrame "Stdin" >>|> vbox
    createTextAreaFrame (Just "Stdout") (Just (toLines $ eStdOut env)) False >>|> vbox
    return (vbox, getTextViewsText stdin)
\end{code}

Create the menu that contains the file menu.

This menu is repsonsible for supplying the ability to open other files.  An exit
button is also created which is just another way for the user to quit the
application.

\begin{code}
createTheMenu :: (ContainerClass c) => Window -> c -> IORef Int -> IO MenuBar
createTheMenu window container counter = do
    open <- createFileChooser
        "Open..."
        (Just "Choose Assembly File")
        (Just window)
        (redrawFromFile container counter)
    separator <- separatorMenuItemNew
    exit <- createMenuItem "Exit" (const mainQuit)
    fileMenu <- createMenu [open, toMenuItem separator, exit]
    file <- menuItemNewWithLabel "File"    
    menuItemSetSubmenu file fileMenu
    createMenuBar [file]
\end{code}

Create the toolbar which contains all the buttons that manage the emulation.

The toolbar contains a play button, a stop button, a next button, a previous
button and a text view that show the current counters value.

The play is disabled if the emulation is running and the stop, next and previous
buttons are disabled if the emulation is not running.

\begin{code}
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
\end{code}

Create the play button.

\begin{code}
createPlayBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createPlayBtn = createToolbarButtonFromStock playButton playButtonDisabled
\end{code}

Create the stop button.

\begin{code}
createStopBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createStopBtn running = createToolbarButtonFromStock stopButton stopButtonDisabled (not running)
\end{code}

Create the next button.

\begin{code}
createNextBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createNextBtn running = createToolbarButtonFromStock nextButton nextButtonDisabled (not running)
\end{code}

Create the previous button.

\begin{code}
createPreviousBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createPreviousBtn running = createToolbarButtonFromStock previousButton previousButtonDisabled (not running)
\end{code}

Create the current counters display.

\begin{code}
createCounterBtn :: IORef Int -> IO ToolButton
createCounterBtn counter = do
    counter' <- show <$> readIORef counter
    counterEntry <- entryNew
    entrySetText counterEntry counter'
    entrySetWidthChars counterEntry 4
    btn <- toolButtonNew (Just counterEntry) (Nothing :: Maybe String)
    widgetSetSensitive btn False
    return btn
\end{code}

Create a ToolButon from an active StockId, a disabledStockId, a Bool indicating
whether the button is disabled and a callback function which is invoked when the
button is activated, as long as the button is not disabled.

\begin{code}
createToolbarButtonFromStock :: StockId -> StockId -> Bool -> (() -> IO ()) -> IO ToolButton
createToolbarButtonFromStock iconActive iconDisabled disabled f
    | disabled  = createToolButtonFromStock iconDisabled True Nothing
    | otherwise = createToolButtonFromStock iconActive False (Just f)
\end{code}
