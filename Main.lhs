The \highlighttt{Main} module provides the main entry point of the application.  It is also responsible for setting up the GUI and performing the actual logic of the program.

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

\noindent The \highlighttt{main} function sets up the GUI and run gtks main event loop.  The GUI consists of a \highlighttt{VBox} which contains a \highlighttt{MenuBar} and a container.  This container contains all of the GUI elements that change when the user progresses through the emulation.  Therefore everything in the container is redrawn on a regular basis.

The application starts with the emulation not running and allows the user to edit the stdin and assembly source code before they opt to run the emulation. Once the emulation has started running then the stdin and assembly source code widgets become disabled.

Throughout the program there are several operators that are used to add widgets to containers.  Please look at the Operators module (\ref{source:Operators}) for more information.

\begin{code}
main :: IO ()
main = do
    initGUI
    preloadIcons
    -- A counter which represents the current position of the
    -- emulation.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 1200 800
    -- Get the assembly source code and all the environments from
    -- reading the file in the command line arguments.
    (assembly, envs) <- environmentFromArgs
    -- Contains all GUI elements.
    vbox <- vBoxNew False 10
    -- The container that contains all gui elements that must be
    -- redrawn when the user progresses through the emulation.
    container <- vBoxNew False 0
    -- The main menu, not to be confused with the tool bar.
    menubar <- createTheMenu window container counter 
    -- Add the menu and the container to the VBox.
    boxPackStart vbox menubar PackNatural 5
    container >|> vbox >>|> window
    -- Draw the Window.
    redraw container counter assembly envs False
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ liftIO mainQuit
    -- Show the window
    widgetShowAll window
    -- Start listening for events.
    mainGUI
\end{code}

\noindent \textbf{Loading/Modifying Environment\newline}

\noindent \highlighttt{currentEnvironment x envs} retrieves the current environment from an environments \highlighttt{Array}.

\begin{code}
currentEnvironment
    :: IORef Int
    -> Array Int Environment
    -> IO Environment
currentEnvironment x envs = readIORef x >>= (envs !)
\end{code}

\noindent \highlighttt{environmentFromArgs} fetches the assembly source code and the environments for that source code from loading the file specified in the command line arguments.

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

\noindent \highlighttt{environmentFromFile filename stdin} reads the contents of a file and create the initial environment from the contents. Returns the source code and the initial environment in a tuple.

\begin{code}
environmentFromFile :: String -> [Int] -> IO (String, IO Environment)
environmentFromFile filename stdin = do
    ass <- readFile filename
    return (ass, makeEnvFromAss ass stdin)
\end{code}

\noindent \highlighttt{fileFromArgs} loads the assembly source code and stdin from the command line arguments.

\begin{code}
fileFromArgs :: IO (Maybe String, [Int])
fileFromArgs = do
    args <- getArgs
    case args of
        []            -> return (Nothing, [])
        file : stdins -> return
            (Just file, (\s -> read s :: Int) <$> (stdins >>= lines))
\end{code}

\noindent \highlighttt{resetCounter counter} resets the counter back to zero.

\begin{code}
resetCounter :: IORef Int -> IO ()
resetCounter counter = modifyIORef' counter (const 0)
\end{code}

\noindent \textbf{Drawing\newline}

\noindent \highlighttt{redrawFromFile container counter fileName} redraws everything in the container by extracting the assembly and environments from a file.  This function also resets the counter to 0 and stops the emulation from running.

\begin{code}
redrawFromFile
    :: (ContainerClass c)
    => c
    -> IORef Int
    -> String
    -> IO ()
redrawFromFile container counter fileName = do
    (assembly, env) <- environmentFromFile fileName []
    envs <- env >>= getFullProgEnv 
    -- Reset the counter to 0
    modifyIORef' counter (const 0)
    -- Draw the Window.
    redraw container counter assembly envs False
\end{code}

\noindent \highlighttt{redraw container counter assembly envs running} redraws everything in the container.  This involves removing everything in the container then adding it all back in.  This function delegates the actual drawing to \highlighttt{createDrawing} and simply removes everything from the container before delegating.

\begin{code}
redraw
    :: (ContainerClass c)
    => c
    -> IORef Int
    -> String
    -> Array Int Environment
    -> Bool
    -> IO ()
redraw container counter assembly envs running = do
    containerForeach container (containerRemove container)
    createDrawing container counter assembly envs running
\end{code}

\noindent \highlighttt{createDrawing container counter assembly envs running} creates everything in the container.  This includes creating the toolbar, the assembly source code area, the ram and registers and the IO section.  All of this is added to \highlighttt{container}.

\begin{code}
createDrawing
    :: (ContainerClass c)
    => c
    -> IORef Int
    -> String
    -> Array Int Environment
    -> Bool
    -> IO ()
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
    bar <- createToolbarMenu
        container
        counter
        assembly
        envs
        running
        (getTextViewsText assemblyTextView)
        stdinTextGetter
    boxPackStart vbox bar PackNatural 0
    hbox >|> vbox >>|> container
    widgetShowAll container
\end{code}

\noindent \highlighttt{createIO env running} creates the Stdin and Stdout \highlighttt{Frames} and adds them to a \highlighttt{VBox}.  This function returns the getter for the stdin text which is needed when attempting to retrieve the values for stdin and converting them to ints.

\begin{code}
createIO :: Environment -> Bool -> IO (VBox, Bool -> IO String)
createIO env running = do
    vbox   <- vBoxNew True 10
    stdin <- createTextArea
        (Just (toLines $ eStdIn env))
        (not running)
    stdin >|>> createFrame "Stdin" >>|> vbox
    createTextAreaFrame
        (Just "Stdout")
        (Just (toLines $ eStdOut env)) False
        >>|> vbox
    return (vbox, getTextViewsText stdin)
\end{code}

\noindent \highlighttt{createTheMenu window container counter} creates the main menu that contains the file menu.  This menu is repsonsible for supplying the ability to open other files.  An exit button is also created which is just another way for the user to quit the application.

\begin{code}
createTheMenu
    :: (ContainerClass c)
    => Window
    -> c
    -> IORef Int
    -> IO MenuBar
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

\noindent \highlighttt{createToolbarMenu container counter assembly envs running assemblySource stdinSource} creates the toolbar which contains all the buttons that manage the emulation.  The toolbar contains a play button, a stop button, a next button, a previous button and a text view that show the current counters value.  The play is disabled if the emulation is running and the stop, next and previous buttons are disabled if the emulation is not running.

\begin{code}
createToolbarMenu
    :: (ContainerClass c)
    => c
    -> IORef Int
    -> String
    -> Array Int Environment
    -> Bool
    -> (Bool -> IO String)
    -> (Bool -> IO String)
    -> IO Toolbar
createToolbarMenu
    container
    counter
    assembly
    envs
    running
    assemblySource
    stdinSource = do
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
                modifyIORef'
                    counter
                    (changeWithPredicate (< length envs) (+ 1))
                redraw container counter assembly envs True
            )
        previousBtn <- createPreviousBtn running (\_ -> do
                modifyIORef'
                    counter
                    (changeWithPredicate (>= 0) (flip (-) 1))
                redraw container counter assembly envs True
            )
        counterBtn <- createCounterBtn counter
        createToolbar
            (toToolItem <$> [
                    playBtn,
                    stopBtn,
                    previousBtn,
                    counterBtn,
                    nextBtn
                ]
            )
\end{code}

\noindent \highlighttt{createPlayBtn} creates the play button.

\begin{code}
createPlayBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createPlayBtn = createToolbarButtonFromStock
    playButton
    playButtonDisabled
\end{code}

\noindent \highlighttt{createStopBtn} creates the stop button.

\begin{code}
createStopBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createStopBtn running = createToolbarButtonFromStock
    stopButton
    stopButtonDisabled
    (not running)
\end{code}

\noindent \highlighttt{createNextBtn} creates the next button.

\begin{code}
createNextBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createNextBtn running = createToolbarButtonFromStock
    nextButton
    nextButtonDisabled
    (not running)
\end{code}

\noindent \highlighttt{createPreviousBtn} creates the previous button.

\begin{code}
createPreviousBtn :: Bool -> (() -> IO ()) -> IO ToolButton
createPreviousBtn running = createToolbarButtonFromStock
    previousButton
    previousButtonDisabled
    (not running)
\end{code}

\noindent \highlighttt{createPlayBtn} create the current counters display.

\begin{code}
createCounterBtn :: IORef Int -> IO ToolButton
createCounterBtn counter = do
    counter' <- show <$> readIORef counter
    counterEntry <- entryNew
    entrySetText counterEntry counter'
    entrySetWidthChars counterEntry 4
    btn <- toolButtonNew
        (Just counterEntry)
        (Nothing :: Maybe String)
    widgetSetSensitive btn False
    return btn
\end{code}

\noindent \highlighttt{createToolbarButtonFromStock iconActive iconDisabled diabled f} creates a ToolButon from an active \highlighttt{StockId}, a disabled \highlighttt{StockId}, a \highlighttt{Bool} indicating whether the button is disabled and a callback function which is invoked when the button is activated, as long as the button is not disabled.

\begin{code}
createToolbarButtonFromStock
    :: StockId
    -> StockId
    -> Bool
    -> (() -> IO ())
    -> IO ToolButton
createToolbarButtonFromStock iconActive iconDisabled disabled f
    | disabled  = createToolButtonFromStock iconDisabled True Nothing
    | otherwise = createToolButtonFromStock iconActive False (Just f)
\end{code}
