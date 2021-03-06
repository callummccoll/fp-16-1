The \highlighttt{Icons} module supplies default images for icons that may not exist accross multiple platforms.  This module is particularly important as the media icons do not exist on the Mac OSX platform and thus require this preloading from the assets directory to use them.  The assets directory is located within the root of the projects folder.

\begin{code}
module Icons where

import "gtk3" Graphics.UI.Gtk

import Data.String
import System.Directory
\end{code}

\noindent \highlighttt{preloadIcons} preloads all of the icons that we are using within the application.

\begin{code}
preloadIcons :: IO ()
preloadIcons = do
    dir <- assetsDirectory
    icons <- allCustomIcons
    preloadIconsFromDirectory dir "svg" icons
\end{code}

\noindent Given a directory, a file extension and a list of icons to load,\newline \highlighttt{preloadIconsFromDirectory dir ext icons} constructs the filepaths and loads each image so that it is usable within the application.  This function assumes that the name of the file for each icon is identical to each icons \highlighttt{StockId}.  The function also assumes that the icons have all the same extensions.

\begin{code}
preloadIconsFromDirectory :: FilePath -> String -> [StockId] -> IO ()
preloadIconsFromDirectory dir ext icons = case icons of
    []            -> return ()
    icon : icons' -> do
        preloadIconFromDirectory dir ext icon
        preloadIconsFromDirectory dir ext icons'
\end{code}

\noindent \highlighttt{preloadIconFromDirectory dir ext icon} preloads a specific icon from a specific directory.  This function constructs the file path from the given directory, extension and name of the icon.  This function assumes that the name of the file for the icon is identical to the \highlighttt{StockId} for the icon.

\begin{code}
preloadIconFromDirectory :: FilePath -> String -> StockId -> IO ()
preloadIconFromDirectory dir ext icon = do
    let file = dir ++ "/" ++ read (show icon) ++ "." ++ ext
    preloadIcon file icon
\end{code}

\noindent \highlighttt{preloadIcon path icon} preloads an icon located at a file path, identified by a \highlighttt{StockId}.

\begin{code}
preloadIcon :: FilePath -> StockId -> IO ()
preloadIcon path icon = do
    factory <- iconFactoryNew
    set <- imageNewFromFile path >>= imageGetPixbuf >>= iconSetNewFromPixbuf
    iconFactoryAdd factory icon set
    iconFactoryAddDefault factory
\end{code}

\noindent \highlighttt{assetsDirectory} returns the location of the assets directory.  By default this is just located in the current working directory.

\begin{code}
assetsDirectory :: IO String
assetsDirectory = do
    current <- getCurrentDirectory
    return (current ++ "/assets")
\end{code}

\noindent \highlighttt{playButton} is the identifier for the play button: media-playback-start.

\begin{code}
playButton :: StockId
playButton = fromString "media-playback-start"
\end{code}

\noindent \highlighttt{playButtonDisabled} is the identifier for the disabled play button: media-playback-start-symbolic.

\begin{code}
playButtonDisabled :: StockId
playButtonDisabled = fromString "media-playback-start-symbolic"
\end{code}

\noindent \highlighttt{stopButton} is the identifier for the stop button: media-playback-stop.

\begin{code}
stopButton :: StockId
stopButton = fromString "media-playback-stop"
\end{code}

\noindent \highlighttt{stopButtonDisabled} is the identifier for the disabled stop button: media-playback-stop-symbolic.

\begin{code}
stopButtonDisabled :: StockId
stopButtonDisabled = fromString "media-playback-stop-symbolic"
\end{code}

\noindent \highlighttt{nextButton} is the identifier for the next button: go-next

\begin{code}
nextButton :: StockId
nextButton = fromString "go-next"
\end{code}

\noindent \highlighttt{nextButtonDisabled} is the identifier for the disabled next button: go-next-symbolic.

\begin{code}
nextButtonDisabled :: StockId
nextButtonDisabled = fromString "go-next-symbolic"
\end{code}

\noindent \highlighttt{previousButton} is the identifier for the previous button: go-previous.

\begin{code}
previousButton :: StockId
previousButton = fromString "go-previous"
\end{code}

\noindent \highlighttt{previousButtonDisabled} is the identifier for the disabled previous button: go-previous-symbolic.

\begin{code}
previousButtonDisabled :: StockId
previousButtonDisabled = fromString "go-previous-symbolic"
\end{code}

\noindent \highlighttt{allCustomIcons} is a list of all the preloaded icons.

\begin{code}
allCustomIcons :: IO [StockId]
allCustomIcons = return [
        playButton,
        playButtonDisabled,
        stopButton,
        stopButtonDisabled,
        nextButton,
        nextButtonDisabled,
        previousButton,
        previousButtonDisabled
    ]
\end{code}
