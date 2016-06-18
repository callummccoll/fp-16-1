module Icons where

import "gtk3" Graphics.UI.Gtk

import Data.String
import System.Directory

preloadIcons :: IO ()
preloadIcons = do
    putStrLn "hello"
    defaultTheme <- iconThemeGetDefault
    dir <- assetsDirectory
    putStrLn dir
    icons <- allCustomIcons
    putStrLn (show icons)
    preloadIconsFromDirectory dir "svg" icons

preloadIconsFromDirectory :: FilePath -> String -> [StockId] -> IO ()
preloadIconsFromDirectory dir ext icons = case icons of
    []            -> return ()
    icon : icons' -> do
        preloadIconFromDirectory dir ext icon
        preloadIconsFromDirectory dir ext icons'

preloadIconFromDirectory :: FilePath -> String -> StockId -> IO ()
preloadIconFromDirectory dir ext icon = do
    file <- return (dir ++ "/" ++ (read $ show icon) ++ "." ++ ext)
    preloadIcon file icon

preloadIcon :: FilePath -> StockId -> IO ()
preloadIcon path icon = do
    putStrLn ("preload " ++ (read $ show icon) ++ ": " ++ path)
    factory <- iconFactoryNew
    set <- (imageNewFromFile path) >>= imageGetPixbuf >>= iconSetNewFromPixbuf
    iconFactoryAdd factory icon set
    iconFactoryAddDefault factory

assetsDirectory :: IO String
assetsDirectory = do
    current <- getCurrentDirectory
    return (current ++ "/assets")

playButton :: StockId
playButton = fromString "media-playback-start"

playButtonDisabled :: StockId
playButtonDisabled = fromString "media-playback-start-symbolic"

stopButton :: StockId
stopButton = fromString "media-playback-stop"

stopButtonDisabled :: StockId
stopButtonDisabled = fromString "media-playback-stop-symbolic"

nextButton :: StockId
nextButton = fromString "go-next"

nextButtonDisabled :: StockId
nextButtonDisabled = fromString "go-next-symbolic"

previousButton :: StockId
previousButton = fromString "go-previous"

previousButtonDisabled :: StockId
previousButtonDisabled = fromString "go-previous-symbolic"

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
