module Menus where

import "gtk3" Graphics.UI.Gtk

import Control.Applicative
import Control.Monad.Trans

createMenu :: Window -> (String -> IO ()) -> IO MenuBar
createMenu window f = do
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
                fileName <- fileChooserGetFilename dialog
                case fileName of
                    Just fileName' -> f fileName'
            _              -> return ()
        widgetDestroy dialog
        return ()
    exit `on` menuItemActivate $ do
        liftIO $ mainQuit
    return menubar

createToolbar :: [ToolItem] -> IO Toolbar
createToolbar items = do
    --counter' <- (readIORef counter) >>= (\num -> return (show num))
    bar <- toolbarNew
    insertItems bar items 0
    return bar

insertItems :: Toolbar -> [ToolItem] -> Int -> IO ()
insertItems bar items pos = do
    case items of
        []            -> return ()
        item : items' -> do
            toolbarInsert bar item pos
            insertItems bar items' (pos + 1)

createToolButtonFromStock :: StockId -> Bool -> Maybe (() -> IO ()) -> IO ToolButton
createToolButtonFromStock stockId disabled f = do
    btn <- toolButtonNewFromStock stockId
    widgetSetSensitive btn (disabled == False)
    case f of
        Nothing -> return btn
        Just f' -> do
            onToolButtonClicked btn $ do
                f' ()
            return btn







    {-
    playStock <- buttonNewFromStock stockMediaPlay
    playStock' <- toolButtonNewFromStock stockMediaPlay
    stopStock <- toolButtonNewFromStock stockMediaStop
    backStock <- toolButtonNewFromStock stockGoBack
    forwardStock <- toolButtonNewFromStock stockGoForward
    case running of
        True -> do
            widgetSetSensitive playStock' False
        False -> do
            widgetSetSensitive stopStock False
            widgetSetSensitive backStock False
            widgetSetSensitive forwardStock False
    counterEntry <- entryNew
    entrySetText counterEntry counter'
    entrySetWidthChars counterEntry 4
    counterButton <- toolButtonNew (Just counterEntry) (Nothing :: Maybe String)
    widgetSetSensitive counterButton False
    play <- toolbarInsert bar playStock' 0
    stop <- toolbarInsert bar stopStock 1
    prev <- toolbarInsert bar backStock 2
    counterBtn <- toolbarInsert bar counterButton 3
    next <- toolbarInsert bar forwardStock 4
    onToolButtonClicked playStock' $ do
        case running of
            True -> return ()
            False -> do
                stdin <- (stdinSource False) >>= strToInts
                assembly' <- assemblySource False
                envs' <- (makeEnvFromAss assembly' stdin) >>= getFullProgEnv 
                resetCounter counter
                redraw container counter assembly' envs' True
    onToolButtonClicked stopStock $ do
        case running of
            False -> return ()
            True -> do
                resetCounter counter
                redraw container counter assembly envs False
    onToolButtonClicked forwardStock $ do
        case running of
            False -> return ()
            True -> do
                modifyIORef' counter (changeWithPredicate (< (length envs)) (+ 1))
                redraw container counter assembly envs True
    onToolButtonClicked backStock $ do
        case running of
            False -> return ()
            True -> do
                modifyIORef' counter (changeWithPredicate (>= 0 ) (flip (-) 1))
                redraw container counter assembly envs True
    -}
