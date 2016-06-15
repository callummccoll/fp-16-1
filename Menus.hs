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
                    Nothing        -> return ()
            _ -> return ()
        widgetDestroy dialog
        return ()
    exit `on` menuItemActivate $ do
        liftIO $ mainQuit
    return menubar

createToolbar :: [ToolItem] -> IO Toolbar
createToolbar items = do
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

createToolButtonFromIcon :: String -> Bool -> Maybe (() -> IO ()) -> IO ToolButton
createToolButtonFromIcon icon disabled f = do
    image <- imageNewFromIconName icon IconSizeMenu
    btn <- toolButtonNew (Just image) (Nothing :: Maybe String)
    widgetSetSensitive btn (disabled == False)
    case f of
        Nothing -> return btn
        Just f' -> do
            onToolButtonClicked btn $ do
                f' ()
            return btn
