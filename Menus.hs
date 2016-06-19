module Menus where

import "gtk3" Graphics.UI.Gtk

import Control.Applicative
import Control.Monad.Trans

createMenuItem :: String -> (() -> IO ()) -> IO MenuItem
createMenuItem label f = do
    item <- menuItemNewWithLabel label
    item `on` menuItemActivate $ f ()
    return item

createFileChooser :: String -> Maybe String -> Maybe Window -> (String -> IO ()) -> IO MenuItem
createFileChooser label instructions window f = createMenuItem label (\_ -> do
        dialog <- fileChooserDialogNew
            instructions
            window
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
    )

createMenuBar :: (MenuItemClass i) => [i] -> IO MenuBar
createMenuBar items = do
    bar <- menuBarNew
    attachItemsToMenu bar items
    return bar

createMenu :: (MenuItemClass i) => [i] -> IO Menu
createMenu items = do
    menu <- menuNew
    attachItemsToMenu menu items
    return menu

attachItemsToMenu :: (MenuShellClass s, MenuItemClass i) => s -> [i] -> IO ()
attachItemsToMenu menu items = case items of
    [] -> return ()
    item : items' -> do
        menuShellAppend menu item
        attachItemsToMenu menu items'

createToolbar :: [ToolItem] -> IO Toolbar
createToolbar items = do
    bar <- toolbarNew
    insertItems bar items 0
    return bar

insertItems :: Toolbar -> [ToolItem] -> Int -> IO ()
insertItems bar items pos = case items of
    []            -> return ()
    item : items' -> do
        toolbarInsert bar item pos
        insertItems bar items' (pos + 1)

createToolButtonFromStock :: StockId -> Bool -> Maybe (() -> IO ()) -> IO ToolButton
createToolButtonFromStock stockId disabled f = do
    btn <- toolButtonNewFromStock stockId
    widgetSetSensitive btn (not disabled)
    case f of
        Nothing -> return btn
        Just f' -> do
            onToolButtonClicked btn $ f' ()
            return btn
