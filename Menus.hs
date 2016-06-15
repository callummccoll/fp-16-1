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

