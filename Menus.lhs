The \highlighttt{Menus} module defines several functions which make the process of creating and managing menus and toolbars easier.

\begin{code}
module Menus where

import "gtk3" Graphics.UI.Gtk
\end{code}

\noindent \highlighttt{createMenuItem label f} creates a button for a menu.  Uses a callback function which is executed when the button has been activated.

\begin{code}
createMenuItem :: String -> (() -> IO ()) -> IO MenuItem
createMenuItem label f = do
    item <- menuItemNewWithLabel label
    item `on` menuItemActivate $ f ()
    return item
\end{code}

\noindent \highlighttt{createFileChooser label instructions window f} creates a button for a menu that open a file chooser dialog.  Takes a label for the button, some text to display to the user when the file dialog opens, the window that owns the dialog and a callback function which is executed when the file has been chosen.

\begin{code}
createFileChooser
    :: String
    -> Maybe String
    -> Maybe Window
    -> (String -> IO ())
    -> IO MenuItem
createFileChooser
    label
    instructions
    window
    f = createMenuItem label (\_ -> do
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
\end{code}

\noindent \highlighttt{createMenuBar items} creates a \highlighttt{MenuBar} and attaches the given list of \highlighttt{MenuItems} to it.

\begin{code}
createMenuBar :: (MenuItemClass i) => [i] -> IO MenuBar
createMenuBar items = do
    bar <- menuBarNew
    attachItemsToMenu bar items
    return bar
\end{code}

\noindent \highlighttt{createMenu items} creates a \highlighttt{Menu} and attach the given list of \highlighttt{MenuItems} to it.  This function should be used when creating sub menus of a \highlighttt{MenuBar}.

\begin{code}
createMenu :: (MenuItemClass i) => [i] -> IO Menu
createMenu items = do
    menu <- menuNew
    attachItemsToMenu menu items
    return menu
\end{code}

\noindent \highlighttt{attachItemsToMenu menu items} attaches items to a menu.

\begin{code}
attachItemsToMenu
    :: (MenuShellClass s, MenuItemClass i)
    => s
    -> [i]
    -> IO ()
attachItemsToMenu menu items = case items of
    [] -> return ()
    item : items' -> do
        menuShellAppend menu item
        attachItemsToMenu menu items'
\end{code}

\noindent \highlighttt{createToolbar items} creates a \highlighttt{Toolbar} and attaches \highlighttt{ToolItems} to it.

\begin{code}
createToolbar :: [ToolItem] -> IO Toolbar
createToolbar items = do
    bar <- toolbarNew
    insertItems bar items 0
    return bar
\end{code}

\noindent \highlighttt{insertItems bar items pos} attaches \highlighttt{ToolItems} to a \highlighttt{ToolBar} starting at a specific position.

\begin{code}
insertItems :: Toolbar -> [ToolItem] -> Int -> IO ()
insertItems bar items pos = case items of
    []            -> return ()
    item : items' -> do
        toolbarInsert bar item pos
        insertItems bar items' (pos + 1)
\end{code}

\noindent \highlighttt{createToolButtonFromStock stockId disabled f} creates a \highlighttt{ToolButton} from a \highlighttt{StockId}.  This function takes the \highlighttt{StockId}, whether the button is disabled and an optional function which is executed when the button is pressed.

\begin{code}
createToolButtonFromStock
    :: StockId
    -> Bool
    -> Maybe (() -> IO ())
    -> IO ToolButton
createToolButtonFromStock stockId disabled f = do
    btn <- toolButtonNewFromStock stockId
    widgetSetSensitive btn (not disabled)
    case f of
        Nothing -> return btn
        Just f' -> do
            onToolButtonClicked btn $ f' ()
            return btn
\end{code}
