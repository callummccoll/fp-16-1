Defines several functions which make creating and managing gtk objects easier.

\begin{code}
module GTKHelpers where

import Operators

import "gtk3" Graphics.UI.Gtk
\end{code}

Create a button that executes the given function when pressed.

\begin{code}
createButton :: (() -> IO Button) -> (() -> IO ()) -> IO Button
createButton factory action = do
    button <- factory ()
    -- Increment the counter when the button is pressed.
    button `on` buttonActivated $ action ()
    return button
\end{code}

Create a text are within a frame.

Takes a title, the content of the text area and whether the text area is
editable.

\begin{code}
createTextAreaFrame :: Maybe String -> Maybe String -> Bool -> IO Frame
createTextAreaFrame title content editable = do
    frame <- case title of
               Nothing -> frameNew
               Just s  -> createFrame s
    createTextArea content editable >>|>> pad (5, 5, 5, 5) >>|> frame
\end{code}

Create a text area with some content and whether the text area is editable.

\begin{code}
createTextArea :: Maybe String -> Bool -> IO TextView
createTextArea content editable = case content of
    Nothing -> createEmptyTextArea editable
    Just s  -> do
        buffer <- textBufferNew Nothing
        textBufferSetText buffer s
        area <- textViewNewWithBuffer buffer >>:= [textViewEditable := editable]
        textViewSetLeftMargin area 5
        textViewSetRightMargin area 5
        return area
\end{code}

Create an empty text area and specify whether the text area is editable.

\begin{code}
createEmptyTextArea :: Bool -> IO TextView
createEmptyTextArea editable = textViewNew >>:= [textViewEditable := editable]
\end{code}

Create a frame with a centred title.

\begin{code}
createFrame :: String -> IO Frame
createFrame s = do
    frame <- frameNew
    frameSetLabel frame s
    frameSetLabelAlign frame 0.5 0.5
    return frame
\end{code}

\begin{code}
getTextViewsText :: (TextViewClass self) => self -> Bool -> IO String
getTextViewsText textView includeHidden = do
    buffer <- textViewGetBuffer textView
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    textBufferGetText buffer start end includeHidden
\end{code}

Create a new Alignment that is padded by the specified amount.  This function
aligns all text to the left and to the top.

\begin{code}
pad :: (Int, Int, Int, Int) -> IO Alignment
pad padding = padWithAlignment padding (0, 0, 1, 1)
\end{code}

Create a new Alignment that is padded by the specified amount, but also is
aligned according to the given values.

\begin{code}
padWithAlignment :: (Int, Int, Int, Int) -> (Float, Float, Float, Float) -> IO Alignment
padWithAlignment (top, bottom, left, right) (xalign, yalign, xscale, yscale) = do
    a <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding a top bottom left right
    return a
\end{code}
