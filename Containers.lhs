The \highlighttt{Containers} module defines several functions which make creating and managing gtk containers easier.

\begin{code}
module Containers where

import Operators

import "gtk3" Graphics.UI.Gtk
\end{code}

\noindent \highlighttt{createEmptyTextArea editable} creates an empty \highlighttt{TextView}, and takes a boolean value indicating whether it is editable.  To retrieve the text out of the \highlighttt{TextView}, use the \hyperref[source:containers:getTextViewsText]{getTextViewsText} function.

\begin{code}
createEmptyTextArea :: Bool -> IO TextView
createEmptyTextArea editable = textViewNew >>:= [textViewEditable := editable]
\end{code}

\noindent \highlighttt{createTextArea content editable} creates a \highlighttt{TextView}.  Can take some content and a boolean indicating whether the \highlighttt{TextView}is editable or not.  To retrieve the text out of the text view, use the \hyperref[source:containers:getTextViewsText]{getTextViewsText} function.
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

\noindent \highlighttt{createTextAreaFrame title content editable} creates a \highlighttt{TextView} within a \highlighttt{Frame}.  By using this method you cannot easily retrive the \highlighttt{TextView} that was created, therefore you should only use this function to display text that does not change.

\begin{code}
createTextAreaFrame :: Maybe String -> Maybe String -> Bool -> IO Frame
createTextAreaFrame title content editable = do
    frame <- case title of
        Nothing -> frameNew
        Just s  -> createFrame s
    createTextArea content editable >>|>> pad (5, 5, 5, 5) >>|> frame
\end{code}

\noindent \highlighttt{createFrame s} creates a \highlighttt{Frame} with a centred title.

\begin{code}
createFrame :: String -> IO Frame
createFrame s = do
    frame <- frameNew
    frameSetLabel frame s
    frameSetLabelAlign frame 0.5 0.5
    return frame
\end{code}

\noindent \highlighttt{getTextViewsText textView includeHidden}\label{source:containers:getTextViewsText} retrieves the content of a \highlighttt{TextView}.
\begin{code}
getTextViewsText :: (TextViewClass self) => self -> Bool -> IO String
getTextViewsText textView includeHidden = do
    buffer <- textViewGetBuffer textView
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    textBufferGetText buffer start end includeHidden
\end{code}

\noindent \highlighttt{pad (top, bottom, left, right)} creates a new \highlighttt{Alignment} that is padded by the specified amount.  This function aligns all text to the left and to the top.

\begin{code}
pad :: (Int, Int, Int, Int) -> IO Alignment
pad padding = padWithAlignment padding (0, 0, 1, 1)
\end{code}

\noindent \highlighttt{padWithAlignment (top, bottom, left, right) (xalign, yalign, xscale, yscale)} creates a new \highlighttt{Alignment} that is padded by the specified amount, but also is aligned according to the given values.

\begin{code}
padWithAlignment ::
    (Int, Int, Int, Int)
    -> (Float, Float, Float, Float)
    -> IO Alignment
padWithAlignment
    (top, bottom, left, right)
    (xalign, yalign, xscale, yscale) = do
        a <- alignmentNew xalign yalign xscale yscale
        alignmentSetPadding a top bottom left right
        return a
\end{code}
