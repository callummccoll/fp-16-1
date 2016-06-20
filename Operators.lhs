\label{source:Operators}

The \highlighttt{Operators} module defines several operators which provide chaining and a nicer syntax for common gtk operations.

\begin{code}
module Operators where

import "gtk3" Graphics.UI.Gtk
\end{code}

\noindent The three flavours of operators are \highlighttt{>|>}, \highlighttt{<|<} and \highlighttt{>:=} operators.  Each of these three types of operators have a few variants which deal with the fact that things might be wrapped up in the IO monad.  Thus \highlighttt{<|<<} is used if the operand on the right side of the operator is wrapped in the IO monad and the operator will automatically unwrap it and apply the \highlighttt{<|<} with the unwrapped value.

Every operator returns the operand that is on the right side.  By doing this you can chain operators and do things on a single line.  For example:

\begin{examplecode}
createFrame "1" >>|>> createFrame "2" >>:= [frameLabelXAlign := 0.1]
\end{examplecode}

\noindent Creates two frames where the '1' frame is inside the '2' frame and the '2' frame has its title aligned to the left.  Without the operators we would have to resort to the following:

\begin{examplecode}
frame1 <- createFrame "1"
frame2 <- createFrame "2"
set frame2 [frameLabelXAlign := 0.1]
containerAdd frame2 frame1
return frame2
\end{examplecode}

\noindent As you can see the operators do minimize the amount of code that we have to write.

\paragraph{List Of Operators}

The \highlighttt{widget >|> container} operator adds a widget to a container.
\begin{code}
infixl 2 >|>
(>|>) :: (ContainerClass c, WidgetClass w) => w -> c -> IO c
widget >|> container = do
    containerAdd container widget
    return container
\end{code}

\noindent The \highlighttt{widget >|>> container} operator adds a widget to a container that is wrapped in the IO monad.

\begin{code}
infixl 2 >|>>
(>|>>) :: (ContainerClass c, WidgetClass w) => w -> IO c -> IO c
widget >|>> container = do
    c <- container
    containerAdd c widget
    return c
\end{code}

\noindent The \highlighttt{widget >>|> container} operator adds a widget that is wrapped in the IO monad to a container.

\begin{code}
infixl 2 >>|>
(>>|>) :: (ContainerClass c, WidgetClass w) => IO w -> c -> IO c
widget >>|> container = do
    widget >>= containerAdd container
    return container
\end{code}

\noindent The \highlighttt{widget >>|>> container} operator adds a widget which is wrapped in the IO monad to a container that is wrapped in the IO monad.

\begin{code}
infixl 2 >>|>>
(>>|>>) :: (ContainerClass c, WidgetClass w) => IO w -> IO c -> IO c
widget >>|>> container = do
    w <- widget
    c <- container
    containerAdd c w
    return c
\end{code}

\noindent The \highlighttt{container <|< widget} operator adds a widget to a container.

\begin{code}
infixl 1 <|<
(<|<) :: (ContainerClass c, WidgetClass w) => c -> w -> IO w
container <|< widget = do
    containerAdd container widget
    return widget
\end{code}

\noindent The \highlighttt{container <<|< widget} operator add a widget to a container that is wrapped in the IO monad.

\begin{code}
infixl 1 <<|<
(<<|<) :: (ContainerClass c, WidgetClass w) => IO c -> w -> IO w
container <<|< widget = do
    c <- container
    containerAdd c widget
    return widget
\end{code}

\noindent The \highlighttt{container <|<< widget} operator add a widget that is wrapped in the IO monad to a container.

\begin{code}
infixl 1 <|<<
(<|<<) :: (ContainerClass c, WidgetClass w) => c -> IO w -> IO w
container <|<< widget = do
    widget >>= containerAdd container
    widget
\end{code}

\noindent The \highlighttt{container <<|<< widget} operator adds a widget that is wrapped in the IO monad to a container that is wrapped in the IO monad.

\begin{code}
infixl 1 <<|<<
(<<|<<) :: (ContainerClass c, WidgetClass w) => IO c -> IO w -> IO w
container <<|<< widget = do
    c <- container
    w <- widget
    containerAdd c w
    return w
\end{code}

\noindent The \highlighttt{object >:= attributes} operator sets a list of attributes on an object.

\begin{code}
infixl 2 >:=
(>:=) :: o -> [AttrOp o] -> IO o
object >:= attributes = do
    set object attributes
    return object
\end{code}

\noindent The \highlighttt{object >>:= attributes} operator sets a list of attributes on an object that is wrapped in the IO monad.

\begin{code}
infixl 2 >>:=
(>>:=) :: IO o -> [AttrOp o] -> IO o
object >>:= attributes = do
    o <- object
    set o attributes
    return o
\end{code}
