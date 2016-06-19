This modules defines several operators which provide a nicer syntax and the
ability to chain common operations for gtk applications.

\begin{code}
module Operators where

import "gtk3" Graphics.UI.Gtk
\end{code}

Add the given widget to the given container.

\begin{code}
infixl 2 >|>
(>|>) :: (ContainerClass c, WidgetClass w) => w -> c -> IO c
widget >|> container = do
    containerAdd container widget
    return container
\end{code}

Add the given widget to the given container that is wrapped in the IO monad.  
This function returns the container wrapped in the IO monad.  By doing this we
can chain these operations and add multiple widgets to multiple containers on a
single line.

\begin{code}
infixl 2 >|>>
(>|>>) :: (ContainerClass c, WidgetClass w) => w -> IO c -> IO c
widget >|>> container = do
    c <- container
    containerAdd c widget
    return c
\end{code}

Add the given widget that is wrapped in the IO monad to the given container.
This function returns the container wrapped in the IO monad.  By doing this we
can chain these operations and add multiple widgets to multiple containers on a
single line.

\begin{code}
infixl 2 >>|>
(>>|>) :: (ContainerClass c, WidgetClass w) => IO w -> c -> IO c
widget >>|> container = do
    widget >>= containerAdd container
    return container
\end{code}

Add the given widget which is wrapped in the IO monad to the given container
which is also wrapped in the IO monad.  This function returns the container
wrapped in the IO monad.  By doing this we can chain these operations and add
multiple widgets to multiple containers on a single line.

\begin{code}
infixl 2 >>|>>
(>>|>>) :: (ContainerClass c, WidgetClass w) => IO w -> IO c -> IO c
widget >>|>> container = do
    w <- widget
    c <- container
    containerAdd c w
    return c
\end{code}

Add the given widget to the given container.  This function returns the widget
wrapped in the IO monad.  By doing this we can chain these operations and add
multiple widgets to multiple containers on a single line.

\begin{code}
infixl 1 <|<
(<|<) :: (ContainerClass c, WidgetClass w) => c -> w -> IO w
container <|< widget = do
    containerAdd container widget
    return widget
\end{code}

Add the given widget to the given container that is wrapped in the IO monad.  
This function returns the widget wrapped in the IO monad.  By doing this we can
chain these operations and add multiple widgets to multiple containers on a
single line.

\begin{code}
infixl 1 <<|<
(<<|<) :: (ContainerClass c, WidgetClass w) => IO c -> w -> IO w
container <<|< widget = do
    c <- container
    containerAdd c widget
    return widget
\end{code}

Add the given widget that is wrapped in the IO monad to the given container.
This function returns the widget wrapped in the IO monad.  By doing this we can
chain these operations and add multiple widgets to multiple containers on a
single line.

\begin{code}
infixl 1 <|<<
(<|<<) :: (ContainerClass c, WidgetClass w) => c -> IO w -> IO w
container <|<< widget = do
    widget >>= containerAdd container
    widget
\end{code}

Add the given widget that is wrapped in the IO monad to the given container that
is also wrapped in the IO monad.  This function returns the widget wrapped in
the IO monad.  By doing this we can chain these operations and add multiple
widgets to multiple containers on a single line.

\begin{code}
infixl 1 <<|<<
(<<|<<) :: (ContainerClass c, WidgetClass w) => IO c -> IO w -> IO w
container <<|<< widget = do
    c <- container
    w <- widget
    containerAdd c w
    return w
\end{code}

Set a list of attributes on the given object.  This function returns the object
wrapped in the IO monad which should allow you to chain this with other
operators.

\begin{code}
infixl 2 >:=
(>:=) :: o -> [AttrOp o] -> IO o
object >:= attributes = do
    set object attributes
    return object
\end{code}

Set a list of attributes on the given object that is wrapped in the IO monad.
This function returns the object wrapped in the IO monad which should allow you
to chain this with other operators.

\begin{code}
infixl 2 >>:=
(>>:=) :: IO o -> [AttrOp o] -> IO o
object >>:= attributes = do
    o <- object
    set o attributes
    return o
\end{code}
