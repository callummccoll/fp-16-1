Provdes some helper function that are used throughout the application.

\begin{code}
module Helpers where

import Operators

import "gtk3" Graphics.UI.Gtk
import Data.IORef
\end{code}

Execute a function which changes the value of something based on the current
value, however, only apply the change if it satisfies the predicate function.
If it does not then the original value is returned, otherwise the new value is
returned.

\begin{code}
changeWithPredicate :: (a -> Bool) -> (a -> a) -> a -> a
changeWithPredicate p f x
  | p x' = x'
  | otherwise = x
  where x' = f x
\end{code}

Take a list of things that are showable and show each one, concatenate all of
them and add new line characters in between them.  Returns the resulting string.

\begin{code}
toLines :: (Show a) => [a] -> String
toLines xs = concat ((\x -> show x ++ "\n") <$> xs)
\end{code}
