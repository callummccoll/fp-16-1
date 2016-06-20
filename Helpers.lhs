The \highlighttt{Helpers} module provides some helper function that are used throughout the application.

\begin{code}
module Helpers where
\end{code}

\noindent The \highlighttt{changeWithPredicate p f x} function performs a change on a value only if the change satisfies some predicate.

\begin{code}
changeWithPredicate :: (a -> Bool) -> (a -> a) -> a -> a
changeWithPredicate p f x
  | p x' = x'
  | otherwise = x
  where x' = f x
\end{code}

\noindent The function executes \highlighttt{f} which is given \highlighttt{x} and returns a new value.  \highlighttt{p} is then invoked with the new value which returns a Bool indicating whether the new value is valid or not.  If the new value is valid then it is returned otherwise \highlighttt{x} is returned.

\highlighttt{toLines xs} takes a list of things that are showable and shows each one, concatenates all of them and adds a new line characters in between them.

\begin{code}
toLines :: (Show a) => [a] -> String
toLines xs = concat ((\x -> show x ++ "\n") <$> xs)
\end{code}

\noindent \highlighttt{strToInts str} converts a string to a list of ints.

\begin{code}
strToInts :: String -> IO [Int]
strToInts str = return ((\s -> read s :: Int) <$> lines str)
\end{code}
