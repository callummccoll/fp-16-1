The \highlighttt{Presentation} module defines several functions that are responsible for formatting assembly symbols so that they are presentable to the user.

\begin{code}
module Presentation where

import Assembly
import Environment
\end{code}

\noindent \highlighttt{showInstruction i} represents an \highlighttt{Instruction} in a user readable format.

\begin{code}
showInstruction :: Instruction -> String
showInstruction i = case i of
    MOVE _ s d -> presentInstruction "MOVE" s d
    ADD  _ s d -> presentInstruction "ADD" s d
    SUB _ s d  -> presentInstruction "SUB" s d
    MULT _ s d -> presentInstruction "MULT" s d
    DIV _ s d  -> presentInstruction "DIV" s d 
    MOD _ s d  -> presentInstruction "MOD" s d 
    JUMP _ v   -> "JUMP" ++ " " ++ showValue v
    BEQ _ v    -> "BEQ" ++ " " ++ showValue v
    BNE _ v    -> "BNE" ++ " " ++ showValue v
    BLT _ v    -> "BLT" ++ " " ++ showValue v
    BGT _ v    -> "BGT" ++ " " ++ showValue v
    BLE _ v    -> "BLE" ++ " " ++ showValue v
    BGE _ v    -> "BGE" ++ " " ++ showValue v
    CALL _ v   -> "CALL" ++ " " ++ showValue v
    RET _      -> "RETURN"
    HALT _     -> "HALT"
\end{code} 

\noindent \highlighttt{presentInstruction i s d} is a helper function which takes an instruction as a string, a \highlighttt{Source} and a \highlighttt{Dest}, and presents them so that they are in a format which the user can read.

\begin{code}
presentInstruction :: String -> Source -> Dest -> String
presentInstruction i s d = i ++ " " ++ showSource s ++ " " ++ showDest d
\end{code}

\noindent \highlighttt{showSource s} represents a \highlighttt{Source} in a user readable format.

\begin{code}
showSource :: Source -> String
showSource s = case sVal s of
    Left d  -> showDest d
    Right v -> showSourceValue v
\end{code}

\noindent \highlighttt{showDest d} represents a \highlighttt{Dest} in a user readable format.

\begin{code}
showDest :: Dest -> String
showDest d = case d of
    DRegister _ r -> showRegister r
    DValue _ v    -> showValue v
    DIndex _ l v  -> "(" ++ showLocation l ++ ")" ++ showValue v
    DPostInc _ l  -> showLocation l
    DPostDec _ l  -> showLocation l
    DPreInc _ l   -> showLocation l
    DPreDec _ l   -> showLocation l
    DIndirect _ l -> "(" ++ showLocation l ++ ")"
\end{code}

\noindent \highlighttt{showLocation l} represents a \highlighttt{Location} in a user readable format.

\begin{code}
showLocation :: Location -> String
showLocation l = case lLoc l of
    Left r  -> showRegister r
    Right v -> showValue v
\end{code}

\noindent \highlighttt{showSourceValue v} represent a \highlighttt{Value} which is the source of an \highlighttt{Instruction} in a user readable format.

\begin{code}
showSourceValue :: Value -> String
showSourceValue v = case vVal v of
    Left id  -> showIdentifier id
    Right ui -> "#" ++ showUInt ui
\end{code}

\noindent \highlighttt{showValue v} represents a \highlighttt{Value} which is not the source of an \highlighttt{Instruction} in a user readable format.

\begin{code}
showValue :: Value -> String
showValue v = case vVal v of
    Left id  -> showIdentifier id
    Right ui -> showUInt ui
\end{code}

\noindent \highlighttt{showIdentifier i} represents an \highlighttt{Identifier} in a user readable format.

\begin{code}
showIdentifier :: Identifier -> String
showIdentifier = idName
\end{code}

\noindent \highlighttt{showUInt ui} represents a \highlighttt{Uint} in a user readable format.

\begin{code}
showUInt :: Uint -> String
showUInt ui = show $ uiVal ui
\end{code}

\noindent \highlighttt{showRegister r} represents a \highlighttt{Register} in a user readable format.

\begin{code}
showRegister :: Register -> String
showRegister = rVal
\end{code}

\noindent \highlighttt{showCVal cv} represents a \highlighttt{CVal} in a user readable format.

\begin{code}
showCVal :: CVal -> String
showCVal cv = case cv of
    Undefined -> "-"
    Int x     -> show x
    Inst ins  -> showInstruction ins
\end{code}
