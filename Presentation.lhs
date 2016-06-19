Defines several functions that are responsible for formatting assembly symbols,
so that they are presentable to the user.

\begin{code}
module Presentation where

import Assembly
import Environment
\end{code}

Represent an Instruction in a user readable format.

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

A helper function which takes a instruction as a string, a Source and a Dest,
and presents them so that they are in a format which the user can read.

\begin{code}
presentInstruction :: String -> Source -> Dest -> String
presentInstruction i s d = i ++ " " ++ showSource s ++ " " ++ showDest d
\end{code}

Represent a Source in a user readable format.

\begin{code}
showSource :: Source -> String
showSource s = case sVal s of
    Left d  -> showDest d
    Right v -> showSourceValue v
\end{code}

Represent a Dest in a user readable format.

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

Represent a Location in a user readable format.

\begin{code}
showLocation :: Location -> String
showLocation l = case lLoc l of
    Left r  -> showRegister r
    Right v -> showValue v
\end{code}

Represent a Value which is the source of an Instruction in a user readable
format.

\begin{code}
showSourceValue :: Value -> String
showSourceValue v = case vVal v of
    Left id  -> showIdentifier id
    Right ui -> "#" ++ showUInt ui
\end{code}

Represent a Value which is not the source of an Instruction in a user readable
format.

\begin{code}
showValue :: Value -> String
showValue v = case vVal v of
    Left id  -> showIdentifier id
    Right ui -> showUInt ui
\end{code}

Represent an Identifier in a user readable format.

\begin{code}
showIdentifier :: Identifier -> String
showIdentifier = idName
\end{code}

Represent a Uint in a user readable format.

\begin{code}
showUInt :: Uint -> String
showUInt ui = show $ uiVal ui
\end{code}

Represent a Register in a user readable format.

\begin{code}
showRegister :: Register -> String
showRegister = rVal
\end{code}

Represent a CVal in a user readable format.

\begin{code}
showCVal :: CVal -> String
showCVal cv = case cv of
    Undefined -> "-"
    Int x     -> show x
    Inst ins  -> showInstruction ins
\end{code}
