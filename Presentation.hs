module Presentation where

import Assembly

showInstruction :: Instruction -> String
showInstruction i = case i of
    MOVE _ s d -> presentInstruction "MOVE" s d
    ADD  _ s d -> presentInstruction "ADD" s d
    SUB _ s d  -> presentInstruction "SUB" s d
    MULT _ s d -> presentInstruction "MULT" s d
    DIV _ s d  -> presentInstruction "DIV" s d 
    MOD _ s d  -> presentInstruction "MOD" s d 
    JUMP _ v   -> "JUMP" ++ " " ++ (showValue v)
    BEQ _ v    -> "BEQ" ++ " " ++ (showValue v)
    BNE _ v    -> "BNE" ++ " " ++ (showValue v)
    BLT _ v    -> "BLT" ++ " " ++ (showValue v)
    BGT _ v    -> "BGT" ++ " " ++ (showValue v)
    BLE _ v    -> "BLE" ++ " " ++ (showValue v)
    BGE _ v    -> "BGE" ++ " " ++ (showValue v)
    CALL _ v   -> "CALL" ++ " " ++ (showValue v)
    RET _      -> "RETURN"
    HALT _     -> "HALT"
 
presentInstruction :: String -> Source -> Dest -> String
presentInstruction i s d = i ++ " " ++ (showSource s) ++ " " ++ (showDest d)

showSource :: Source -> String
showSource s = case (sVal s) of
    Left d  -> showDest d
    Right v -> showSourceValue v

showDest :: Dest -> String
showDest d = case d of
    DRegister _ r -> showRegister r
    DValue _ v    -> showValue v
    DIndex _ l v  -> "(" ++ (showLocation l) ++ ")" ++ (showValue v)
    DPostInc _ l  -> showLocation l
    DPostDec _ l  -> showLocation l
    DPreInc _ l   -> showLocation l
    DPreDec _ l   -> showLocation l
    DIndirect _ l -> "(" ++ (showLocation l) ++ ")"

showLocation :: Location -> String
showLocation l = case (lLoc l) of
    Left r  -> showRegister r
    Right v -> showValue v

showSourceValue :: Value -> String
showSourceValue v = case (vVal v) of
    Left id  -> showIdentifier id
    Right ui -> "#" ++ (showUInt ui)

showValue :: Value -> String
showValue v = case (vVal v) of
    Left id  -> showIdentifier id
    Right ui -> showUInt ui

showIdentifier :: Identifier -> String
showIdentifier id = idName id

showUInt :: Uint -> String
showUInt ui = show $ uiVal ui

showRegister :: Register -> String
showRegister r = (rVal r)
