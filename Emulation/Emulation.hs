-----fno-warn-tabs

{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where

import Data.Array
import Data.Array.IO
import Environment

data Symbol = Symbol {
	label :: String,
	addr :: Int
}
	deriving Show

symTable :: [Symbol]
symTable = [(Symbol "X" 12), (Symbol "Main" 3)]


main :: IO ()
main = do
	env <- loadExmapleEnv
	
	env' <- getExeStep env 15
	
	env'' <- freezeEnv env'
	putStr (show env'')
	return ()
	
getExeStep :: Environment -> Int -> IO Environment
getExeStep env steps = case steps of
	0 -> return env
	_ -> do 
		env' <- doExecutionStep env
		getExeStep env' (steps-1)				

doExecutionStep :: Environment -> IO Environment
doExecutionStep env = do 
	let Left ram = (eRAM env)
	cell <- readArray ram (ePC env)
	let inst = words (getCellInst cell)
	putStr (show inst ++ "\n")
	readInstruction inst env
		
readInstruction :: [String] -> Environment -> IO Environment
readInstruction inst env = case inst of 
	[] -> return env ----this means the step is greater than the program allows.
	(x:xs) -> case x of
		"HALT" -> return (actionHalt env)
		"CALL" -> (actionCall (head xs) env)
		"RETURN" -> actionReturn env
		"MOVE" -> actionMove (head xs) (last xs) env
		"ADD" -> actionAdd (head xs) (last xs) env
		"SUB" -> actionSub (head xs) (last xs) env
		"MULT" -> actionMult (head xs) (last xs) env
		"DIV" -> return env
		"MOD" -> return env
		"JUMP" -> return env
		"BEQ" -> return env
		"BNE" -> return env
		"BLT" -> return env
		"BGT" -> return env
		"BLE" -> return env
		"BGE" -> return env
		_ -> return env --invalid instruction?
	
-- HALT: Halting simply returns the same machine. No action is taken.
actionHalt :: Environment -> Environment
actionHalt machine = machine

-- CALL: set PC to addr given, then place return address into SP pointer, reduce SP.
actionCall :: String -> Environment -> IO Environment
actionCall lbl env = case lbl of
	"print" -> incrementPC env
	"read" -> incrementPC env
	_ -> do
		env' <- addToStack (ePC env) env
		let addr' = (addr (getSymFromLabel lbl symTable))
		setDestValue "PC" addr' env'

-- RETURN: take the addr from where SP is pointing and put that into the PC
actionReturn :: Environment -> IO Environment
actionReturn env = do
	v <- (getSourceValue "(SP)" env)
	env' <- setDestValue "PC" v env
	env'' <- setDestValue "SP" ((eSP env')+1) env'
	env''' <- incrementPC env''
	return env'''

-- MOVE: Moving a value from src to dest. 
actionMove :: String -> String -> Environment -> IO Environment
actionMove src dest env = do
	v <- getSourceValue src env
	env' <- setDestValue dest v env
	incrementPC env'

-- Add: Adds dest to src and stores in dest
actionAdd :: String -> String -> Environment -> IO Environment
actionAdd src dest env = do
		v1 <- getSourceValue src env
		v2 <- getSourceValue dest env
		env' <- setDestValue dest (v2 + v1) env
		incrementPC env'
	
-- SUB: Subtracts dest from src and stores in dest
actionSub :: String -> String -> Environment -> IO Environment
actionSub src dest env = do
		v1 <- getSourceValue src env
		v2 <- getSourceValue dest env
		env' <- setDestValue dest (v2 - v1) env
		incrementPC env'
		
-- MULT: Multiplies dest with src and stores in dest
actionMult :: String -> String -> Environment -> IO Environment
actionMult src dest env = do
		v1 <- getSourceValue src env
		v2 <- getSourceValue dest env
		env' <- setDestValue dest (v2 * v1) env
		incrementPC env'
		
-----------------------------------------------------------
setDestValue :: String -> Int -> Environment -> IO Environment
setDestValue dest srcValue env = case dest of
	('#':xs) -> setIDestValue (convertStringToInt xs 0) srcValue env
	"(PC)" -> setIDestValue (ePC env) srcValue env -- ?
	"(SP)" -> setIDestValue (eSP env) srcValue env
	"(A)"  -> setIDestValue (getIntFromCell(eA env)) srcValue env -- ?
	"PC" -> return env {ePC = srcValue}
	"SP" -> return env {eSP = srcValue}
	"A" -> return env {eA = (getCellFromInt srcValue)}
	xs -> case xs of		--at this point we have a nnn indreicton or symbol
		('(':'P':'C':')':xs') -> setIDestValue ((ePC env)+(convertStringToInt xs' 0)) srcValue env
		('(':'S':'P':')':xs') -> setIDestValue ((eSP env)+(convertStringToInt xs' 0)) srcValue env
		('(':'A':')':xs') -> setIDestValue ((getIntFromCell(eA env))+(convertStringToInt xs' 0)) srcValue env
		xs' -> setIDestValue (addr(getSymFromLabel xs' symTable)) srcValue env
	--Need to add indrection calls -(...) +(...) (...)nnn etc
	
setIDestValue :: Int -> Int -> Environment -> IO Environment
setIDestValue addr value env = let
		Left ram = (eRAM env)
	in do
		--putStr ("=== " ++ (show addr) ++ " " ++ (show value) ++ "\n")
		writeArray ram addr (getCellFromInt value)
		return env
	
getSourceValue :: String -> Environment -> IO Int
getSourceValue src env = case src of
	('#':xs) -> return (convertStringToInt xs 0)
	"(PC)" -> getIDestValue (ePC env) env
	"(SP)" -> getIDestValue (eSP env) env
	"(A)"  -> getIDestValue (getIntFromCell (eA env)) env
	"PC" -> return (ePC env)
	"SP" -> return (eSP env)
	"A" -> return (getIntFromCell (eA env))
	xs -> case xs of		--at this point we have a nnn indreicton or symbol
		('(':'P':'C':')':xs') -> getIDestValue ((ePC env)+(convertStringToInt xs' 0)) env
		('(':'S':'P':')':xs') -> getIDestValue ((eSP env)+(convertStringToInt xs' 0)) env
		('(':'A':')':xs') -> getIDestValue ((getIntFromCell (eA env))+(convertStringToInt xs' 0)) env
		xs' -> getIDestValue (addr(getSymFromLabel xs' symTable)) env
	--Need to add indrection calls -(...) +(...) (...)nnn etc	
	
getIDestValue :: Int -> Environment -> IO Int
getIDestValue addr env = let
		Left ram = (eRAM env)
	in do
		--putStr ("=== " ++ (show addr) ++ "\n")
		cell <- (readArray ram addr)
		return (getIntFromCell cell)
		
getSymFromLabel :: String -> [Symbol] -> Symbol
getSymFromLabel lbl syms = case syms of
	(x:xs) -> if lbl == (label x)
		then x
		else getSymFromLabel lbl xs
	
-- A convenience function that puts an Int onto the stack and decrements the SP
addToStack :: Int -> Environment -> IO Environment
addToStack x env = do
	env' <- setDestValue "SP" ((eSP env)-1) env
	setDestValue "(SP)" x env'
	
-- A convenience function that increments the PC.
incrementPC :: Environment -> IO Environment
incrementPC env = setDestValue "PC" ((ePC env)+1) env

	
convertCharToInt :: Char -> Int
convertCharToInt x = case x of
		'0' -> 0
		'1' -> 1
		'2' -> 2
		'3' -> 3
		'4' -> 4
		'5' -> 5
		'6' -> 6
		'7' -> 7
		'8' -> 8
		'9' -> 9
	
convertStringToInt :: String -> Int -> Int
convertStringToInt xs num = case xs of
	"" -> num
	(x:xs) -> case x of
		'0' -> convertStringToInt xs (num * 10)
		'1' -> convertStringToInt xs ((num * 10)+1)
		'2' -> convertStringToInt xs ((num * 10)+2)
		'3' -> convertStringToInt xs ((num * 10)+3)
		'4' -> convertStringToInt xs ((num * 10)+4)
		'5' -> convertStringToInt xs ((num * 10)+5)
		'6' -> convertStringToInt xs ((num * 10)+6)
		'7' -> convertStringToInt xs ((num * 10)+7)
		'8' -> convertStringToInt xs ((num * 10)+8)
		'9' -> convertStringToInt xs ((num * 10)+9)
		_ -> convertStringToInt xs num


		



	

	
	