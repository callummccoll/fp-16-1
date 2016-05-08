-----fno-warn-tabs

{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where

import Data.Array
import Data.Array.IO
import Environment

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
		"DIV" -> actionDiv (head xs) (last xs) env
		"MOD" -> actionMod (head xs) (last xs) env
		"JUMP" -> actionJump (head xs) env
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
actionCall name env = case name of
	"print" -> let
			env' = functionPrint (getIntFromCell (eA env)) env
		in
			incrementPC env'
	"read" -> let
			env' = functionRead env
		in
			incrementPC env'
	_ -> do
		env' <- addToStack (ePC env) env
		let addr' = (sAddr (getSymFromLabel name (eSymTable env')))
		setDestValue "PC" addr' env'

-- RETURN: take the addr from where SP is pointing and put that into the PC
actionReturn :: Environment -> IO Environment
actionReturn env = do
		tS <- (getSourceValue "(SP)" env)
		let env' = fst tS
		let v = snd tS
		env'' <- setDestValue "PC" v env
		env''' <- setDestValue "SP" ((eSP env'')+1) env''
		env'''' <- incrementPC env'''
		return env''''

-- MOVE: Moving a value from src to dest. 
actionMove :: String -> String -> Environment -> IO Environment
actionMove src dest env = do
		tS <- getSourceValue src env
		let env' = fst tS
		let v = snd tS
		env'' <- setDestValue dest v env'
		incrementPC env''

-- Add: Adds dest to src and stores in dest
actionAdd :: String -> String -> Environment -> IO Environment
actionAdd src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	tD <- getSourceValue dest env'
	let	env'' = fst tD
	let	vD = snd tD
	env''' <- setDestValue dest (vD + vS) env''
	incrementPC env'''
	
-- SUB: Subtracts src from dest and stores in dest
actionSub :: String -> String -> Environment -> IO Environment
actionSub src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	tD <- getSourceValue dest env'
	let	env'' = fst tD
	let	vD = snd tD	
	env''' <- setDestValue dest (vD - vS) env''
	incrementPC env'''
		
-- MULT: Multiplies dest with src and stores in dest
actionMult :: String -> String -> Environment -> IO Environment
actionMult src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	tD <- getSourceValue dest env'
	let	env'' = fst tD
	let	vD = snd tD
	env''' <- setDestValue dest (vD * vS) env''
	incrementPC env'''
		
-- DIV: Intger Divides dest with src and stores in dest
actionDiv :: String -> String -> Environment -> IO Environment
actionDiv src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	tD <- getSourceValue dest env'
	let	env'' = fst tD
	let	vD = snd tD
	env''' <- setDestValue dest (div vD vS) env''
	incrementPC env'''
		
-- MOD: Does dest mod src and stores in dest
actionMod :: String -> String -> Environment -> IO Environment
actionMod src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	tD <- getSourceValue dest env'
	let	env'' = fst tD
	let	vD = snd tD
	env''' <- setDestValue dest (mod vD vS) env''
	incrementPC env'''

-- JUMP: Sets PC to address it is given.
actionJump :: String -> Environment -> IO Environment
actionJump addr env = setDestValue "PC" (getAddress addr env) env

-- BEQ: If Accumulator == 0, PC is set to addr given
actionBEQ :: String -> Environment -> IO Environment
actionBEQ addr env = do
	tS <- getSourceValue "A" env
	let env' = fst tS
	let aV = snd tS
	if aV == 0
	then setDestValue "PC" (getAddress addr env') env'
	else incrementPC env'
		
-- BNE: If Accumulator != 0, PC is set to addr given
actionBNE :: String -> Environment -> IO Environment
actionBNE addr env = do
	tS <- getSourceValue "A" env
	let env' = fst tS
	let aV = snd tS
	if aV /= 0
	then setDestValue "PC" (getAddress addr env') env'
	else incrementPC env'
		
-- BLT: If Accumulator < 0, PC is set to addr given
actionBLT :: String -> Environment -> IO Environment
actionBLT addr env = do
	tS <- getSourceValue "A" env
	let env' = fst tS
	let aV = snd tS
	if aV < 0
	then setDestValue "PC" (getAddress addr env') env'
	else incrementPC env'

-- BGT: If Accumulator > 0, PC is set to addr given
actionBGT :: String -> Environment -> IO Environment
actionBGT addr env = do
	tS <- getSourceValue "A" env
	let env' = fst tS
	let aV = snd tS
	if aV > 0
	then setDestValue "PC" (getAddress addr env') env'
	else incrementPC env'

-- BLE: If Accumulator <= 0, PC is set to addr given
actionBLE :: String -> Environment -> IO Environment
actionBLE addr env = do
	tS <- getSourceValue "A" env
	let env' = fst tS
	let aV = snd tS
	if aV <= 0
	then setDestValue "PC" (getAddress addr env') env'
	else incrementPC env'

-- BGE: If Accumulator >= 0, PC is set to addr given
actionBGE :: String -> Environment -> IO Environment
actionBGE addr env = do
	tS <- getSourceValue "A" env
	let env' = fst tS
	let aV = snd tS
	if aV >= 0
	then setDestValue "PC" (getAddress addr env') env'
	else incrementPC env'

-- Print: Takes the given Int and puts it into the StdOut
functionPrint :: Int -> Environment -> Environment
functionPrint x env = env {eStdOut = ((eStdOut env) ++ [x])}

-- Read: Takes an element from StdIn and returns it. If StdIn is empty, will return Undefined.
functionRead :: Environment -> Environment
functionRead env = let
		stdIn = (eStdIn env)
	in case stdIn of
		[] -> env {eA = getCellFromUndefined}
		(x:xs) -> env {eA = (getCellFromInt x)}
		
-----------------------------------------------------------
setDestValue :: String -> Int -> Environment -> IO Environment
setDestValue dest srcValue env = case dest of
	('#':xs) -> setIDestValue (convertStringToInt xs 0) srcValue env
	"(PC)" -> setIDestValue (ePC env) srcValue env
	"(SP)" -> setIDestValue (eSP env) srcValue env
	"(A)"  -> setIDestValue (getIntFromCell(eA env)) srcValue env
	"+(PC)" -> do
		env' <- setDestValue "PC" ((ePC env)+1) env
		setIDestValue (ePC env') srcValue env'
	"+(SP)" -> do
		env' <- setDestValue "SP" ((eSP env)+1) env
		setIDestValue (eSP env') srcValue env'
	"+(A)"  -> do
		env' <- setDestValue "A" ((getIntFromCell (eA env))+1) env
		setIDestValue (getIntFromCell(eA env')) srcValue env'
	"-(PC)" -> do
		env' <- setDestValue "PC" ((ePC env)-1) env
		setIDestValue (ePC env') srcValue env'
	"-(SP)" -> do
		env' <- setDestValue "SP" ((eSP env)-1) env
		setIDestValue (eSP env') srcValue env'
	"-(A)"  -> do
		env' <- setDestValue "A" ((getIntFromCell (eA env))-1) env
		setIDestValue (getIntFromCell(eA env')) srcValue env'
	"(PC)+" -> do
		env' <- setIDestValue (ePC env) srcValue env
		setDestValue "PC" ((ePC env)+1) env'
	"(SP)+" -> do
		env' <- setIDestValue (eSP env) srcValue env
		setDestValue "SP" ((eSP env)+1) env'
	"(A)+"  -> do
		env' <- setIDestValue (getIntFromCell(eA env)) srcValue env
		setDestValue "A" ((getIntFromCell (eA env))+1) env'
	"(PC)-" -> do
		env' <- setIDestValue (ePC env) srcValue env
		setDestValue "PC" ((ePC env)-1) env'
	"(SP)-" -> do
		env' <- setIDestValue (eSP env) srcValue env
		setDestValue "SP" ((eSP env)-1) env'
	"(A)-"  -> do
		env' <- setIDestValue (getIntFromCell(eA env)) srcValue env
		setDestValue "A" ((getIntFromCell (eA env))-1) env'
	"PC" -> return env {ePC = srcValue}
	"SP" -> return env {eSP = srcValue}
	"A" -> return env {eA = (getCellFromInt srcValue)}
	xs -> case xs of		--at this point we have a nnn indreicton or symbol
		('(':'P':'C':')':xs') -> setIDestValue ((ePC env)+(convertStringToInt xs' 0)) srcValue env
		('(':'S':'P':')':xs') -> setIDestValue ((eSP env)+(convertStringToInt xs' 0)) srcValue env
		('(':'A':')':xs') -> setIDestValue ((getIntFromCell(eA env))+(convertStringToInt xs' 0)) srcValue env
		xs' -> setIDestValue (sAddr(getSymFromLabel xs' (eSymTable env))) srcValue env
	
setIDestValue :: Int -> Int -> Environment -> IO Environment
setIDestValue addr value env = let
		Left ram = (eRAM env)
	in do
		writeArray ram addr (getCellFromInt value)
		return env
	
getSourceValue :: String -> Environment -> IO (Environment, Int)
getSourceValue src env = case src of
	('#':xs) -> return (env, (convertStringToInt xs 0))
	"(PC)" -> do
		v <- getIDestValue (ePC env) env
		return (env, v)
	"(SP)" -> do
		v <- getIDestValue (eSP env) env
		return (env, v)
	"(A)"  -> do
		v <- getIDestValue (getIntFromCell (eA env)) env
		return (env, v)
	"+(PC)" -> do
		env' <- setDestValue "PC" ((ePC env)+1) env
		v <- getIDestValue (ePC env') env'
		return (env', 0)
	"+(SP)" -> do
		env' <- setDestValue "SP" ((eSP env)+1) env
		v <- getIDestValue (eSP env') env'
		return (env', 0)
	"+(A)"  -> do
		env' <- setDestValue "A" ((getIntFromCell (eA env))+1) env
		v <- getIDestValue (getIntFromCell (eA env')) env'
		return (env', 0)
	"-(PC)" -> do
		v <- getIDestValue (ePC env) env
		env' <- setDestValue "PC" ((ePC env)-1) env
		return (env', 0)
	"-(SP)" -> do
		v <- getIDestValue (eSP env) env
		env' <- setDestValue "SP" ((eSP env)-1) env
		return (env', 0)
	"-(A)"  -> do
		env' <- setDestValue "A" ((getIntFromCell (eA env))-1) env
		v <- getIDestValue (getIntFromCell (eA env')) env'		
		return (env', 0)	
	"(PC)+" -> do
		env' <- setDestValue "PC" ((ePC env)+1) env
		v <- getIDestValue (ePC env') env'
		return (env', 0)
	"(SP)+" -> do
		env' <- setDestValue "SP" ((eSP env)+1) env
		v <- getIDestValue (eSP env') env'
		return (env', 0)
	"(A)+"  -> do
		v <- getIDestValue (getIntFromCell (eA env)) env
		env' <- setDestValue "A" ((getIntFromCell (eA env))+1) env
		return (env', 0)
	"(PC)-" -> do
		v <- getIDestValue (ePC env) env
		env' <- setDestValue "PC" ((ePC env)-1) env
		return (env', 0)
	"(SP)-" -> do
		v <- getIDestValue (eSP env) env
		env' <- setDestValue "SP" ((eSP env)-1) env
		return (env', 0)
	"(A)-"  -> do
		v <- getIDestValue (getIntFromCell (eA env)) env
		env' <- setDestValue "A" ((getIntFromCell (eA env))-1) env
		return (env', 0)
	"PC" -> return (env, (ePC env))
	"SP" -> return (env, (eSP env))
	"A" -> return (env, (getIntFromCell (eA env)))
	xs -> case xs of		--at this point we have a nnn indreicton or symbol
		('(':'P':'C':')':xs') -> do
			v <- getIDestValue ((ePC env)+(convertStringToInt xs' 0)) env
			return (env, v)
		('(':'S':'P':')':xs') -> do
			v <- getIDestValue ((eSP env)+(convertStringToInt xs' 0)) env
			return (env, v)
		('(':'A':')':xs') -> do
			v <- getIDestValue ((getIntFromCell (eA env))+(convertStringToInt xs' 0)) env
			return (env, v)
		('(':xs') -> let -- Label with indirection
				lbl = reverse (tail (reverse (xs')))
				v = getAddress lbl env
			in do
				addr <- getIDestValue v env
				v' <- getIDestValue addr env
				return (env, v')				
		xs' -> do
			v <- getIDestValue (sAddr(getSymFromLabel xs' (eSymTable env))) env
			return (env, v)

getIDestValue :: Int -> Environment -> IO Int
getIDestValue addr env = let
		Left ram = (eRAM env)
	in do
		cell <- (readArray ram addr)
		return (getIntFromCell cell)
		
getAddress :: String -> Environment -> Int
getAddress x env = let
		exists = symExists x (eSymTable env)
	in if exists == True
		then (sAddr (getSymFromLabel x (eSymTable env)))
		else convertStringToInt x 0
	
-- A convenience function that puts an Int onto the stack and decrements the SP
addToStack :: Int -> Environment -> IO Environment
addToStack x env = do
	env' <- setDestValue "SP" ((eSP env)-1) env
	setDestValue "(SP)" x env'
	
-- A convenience function that increments the PC.
incrementPC :: Environment -> IO Environment
incrementPC env = setDestValue "PC" ((ePC env)+1) env
	
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


		



	

	
	