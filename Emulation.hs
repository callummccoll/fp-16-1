----
-- Compiling:
-- ghc Emulation.hs -fno-warn-tabs -i../../machine/parser

-- Windows
-- Emulation.exe < test.ass

{-# LANGUAGE ScopedTypeVariables #-}

module Emulation where
--module Main (main) where

import Data.Array
import Data.Array.IO
import Environment
import SymbolTable
import System.IO

import Assembly

getFullProgEnv :: Environment -> IO (Array Int Environment)
getFullProgEnv env = case (eRAM env) of
	Left ram -> do
		-- Store the initial state of the environment
		env' <- eFreezeEnv env
		-- Send it through the emulator to store all the steps.
		envs <- getProgList env 0 [env']
		return (listArray (0, ((length envs)-1)) envs)
	Right r -> error $ "Cannot Emulate with frozen RAM"
	
getProgList :: Environment -> Int -> [Environment] -> IO [Environment]
getProgList env count envs = do
	env' <- getExeStep env 1
	if (ePC env') == (ePC env)
	then do
		return envs
	else do
		env'' <- eFreezeEnv env'
		getProgList env' (count+1) (envs ++ [env''])
	
getExeStep :: Environment -> Int -> IO Environment
getExeStep env steps = case steps of
	0 -> return env
	_ -> do 
		env' <- doExecutionStep env
		getExeStep env' (steps-1)				

doExecutionStep :: Environment -> IO Environment
doExecutionStep env = case (eRAM env) of 
	Left ram -> do
		cell <- readArray ram (ePC env)
		case (cVal cell) of
			Int i -> error $ "Memory Error: " ++ show i
			Inst i -> do
				--putStr (getStringFromCVal (cVal cell) ++ "\n")
				--putStr (show inst ++ "\n")
				readInstruction i env
			_ -> error $ "Memory Error: Undefined"
	Right ran -> error $ "Cannot Emulate with frozen RAM"
	
		
readInstruction :: Instruction -> Environment -> IO Environment
readInstruction inst env = case inst of
		MOVE _ s d -> actionMove s d env
		ADD _ s d -> actionAdd s d env
		SUB _ s d -> actionSub s d env
		MULT _ s d -> actionMult s d env
		DIV _ s d -> actionDiv s d env
		MOD _ s d -> actionMod s d env
		JUMP _ v -> actionJump v env
		CALL _ v -> actionCall v env
		BEQ _ v -> actionBEQ v env
		BNE _ v -> actionBNE v env
		BLT _ v -> actionBLT v env
		BGT _ v -> actionBGT v env
		BLE _ v -> actionBLE v env
		BGE _ v -> actionBGE v env
		RET _ -> actionReturn env
		HALT _ -> return (actionHalt env)
		

-- HALT: Halting simply returns the same machine. No action is taken.
actionHalt :: Environment -> Environment
actionHalt machine = machine

-- CALL: set PC to addr given, then place return address into SP pointer, reduce SP.
actionCall :: Value -> Environment -> IO Environment
actionCall name env = case (vVal name) of
	Left iden -> do
		let n = (idName iden)
		case n of
			"print" -> let 
					v = eA env
				in case v of 
					Int i -> do
						let env' = functionPrint i env
						incrementPC env'
					i -> error $ "Accumulator Error: " ++ getStringFromCVal i
			"read" -> do
				let env' = functionRead env
				case eA env' of
					Undefined -> return env'
					Int _ -> incrementPC env'
					Inst _ -> return env'
			_ -> do
				env' <- addToStack (ePC env) env
				let addr = (getAddress n env')
				let d = DRegister (vPos name) (Register (vPos name) "PC")
				setDestValue d addr env'
	Right ui -> do
		let x = (uiVal ui)
		let d = DRegister (vPos name) (Register (vPos name) "PC")
		setDestValue d x env

-- RETURN: take the addr from where SP is pointing and put that into the PC
actionReturn :: Environment -> IO Environment
actionReturn env = do
		let s = Source (0,0) (Left (DIndirect (0,0) (Location (0,0) (Left (Register (0,0) "SP")))))
		tS <- getSourceValue s env
		let env' = fst tS
		let v = snd tS
		let d = DRegister (0,0) (Register (0,0) "PC")
		env'' <- setDestValue d v env'
		let d' = DRegister (0,0) (Register (0,0) "SP")
		env''' <- setDestValue d' ((eSP env'')+1) env''
		env'''' <- incrementPC env'''
		return env''''

-- MOVE: Moving a value from src to dest. 
actionMove :: Source -> Dest -> Environment -> IO Environment
actionMove src dest env = do
		tS <- getSourceValue src env
		let env' = fst tS
		let v = snd tS
		env'' <- setDestValue dest v env'
		incrementPC env''

-- Add: Adds dest to src and stores in dest
actionAdd :: Source -> Dest -> Environment -> IO Environment
actionAdd src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	let s = Source (dPos dest) (Left dest)
	tD <- getSourceValue s env'
	let	env'' = fst tD
	let	vD = snd tD	
	env''' <- setDestValue dest (vD + vS) env''
	incrementPC env'''

-- SUB: Subtracts src from dest and stores in dest
actionSub :: Source -> Dest -> Environment -> IO Environment
actionSub src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	let s = Source (dPos dest) (Left dest)
	tD <- getSourceValue s env'
	let	env'' = fst tD
	let	vD = snd tD	
	env''' <- setDestValue dest (vD - vS) env''
	incrementPC env'''
		
-- MULT: Multiplies dest with src and stores in dest
actionMult :: Source -> Dest -> Environment -> IO Environment
actionMult src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	let s = Source (dPos dest) (Left dest)
	tD <- getSourceValue s env'
	let	env'' = fst tD
	let	vD = snd tD
	env''' <- setDestValue dest (vD * vS) env''
	incrementPC env'''
		
-- DIV: Intger Divides dest with src and stores in dest
actionDiv :: Source -> Dest -> Environment -> IO Environment
actionDiv src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	let s = Source (dPos dest) (Left dest)
	tD <- getSourceValue s env'
	let	env'' = fst tD
	let	vD = snd tD
	env''' <- setDestValue dest (div vD vS) env''
	incrementPC env'''
		
-- MOD: Does dest mod src and stores in dest
actionMod :: Source -> Dest -> Environment -> IO Environment
actionMod src dest env = do
	tS <- getSourceValue src env
	let env' = fst tS
	let vS = snd tS
	let s = Source (dPos dest) (Left dest)
	tD <- getSourceValue s env'
	let	env'' = fst tD
	let	vD = snd tD
	env''' <- setDestValue dest (mod vD vS) env''
	incrementPC env'''

-- JUMP: Sets PC to address it is given.
actionJump :: Value -> Environment -> IO Environment
actionJump addr env = do
	let dPC = DRegister (vPos addr) (Register (vPos addr) "PC")
	let s = Source (vPos addr) (Left (DValue (vPos addr) addr))
	x <- getSourceValue s env
	setDestValue dPC (snd x) (fst x)

-- BEQ: If Accumulator == 0, PC is set to addr given
actionBEQ :: Value -> Environment -> IO Environment
actionBEQ addr env = do
	let s = Source (0,0) (Left (DRegister (0,0) (Register (0,0) "A")))
	tS <- getSourceValue s env
	let env' = fst tS
	let aV = snd tS
	if aV == 0
	then case (vVal addr) of
		Left iden -> do 
			let s = Source (0,0) (Left (DValue (0,0) addr))
			x <- getSourceValue s env
			return (fst x) {ePC = (snd x)}
		Right ui -> return env {ePC = (uiVal ui)}
	else incrementPC env'
	
-- BNE: If Accumulator != 0, PC is set to addr given
actionBNE :: Value -> Environment -> IO Environment
actionBNE addr env = do
	let s = Source (0,0) (Left (DRegister (0,0) (Register (0,0) "A")))
	tS <- getSourceValue s env
	let env' = fst tS
	let aV = snd tS
	if aV /= 0
	then case (vVal addr) of
		Left iden -> do 
			let s = Source (0,0) (Left (DValue (0,0) addr))
			x <- getSourceValue s env
			return (fst x) {ePC = (snd x)}
		Right ui -> return env {ePC = (uiVal ui)}
	else incrementPC env'
		
-- BLT: If Accumulator < 0, PC is set to addr given
actionBLT :: Value -> Environment -> IO Environment
actionBLT addr env = do
	let s = Source (0,0) (Left (DRegister (0,0) (Register (0,0) "A")))
	tS <- getSourceValue s env
	let env' = fst tS
	let aV = snd tS
	if aV <= 0
	then case (vVal addr) of
		Left iden -> do 
			let s = Source (0,0) (Left (DValue (0,0) addr))
			x <- getSourceValue s env
			return (fst x) {ePC = (snd x)}
		Right ui -> return env {ePC = (uiVal ui)}
	else incrementPC env'

-- BGT: If Accumulator > 0, PC is set to addr given
actionBGT :: Value -> Environment -> IO Environment
actionBGT addr env = do
	let s = Source (0,0) (Left (DRegister (0,0) (Register (0,0) "A")))
	tS <- getSourceValue s env
	let env' = fst tS
	let aV = snd tS
	if aV >= 0
	then case (vVal addr) of
		Left iden -> do 
			let s = Source (0,0) (Left (DValue (0,0) addr))
			x <- getSourceValue s env
			return (fst x) {ePC = (snd x)}
		Right ui -> return env {ePC = (uiVal ui)}
	else incrementPC env'

-- BLE: If Accumulator <= 0, PC is set to addr given
actionBLE :: Value -> Environment -> IO Environment
actionBLE addr env = do
	let s = Source (0,0) (Left (DRegister (0,0) (Register (0,0) "A")))
	tS <- getSourceValue s env
	let env' = fst tS
	let aV = snd tS
	if aV <= 0
	then case (vVal addr) of
		Left iden -> do 
			let s = Source (0,0) (Left (DValue (0,0) addr))
			x <- getSourceValue s env
			return (fst x) {ePC = (snd x)}
		Right ui -> return env {ePC = (uiVal ui)}
	else incrementPC env'

-- BGE: If Accumulator >= 0, PC is set to addr given
actionBGE :: Value -> Environment -> IO Environment
actionBGE addr env = do
	let s = Source (0,0) (Left (DRegister (0,0) (Register (0,0) "A")))
	tS <- getSourceValue s env
	let env' = fst tS
	let aV = snd tS
	if aV >= 0
	then case (vVal addr) of
		Left iden -> do 
			let s = Source (0,0) (Left (DValue (0,0) addr))
			x <- getSourceValue s env
			return (fst x) {ePC = (snd x)}
		Right ui -> return env {ePC = (uiVal ui)}
	else incrementPC env'

-- Print: Takes the given Int and puts it into the StdOut
functionPrint :: Int -> Environment -> Environment
functionPrint x env = env {eStdOut = ((eStdOut env) ++ [x])}

-- Read: Takes an element from StdIn and returns it. If StdIn is empty, will return Undefined.
functionRead :: Environment -> Environment
functionRead env = let
		stdIn = (eStdIn env)
	in case stdIn of
		[] -> env {eA = Undefined}
		(x:xs) -> env {eA = (Int x)}

-----------------------------------------------------------
setDestValue :: Dest -> Int -> Environment -> IO Environment
setDestValue dest srcValue env = case dest of
	DRegister _ r -> case (rVal r) of 
		"PC" -> return env {ePC = srcValue}
		"SP" -> return env {eSP = srcValue}
		"A" -> return env {eA = (Int srcValue)}
	DValue _ v -> case (vVal v) of 
		Left iden -> setIDestValue (getAddress (idName iden) env) srcValue env
		Right ui -> setIDestValue (uiVal ui) srcValue env
	DIndex p l v -> do
		let s = Source p (Right v) -- first find what v is 
		x <- getSourceValue s env
		case (lLoc l) of 
			Left reg -> let
					d' = (DRegister p reg)
					s' = Source p (Left d')
				in do
					x' <- getSourceValue s' env
					let dv = DValue p (Value p (Right (Uint p ((snd x') + (snd x)))))
					setDestValue dv srcValue env
			Right v -> let
					d' = (DValue p v)
					s' = Source p (Left d')
				in do
					x' <- getSourceValue s' env
					let dv = DValue p (Value p (Right (Uint p ((snd x') + (snd x)))))
					setDestValue dv srcValue env
	DPostInc p l -> case (lLoc l) of 
		Left reg -> let
				id = DIndirect p (Location p (Left reg))
				d = (DRegister p reg)
				s = Source p (Left d)
			in do
				env' <- setDestValue id srcValue env
				x <- getSourceValue s env'
				setDestValue d ((snd x)+1) (fst x)
		Right v -> let				
				id = DIndirect p (Location p (Right v))
				d = (DValue p v)
				s = Source p (Left d)
			in do
				env' <- setDestValue id srcValue env
				x <- getSourceValue s env'
				setDestValue d ((snd x)+1) (fst x)
	DPostDec p l -> case (lLoc l) of 
		Left reg -> let
				id = DIndirect p (Location p (Left reg))
				d = (DRegister p reg)
				s = Source p (Left d)
			in do
				env' <- setDestValue id srcValue env
				x <- getSourceValue s env'
				setDestValue d ((snd x)-1) (fst x)
		Right v -> let				
				id = DIndirect p (Location p (Right v))
				d = (DValue p v)
				s = Source p (Left d)
			in do
				env' <- setDestValue id srcValue env
				x <- getSourceValue s env'
				setDestValue d ((snd x)-1) (fst x)
	DPreInc p l -> case (lLoc l) of 
		Left reg -> do
			let d = (DRegister p reg)
			let s = Source p (Left d)
			x <- getSourceValue s env
			env' <- setDestValue d ((snd x)+1) (fst x)
			let id = DIndirect p (Location p (Left reg))
			setDestValue id srcValue env'
		Right v -> do
			let d = (DValue p v)
			let s = Source p (Left d)
			x <- getSourceValue s env
			env' <- setDestValue d ((snd x)+1) (fst x)
			let id = DIndirect p (Location p (Right v))
			setDestValue id srcValue env'
	DPreDec p l -> case (lLoc l) of 
		Left reg -> do
			let d = (DRegister p reg)
			let s = Source p (Left d)
			x <- getSourceValue s env
			env' <- setDestValue d ((snd x)-1) (fst x)
			let id = DIndirect p (Location p (Left reg))
			setDestValue id srcValue env'
		Right v -> do
			let d = (DValue p v)
			let s = Source p (Left d)
			x <- getSourceValue s env
			env' <- setDestValue d ((snd x)-1) (fst x)
			let id = DIndirect p (Location p (Right v))
			setDestValue id srcValue env'
	DIndirect p l -> case (lLoc l) of 
		Left reg -> let
				d = (DRegister p reg)
				s = Source p (Left d)
			in do
				x <- getSourceValue s env
				let dv = DValue p (Value p (Right (Uint p (snd x))))
				setDestValue dv srcValue (fst x)
		Right v -> let
				d = (DValue p v)
				s = Source p (Left d)
			in do
				x <- getSourceValue s env
				let dv = DValue p (Value p (Right (Uint p (snd x))))
				setDestValue dv srcValue (fst x)
	
setIDestValue :: Int -> Int -> Environment -> IO Environment
setIDestValue addr value env = let
		Left ram = (eRAM env)
	in do
		cell <- (readArray ram addr)
		writeArray ram addr (cell {cVal = (Int value)})
		return env
	
getSourceValue :: Source -> Environment -> IO (Environment, Int)
getSourceValue src env = case (sVal src) of
	Left dest -> case dest of 
		DRegister _ r -> case (rVal r) of 
			"PC" -> return (env, (ePC env))
			"SP" -> return (env, (eSP env))
			"A" -> let 
					v = eA env
				in case v of 
					Int i -> return (env, i)
					i -> error $ "Accumulator Error: " ++ getStringFromCVal i
		DValue _ v -> case (vVal v) of
			Left iden -> do
				x <- getIDestValue (getAddress (idName iden) env) env
				return (env, x)
			Right ui -> do
				x <- getIDestValue (uiVal ui) env
				return (env, x)
		DIndex p l v -> do
			let s = Source (0,0) (Right v) -- first find what v is 
			x <- getSourceValue s env
			case (lLoc l) of 
				Left reg -> do
					let d' = (DRegister (0,0) reg)
					let s' = Source (0,0) (Left d')
					x' <- getSourceValue s' (fst x)
					let s'' = Source (0,0) (Left (DValue (0,0) (Value (0,0) (Right (Uint (0,0) ((snd x)+(snd x')))))))
					getSourceValue s'' (fst x')
				Right v -> do
					let d' = (DValue (0,0) v)
					let s' = Source (0,0) (Left d')
					x' <- getSourceValue s' (fst x)
					let s'' = Source (0,0) (Left (DValue (0,0) (Value (0,0) (Right (Uint (0,0) ((snd x)+(snd x')))))))
					getSourceValue s'' (fst x')
		DPostInc p l -> case (lLoc l) of 
			Left reg -> do
				let id = DIndirect (0,0) (Location (0,0) (Left reg))
				let s = Source (0,0) (Left id)
				x <- getSourceValue s env
				let d = DRegister (0,0) reg
				let s' = Source (0,0) (Left d)
				x' <- getSourceValue s' (fst x)
				env' <- setDestValue d ((snd x')+1) (fst x')
				return (env', snd x)
			Right v -> do
				let id = DIndirect (0,0) (Location (0,0) (Right v))
				let s = Source (0,0) (Left id)
				x <- getSourceValue s env
				let d = DValue (0,0) v
				let s' = Source (0,0) (Left d)
				x' <- getSourceValue s' (fst x)
				env' <- setDestValue d ((snd x')+1) (fst x')
				return (env', snd x)
		DPostDec p l -> case (lLoc l) of 
			Left reg -> do
				let id = DIndirect (0,0) (Location (0,0) (Left reg))
				let s = Source (0,0) (Left id)
				x <- getSourceValue s env
				let d = DRegister (0,0) reg
				let s' = Source (0,0) (Left d)
				x' <- getSourceValue s' (fst x)
				env' <- setDestValue d ((snd x')-1) (fst x')
				return (env', snd x)
			Right v -> do
				let id = DIndirect (0,0) (Location (0,0) (Right v))
				let s = Source (0,0) (Left id)
				x <- getSourceValue s env
				let d = DValue (0,0) v
				let s' = Source (0,0) (Left d)
				x' <- getSourceValue s' (fst x)
				env' <- setDestValue d ((snd x')-1) (fst x')
				return (env', snd x)
		DPreInc (0,0) l -> case (lLoc l) of 
			Left reg -> do
				let d = (DRegister (0,0) reg)
				let s = Source (0,0) (Left d)
				x <- getSourceValue s env
				env' <- setDestValue d ((snd x)+1) (fst x)
				let id = DIndirect (0,0) (Location (0,0) (Left reg))
				let s' = Source (0,0) (Left id)
				getSourceValue s' env'
			Right v -> do
				let d = (DValue (0,0) v)
				let s = Source (0,0) (Left d)
				x <- getSourceValue s env
				env' <- setDestValue d ((snd x)+1) (fst x)
				let id = DIndirect (0,0) (Location (0,0) (Right v))
				let s' = Source (0,0) (Left id)
				getSourceValue s' env'
		DPreDec p l -> case (lLoc l) of 
			Left reg -> do
				let d = (DRegister (0,0) reg)
				let s = Source (0,0) (Left d)
				x <- getSourceValue s env
				env' <- setDestValue d ((snd x)-1) (fst x)
				let id = DIndirect (0,0) (Location (0,0) (Left reg))
				let s' = Source (0,0) (Left id)
				getSourceValue s' env'
			Right v -> do
				let d = (DValue (0,0) v)
				let s = Source (0,0) (Left d)
				x <- getSourceValue s env
				env' <- setDestValue d ((snd x)-1) (fst x)
				let id = DIndirect (0,0) (Location (0,0) (Right v))
				let s' = Source (0,0) (Left id)
				getSourceValue s' env'
		DIndirect p l -> case (lLoc l) of 
			Left reg -> do
				let d = (DRegister (0,0) reg)
				let s = Source (0,0) (Left d)
				x <- getSourceValue s env
				let s' = Source (0,0) (Left (DValue (0,0) (Value (0,0) (Right (Uint (0,0) (snd x))))))
				getSourceValue s' (fst x)
			Right v -> do
				let d = (DValue (0,0) v)
				let s = Source (0,0) (Left d)
				x <- getSourceValue s env
				let s' = Source (0,0) (Left (DValue (0,0) (Value (0,0) (Right (Uint (0,0) (snd x))))))
				getSourceValue s' (fst x)
	Right value -> case (vVal value) of 
		Left iden -> return (env, getAddress (idName iden) env)
		Right ui -> return (env, uiVal ui)

getIDestValue :: Int -> Environment -> IO Int
getIDestValue addr env = let
		Left ram = (eRAM env)
	in do
		cell <- (readArray ram addr)
		case (cVal cell) of
			Int i -> return i
			i -> error $ "Cell Error: " ++ getStringFromCVal i
		
getAddress :: String -> Environment -> Int
getAddress lbl env = let
		sym = lookupST lbl (eSymTable env)
	in case sym of
		Just s -> case stValue s of
			Known i -> i
			_ -> -1	--should never happen if Symbol is built.
		Nothing -> read lbl

-- A convenience function that puts an Int onto the stack and decrements the SP
addToStack :: Int -> Environment -> IO Environment
addToStack x env = do
	env' <- return env {eSP = (eSP env)-1}
	let d = DIndirect (0,0) (Location (0,0) (Left (Register (0,0) "SP")))
	setDestValue d x env'
	
-- A convenience function that increments the PC.
incrementPC :: Environment -> IO Environment
incrementPC env = return env {ePC = (ePC env)+1}
