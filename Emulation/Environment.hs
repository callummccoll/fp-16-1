{-# LANGUAGE ScopedTypeVariables #-}

module Environment (Cell, getCellInst, getIntFromCell, getCellFromInt, Environment, eA, eSP, ePC, eRAM, eStaticSize, eStdIn, eStdOut, initEnvF, thawEnv, freezeEnv, loadExmapleEnv) where

import Data.Array
import Data.Array.IO

-- the number of cells, indexed from 0,
memSize :: Int
memSize = 18

type Instruction = (String, Int, Int)

data Cell = 
	  Undefined
	| Int Int
	| Instruction String Int Int
	deriving Show
	
getCellInst :: Cell -> String
getCellInst (Instruction a b c) = a

getIntFromCell :: Cell -> Int
getIntFromCell (Int a) = a

getCellFromInt :: Int -> Cell
getCellFromInt x = Int x
	
data Environment = Environment {
		eA :: Cell,
		eSP :: Int,
		ePC :: Int,
		eRAM :: Either (IOArray Int Cell) (Array Int Cell),
		eStaticSize :: Int, -- the number of cells filled up by the ass prog, where the heap would start
		eStdIn :: [Int],
		eStdOut :: [Cell]
}
   
instance Show Environment where
  showsPrec _ e = 
	let 
		Right r = eRAM e
	in 
		showString "ENVIRONMENT [" .
		showString "\n   A = " . shows (eA e) .
		showString "\n   SP = " . shows (eSP e) .
		showString "\n   PC = " . shows (ePC e) .
		showString "\n   RAM = " . shows r .
		showString "\n   SstaticSize = " . shows (eStaticSize e) .
		showString "\n]\n" 
	
initEnvF :: Environment
initEnvF = Environment {
      eA = Undefined,
      eSP = memSize,
      ePC = 0,
      eRAM = Right $ listArray (0, memSize - 1) (repeat Undefined),
      eStaticSize = 0,
      eStdIn = [],
      eStdOut = []
   }
   
thawEnv :: Environment -> IO Environment
thawEnv e = do
	let Right r = eRAM e
	r' <- thaw r
	return $ e {eRAM = Left r'}
   
freezeEnv :: Environment -> IO Environment
freezeEnv e = do
	let Left r = eRAM e
	r' <- freeze r
	return $ e {eRAM = Right r'}

loadExmapleEnv :: IO Environment
loadExmapleEnv = do
	ram :: IOArray Int Cell <- newArray (0, memSize) Undefined
	writeArray ram 0 (Instruction "MOVE #1 X"		0 0)
	writeArray ram 1 (Instruction "CALL Main" 		1 0)
	writeArray ram 2 (Instruction "HALT" 			2 3)
	writeArray ram 3 (Instruction "SUB #2 SP" 		3 3)
	writeArray ram 4 (Instruction "MOVE #4 (SP)" 	4 5)
	writeArray ram 5 (Instruction "MOVE (SP) A" 	5 5)
	writeArray ram 6 (Instruction "ADD X A"		6 5)
	writeArray ram 7 (Instruction "MOVE A (SP)1"	7 6)
	writeArray ram 8 (Instruction "MOVE (SP)1 A"	8 6)
	writeArray ram 9 (Instruction "CALL print" 		9 7)
	writeArray ram 10 (Instruction "ADD #2 SP" 		10 7)
	writeArray ram 11 (Instruction "RETURN" 		11 7)
	
	let e = initEnvF {eSP = memSize+1, eRAM = Left ram}
	return e
	
	
	{-
	writeArray ram 0 (Instruction "MOV #1 X" 	0 0)
	writeArray ram 1 (Instruction "CALL main" 	1 0)
	writeArray ram 2 (Instruction "HALT" 		2 3)
	writeArray ram 3 (Instruction "SUB #2 SP" 	3 3)
	writeArray ram 4 (Instruction "MOV #3 (SP)" 4 5)
	writeArray ram 5 (Instruction "MOV (SP) A" 	5 5)
	writeArray ram 6 (Instruction "ADD X A" 	6 5)
	writeArray ram 7 (Instruction "MOV A 1(SP)" 7 6)
	writeArray ram 8 (Instruction "MOV 1(SP) A" 8 6)
	writeArray ram 9 (Instruction "CALL print" 	9 7)
	writeArray ram 10 (Instruction "ADD #1 SP" 	10 7)
	writeArray ram 11 (Instruction "ADD #2 SP" 	11 7)
	writeArray ram 12 (Instruction "RETURN" 	12 7)
	-}
	
	

	
	
	
	
	
	
	
	
