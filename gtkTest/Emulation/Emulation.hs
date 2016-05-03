-----fno-warn-tabs

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

type Instruction = [String]

data MemBlock = MemBlock {
	label :: String,
	value :: String
}
	deriving (Eq, Show)

data Machine = Machine {
	regPC :: Int,
	regSP :: Int,
	regA :: Int,
	stack :: [MemBlock],
	stdout :: String
}
	deriving (Show)
	
main :: IO ()
main = do
	m <- getExeStep (Machine 0 5 0 [(MemBlock "" "CALL #2"), (MemBlock "" "HALT"), (MemBlock "MOVE #4 A" "RETURN"), (MemBlock "" "RETURN"), (MemBlock "" ""), (MemBlock "" "")] "") 3
	putStr (show m)
	return ()

getExeStep :: Machine -> Int -> IO Machine
getExeStep machine steps = case steps of
	0 -> return machine
	_ -> getExeStep (doExecutionStep machine) (steps-1)				
	
doExecutionStep :: Machine -> Machine
doExecutionStep machine = let
		inst = getCurInstruction (stack machine) (regPC machine)
	in case inst of 
		[] -> machine ----this means the step is greater than the program allows.
		(x:xs) -> case x of
			"HALT" -> actionHalt machine
			"CALL" -> actionCall (head xs) machine
			"RETURN" -> actionReturn machine
			"MOVE" -> actionMove (head xs) (last xs) machine
			"ADD" -> machine
			"SUB" -> machine
			"MULT" -> machine
			"DIV" -> machine
			"MOD" -> machine
			"JUMP" -> machine
			"BEQ" -> machine
			"BNE" -> machine
			"BLT" -> machine
			"BGT" -> machine
			"BLE" -> machine
			"BGE" -> machine
			_ -> machine --invalid instruction?

--- Halting simply returns the same machine. No action is taken.
actionHalt :: Machine -> Machine
actionHalt machine = machine

--set PC to addr given, then place return address into SP pointer, reduce SP.
actionCall :: String -> Machine -> Machine
actionCall addr machine = setDestValue "PC" (getSourceValue addr machine) $ setDestValue "SP" ((regSP machine)-1) $ setDestValue "(SP)" (regPC machine) machine

--take the addr from where SP is pointing and put that into the PC
actionReturn :: Machine -> Machine
actionReturn machine = setDestValue "SP" ((regSP machine)+1) $ setDestValue "PC" (regSP machine) machine

--Moving a value from a source to a destination. 
actionMove :: String -> String -> Machine -> Machine
actionMove source dest machine = setDestValue "PC" ((regPC machine)+1) $ setDestValue dest (getSourceValue source machine) machine

-----------------------------------------------------------
getCurInstruction :: [MemBlock] -> Int -> Instruction
getCurInstruction stack pc = case stack of
	[] -> []
	(x:xs) -> case pc of
		0 -> words (value x)
		_ -> getCurInstruction xs (pc-1)
	
setDestValue :: String -> Int -> Machine -> Machine
setDestValue dest srcValue machine = case dest of
	('(':xs) -> case xs of
		"PC)" -> machine-- ?
		"SP)" -> machine {stack = setIDestValue (regSP machine) srcValue (stack machine)}
		"A)"  -> machine-- ?
	"PC" -> machine {regPC = srcValue}
	"SP" -> machine {regSP = srcValue}
	"A" -> machine {regA = srcValue}
	_ -> machine
	--Need to add indrection calls -(...) +(...) (...)nnn etc
	
setIDestValue :: Int -> Int -> [MemBlock] -> [MemBlock]
setIDestValue addr value stack = case stack of
	[] -> []
	(x:xs) -> case addr of
		0 -> [MemBlock "" (show value)] ++ xs
		_ -> [x] ++ setIDestValue (addr-1) value xs
	
getSourceValue :: String -> Machine -> Int
getSourceValue src machine = case src of
	('#':xs) -> convertStringToInt xs 0
	"PC" -> (regPC machine)
	"SP" -> (regSP machine)
	"A" -> (regA machine)
	_ -> 0 -- Review this?
	--Need to add indrection calls -(...) +(...) (...)nnn etc

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
		


		



	

	
	