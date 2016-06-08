\ignore{
\begin{code}
----
-- Compiling:
-- ghc Emulation.hs -fno-warn-tabs -i../../machine/parser

-- Windows
-- Emulation.exe < test.ass
\end{code}
}

\noindent This is the \highlighttt{Emulation} module used by the GUI in order to 
execute an assembly program.

\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Emulation where
--module Main (main) where

import Data.Array
import Data.Array.IO
import Environment
import SymbolTable

import Assembly
\end{code}

\subsubsection{Emulation Functions}
\highlighttt{getFullProgEnv} is the highlest level function of the emulator that builds a complete listing of environment states for a program. This is used by the GUI in order to emulate a program. It takes in the starting environment and then sends it to the recursive function \highlighttt{getProgList} to be stepped through. Once the entire program has been emulated, the list of environments is converted an \highlighttt{Array} and returned.

\begin{code}
getFullProgEnv :: Environment -> IO (Array Int Environment)
getFullProgEnv env = case (eRAM env) of
   Left ram -> do
      -- Store the initial state of the environment
      env' <- eFreezeEnv env
      -- Send it through the emulator to store all the steps.
      envs <- getProgList env 0 [env']
      return (listArray (0, ((length envs)-1)) envs)
   Right r -> error $ "Cannot Emulate with frozen RAM"
\end{code}

\noindent \highlighttt{getProgList} is the connected function to \highlighttt{getFullProgEnv}. Each time it recurs on itself, it executes another instruction from the assembly program. After executing a step, a copy of the new environment is frozen and stored at the end of a list, and the environment is fed back into this function again. The environment contains a propery called eComplete which is set to \highlighttt{True} when the HALT instruction is called, or the program attempts to read input when there is none left. 
\begin{code}
getProgList :: Environment -> Int -> [Environment] -> IO [Environment]
getProgList env count envs = do
   env' <- getExeStep env 1
   if (eComplete env') == True
   then do
      return envs
   else do
      env'' <- eFreezeEnv env'
      getProgList env' (count+1) (envs ++ [env''])
\end{code}

\noindent \highlighttt{getExeStep} is the second function for emulating an assembly program, given an nvironment and a number of steps, it will emulate that many steps and return the resulting environment.

\begin{code}
getExeStep :: Environment -> Int -> IO Environment
getExeStep env steps = case steps of
   0 -> return env
   _ -> do 
      env' <- doExecutionStep env
      getExeStep env' (steps-1)            
\end{code}

\noindent \highlighttt{doExecutionStep} is a function that reads an instruction from RAM based on the PC, then sends it to be executed. Some error checking happens at this point, first we make sure the eRAM is a mutable array, then we check that what we pull out of memory is actually an instruction. If either of those happen, we crash with an appropriate error message.

\begin{code}
doExecutionStep :: Environment -> IO Environment
doExecutionStep env = case (eRAM env) of 
   Left ram -> do
      cell <- readArray ram (ePC env)
      case (cVal cell) of
         Inst i -> readInstruction i env
         Int i -> error $ "Memory Error: " ++ (show i) ++ 
                          " is not an Instruction. " ++ 
                          "Memory[" ++ (show (ePC env)) ++ "]"

         _ -> error $ "Memory Error: " ++
                      "Undefined is not an Instruction. " ++
                      "Memory[" ++ (show (ePC env)) ++ 
                      "]"
   Right ran -> error $ "Cannot Emulate with frozen RAM"
\end{code}

\noindent \highlighttt{readInstruction} is a simple function that breaks an \highlighttt{Instruction} appart and executes its connected action. Each instruction takes a certain number of paramters along with the environment, once executed, the new environment is then returned, and readInstruction also returns that environment.

\begin{code}
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
      HALT _ -> actionHalt env
\end{code}

\subsubsection{Actions}

\noindent The \highlighttt{actionHalt} function emulates the HALT instruction on the environment. The same environment is returned, with the \highlighttt{eComplete} flag set to true.

\begin{code}
-- HALT: Halting simply returns the same machine. No action is taken.
actionHalt :: Environment -> IO Environment
actionHalt env = return (env {eComplete = True})
\end{code}

\noindent The \highlighttt{actionCall} function emulates the CALL instruction on the environment. CALL is supplied with one arguement which is the destination where the PC should go to. First the current PC value is stored on the top of the stack, this is used when by a future RETURN instruction. Then using the destination provided, a memory location is determined and the PC is set to that location. 

There are two special case destinations, \highlighttt{read} and \highlighttt{print}, which are handled seperatly by functions within the emulator. For both of these, their special action is executed, then the PC is incremented. In the case of \highlighttt{read}, if there was nothing left to read, we halt the program.

\begin{code}
-- CALL: set PC to addr given, then place return address into 
-- SP pointer, reduce SP.
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
               i -> error $ "Print Error: Accumulator = " ++ 
			                getStringFromCVal i
         "read" -> do
            let env' = functionRead env
            case eA env' of
               Undefined -> actionHalt env'
               Int _ -> incrementPC env'
               Inst _ -> actionHalt env'
         _ -> do
            env' <- addToStack (ePC env) env
            let addr = (getAddress n env')
            let d = DRegister (vPos name) (Register (vPos name) "PC")
            setDestValue d addr env'
   Right ui -> do
      let x = (uiVal ui)
      let d = DRegister (vPos name) (Register (vPos name) "PC")
      setDestValue d x env
\end{code}

\noindent The \highlighttt{actionReturn} function emulates the RETURN instruction on the environment. This instruction has no arguements and simply takes the value stored in the memory where the SP is pointing to and assigns to the PC. Then the SP is reduced by 1, the PC is incremented and the environment is returned.

\begin{code}
-- RETURN: take the addr from where SP is pointing 
-- and put that into the PC
actionReturn :: Environment -> IO Environment
actionReturn env = do
      let s = Source (0,0) (Left (DIndirect (0,0) 
	                             (Location (0,0) 
	                             (Left (Register (0,0) "SP")))))
      tS <- getSourceValue s env
      let env' = fst tS
      let v = snd tS
      let d = DRegister (0,0) (Register (0,0) "PC")
      env'' <- setDestValue d v env'
      let d' = DRegister (0,0) (Register (0,0) "SP")
      env''' <- setDestValue d' ((eSP env'')+1) env''
      env'''' <- incrementPC env'''
      return env''''
\end{code}

\noindent The \highlighttt{actionMove} function emulates the MOVE instruction on the environment. This function takes a source and destination and stores the value provided by the source in the destination, overwriting whatever was there. It then increments the PC and returns the environment.

\begin{code}
-- MOVE: Moving a value from src to dest. 
actionMove :: Source -> Dest -> Environment -> IO Environment
actionMove src dest env = do
      tS <- getSourceValue src env
      let env' = fst tS
      let v = snd tS
      env'' <- setDestValue dest v env'
      incrementPC env''
\end{code}

\noindent The \highlighttt{actionAdd} function emulates the ADD instruction on the environment and takes three arguements, a source, destination and the current environment. It adds together the values stored in the source and destination, and stores the result in the destination. It then increments the PC and returns the environment.

\begin{code}
-- Add: Adds dest to src and stores in dest
actionAdd :: Source -> Dest -> Environment -> IO Environment
actionAdd src dest env = do
   tS <- getSourceValue src env
   let env' = fst tS
   let vS = snd tS
   let s = Source (dPos dest) (Left dest)
   tD <- getSourceValue s env'
   let   env'' = fst tD
   let   vD = snd tD   
   env''' <- setDestValue dest (vD + vS) env''
   incrementPC env'''
\end{code}

\noindent The \highlighttt{actionSub} function emulates the SUB instruction on the environment a and takes three arguements, a source, destination and the current environment. It subtracts value from the destination with the value from the source, and stores the result in the destination. It then increments the PC and returns the environment.

\begin{code}
-- SUB: Subtracts src from dest and stores in dest
actionSub :: Source -> Dest -> Environment -> IO Environment
actionSub src dest env = do
   tS <- getSourceValue src env
   let env' = fst tS
   let vS = snd tS
   let s = Source (dPos dest) (Left dest)
   tD <- getSourceValue s env'
   let   env'' = fst tD
   let   vD = snd tD   
   env''' <- setDestValue dest (vD - vS) env''
   incrementPC env'''
\end{code}

\noindent The \highlighttt{actionMult} function emulates the MULT instruction on the environment a and takes three arguements, a source, destination and the current environment. It multiplies value from the destination with the value from the source, and stores the result in the destination. It then increments the PC and returns the environment.

\begin{code}
-- MULT: Multiplies dest with src and stores in dest
actionMult :: Source -> Dest -> Environment -> IO Environment
actionMult src dest env = do
   tS <- getSourceValue src env
   let env' = fst tS
   let vS = snd tS
   let s = Source (dPos dest) (Left dest)
   tD <- getSourceValue s env'
   let   env'' = fst tD
   let   vD = snd tD
   env''' <- setDestValue dest (vD * vS) env''
   incrementPC env'''
\end{code}

\noindent The \highlighttt{actionDiv} function emulates the DIV instruction on the environment a and takes three arguements, a source, destination and the current environment. It does integer division on the value from the destination with the value from the source, and stores the result in the destination. It then increments the PC and returns the environment.

\begin{code}
-- DIV: Intger Divides dest with src and stores in dest
actionDiv :: Source -> Dest -> Environment -> IO Environment
actionDiv src dest env = do
   tS <- getSourceValue src env
   let env' = fst tS
   let vS = snd tS
   let s = Source (dPos dest) (Left dest)
   tD <- getSourceValue s env'
   let   env'' = fst tD
   let   vD = snd tD
   env''' <- setDestValue dest (div vD vS) env''
   incrementPC env'''
\end{code}

\noindent The \highlighttt{actionMod} function emulates the MOD instruction on the environment a and takes three arguements, a source, destination and the current environment. It applies the modular operation to the value from the destination with the value from the source, and stores the result in the destination. It then increments the PC and returns the environment.

\begin{code}
-- MOD: Does dest mod src and stores in dest
actionMod :: Source -> Dest -> Environment -> IO Environment
actionMod src dest env = do
   tS <- getSourceValue src env
   let env' = fst tS
   let vS = snd tS
   let s = Source (dPos dest) (Left dest)
   tD <- getSourceValue s env'
   let   env'' = fst tD
   let   vD = snd tD
   env''' <- setDestValue dest (mod vD vS) env''
   incrementPC env'''
\end{code}

\noindent The \highlighttt{actionJump} function emulates the JUMP instruction on the environment and takes two arguements, a value and the current environment. The PC is then set to the given value and the environment returned.

\begin{code}
-- JUMP: Sets PC to address it is given.
actionJump :: Value -> Environment -> IO Environment
actionJump addr env = do
   let dPC = DRegister (vPos addr) (Register (vPos addr) "PC")
   let s = Source (vPos addr) (Left (DValue (vPos addr) addr))
   x <- getSourceValue s env
   setDestValue dPC (snd x) (fst x)
\end{code}

\noindent The \highlighttt{actionBEQ} function emulates the JUMP instruction on the environment and takes two arguements, a value and the current environment. If the accumulator equals 0, then the PC is set to the value provided, if not, then the PC is incremented. In both instances, the environment is then returned.

\begin{code}
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
\end{code}

\noindent The \highlighttt{actionBNE} function emulates the JUMP instruction on the environment and takes two arguements, a value and the current environment. If the accumulator does \textbf{not} equal 0, then the PC is set to the value provided, if not, then the PC is incremented. In both instances, the environment is then returned.

\begin{code}
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
\end{code}

\noindent The \highlighttt{actionBLT} function emulates the JUMP instruction on the environment and takes two arguements, a value and the current environment. If the accumulator is less than 0, then the PC is set to the value provided, if not, then the PC is incremented. In both instances, the environment is then returned.

\begin{code}
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
\end{code}

\noindent The \highlighttt{actionBGT} function emulates the JUMP instruction on the environment and takes two arguements, a value and the current environment. If the accumulator is more than 0, then the PC is set to the value provided, if not, then the PC is incremented. In both instances, the environment is then returned.

\begin{code}
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
\end{code}

\noindent The \highlighttt{actionBLE} function emulates the JUMP instruction on the environment and takes two arguements, a value and the current environment. If the accumulator is less than or equal to 0, then the PC is set to the value provided, if not, then the PC is incremented. In both instances, the environment is then returned.

\begin{code}
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
\end{code}

\noindent The \highlighttt{actionBGE} function emulates the JUMP instruction on the environment and takes two arguements, a value and the current environment. If the accumulator is more than or equal to 0, then the PC is set to the value provided, if not, then the PC is incremented. In both instances, the environment is then returned.

\begin{code}
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
\end{code}

\noindent The \highlighttt{functionPrint} function emulates the \highlighttt{print} function call used with CALL. It has two arguements, an Int that is to be printed, and the environment. The Int given is placed at the end of the eStdOut property of the environment and the environment is then returned.

\begin{code}
-- Print: Takes the given Int and puts it into the StdOut
functionPrint :: Int -> Environment -> Environment
functionPrint x env = env {eStdOut = ((eStdOut env) ++ [x])}
\end{code}

\noindent The \highlighttt{functionRead} function emulates the \highlighttt{read} function call used with CALL. It has one arguements, the  current environment. The frontmost value from the \highlighttt{eStdIn} property of the environment is removed and placed into the accumulator. If there are no values left in \highlighttt{eStdIn}, then Undefined is placed instead. This problem is then resolved by the \highlighttt{actionCall} function.

\begin{code}
-- Read: Takes an element from StdIn and returns it. 
-- If StdIn is empty, will return Undefined.
functionRead :: Environment -> Environment
functionRead env = let
      stdIn = (eStdIn env)
   in case stdIn of
      [] -> env {eA = Undefined}
      (x:xs) -> env {eA = (Int x)}
\end{code}

\subsubsection{Memory Manipulation}
This section is some of the more complex part of the emulator as it has to do with reading and writing values to and from memory. The \highlighttt{setDestValue} and \highlighttt{getSourceValue} are both recursive and use each other to determine values.
\linebreak
\linebreak
\noindent \highlighttt{setDestValue} is a function that has three arguements, a Destination, an Int and an Environment. The aim of the function is to store the Int value in the destination. First the type of destination must be determined:
\begin{itemize}
   \item \highlighttt{DRegister}: If its just a register with no indirection, the value can be put into that registor.
   \item \highlighttt{DValue} : A value represents a direct memory address, such as using a symbol. In this case, the value given can be stored at that address.
   \item \highlighttt{DIndirect}: This is a destination with indirection, with an integer offset. First the value stored in the destination is read with \highlighttt{getSourceValue}, the index is then found with \highlighttt{getSourceValue} and added to it, and that new value is sent back into \highlighttt{setDestValue} as a DValue.
   \item \highlighttt{DPostInc}: This is a destination with indirection, and the value in the destination is incremented after this operation. First the value stored in the destination is read with \highlighttt{getSourceValue} and that new value is sent back into \highlighttt{setDestValue} as a DValue. Then the value in the destination is incremented. 
   \item \highlighttt{DPostDec}: This is a destination with indirection, and the value in the destination is incremented after this operation. First the value stored in the destination is read with \highlighttt{getSourceValue} and that new value is sent back into \highlighttt{setDestValue} as a DValue. Then the value in the destination is decremented. 
   \item \highlighttt{DPostInc}: This is a destination with indirection, and the value in the destination is incremented before this operation. First the the value in the destination is incremented, then the value stored in the destination is sent back into \highlighttt{setDestValue} as a DValue.
   \item \highlighttt{DPreInc}: This is a destination with indirection, and the value in the destination is decremented before this operation. First the the value in the destination is decremented, then the value stored in the destination is sent back into \highlighttt{setDestValue} as a DValue.	
   \item \highlighttt{DIndirect}: This is a destination with indirection, so the value stored in the destination is used as a memory address. Once the value is extracted using \highlighttt{getSourceValue}, it is then fed back into \highlighttt{setDestValue} as a DValue.
\end{itemize}

\noindent At the end of all these cases, the new environment is then returned.

\begin{code}
setDestValue :: Dest -> Int -> Environment -> IO Environment
setDestValue dest srcValue env = case dest of
   DRegister _ r -> case (rVal r) of 
      "PC" -> return env {ePC = srcValue}
      "SP" -> return env {eSP = srcValue}
      "A" -> return env {eA = (Int srcValue)}
   DValue _ v -> case (vVal v) of 
      Left iden -> setIDestValue (getAddress (idName iden) env) 
	                             srcValue 
								 env
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
               let dv = DValue p (Value p 
                                    (Right (Uint p 
                                       ((snd x') + (snd x)))))
               setDestValue dv srcValue env
         Right v -> let
               d' = (DValue p v)
               s' = Source p (Left d')
            in do
               x' <- getSourceValue s' env
               let dv = DValue p (Value p 
                                    (Right (Uint p 
                                       ((snd x') + (snd x)))))
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
\end{code}

\noindent \highlighttt{setIDestValue} is a function that takes two Ints and an Environment. The first Int a memory address and the second Int is a value that is to be stored at that address. Once the value has been copied into the memory, the environment is returned. This function is used by \highlighttt{setDestValue} to store values into memory.

\begin{code}
setIDestValue :: Int -> Int -> Environment -> IO Environment
setIDestValue addr value env = let
      Left ram = (eRAM env)
   in do
      cell <- (readArray ram addr)
      writeArray ram addr (cell {cVal = (Int value)})
      return env
\end{code}

\noindent \highlighttt{getSourceValue} is a function that has two arguements, a Source  and an Environment. The aim of the function is to read what value is stored at the source and return it. This function returns a tuple, the Integer value that the source was storing, and the environment, since some sources with pre and post increments/decrements, also modify values.
First the type of source must be determined, it can be either a Value, or a Destination. In the case of a Value, the integer of that Value is simply returned, however a Destination is more compelx.
\begin{itemize}
   \item \highlighttt{DRegister}: If its just a register with no indirection, the value can be read straight from the environment property and returned. In the case of the accumulator, if there is an Instruction or Undefined stored there, an error will be thrown.
   \item \highlighttt{DValue}: In this case, the Value is used as an address to get an Int from memory. This Int is then returned.
   \item \highlighttt{DIndex}: This is a source with indrection with an offset. First the offset must be determined, then this is added to the value stored in the source, which is then used as an address to find a value. The value is sent back through \highlighttt{getSourceValue} as a DValue in order to find the Int at that memery location.
   \item \highlighttt{DPostInc}: This is a source with indrection and the value in the source is incremented after the operation. First the value of the source is found, this is then used as an address and sent back through \highlighttt{getSourceValue} as a DValue. After this returns an Int, the value of source is incremented and the Int and environment is returned.
   \item \highlighttt{DPostDec}: This is a source with indrection and the value in the source is decremented after the operation. First the value of the source is found, this is then used as an address and sent back through \highlighttt{getSourceValue} as a DValue. After this returns an Int, the value of source is decremented and the Int and environment is returned.
   \item \highlighttt{DPreInc}: This is a source with indrection and the value in the source is incremented before the lookup. First the value of source is incremented, then it is used as an address and sent back through \highlighttt{getSourceValue} as a DValue. The Int and Environment is then returned.
   \item \highlighttt{DPreDec}: This is a source with indrection and the value in the source is decremented before the lookup. First the value of source is decremented, then it is used as an address and sent back through \highlighttt{getSourceValue} as a DValue. The Int and Environment is then returned.
   \item \highlighttt{DIndirect}: This is a source with indrection. First the value of source is decremented, then it is used as an address and sent back through \highlighttt{getSourceValue} as a DValue. The Int is then returned with the unchanged environemnt.
\end{itemize}

\begin{code}
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
               let s'' = Source (0,0) (Left (DValue (0,0) 
                                         (Value (0,0) 
                                            (Right (Uint (0,0) 
                                               ((snd x)+(snd x')))))))
               getSourceValue s'' (fst x')
            Right v -> do
               let d' = (DValue (0,0) v)
               let s' = Source (0,0) (Left d')
               x' <- getSourceValue s' (fst x)
               let s'' = Source (0,0) (Left (DValue (0,0) 
                                         (Value (0,0) 
                                            (Right (Uint (0,0) 
                                               ((snd x)+(snd x')))))))
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
            let s' = Source (0,0) (Left (DValue (0,0) 
                                     (Value (0,0) 
                                        (Right (Uint (0,0) (snd x))))))
            getSourceValue s' (fst x)
         Right v -> do
            let d = (DValue (0,0) v)
            let s = Source (0,0) (Left d)
            x <- getSourceValue s env
            let s' = Source (0,0) (Left (DValue (0,0) 
                                     (Value (0,0) 
                                        (Right (Uint (0,0) (snd x))))))
            getSourceValue s' (fst x)
   Right value -> case (vVal value) of 
      Left iden -> return (env, getAddress (idName iden) env)
      Right ui -> return (env, uiVal ui)
\end{code}

\begin{code}
getIDestValue :: Int -> Environment -> IO Int
getIDestValue addr env = let
      Left ram = (eRAM env)
   in do
      cell <- (readArray ram addr)
      case (cVal cell) of
         Int i -> return i
         i -> error $ "Cell Error: " ++ getStringFromCVal i
\end{code}

\subsubsection{Utility Functions}

\begin{code}
getAddress :: String -> Environment -> Int
getAddress lbl env = let
      sym = lookupST lbl (eSymTable env)
   in case sym of
      Just s -> case stValue s of
         Known i -> i
         _ -> -1   --should never happen if Symbol is built.
      Nothing -> read lbl
\end{code}

\begin{code}
-- A convenience function that puts an Int onto the stack and decrements the SP
addToStack :: Int -> Environment -> IO Environment
addToStack x env = do
   env' <- return env {eSP = (eSP env)-1}
   let d = DIndirect (0,0) (Location (0,0) (Left (Register (0,0) "SP")))
   setDestValue d x env'
\end{code}

\begin{code}
-- A convenience function that increments the PC.
incrementPC :: Environment -> IO Environment
incrementPC env = return env {ePC = (ePC env)+1}
\end{code}