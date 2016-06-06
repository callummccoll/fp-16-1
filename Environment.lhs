

\noindent This is the \highlighttt{Environment} module used by the emulator and GUI that is used to store 
a state of the machine.

\begin{code}
module Environment (CVal(..), Cell(..), getStringFromCVal, 
               Environment, eA, eSP, ePC, eRAM, eStaticSize, eStdIn, 
               eStdOut, eSymTable, initEnvF, eThawEnv, freezeEnv, 
               makeEnvFromAss, convertInstToString) where
\end{code}

\subsubsection{Dependencies}

\noindent As a module that sits between the emulator and GUI, it has a few dependancies. 
From outside the project it relies on IOArray and Array in order to store memory 
data. From within this project it relies on the Assember, Symbol Table and the 
ABR Parser modules.

\begin{code}
import Data.Array
import Data.Array.IO
import Data.List

import Assembly
import SymbolTable
import ABR.Parser
import ABR.Parser.Lexers
\end{code}

\subsubsection{Constants}
Enviornment only has one constant called \highlighttt{memSize}

\noindent The number of memory cells the emulator has to work with is set at compile-time 
using memSize. A value of 24 means that the array can be indexed from 0 to 23.

\begin{code}
-- the number of cells, indexed from 0,
memSize :: Int
memSize = 24
\end{code}

\subsubsection{CVal}

\highlighttt{CVal} is the data type for the value of a memory cell used by the 
RAM. It is either:
\begin{itemize}
   \item \highlighttt{Undefined}
   \item an \highlighttt{Int}
   \item an \highlighttt{Instruction}
\end{itemize}

\noindent An \highlighttt{Instruction} is defined in the Assember modules.

\begin{code}
data CVal = 
     Undefined
   | Int Int
   | Inst Instruction
\end{code}

\noindent The \highlighttt{show} instance has been written so that it the type 
and value is easier to understand.

\begin{code}
instance Show CVal where
   showsPrec _ c = case c of
      Undefined -> showString "Undefined"
      Int i -> showString "Int " . shows i
      Inst i -> showString "Inst " . showString (convertInstToString i)
\end{code}

\noindent The \highlighttt{getStringFromCVal} method returns a String based on 
the value of a CVal. This is used by the GUI in order to display the value of 
memory cells.

\begin{code}
getStringFromCVal :: CVal -> String
getStringFromCVal cell = case cell of
   Int c -> show c
   Inst c -> convertInstToString c
   _ -> "Undefined"
\end{code}
   
\subsubsection{Cell}
The \highlighttt{Cell} data type is used as a memory block for the RAM. Each 
Cell stores a \highlighttt{String} and a \highlighttt{CVal}, representing a 
label (\highlighttt{cLabel}) and a stored value (\highlighttt{cVal})

\begin{code}
data Cell = Cell {
   cLabel :: String,
   cVal :: CVal
}
\end{code}
   
\noindent The \highlighttt{show} instance has been written so the label and CVal 
are easier to read.

\begin{code}
instance Show Cell where
   showsPrec _ c = shows (cLabel c) . showString " " . shows (cVal c)
\end{code}

\subsubsection{Environment}

The \highlighttt{Environment} data type stores all the information for a state 
of the emulator. It stores a number of values:
\begin{itemize}
   \item \highlighttt{eA} (CVal) : The Acculator where a value can be stored.
   \item an \highlighttt{eSP} (Int) : The Stack Pointer. An Index in memory.
   \item an \highlighttt{ePC} (Int) : The Program Counter. An Index in memory.
   \item an \highlighttt{eRAM} (IOArray Int Cell | Array Int Cell) : The RAM. An 
array storing all memory cells.
   \item an \highlighttt{eStaticSize} (Int) : The size of the program in memory. 
Used to determine where the heap would start. \textbf{Not Used}.
   \item an \highlighttt{eStdIn} (Int) : A list of Ints representing all inputs 
from stdin.
   \item an \highlighttt{eStdOut} (Int) : A list of Ints representing all outputs  
to stdout.
   \item an \highlighttt{eSymTable} (SymbolTable) : The symbol table. See the 
SymbolTable module for more information.
\end{itemize}

\begin{code}
data Environment = Environment {
      eA :: CVal,
      eSP :: Int,
      ePC :: Int,
      eRAM :: Either (IOArray Int Cell) (Array Int Cell),
   -- the number of cells filled up by the ass prog, where the heap would start
      eStaticSize :: Int, 
      eStdIn :: [Int],
      eStdOut :: [Int],
      eSymTable :: SymbolTable
   }
\end{code}

\noindent The \highlighttt{show} instance has been written so an Environment 
can be read easily. In order for an Environment to be shown, it must have it's 
RAM frozen with \highlighttt{freezeEnv}. Most of note is how the RAM is shown, 
each cell is shown on a seperate line using the show instances for 
\highlighttt{Cell} and \highlighttt{CVal}.

\begin{code}
instance Show Environment where
  showsPrec _ e = 
   let 
      Right r = eRAM e
   in 
      showString "ENVIRONMENT [" .
      showString "\n   A = " . shows (eA e) .
      showString "\n   SP = " . shows (eSP e) .
      showString "\n   PC = " . shows (ePC e) .
      showString "\n   StdIn = " . shows (eStdIn e) .
      showString "\n   StdOut = " . shows (eStdOut e) .
      showString "\n   SstaticSize = " . shows (eStaticSize e) .
      showString "\n   RAM = " . (showString 
         (concat (intersperse "\n         " (map show (elems r))))) .
      showString "\n   Symbols = " . shows (eSymTable e) .
      showString "\n]\n" 
\end{code}

\noindent \highlighttt{initEnvF} is a method that creates an empty Enviroment with all 
properties set to default values.

\begin{code}
initEnvF :: Environment
initEnvF = Environment {
      eA = Undefined,
      eSP = memSize,
      ePC = 0,
      eRAM = Right $ listArray (0, memSize - 1) (repeat (Cell "" Undefined)),
      eStaticSize = 0,
      eStdIn = [],
      eStdOut = [],
      eSymTable = emptyST
   }
\end{code}

\noindent \highlighttt{eThawEnv} is a method that thaws the \highlighttt{eRAM} of an 
Environment changing it from an \highlighttt{Array} to an \highlighttt{IOArray}. 
The a new environment is then returned.

\begin{code}
eThawEnv :: Environment -> IO Environment
eThawEnv e = do
   let Right r = eRAM e
   r' <- thaw r
   return $ e {eRAM = Left r'}
\end{code}

\noindent \highlighttt{eThawEnv} is a method that freezes the \highlighttt{eRAM} of an 
Environment changing it from an \highlighttt{IOArray} to an \highlighttt{Array}. 
The a new environment is then returned.

\begin{code}
freezeEnv :: Environment -> IO Environment
freezeEnv e = do
   let Left r = eRAM e
   r' <- freeze r
   return $ e {eRAM = Right r'}   
\end{code}

\noindent \highlighttt{makeEnvFromAss} is a method that creates and initialises an 
enviroment based on the contents of an assembly program and some inputs. The 
content of the program is provided by a String (this is the contents, not a 
filename), which is then parsed using the \highlighttt{Assembly} module. Once a 
parse tree is built, this is used to create a symbol table using the 
\highlighttt{SymbolTable} module. 

An empty enviroment is then created with \highlighttt{initEnvF} and the symbol 
table and stdin are insterted into it. The RAM is thawed so that it can be 
modified by the emulator, then using the parse tree and symbol table, 
the program is loaded into memory using \highlighttt{loadMemory}.

\begin{code}
makeEnvFromAss :: String -> [Int] -> IO Environment
makeEnvFromAss source stdIn = do
   prog <- parseAss source
   
   st <- buildST prog
   --add our special case symbols that point nowhere.
   let st' = insertST "print" (STEntry (0,0) (Known 0)) st
   let st'' = insertST "read" (STEntry (0,0) (Known 0)) st'
   st''' <- resolveST prog st''
   st'''' <-verifyST prog st'''

   let env = initEnvF {eSP = memSize, eSymTable = st'''', eStdIn = stdIn}
   env' <- eThawEnv env

   env'' <- loadMemory prog 0 env'

   --t <- freezeEnv env''
   --print t

   return env''
\end{code}

\noindent \highlighttt{parseAss} is a method that interacts with the 
\highlighttt{Assembly} module to create a parse tree from a string.

\begin{code}
parseAss :: String -> IO Program
parseAss source = do
   let cps = preLex source
   let lexRes = (dropWhite $ nofail $ total assemblyL) cps
   case lexRes of
      Error _ _    -> error "LEXER FAILED"
      OK (tlps, _) -> do
         let parseRes = (nofail $ total programP) tlps
         case parseRes of
            Error _ _     -> error "PARSER FAILED"
            OK (program, _) -> do
            return program   
\end{code}

\noindent \highlighttt{loadMemory} is a recursive method that takes in a 
parse tree, cell index and enviroment and loads the program into RAM. Each 
\highlighttt{Declaration} in the \highlighttt{Program} is read and an action is 
taken depending on what it is:

\begin{itemize}
   \item LabelHere: The current cell is given the label of the LabelHere. Since 
the LabelHere always comes before the value of the cell is set, the cell value 
is set to \highlighttt{Undefined}.
   \item LabelBind: These only apply to the SymbolTable, therefore no action is 
taken.
   \item Allocation: The cell index is increased by however many allocations are 
made. All allocated cells have a value of \highlighttt{Undefined}.
   \item Instruction: The instruction is stored in the cell and the cell index 
is incremented by one.
   \item Value: The value is stored in the current cell and the cell index is 
incremented by one.
\end{itemize}

\noindent After each action, \highlighttt{loadMemory} is called again without the 
declaration that was just read until there are not declarations left in the 
program.

\begin{code}
loadMemory :: Program -> Int -> Environment -> IO Environment
loadMemory (Program ds) index env = case ds of
   [] -> return env
   (x:xs) -> let
         Left ram = (eRAM env)
      in case x of
         DcLH p l -> do
            writeArray ram index (Cell (idName (lhId l)) Undefined)
            loadMemory (Program xs) (index) env
         DcLB p l -> 
            loadMemory (Program xs) (index) env
         DcAlloc p a -> case (aVal a) of
            Just v -> case (vVal v) of
               Right ui -> do
                  env' <- loadAlloc index (uiVal ui) env
                  loadMemory (Program xs) (index+(uiVal ui)) env
               Left iden -> error $ "Symbol Table Not Resolved"
            Nothing -> do
               env' <- loadAlloc index 1 env
               loadMemory (Program xs) (index+1) env'
         DcInst p i -> do
            cell <- readArray ram index
            writeArray ram index (cell {cVal = (Inst i)})
            loadMemory (Program xs) (index+1) env
         DcVal p v -> case (vVal v) of
            Right ui -> do
               cell <- readArray ram index
               writeArray ram index (cell {cVal = (Int (uiVal ui))})
               loadMemory (Program xs) (index+1) env
            Left iden -> error $ "Symbol Table Not Resolved"
\end{code}

\noindent \highlighttt{loadMemory} is a method used by \highlighttt{loadMemory} 
to step through a number of cell spaces, setting each to Undefined. An int 
for the current index, an int for the number of cells left to fill and the 
environment are provided and this method loops until the count reaches zero.

\begin{code}
loadAlloc :: Int -> Int-> Environment -> IO Environment
loadAlloc index count env = let 
      Left ram = (eRAM env)
   in case count of
      0 -> return env
      i -> do
         cell <- readArray ram (index+(count-1))
         writeArray ram (index+(count-1)) (cell {cVal = Undefined})
         loadAlloc index (count-1) env
\end{code}

\subsubsection{Utility}
In order for the \highlighttt{getStringFromCVal} method to work, there are a 
number of connected string conversion functions that convert data types from 
the assembly parse tree back to displayable strings.

\noindent \highlighttt{convertInstToString} converts an instruction to a string. 
It appears as the instruction name, then each parameter it has seperated by a 
space.

\begin{code}
convertInstToString :: Instruction -> String
convertInstToString inst = case inst of
   MOVE _ s d -> "MOVE " ++ convertSrcToString s ++ " " ++ convertDestToString d
   ADD _ s d -> "ADD " ++ convertSrcToString s ++ " " ++ convertDestToString d
   SUB _ s d -> "SUB " ++ convertSrcToString s ++ " " ++ convertDestToString d
   MULT _ s d -> "MULT " ++ convertSrcToString s ++ " " ++ convertDestToString d
   DIV _ s d -> "DIV " ++ convertSrcToString s ++ " " ++ convertDestToString d
   MOD _ s d -> "MOD " ++ convertSrcToString s ++ " " ++ convertDestToString d
   JUMP _ v -> "JUMP " ++ convertValueToString v
   CALL _ v -> "CALL " ++ convertValueToString v
   BEQ _ v -> "BEQ " ++ convertValueToString v
   BNE _ v -> "BNE " ++ convertValueToString v
   BLT _ v -> "BLT " ++ convertValueToString v
   BGT _ v -> "BGT " ++ convertValueToString v
   BLE _ v -> "BLE " ++ convertValueToString v
   BGE _ v -> "BGE " ++ convertValueToString v
   RET _ -> "RETURN"
   HALT _ -> "HALT"
\end{code}

\noindent \highlighttt{convertDestToString} converts a \highlighttt{Dest} 
(Destination) to a string. 

\begin{code}
convertDestToString :: Dest -> String
convertDestToString dest = case dest of
   DRegister _ r -> convertRegToString r
   DValue _ v -> convertValueToString v
   DIndex _ l v -> "(" ++ convertLocationToString l ++ ")" ++ convertValueToString v
   DPostInc _ l -> "(" ++ convertLocationToString l ++ ")+"
   DPostDec _ l -> "(" ++ convertLocationToString l ++ ")-"
   DPreInc _ l -> "+(" ++ convertLocationToString l ++ ")"
   DPreDec _ l -> "-(" ++ convertLocationToString l ++ ")"
   DIndirect _ l -> "(" ++ convertLocationToString l ++ ")"
\end{code}

\noindent \highlighttt{convertSrcToString} converts a \highlighttt{Source} to a 
string.

\begin{code}
convertSrcToString :: Source -> String
convertSrcToString src = case (sVal src) of
   Left x -> convertDestToString x   --Dest
   Right x -> "#" ++ convertValueToString x   --Value
\end{code}

\noindent \highlighttt{convertLocationToString} converts a 
\highlighttt{Location} to a string.

\begin{code}
convertLocationToString :: Location -> String
convertLocationToString loc = case (lLoc loc) of
   Left x -> convertRegToString x   --Reg
   Right x -> convertValueToString x   --Value
\end{code}

\noindent \highlighttt{convertRegToString} converts a \highlighttt{Register} to 
a string.

\begin{code}
convertRegToString :: Register -> String
convertRegToString reg = rVal reg
\end{code}

\noindent \highlighttt{convertValueToString} converts a \highlighttt{Value} to 
a string.

\begin{code}
convertValueToString :: Value -> String
convertValueToString value = case (vVal value) of
   Left x -> (idName x)   --Indentifier
   Right x -> show (uiVal x)   --Uint
\end{code}