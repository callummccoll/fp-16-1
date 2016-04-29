----

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import "gtk3" Graphics.UI.Gtk
import "gtk3" Graphics.UI.Gtk.General.Enums
import Control.Monad.Trans
import Data.IORef

data GUI = GUI {
	builder :: Graphics.UI.Gtk.Builder,
	window :: Graphics.UI.Gtk.Window,
	cCodeBox :: Graphics.UI.Gtk.Box,
	aCodeBox :: Graphics.UI.Gtk.Box,
	pcLabel :: Graphics.UI.Gtk.Label,
	spLabel :: Graphics.UI.Gtk.Label,
	aLabel :: Graphics.UI.Gtk.Label,
	nextButton :: Graphics.UI.Gtk.Button,
	prevButton :: Graphics.UI.Gtk.Button,
	memLabel :: Graphics.UI.Gtk.Box,
	memValue :: Graphics.UI.Gtk.Box,
	stdinText :: Graphics.UI.Gtk.TextView,
	stdoutText :: Graphics.UI.Gtk.TextView
}

data StateVars = StateVars {
	step :: Int
}

data MemBlock = MemBlock {
	label :: String,
	value :: String
}
	deriving (Eq)

data ExeStep = ExeStep {
	cHighlight :: Int,
	aHighlight :: Int,
	pc :: Int,
	sp :: Int,
	a :: Int,
	stdin :: String,
	stdout :: String,
	stack :: [MemBlock]
}

cCode :: String
cCode = "int x = 1;\n\nint main () {\n    int y = 3;\n    int z;\n    z = y + x;\n    print(z);\n}"

aCode :: String
aCode = "MOV #1 X\nCALL main\nHALT\nSUB #2 SP\nMOV #3 (SP)\nMOV (SP) A\nADD X A\nMOV A 1(SP)\nMOV 1(SP) A\nCALL print\nADD #1 SP\nADD #2 SP\nRETURN"

instructions :: [ExeStep]
instructions = [
	(ExeStep 0 0 0 22 0 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "" ""), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" "")]), 
	(ExeStep 0 1 1 22 0 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" "")]), 
	(ExeStep 3 3 3 22 0 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" "")]), 
	(ExeStep 3 4 4 20 0 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "" "?"), (MemBlock "" "?")]), 
	(ExeStep 5 5 5 20 0 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "y" "3"), (MemBlock "z" "?")]), 
	(ExeStep 5 6 6 20 3 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "y" "3"), (MemBlock "z" "?")]), 
	(ExeStep 5 7 7 20 4 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "y" "3"), (MemBlock "z" "?")]), 
	(ExeStep 6 8 8 20 4 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "y" "3"), (MemBlock "z" "4")]), 
	(ExeStep 6 9 9 20 4 "" "" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "y" "3"), (MemBlock "z" "4")]), 
	(ExeStep 7 10 10 19 4 "" "4" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "" "print"),  (MemBlock "y" "3"), (MemBlock "z" "4")]), 
	(ExeStep 7 11 11 20 4 "" "4" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" ""), (MemBlock "y" "3"), (MemBlock "z" "4")]), 
	(ExeStep 7 12 12 22 4 "" "4" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" "")]), 
	(ExeStep 7 2 2 22 4 "" "4" [(MemBlock "Start" "MOV #1 X"), (MemBlock "" "CALL Main"), (MemBlock "" "HALT"), (MemBlock "Main" "SUB #1 SP"),  (MemBlock "" "MOV #3 (SP)"), (MemBlock "" "MOV (SP) A"), (MemBlock "" "ADD X A"), (MemBlock "" "MOV A 1(SP)"), (MemBlock "" "MOV 1(SP) A"), (MemBlock "" "CALL print"), (MemBlock "" "ADD #1 SP"), (MemBlock "" "ADD #2 SP"), (MemBlock "" "RETURN"), (MemBlock "X" "1"), (MemBlock "Heap" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "" ""), (MemBlock "Stack" "")])
	]

main :: IO ()
main = let 
		sV = StateVars 0
	in do
		stateVars <- newIORef (sV)
		initStateVars stateVars

		test <- readIORef stateVars

		initGUI
		gui <- createGUI
		createEvents gui stateVars
		drawScreen gui test
		mainGUI


initStateVars :: IORef (StateVars) -> IO ()
initStateVars stateVars = 
	let 
		step = 0 :: Int
		sV = StateVars step
	in do
		writeIORef stateVars sV
		return ()
	
createGUI :: IO GUI
createGUI = do 
	builder <- builderNew
	builderAddFromFile builder "window.glade"

	window <- builderGetObject builder castToWindow "window1"
	--widgetModifyBg window StateNormal (Color 0 58982 0)

	cCodeBox <- builderGetObject builder castToBox "cCode_box"
	aCodeBox <- builderGetObject builder castToBox "aCode_box"

	pcLabel <- builderGetObject builder castToLabel "pc_lbl"
	spLabel <- builderGetObject builder castToLabel "sp_lbl"
	aLabel <- builderGetObject builder castToLabel "a_lbl"

	nextButton <- builderGetObject builder castToButton "next_btn"
	prevButton <- builderGetObject builder castToButton "prev_btn"

	memL <- builderGetObject builder castToBox "memLabel_lst"
	memV <- builderGetObject builder castToBox "memValue_lst"

	stdinText <- builderGetObject builder castToTextView "stdin_txt"
	stdoutText <- builderGetObject builder castToTextView "stdout_txt"

	return $ GUI builder window cCodeBox aCodeBox pcLabel spLabel aLabel nextButton prevButton memL memV stdinText stdoutText

createEvents :: GUI -> IORef (StateVars) -> IO ()
createEvents gui stateVars = do 
	(window gui) `on` deleteEvent $ liftIO mainQuit >> return False
	(nextButton gui) `on` buttonActivated $ (nextButtonClick gui stateVars)
	(prevButton gui) `on` buttonActivated $ (prevButtonClick gui stateVars)
	return ()

drawScreen :: GUI -> StateVars -> IO ()
drawScreen gui stateVars = let
		instruction = getInstruction instructions 0 (step stateVars)
		colorY = Color 65535 65535 0
	in do
----------------------Clear out all the boxes we are about to fill
		children1 <- (containerGetChildren (cCodeBox gui))
		clearGUIBox children1 (cCodeBox gui)
		children2 <- (containerGetChildren (aCodeBox gui))
		clearGUIBox children2 (aCodeBox gui)
		children3 <- (containerGetChildren (memLabel gui))
		clearGUIBox children3 (memLabel gui)
		children4 <- (containerGetChildren (memValue gui))
		clearGUIBox children4 (memValue gui)
---------------------------------------cCode
		addCCode (cCodeBox gui) instruction
-----------------------------------------aCode
		addACode (aCodeBox gui) instruction
-------------------------Stack
		addMemBoxes (memLabel gui) (memValue gui) (stack instruction)
-------------------------Registers
		labelSetLabel (pcLabel gui) (show (pc instruction))
		labelSetLabel (spLabel gui) (show (sp instruction))
		labelSetLabel (aLabel gui) (show (a instruction))
------------------------stdin
		buffer1 <- textViewGetBuffer (stdinText gui)
		textBufferSetText buffer1 (stdin instruction)
------------------------stdout
		buffer2 <- textViewGetBuffer (stdoutText gui)
		textBufferSetText buffer2 (stdout instruction)
		
		widgetShowAll (window gui)
		return ()

addCCode :: Box -> ExeStep -> IO ()
addCCode box instruc = do

	lnEBoxB <- eventBoxNew
	widgetModifyBg lnEBoxB StateNormal (Color 0 0 0)

	alignment1 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment1 1 1 1 0

	lnEBoxW <- eventBoxNew
	widgetModifyBg lnEBoxW StateNormal (Color 65535 65535 65535)

	alignment2 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment2 10 10 4 4

	lnBox <- vBoxNew False 0	

	addCodeLineNumbers lnBox 0 (length (getPreHighlightLines (lines cCode) (cHighlight instruc))) False
	addCodeLineNumbers lnBox (cHighlight instruc) ((cHighlight instruc)+1) True
	addCodeLineNumbers lnBox ((cHighlight instruc)+1) (length (lines cCode)) False
----------------
	newEBoxB <- eventBoxNew
	widgetModifyBg newEBoxB StateNormal (Color 0 0 0)

	alignment3 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment3 1 1 1 1

	newEBoxW <- eventBoxNew
	widgetModifyBg newEBoxW StateNormal (Color 65535 65535 65535)

	alignment4 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment4 10 10 10 10

	newBox <- vBoxNew False 0
	set newBox [widgetHExpand := True]

	addCCodeLines newBox (getPreHighlightLines (lines cCode) (cHighlight instruc)) False
	addCCodeLines newBox (getHighlightLine (lines cCode) (cHighlight instruc)) True
	addCCodeLines newBox (getPostHighlightLines (lines cCode) (cHighlight instruc)) False
----------------assemble the gui

	containerAdd alignment2 lnBox
	containerAdd lnEBoxW alignment2
	containerAdd alignment1 lnEBoxW
	containerAdd lnEBoxB alignment1
	containerAdd box lnEBoxB

	containerAdd alignment4 newBox
	containerAdd newEBoxW alignment4
	containerAdd alignment3 newEBoxW
	containerAdd newEBoxB alignment3
	containerAdd box newEBoxB

addCCodeLines :: VBox -> [String] -> Bool -> IO ()
addCCodeLines box xs highlight = case xs of
	[] -> return ()
	(x:xs') -> do
		newLineBox <- hBoxNew False 0
		addCCodeToLine newLineBox (wordsWithWP x "") highlight
		widgetSetVAlign newLineBox AlignStart
		boxPackStart box newLineBox PackNatural 0
		addCCodeLines box xs' highlight

addCCodeToLine :: HBox -> [String] -> Bool -> IO ()
addCCodeToLine box xs highlight = case xs of
	[] -> return ()
	(x:xs') -> do
		newLabel <- labelNew (Just "")
		labelSetMarkup newLabel ("<tt><span color=\""++(getColorForSymbol x)++"\">"++ x ++"</span></tt>")
		widgetSetTooltipMarkup newLabel (Just (getValueForSymbol (removeWhitespace x)))
		miscSetAlignment newLabel 0 0
		labelSetSelectable newLabel False
		widgetSetHAlign newLabel AlignFill
		if highlight == True
		then widgetModifyBg newLabel StateNormal (Color 65535 65535 0)
		else widgetModifyBg newLabel StateNormal (Color 65535 65535 65535)
		boxPackStart box newLabel PackNatural 0
		addCCodeToLine box xs' highlight

addACode :: Box -> ExeStep -> IO ()
addACode box instruc = do

	lnEBoxB <- eventBoxNew
	widgetModifyBg lnEBoxB StateNormal (Color 0 0 0)

	alignment1 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment1 1 1 1 0

	lnEBoxW <- eventBoxNew
	widgetModifyBg lnEBoxW StateNormal (Color 65535 65535 65535)

	alignment2 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment2 10 10 4 4

	lnBox <- vBoxNew False 0

	addCodeLineNumbers lnBox 0 (length (getPreHighlightLines (lines aCode) (aHighlight instruc))) False
	addCodeLineNumbers lnBox (aHighlight instruc) ((aHighlight instruc)+1) True
	addCodeLineNumbers lnBox ((aHighlight instruc)+1) (length (lines aCode)) False
--------------------
	newEBoxB <- eventBoxNew
	widgetModifyBg newEBoxB StateNormal (Color 0 0 0)

	alignment3 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment3 1 1 1 1

	newEBoxW <- eventBoxNew
	widgetModifyBg newEBoxW StateNormal (Color 65535 65535 65535)

	alignment4 <- alignmentNew 0 0 1 1
	alignmentSetPadding alignment4 10 10 10 10

	newBox <- vBoxNew False 0
	set newBox [widgetHExpand := True]

	addACodeLines newBox (getPreHighlightLines (lines aCode) (aHighlight instruc)) False
	addACodeLines newBox (getHighlightLine (lines aCode) (aHighlight instruc)) True 
	addACodeLines newBox (getPostHighlightLines (lines aCode) (aHighlight instruc)) False
----------------assemble the gui

	containerAdd alignment2 lnBox
	containerAdd lnEBoxW alignment2
	containerAdd alignment1 lnEBoxW
	containerAdd lnEBoxB alignment1
	containerAdd box lnEBoxB

	containerAdd alignment4 newBox
	containerAdd newEBoxW alignment4
	containerAdd alignment3 newEBoxW
	containerAdd newEBoxB alignment3
	containerAdd box newEBoxB

addACodeLines :: VBox -> [String] -> Bool -> IO ()
addACodeLines box xs highlight = case xs of
	[] -> return ()
	(x:xs') -> do
		newLabel <- labelNew (Just "")
		labelSetMarkup newLabel ("<tt>"++ x ++"</tt>")
		miscSetAlignment newLabel 0 0
		labelSetSelectable newLabel False
		widgetSetHAlign newLabel AlignFill
		if highlight == True
		then widgetModifyBg newLabel StateNormal (Color 65535 65535 0)
		else widgetModifyBg newLabel StateNormal (Color 65535 65535 65535)

		boxPackStart box newLabel PackNatural 0
		addACodeLines box xs' highlight

addCodeLineNumbers :: VBox -> Int -> Int -> Bool -> IO ()
addCodeLineNumbers box count length highlight = 
	if count < length
	then do 
		newLabel <- labelNew (Just "")
		labelSetMarkup newLabel ("<tt><span color=\"#777777\">"++ (show count) ++"</span></tt>")
		miscSetAlignment newLabel 1 0
		labelSetJustify newLabel JustifyRight
		labelSetSelectable newLabel True
		widgetSetHAlign newLabel AlignFill
		if highlight == True
		then widgetModifyBg newLabel StateNormal (Color 65535 65535 0)
		else widgetModifyBg newLabel StateNormal (Color 65535 65535 65535)
		boxPackStart box newLabel PackNatural 0
		addCodeLineNumbers box (count+1) length highlight
	else return ()
	

addMemBoxes :: Box -> Box -> [MemBlock] -> IO ()
addMemBoxes boxL boxV stack = case stack of
	[] -> return ()
	(x:xs') -> let 
			colorB = Color 0 0 0
			colorW = Color 65535 65535 65535
		in do
------------------------------- Bordered value boxes
			alignment <- alignmentNew 0 0 1 1
			if xs' == []
			then alignmentSetPadding alignment 1 1 1 1
			else alignmentSetPadding alignment 1 0 1 1

			newEBoxB <- eventBoxNew
			widgetModifyBg newEBoxB StateNormal colorB

			newEBoxW <- eventBoxNew
			widgetModifyBg newEBoxW StateNormal colorW
		
			newLabel <- labelNew (Just (value x))

			containerAdd newEBoxW newLabel
			containerAdd alignment newEBoxW
			containerAdd newEBoxB alignment
			containerAdd boxV newEBoxB
--------------------------------------- Label boxes
			alignment <- alignmentNew 0 0 1 1
			if xs' == []
			then alignmentSetPadding alignment 1 1 1 1
			else alignmentSetPadding alignment 1 0 1 1

			newEBoxW <- eventBoxNew
			widgetModifyBg newEBoxW StateNormal colorW

			newLabel <- labelNew (Just (label x))

			containerAdd alignment newLabel
			containerAdd newEBoxW alignment
			containerAdd boxL newEBoxW
---------------------------------------
			addMemBoxes boxL boxV xs'

nextButtonClick :: GUI -> IORef (StateVars) -> IO ()
nextButtonClick gui stateVars = do
	sV <- readIORef stateVars
	if (step sV) < ((length instructions)-1)
	then 
		let 
			s = (step sV) + 1
			newSV = StateVars s
		in do
			writeIORef stateVars newSV
			drawScreen gui newSV
	else return ()

prevButtonClick :: GUI -> IORef (StateVars) -> IO ()
prevButtonClick gui stateVars = do
	sV <- readIORef stateVars
	if (step sV) > 0
	then 
		let 
			s = (step sV) - 1
			newSV = StateVars s
		in do
			writeIORef stateVars newSV
			drawScreen gui newSV
	else return ()

clearGUIBox :: [Widget] -> Box -> IO ()
clearGUIBox xs box = case xs of
	[] -> return ()
	(x:xs') -> do 
		widgetDestroy x
		clearGUIBox xs' box

getInstruction :: [ExeStep] -> Int -> Int -> ExeStep
getInstruction xs c v = case xs of
	[] -> (ExeStep 0 0 0 0 0 "" "" [])
	(x:xs') -> if c == v
		then x
		else getInstruction xs' (c+1) v

getPreHighlightLines :: [String] -> Int -> [String]
getPreHighlightLines xs lineNum = case xs of 
	[] -> []
	(x:xs') -> if lineNum > 0
		then [x] ++ (getPreHighlightLines xs' (lineNum-1))
		else []

getHighlightLine :: [String] -> Int -> [String]
getHighlightLine xs lineNum = case xs of 
	[] -> []
	(x:xs') -> if lineNum == 0
		then [x]
		else getHighlightLine xs' (lineNum-1)

getPostHighlightLines :: [String] -> Int -> [String]
getPostHighlightLines xs lineNum = case xs of
	[] -> []
	(x:xs') -> if lineNum < 0 
		then [x] ++ (getPostHighlightLines xs' (lineNum-1))
		else getPostHighlightLines xs' (lineNum-1)

wordsWithWP :: String -> String -> [String]
wordsWithWP xs s = case xs of
	"" -> [s]
	(' ':xs') -> [s++" "] ++ wordsWithWP xs' ""
	('\t':xs') -> [s++"\t"] ++ wordsWithWP xs' ""
	(';':xs') -> [s] ++ [";"] ++ wordsWithWP xs' ""
	('(':xs') -> [s] ++ ["("] ++ wordsWithWP xs' ""
	(')':xs') -> [s] ++ [")"] ++ wordsWithWP xs' ""
	(x:xs') -> wordsWithWP xs' (s++[x])

getColorForSymbol :: String -> String
getColorForSymbol sym = case sym of
	"int " -> "#4f8b57"
	('0':_) -> "#ff33ff"
	('1':_) -> "#ff33ff"
	('2':_) -> "#ff33ff"
	('3':_) -> "#ff33ff"
	('4':_) -> "#ff33ff"
	('5':_) -> "#ff33ff"
	('6':_) -> "#ff33ff"
	('7':_) -> "#ff33ff"
	('8':_) -> "#ff33ff"
	('9':_) -> "#ff33ff"
	_ -> "#000000"

getValueForSymbol :: String -> String
getValueForSymbol sym = case sym of
	"x" -> "1"
	"y" -> "3"
	"z" -> "4"
	_ -> ""

removeWhitespace :: String -> String
removeWhitespace xs = case xs of
	"" -> ""
	(' ':xs') -> (getValueForSymbol xs')
	('\t':xs') -> (getValueForSymbol xs')
	(x:xs') -> [x] ++ (getValueForSymbol xs')







