module Main (main) where

import Assembly
import Emulation
import Environment
import Helpers
import Ram

import "gtk3" Graphics.UI.Gtk

import Control.Applicative
import Control.Monad.Trans
import Data.IORef
import Data.Array.MArray
import Data.Array

main :: IO ()
main = do
    mainLoop
    initGUI
    -- A counter which is incremented when the button is pressed.
    counter <- newIORef 0
    -- The main window.
    window <- windowNew
    windowSetDefaultSize window 1200 800
    -- Draw the Window.
    env <- (environmentFromFile "test.ass")
    redraw window counter (return env) (return env)
    -- Stop the application when the window is closed.
    window `on` deleteEvent $ tryEvent $ do
        liftIO $ mainQuit
    -- Start the application.
    mainGUI

environmentFromFile :: String -> IO Environment
environmentFromFile filename = (readFile filename) >>= makeEnvFromAss

redraw :: Window -> IORef Int -> IO Environment -> IO Environment -> IO ()
redraw window num startEnv env = do
    containerForeach window (\w -> containerRemove window w)
    createDrawing window num startEnv env

createDrawing :: Window -> IORef Int -> IO Environment -> IO Environment -> IO ()
createDrawing window x startEnv env = do
    hbox <- hBoxNew True 10
    (createButtons window x startEnv) >>= (containerAdd hbox) 
    (createFrame $ Just "C") >>= (containerAdd hbox)
    (createFrame $ Just "Assembly") >>= (containerAdd hbox)
    (getRamFromEnvironment env) >>= (containerAdd hbox)
    createIO >>= (containerAdd hbox)
    containerAdd window hbox
    widgetShowAll window

createButtons :: Window -> IORef Int -> IO Environment -> IO VBox
createButtons window x startEnv = do
    vbox <- vBoxNew True 10
    (createButton window x startEnv "next" (changeWithPredicate (<= 11) (+ 1))) >>= (containerAdd vbox)
    (createButton window x startEnv "previous" (changeWithPredicate (>= 0) (flip (-) 1))) >>= (containerAdd vbox)
    return vbox


getRamFromEnvironment :: IO Environment -> IO Frame
getRamFromEnvironment env = do
    env' <- env 
    case (eRAM env') of
        Left ram  -> (freeze ram) >>= (getRamFromArray env')
        Right ram -> getRamFromArray env' ram

getRamFromArray :: Environment -> (Array Int Cell) -> IO Frame
getRamFromArray env ram = (extractRegisters env) >>= (createRam (extractCell <$> ram))

extractRegisters :: Environment -> IO [(String, String)]
extractRegisters env = do
    return [("A", show (eA env)), ("SP", show (eSP env)), ("PC", show (ePC env)) ]

extractCell :: Cell -> (Maybe String, String)
extractCell c = case (cLabel c, cVal c) of
    (l, Int c')  -> (Just l, (show c'))
    (l, Inst c') -> (Just l, (showInstruction c'))
    _            -> (Nothing, "")

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

createIO :: IO VBox
createIO = do
    vbox   <- vBoxNew True 10
    stdin  <- createTextAreaFrame (Just "Stdin") Nothing False
    stdout <- createTextAreaFrame (Just "Stdout") Nothing False
    containerAdd vbox stdin
    containerAdd vbox stdout
    return vbox

createButton :: Window -> IORef Int -> IO Environment -> String -> (Int -> Int) -> IO Button
createButton window counter env name f = do
    button <- (readIORef counter) >>= (\num -> buttonNewWithLabel (name ++ " " ++ (show num)))
    -- Increment the counter when the button is pressed.
    button `on` buttonActivated $ do
        modifyIORef' counter f
        env' <- env
        newEnv <- (readIORef counter) >>= (\num -> getExeStep env' num)
        redraw window counter env (return newEnv)
    return button
