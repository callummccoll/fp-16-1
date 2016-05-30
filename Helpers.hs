module Helpers where

import "gtk3" Graphics.UI.Gtk
import Data.IORef

changeWithPredicate :: (a -> Bool) -> (a -> a) -> a -> a
changeWithPredicate p f x
  | p x' = x'
  | otherwise = x
  where x' = f x

createTextAreaFrame :: Maybe String -> Maybe String -> Bool -> IO Frame
createTextAreaFrame title content editable = do
    frame <- createFrame title
    (createTextArea content editable) >>= (containerAdd frame)
    return frame

createTextArea :: Maybe String -> Bool -> IO TextView
createTextArea content editable = case content of
    Nothing -> createEmptyTextArea editable
    Just s  -> do
        buffer <- textBufferNew Nothing
        textBufferSetText buffer s
        area <- textViewNewWithBuffer buffer
        set area [textViewEditable := editable]
        return area

createEmptyTextArea :: Bool -> IO TextView
createEmptyTextArea editable = do
    area <- textViewNew
    set area [textViewEditable := editable]
    return area

createFrame :: Maybe String -> IO Frame
createFrame s = case s of
    Nothing -> do 
        frame <- frameNew
        return frame
    Just s' -> do
        frame <- frameNew
        frameSetLabel frame s'
        frameSetLabelAlign frame 0.5 0.5
        return frame

toLines :: (Show a) => [a] -> String
toLines xs = concat ((\x -> (show x) ++ "\n") <$> xs)
