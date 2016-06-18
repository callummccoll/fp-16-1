module Helpers where

import Operators

import "gtk3" Graphics.UI.Gtk
import Data.IORef

changeWithPredicate :: (a -> Bool) -> (a -> a) -> a -> a
changeWithPredicate p f x
  | p x' = x'
  | otherwise = x
  where x' = f x

createButton :: (() -> IO Button) -> (() -> IO ()) -> IO Button
createButton factory action = do
    button <- factory ()
    -- Increment the counter when the button is pressed.
    button `on` buttonActivated $ do
        action ()
    return button

createTextAreaFrame :: Maybe String -> Maybe String -> Bool -> IO Frame
createTextAreaFrame title content editable = do
    frame <- case title of
               Nothing -> frameNew
               Just s  -> createFrame s
    (createTextArea content editable) >>|>> (pad (5, 5, 5, 5)) >>|> frame

createTextArea :: Maybe String -> Bool -> IO TextView
createTextArea content editable = case content of
    Nothing -> createEmptyTextArea editable
    Just s  -> do
        buffer <- textBufferNew Nothing
        textBufferSetText buffer s
        area <- textViewNewWithBuffer buffer >>:= [textViewEditable := editable]
        textViewSetLeftMargin area 5
        textViewSetRightMargin area 5
        return area

createEmptyTextArea :: Bool -> IO TextView
createEmptyTextArea editable = textViewNew >>:= [textViewEditable := editable]

createFrame :: String -> IO Frame
createFrame s = do
    frame <- frameNew
    frameSetLabel frame s
    frameSetLabelAlign frame 0.5 0.5
    return frame

toLines :: (Show a) => [a] -> String
toLines xs = concat ((\x -> (show x) ++ "\n") <$> xs)

pad :: (Int, Int, Int, Int) -> IO Alignment
pad padding = padWithAlignment padding (0, 0, 1, 1)

padWithAlignment :: (Int, Int, Int, Int) -> (Float, Float, Float, Float) -> IO Alignment
padWithAlignment (top, bottom, left, right) (xalign, yalign, xscale, yscale) = do
    a <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding a top bottom left right
    return a
