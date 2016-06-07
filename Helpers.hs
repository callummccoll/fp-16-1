module Helpers where

import "gtk3" Graphics.UI.Gtk
import Data.IORef

infixl 2 >|>
(>|>) :: (ContainerClass c, WidgetClass w) => w -> c -> IO c
widget >|> container = do
    containerAdd container widget
    return container

infixl 2 >|>>
(>|>>) :: (ContainerClass c, WidgetClass w) => w -> IO c -> IO c
widget >|>> container = do
    c <- container
    containerAdd c widget
    return c

infixl 2 >>|>
(>>|>) :: (ContainerClass c, WidgetClass w) => IO w -> c -> IO c
widget >>|> container = do
    widget >>= (containerAdd container)
    return container

infixl 2 >>|>>
(>>|>>) :: (ContainerClass c, WidgetClass w) => IO w -> IO c -> IO c
widget >>|>> container = do
    c <- container
    w <- widget
    containerAdd c w
    return c


infixl 1 <|<
(<|<) :: (ContainerClass c, WidgetClass w) => c -> w -> IO w
container <|< widget = do
    containerAdd container widget
    return widget

infixl 1 <<|<
(<<|<) :: (ContainerClass c, WidgetClass w) => IO c -> w -> IO w
container <<|< widget = do
    c <- container
    containerAdd c widget
    return widget

infixl 1 <|<<
(<|<<) :: (ContainerClass c, WidgetClass w) => c -> IO w -> IO w
container <|<< widget = do
    widget >>= (containerAdd container)
    widget

infixl 1 <<|<<
(<<|<<) :: (ContainerClass c, WidgetClass w) => IO c -> IO w -> IO w
container <<|<< widget = do
    c <- container
    w <- widget
    containerAdd c w
    return w

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
    (createTextArea content editable) >>|> frame

createTextArea :: Maybe String -> Bool -> IO TextView
createTextArea content editable = case content of
    Nothing -> createEmptyTextArea editable
    Just s  -> do
        buffer <- textBufferNew Nothing
        textBufferSetText buffer s
        area <- textViewNewWithBuffer buffer
        set area [textViewEditable := editable]
        textViewSetLeftMargin area 2
        textViewSetRightMargin area 2
        return area

createEmptyTextArea :: Bool -> IO TextView
createEmptyTextArea editable = do
    area <- textViewNew
    set area [textViewEditable := editable]
    return area

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
