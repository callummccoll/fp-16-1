-- Ram.hs
-- visualization
-- Created by Callum McColl on 29/05/2016
-- Copyright © 2016 Callum McColl. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above
--    copyright notice, this list of conditions and the following
--    disclaimer in the documentation and/or other materials
--    provided with the distribution.
--
-- 3. All advertising materials mentioning features or use of this
--    software must display the following acknowledgement:
--
--        This product includes software developed by Callum McColl.
--
-- 4. Neither the name of the author nor the names of contributors
--    may be used to endorse or promote products derived from this
--    software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
-- -----------------------------------------------------------------------
-- This program is free software; you can redistribute it and/or
-- modify it under the above terms or under the terms of the GNU
-- General Public License as published by the Free Software Foundation;
-- either version 2 of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, see http://www.gnu.org/licenses/
-- or write to the Free Software Foundation, Inc., 51 Franklin Street,
-- Fifth Floor, Boston, MA  02110-1301, USA.

module Ram (createRam) where

import Helpers

import Graphics.UI.Gtk
import Data.IORef

createRam :: [(Maybe String, String)] -> [(String, String)] -> IO Frame
createRam cs registers = do
    frame <- createFrame $ Just "Ram and Registers"
    hbox  <- hBoxNew False 10
    registers <- createRegisters registers
    table <- createRamTable cs
    containerAdd hbox table
    --set hbox [boxChildPacking table := PackRepel]
    containerAdd hbox registers
    --set hbox [boxChildPacking registers := PackRepel]
    containerAdd frame hbox 
    return frame

createRamTable :: [(Maybe String, String)] -> IO Table
createRamTable cs = do
    table <- (tableNew (length cs) 1 True)
    attachCellsToTable table (createRAMCell <$> cs) 0


attachCellsToTable :: Table -> [IO HBox] -> Int -> IO Table
attachCellsToTable table cells row = case cells of
    []     -> return table
    c : cs -> do
        hBox <- c
        tableAttachDefaults table hBox 0 1 row (row + 1)
        attachCellsToTable table cs (row + 1)

createRAMCell :: (Maybe String, String) -> IO HBox
createRAMCell (label, content) = do
    hbox <- hBoxNew False 10
    frame <- createFrame Nothing
    set frame [ widgetHeightRequest := 5 ]
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    cell <- labelNew (Just content)
    label <- labelNew label
    containerAdd hbox label
    containerAdd eventBox cell
    containerAdd frame eventBox
    containerAdd hbox frame
    return hbox

createRegisters :: [(String, String)] -> IO VBox
createRegisters registers = do
    vbox  <- vBoxNew True 10
    addRegisters vbox (createRegister <$> registers)


createRegister :: (String, String) -> IO HBox
createRegister (register, content) = do
    hbox     <- hBoxNew False 10
    label    <- labelNew (Just register)
    frame    <- createFrame Nothing
    eventBox <- eventBoxNew
    widgetModifyBg eventBox StateNormal (Color 65535 65535 65535)
    value    <- labelNew (Just content)
    containerAdd eventBox value
    containerAdd frame eventBox
    containerAdd hbox label
    containerAdd hbox frame
    return hbox

addRegisters :: VBox -> [IO HBox] -> IO VBox
addRegisters vbox registers = case registers of
    []     -> return vbox
    r : rs -> do
        register <- r
        containerAdd vbox register
        set vbox [boxChildPacking register := PackRepel]
        addRegisters vbox rs
