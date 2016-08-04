-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : spec_reg_pkg
--   Date             : 07/07/2016
--
--   Description      : Mapping of the special register bank
--
--   --------------------------------------------------------------------------------
--   Modifications
--   --------------------------------------------------------------------------------
--   Date             : Ver. : Author           : Modification comments
--   --------------------------------------------------------------------------------
--                    :      :                  :
--   07/07/2016       : 1.0  : B.Lemoine        : First draft
--                    :      :                  :
-- **********************************************************************************
--   MIT License
--   
--   Copyright (c) 07/07/2016, Benjamin Lemoine
--   
--   Permission is hereby granted, free of charge, to any person obtaining a copy
--   of this software and associated documentation files (the "Software"), to deal
--   in the Software without restriction, including without limitation the rights
--   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--   copies of the Software, and to permit persons to whom the Software is
--   furnished to do so, subject to the following conditions:
--   
--   The above copyright notice and this permission notice shall be included in all
--   copies or substantial portions of the Software.
--   
--   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--   SOFTWARE.
-- **********************************************************************************

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package spec_reg_pkg is

   -- Addr of special register in the bank
   constant Addr_PC      : std_logic_vector(3 downto 0) := x"0";
   constant Addr_MSR     : std_logic_vector(3 downto 0) := x"1";
   constant Addr_EAR     : std_logic_vector(3 downto 0) := x"2";
   constant Addr_ESR     : std_logic_vector(3 downto 0) := x"3";
   constant Addr_BTR     : std_logic_vector(3 downto 0) := x"4";
   constant Addr_FSR     : std_logic_vector(3 downto 0) := x"5";
   constant Addr_EDR     : std_logic_vector(3 downto 0) := x"6";
   constant Addr_PID     : std_logic_vector(3 downto 0) := x"7";
   constant Addr_ZPR     : std_logic_vector(3 downto 0) := x"8";
   constant Addr_TLBLO   : std_logic_vector(3 downto 0) := x"9";
   constant Addr_TLBHI   : std_logic_vector(3 downto 0) := x"A";
   constant Addr_TLBX    : std_logic_vector(3 downto 0) := x"B";
   constant Addr_TLBSX   : std_logic_vector(3 downto 0) := x"C";
   constant Addr_PVR     : std_logic_vector(3 downto 0) := x"D";
   
   -- Machine special register (MSR)
   constant MSR_BE       : natural :=  0;
   constant MSR_IE       : natural :=  1;
   constant MSR_C        : natural :=  2;
   constant MSR_BIP      : natural :=  3;
   constant MSR_FSL      : natural :=  4;
   constant MSR_ICE      : natural :=  5;
   constant MSR_DZO      : natural :=  6;
   constant MSR_DCE      : natural :=  7;
   constant MSR_EE       : natural :=  8;
   constant MSR_EIP      : natural :=  9;
   constant MSR_PVR      : natural := 10;
   constant MSR_UM       : natural := 11;
   constant MSR_UMS      : natural := 12;
   constant MSR_VM       : natural := 13;
   constant MSR_VMS      : natural := 14;
   constant MSR_CC       : natural := 31;
   
end spec_reg_pkg;