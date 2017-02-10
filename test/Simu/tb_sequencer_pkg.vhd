-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : tb_sequencer_pkg
--   Date             : 09/02/2017
--
--   Description      : Package for the test bench of the Sequencer unit
--
--   --------------------------------------------------------------------------------
--   Modifications
--   --------------------------------------------------------------------------------
--   Date             : Ver. : Author           : Modification comments
--   --------------------------------------------------------------------------------
--                    :      :                  :
--   09/02/2017      : 1.0  : B.Lemoine        : First draft
--                    :      :                  :
-- **********************************************************************************
--   MIT License
--   
--   Copyright (c) 2017, Benjamin Lemoine
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

package tb_sequencer_pkg is

   -- Constant declaration
   constant D_WIDTH        : integer   := 32;   
   constant SIZE_MEM       : integer   := 4;
   constant c_zero         : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
   constant NB_COL         : integer   := 4;
   constant COL_WIDTH      : integer   := D_WIDTH/4;
   constant C_PERIOD       : time      := 8 ns;
   constant SIZE           : natural   := 2**SIZE_MEM;
   constant NB_WAIT_CLK    : integer   := 100;

   type ram_type is array (SIZE-1 downto 0) of std_logic_vector (D_WIDTH-1 downto 0);

   function instr_A(rD,rA,rB : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector;
   function instr_B(rD,rA,imm : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector;
   function instr_A_bsl(rD,rA,rB : integer; opcode : std_logic_vector(5 downto 0);S,T : std_logic) return std_logic_vector;  
   function instr_B_bsl(rD,rA,imm : integer; opcode : std_logic_vector(5 downto 0);S,T : std_logic) return std_logic_vector;
   function instr_shift(rD,rA : integer; opcode : std_logic_vector(5 downto 0);S,T : std_logic) return std_logic_vector;
   function instr_sext(rD,rA : integer; opcode : std_logic_vector(5 downto 0);s16 : boolean) return std_logic_vector;
   function instr_A_branch_unc(rD,rB : integer; opcode : std_logic_vector(5 downto 0);D,A,L : std_logic) return std_logic_vector;
   function instr_A_branch_cond(rA,rB : integer; opcode : std_logic_vector(5 downto 0);opts : std_logic_vector(4 downto 0)) return std_logic_vector;
   
   
   
   type data_test is
      record
         program : ram_type;
         results : ram_type;
      end record;
   
   constant N : integer := 100;   
   type vect_data_test is array(0 to N-1) of data_test;
   constant c_test : vect_data_test := (
      0 =>  (  -- ADD Rd,Ra,Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
                  1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
                  2 => instr_A( 2, 0, 1, "000000"),  -- Instruction to test (result in r2)
                  3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
                  4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
                  13 => (others => '0'),
                  14 => x"00120000",
                  15 => x"00003456",
                  others => (others => '0')
               ),
               (
                  0 => x"00123456",
                  others => (others => '0')
               )
            ),
      1 =>  ( -- RSUB Rd,Ra,Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
                  1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
                  2 => instr_A( 2, 0, 1, "000001"),  -- Instruction to test (result in r2)
                  3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
                  4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
                  13 => (others => '0'),
                  14 => x"00000000",
                  15 => x"00000004",
                  others => (others => '0')
               ),
               (
                  0 => x"00000004",
                  others => (others => '0')
               )
            ),    
      2 => (( -- ADDC Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 5, 5, 0, "000001"),  -- Set carry to one
               3 => instr_A( 2, 0, 1, "000010"),  -- Instruction to test (result in r2)
               4 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               5 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"00003456",
               15 => x"00120000",
               others => (others => '0')),            
            (
              0 => x"00123457",
              others => (others => '0'))),             
      3 => (( -- RSUBC Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 5, 5, 0, "000001"),  -- Set carry to one
               3 => instr_A( 2, 0, 1, "000011"),  -- Instruction to test (result in r2)
               4 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               5 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"FFFFFFFF",
               15 => x"00120000",
               others => (others => '0')),
            (
              0 => x"00120001",
              others => (others => '0'))),             
      4 => (( -- ADDK Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 5, 5, 0, "000100"),  -- Set carry to one
               3 => instr_A( 2, 0, 1, "000011"),  -- Instruction to test (result in r2)
               4 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               5 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"FFFFFFFF",
               15 => x"00120000",
               others => (others => '0')),  
            (
              0 => x"00120000",
              others => (others => '0'))),             
      5 => (( -- CMP Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 2, 0, 1, "000101"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"00000012",
               15 => x"00000011",
               others => (others => '0')),
            (
              0 => x"80000000",
              others => (others => '0'))),             
      6 => (( -- ADDI Rd,Ra,Imm
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_B( 2, 0, 1544, "001000"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"10305070",
               15 => x"00000011",
               others => (others => '0')),
            (
              0 => x"10305678",
              others => (others => '0'))),             
      7 => (( -- MUL Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 2, 0, 1, "010000"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"00001234",
               15 => x"00010000",
               others => (others => '0')),
            (
              0 => x"12340000",
              others => (others => '0'))),             
      8 => (( -- BSRA Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A_bsl( 2, 0, 1, "010001",'0','1'),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"FFF12345",
               15 => x"00000004",
               others => (others => '0')),
            (
              0 => x"FFFF1234",
              others => (others => '0'))),             
      9 => (( -- BSLL Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A_bsl( 2, 0, 1, "010001",'1','0'),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"FFF12345",
               15 => x"00000004",
               others => (others => '0')),
            (
              0 => x"FF123450",
              others => (others => '0'))), 
      10 => (( -- MULI Rd,Ra,Imm
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_B( 2, 0, 5, "011000"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"00001234",
               15 => x"00010000",
               others => (others => '0')),
            (
              0 => x"00005B04",
              others => (others => '0'))),             
      11 => (( -- BSRLI Rd,Ra,Imm
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_B_bsl( 2, 0, 6, "011001",'0','0'),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"01001234",
               15 => x"00010000",
               others => (others => '0')),
            (
              0 => x"00040048",
              others => (others => '0'))),             
      12 => (( -- BSRAI Rd,Ra,Imm
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_B_bsl( 2, 0, 7, "011001",'0','1'),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"FF245231",
               15 => x"00000000",
               others => (others => '0')),
            (
              0 => x"FFFE48A4",
              others => (others => '0'))),             
      13 => (( -- BSLLI Rd,Ra,Imm
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_B_bsl( 2, 0, 3, "011001",'1','0'),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"01001234",
               15 => x"00010000",
               others => (others => '0')),
            (
              0 => x"080091A0",
              others => (others => '0'))),             
      14 => (( -- OR Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 2, 0, 1, "100000"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"01235555",
               15 => x"7020AAAA",
               others => (others => '0')),     
            (
              0 => x"7123FFFF",
              others => (others => '0'))),             
      15 => (( -- AND Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 2, 0, 1, "100001"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"01235555",
               15 => x"702FAAAA",
               others => (others => '0')),    
            (
              0 => x"00230000",
              others => (others => '0'))),             
      16 => (( -- XOR Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 2, 0, 1, "100010"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"0123AAAA",
               15 => x"702FCCCC",
               others => (others => '0')),
            (
              0 => x"710C6666",
              others => (others => '0'))),             
      17 => (( -- ANDN Rd,Ra,Rb
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_A( 2, 0, 1, "100011"),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"01235555",
               15 => x"702FAAAA",
               others => (others => '0')),
            (
              0 => x"01005555",
              others => (others => '0'))),             
      18 => (( -- SRA Rd,Ra
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_shift( 2, 0, "100100", '0', '0'),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"F123AAA1",
               15 => x"00000000",
               others => (others => '0')),
            (
              0 => x"F891D550",
              others => (others => '0'))),             
     19 => (( -- SRC Rd,Ra
               0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
               1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
               2 => instr_shift( 2, 0, "100100", '0', '1'),  -- Instruction to test (result in r2)
               3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
               4 => instr_shift( 3, 0, "100100", '0', '1'),  -- Instruction to test (result in r2)
               5 => instr_B( 3, 4, 48, "111110"), -- Store r2 at addr 0x14
               6 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
               13 => (others => '0'),
               14 => x"F123AAA1",
               15 => x"00000000",
               others => (others => '0')),
            (
              0 => x"7891D550",
              1 => x"F891D550",
              others => (others => '0'))),
      20 => (  -- SRL Rd,Ra
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
                  1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
                  2 => instr_shift( 2, 0, "100100", '1', '0'),  -- Instruction to test (result in r2)
                  3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
                  4 => instr_shift( 3, 0, "100100", '1', '0'),  -- Instruction to test (result in r2)
                  5 => instr_B( 3, 4, 48, "111110"), -- Store r3 at addr 48
                  6 => instr_shift( 5, 0, "100100", '0', '1'),  -- Instruction to test (result in r2)
                  7 => instr_B( 5, 4, 44, "111110"), -- Store r5 at addr 44
                  8 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
                  13 => (others => '0'),
                  14 => x"F123AAA1",
                  15 => x"00000000",
                  others => (others => '0')
               ),
               (
                  0 => x"7891D550",
                  1 => x"7891D550",
                  2 => x"F891D550",
                  others => (others => '0')
               )
            ),
      21 => (  -- SEXT8 Rd,Ra
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
                  1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
                  2 => instr_sext( 2, 0, "100100", false),  -- Instruction to test (result in r2)
                  3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
                  4 => instr_sext( 3, 1, "100100", false),  -- Instruction to test (result in r2)
                  5 => instr_B( 3, 4, 48, "111110"), -- Store r2 at addr 0x14                  
                  6 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
                  13 => (others => '0'),
                  14 => x"000000C8",
                  15 => x"00000068",
                  others => (others => '0')
               ),
               (
                  0 => x"FFFFFFC8",
                  1 => x"00000068",
                  others => (others => '0')
               )
            ),
      22 => (  -- SEXT16 Rd,Ra
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- Load *(0x18) into r0
                  1 => instr_B( 1, 4, 60, "111010"), -- Load *(0x1C) into r1
                  2 => instr_sext( 2, 0, "100100", true),  -- Instruction to test (result in r2)
                  3 => instr_B( 2, 4, 52, "111110"), -- Store r2 at addr 0x14
                  4 => instr_sext( 3, 1, "100100", true),  -- Instruction to test (result in r2)
                  5 => instr_B( 3, 4, 48, "111110"), -- Store r2 at addr 0x14                  
                  6 => instr_B( 0, 0, 0, "101110"),  -- Jump at 0x10 (while(1))
                  13 => (others => '0'),
                  14 => x"0000C123",
                  15 => x"00005123",
                  others => (others => '0')
               ),
               (
                  0 => x"FFFFC123",
                  1 => x"00005123",
                  others => (others => '0')
               )
            )            ,
      23 => (  -- BR Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"),
                  1 => instr_B( 1, 4, 60, "111010"),
                  2 => instr_A( 7, 7, 1, "100110"),
                  3 => instr_B( 0, 4, 52, "111110"), 
                  4 => instr_B( 0, 4, 48, "111110"),
                  5 => instr_B( 0, 0, 0, "101110"),
                  13 => (others => '0'),
                  14 => x"0000C123",
                  15 => x"00000008",
                  others => (others => '0')
               ),
               (
                  0 => x"00000000",
                  1 => x"0000C123",
                  others => (others => '0')
               )
            ),
      24 => (  -- BRD Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- 0
                  1 => instr_B( 1, 4, 60, "111010"), -- 4
                  2 => instr_A_branch_unc(0, 1, "100110", '1','0','0'), -- 8
                  3 => instr_B( 0, 4, 52, "111110"),  -- 12
                  4 => (others => '0'), -- 16
                  5 => instr_B( 1, 4, 48, "111110"), -- 20
                  6 => instr_B( 0, 0, 0, "101110"), -- 24
                  13 => (others => '0'),
                  14 => x"0000C123",
                  15 => x"0000000C",
                  others => (others => '0')
               ),
               (
                  0 => x"0000C123",
                  1 => x"0000000C",
                  others => (others => '0')
               )
            ),
      25 => (  -- BRLD Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- 0
                  1 => instr_B( 1, 4, 60, "111010"), -- 4
                  2 => instr_A_branch_unc(3, 1, "100110", '1','0','1'), -- 8
                  3 => instr_B( 0, 4, 52, "111110"),  -- 12
                  4 => (others => '0'), -- 16
                  5 => instr_B( 3, 4, 48, "111110"), -- 20
                  6 => instr_B( 0, 0, 0, "101110"), -- 24
                  13 => (others => '0'),
                  14 => x"0000C123",
                  15 => x"0000000C",
                  others => (others => '0')
               ),
               (
                  0 => x"0000C123", -- r0
                  1 => x"00000008", -- pc
                  others => (others => '0')
               )
            ),
      26 => (  -- BRA Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- 0
                  1 => instr_B( 1, 4, 60, "111010"), -- 4
                  2 => instr_A_branch_unc(0, 1, "100110", '0','1','0'), -- 8
                  3 => instr_B( 0, 4, 52, "111110"),  -- 12
                  4 => (others => '0'), -- 16
                  5 => instr_B( 0, 4, 48, "111110"), -- 20
                  6 => instr_B( 0, 0, 0, "101110"), -- 24
                  13 => (others => '0'),
                  14 => x"0000C123",
                  15 => x"00000014",
                  others => (others => '0')
               ),
               (
                  0 => x"00000000", -- r0
                  1 => x"0000C123", -- pc
                  others => (others => '0')
               )
            ),
      27 => (  -- BRAD Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- 0
                  1 => instr_B( 1, 4, 60, "111010"), -- 4
                  2 => instr_A_branch_unc(0, 1, "100110", '1','1','0'), -- 8
                  3 => instr_B( 0, 4, 52, "111110"),  -- 12
                  4 => (others => '0'), -- 16
                  5 => instr_B( 0, 4, 48, "111110"), -- 20
                  6 => instr_B( 0, 0, 0, "101110"), -- 24
                  13 => (others => '0'),
                  14 => x"0000C123",
                  15 => x"00000014",
                  others => (others => '0')
               ),
               (
                  0 => x"0000C123", 
                  1 => x"0000C123", 
                  others => (others => '0')
               )
            ),
      28 => (  -- BRALD Rb
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- 0
                  1 => instr_B( 1, 4, 60, "111010"), -- 4
                  2 => instr_A_branch_unc(3, 1, "100110", '1','1','1'), -- 8
                  3 => instr_B( 0, 4, 52, "111110"),  -- 12
                  4 => (others => '0'), -- 16
                  5 => instr_B( 3, 4, 48, "111110"), -- 20
                  6 => instr_B( 0, 0, 0, "101110"), -- 24
                  13 => (others => '0'),
                  14 => x"0000C123",
                  15 => x"00000014",
                  others => (others => '0')
               ),
               (
                  0 => x"0000C123", 
                  1 => x"00000008", 
                  others => (others => '0')
               )
            ),
      29 => (  -- BEQ
               (
                  0 => instr_B( 0, 4, 56, "111010"), -- 0
                  1 => instr_B( 1, 4, 60, "111010"), -- 4
                  2 => instr_A_branch_cond(1, 0, "100111", "00000"), -- 8
                  3 => instr_B( 0, 4, 52, "111110"),  -- 12
                  4 => instr_A_branch_cond(0, 0, "100111", "00000"), -- 16
                  5 => instr_B( 0, 4, 48, "111110"), -- 20
                  6 => instr_B( 0, 0, 0, "101110"), -- 24
                  13 => (others => '0'),
                  14 => x"00000008",
                  15 => x"00000000",
                  others => (others => '0')
               ),
               (
                  0 => x"00000000", 
                  1 => x"00000008", 
                  others => (others => '0')
               )
            ),            
            
      others => ((others => (others => '0')),(others => (others => '0')))
   );
   
end tb_sequencer_pkg;

package body tb_sequencer_pkg is

   function instr_A(rD,rA,rB : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(15 downto 11) := std_logic_vector(to_unsigned(rB,5));
      
      return result;
   end function;
   
   function instr_B(rD,rA,imm : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(15 downto  0) := std_logic_vector(to_unsigned(imm,16));
      
      return result;
   end function;

   function instr_A_bsl(rD,rA,rB : integer; opcode : std_logic_vector(5 downto 0);S,T : std_logic) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(15 downto 11) := std_logic_vector(to_unsigned(rB,5));
      result(10)           := S;
      result(9)            := T;
      
      return result;
   end function; 

   function instr_B_bsl(rD,rA,imm : integer; opcode : std_logic_vector(5 downto 0);S,T : std_logic) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(10)           := S;
      result(9)            := T;      
      result(4 downto  0)  := std_logic_vector(to_unsigned(imm,5));      
      return result;
   end function;   
      
   function instr_shift(rD,rA : integer; opcode : std_logic_vector(5 downto 0);S,T : std_logic) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(6)            := S;
      result(5)            := T;      
      result(0)            := '1';
      return result;
   end function;

   function instr_sext(rD,rA : integer; opcode : std_logic_vector(5 downto 0);s16 : boolean) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(6)            := '1';
      result(5)            := '1';      
      if s16 then
         result(0)            := '1';
      else
         result(0)            := '0';
      end if;
      return result;
   end function;    
   
   function instr_A_branch_unc(rD,rB : integer; opcode : std_logic_vector(5 downto 0);D,A,L : std_logic) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20)           := D;
      result(19)           := A;
      result(18)           := L;
      result(15 downto 11) := std_logic_vector(to_unsigned(rB,5));
      return result;
   end function; 

   function instr_A_branch_cond(rA,rB : integer; opcode : std_logic_vector(5 downto 0);opts : std_logic_vector(4 downto 0)) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25)           := opts(4);
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(15 downto 11) := std_logic_vector(to_unsigned(rB,5));
      return result;
   end function;   
   

end;
