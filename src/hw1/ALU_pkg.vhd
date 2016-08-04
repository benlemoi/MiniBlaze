-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : ALU_pkg
--   Date             : 07/07/2016
--
--   Description      : Arithmetic Logic Unit Package
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

package ALU_pkg is

   type t_status_alu_out is
      record
         carry    : std_logic;   -- Carry resulting from an addition or a subtraction operation
         zero     : std_logic;   -- All bits of the result (operandC) are logic zero
         negative : std_logic;   -- The result of the arithmetic operation if negative
         overflow : std_logic;   -- 
         parity   : std_logic;   -- Indicates whether an even or odd number of bits on the Y bus are logic one
      end record;
      
   constant c_status_alu_out_null : t_status_alu_out := ('0','0','0','0','0');
      
   type t_opcode_alu is (
      OP_PTA,     -- Pass through reg A
      OP_PTB,     -- Pass through reg B
      OP_ADD,     -- Add
      OP_AND,     -- And
      OP_OR,      -- Or
      OP_SHIFT,   -- Right shift
      OP_XOR,     -- Xor
      OP_SEXT8,   -- Sign extend Byte
      OP_SEXT16,  -- Sign extend Word
      OP_MULT,    -- Multiplication
      OP_BS       -- Barrel Shift
   );
   
   type t_type_carry is (
      CARRY_INPUT,
      CARRY_ONE,
      CARRY_ZERO,
      CARRY_ARITH
   );
   
   type t_type_shift is (
      LEFT_SHIFT,
      RIGHT_SHIFT_ARITH,
      RIGHT_SHIFT_LOGIC
   );
   
   type t_type_mult is (
      LSW,
      HSW
   );
      
   type t_ctrl_op_alu is
      record
         keepCarry   : std_logic;     
         negOperandB : std_logic;
         negOperandA : std_logic;
         whichCarry  : t_type_carry;
         ctrlShift   : t_type_shift;
         multType    : t_type_mult;
      end record;
   constant c_ctrl_op_alu_null : t_ctrl_op_alu := ('0', '0', '0', CARRY_INPUT, LEFT_SHIFT, LSW);
   
   type t_param_alu is
      record
         operation   : t_opcode_alu;
         ctrl_op     : t_ctrl_op_alu;
      end record;
   constant c_param_alu_null : t_param_alu := (OP_PT, c_ctrl_op_alu_null );
    
   function is_zero(a : std_logic_vector) return std_logic;
   function is_negative(a : std_logic_vector) return std_logic;
   function add(a,b : std_logic_vector; c : std_logic) return std_logic_vector;
   function multiply(a, b : std_logic_vector) return std_logic_vector;
   
end ALU_pkg;

package body ALU_pkg is
   
   function is_zero(a : std_logic_vector) return std_logic is
      variable tmp : std_logic_vector(a'range);
   begin
      tmp := (others => '0');
      if tmp = a then
         return '1';
      else
         return '0';
      end if;
   end function;
   
   function is_negative(a : std_logic_vector) return std_logic is
   begin
      if a(a'left) = '1' then
         return '1';
      else
         return '0';
      end if;
   end function;
   
   function add(a,b : std_logic_vector; c : std_logic) return std_logic_vector is
      variable CBIT     : std_logic                   := c;
      variable RESULT   : std_logic_vector(a'left + 1 downto a'right)   := (others => '0');
      alias XA          : std_logic_vector(a'range) is a;
      alias XB          : std_logic_vector(a'range) is b;
   begin
      for i in 0 to a'left loop
         RESULT(i)   := CBIT xor XA(i) xor XB(i);
         CBIT        := (CBIT and XA(i)) or (CBIT and XB(i)) or (XA(i) and XB(i));
      end loop;
      RESULT(a'left + 1) := CBIT;
      return RESULT;
   end add;
   
   -- a & b same size
   function multiply(a, b : std_logic_vector) return std_logic_vector is
      variable A_LEFT   : integer := a'length-1;
      variable result   : std_logic_vector(2*A_LEFT+1 downto 0);
   begin
      result := std_logic_vector(signed(a)*signed(b));
      return result;
   end multiply;
      
      

end;
      