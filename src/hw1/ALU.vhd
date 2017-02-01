-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : ALU
--   Date             : 07/07/2016
--
--   Description      : Arithmetic Logic Unit
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

library work;
use work.ALU_pkg.all;

entity ALU is
   generic(
      DATA_WIDTH  : natural := 32
   );
   port(
      param_i     : in  t_param_alu;
      carry_i     : in  std_logic;
      operandA_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      operandB_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      operandD_o  : out std_logic_vector(DATA_WIDTH - 1 downto 0);
      status_o    : out t_status_alu_out
   );
end ALU;

architecture rtl of ALU is

begin

   p_alu : process(param_i, carry_i, operandA_i, operandB_i)
   
   variable result            : signed(DATA_WIDTH downto 0)                := (others => '0');
   variable result_add        : signed(DATA_WIDTH downto 0)                := (others => '0');
   variable result_mult       : signed(2*(DATA_WIDTH-1)+1 downto 0)        := (others => '0');
   variable carry_in          : std_logic                                  := '0';
   variable carry_out         : std_logic                                  := '0';
   variable zero_out          : std_logic                                  := '0';
   variable negative_out      : std_logic                                  := '0';
   variable overflow_out      : std_logic                                  := '0';
   variable parity_out        : std_logic                                  := '0';
   variable result_out        : std_logic_vector(DATA_WIDTH - 1 downto 0)  := (others => '0');
   variable operandA_sext8    : std_logic_vector(DATA_WIDTH - 9 downto 0)  := (others => '0');
   variable operandA_signed   : signed(DATA_WIDTH - 1 downto 0)            := (others => '0');
   variable operandB_signed   : signed(DATA_WIDTH - 1 downto 0)            := (others => '0');
   variable operandA_unsigned : unsigned(DATA_WIDTH - 1 downto 0)          := (others => '0');
   variable operandB_unsigned : unsigned(DATA_WIDTH - 1 downto 0)          := (others => '0');
   
   begin
   
      -- Chose carry in
      case param_i.ctrl_op.whichCarry is
         when CARRY_INPUT =>
            carry_in := carry_i;
         when CARRY_ONE =>
            carry_in := '1';
         when CARRY_ZERO =>
            carry_in := '0';
         when CARRY_ARITH =>
            carry_in := operandA_i(operandA_i'left);
      end case;
      
      result_add     := signed(add(operandA_i, operandB_i, carry_in)); 
      result_mult    := signed(multiply(operandA_i, operandB_i));
      
      operandA_sext8    := (others => operandA_i(7));
      
      operandA_signed   := signed(operandA_i);
      operandB_signed   := signed(operandB_i);
                        
      operandA_unsigned := unsigned(operandA_i);
      operandB_unsigned := unsigned(operandB_i);      
   
      case param_i.operation is
         when OP_PTA =>
            result   := operandA_signed(31) & operandA_signed;
         when OP_PTB =>
            result   := operandB_signed(31) & operandB_signed;
         when OP_ADD =>
            result   := result_add;
         when OP_AND =>
            result   := signed('0' & (operandA_i and operandB_i));
         when OP_OR =>
            result   := signed('0' & (operandA_i or operandB_i));
         when OP_SHIFT => 
            result   := operandA_signed(0) & carry_in & operandA_signed(operandA_signed'left downto 1);
         when OP_XOR =>
            result   := signed('0' & (operandA_i xor operandB_i));
         when OP_SEXT8 => 
            result   := '0' & signed(operandA_sext8) &  signed(operandA_i(7 downto 0));
         when OP_SEXT16 =>
            result   := '0' & resize(operandA_signed(15 downto 0), DATA_WIDTH);
         when OP_MULT =>
            case param_i.ctrl_op.multType is
               when LSW    => result := result_mult(DATA_WIDTH downto 0);
               when HSW    => result := result_mult(2*(DATA_WIDTH-1)+1) & result_mult(2*(DATA_WIDTH-1)+1 downto DATA_WIDTH);
            end case;
         when OP_BS =>
            case param_i.ctrl_op.ctrlShift is
               when LEFT_SHIFT         => result := '0' & shift_left(operandA_signed, to_integer(operandB_unsigned(4 downto 0)));
               when RIGHT_SHIFT_ARITH  => result := '0' & shift_right(operandA_signed, to_integer(operandB_unsigned(4 downto 0))); -- Insert left most b
               when RIGHT_SHIFT_LOGIC  => result := '0' & signed(shift_right(operandA_unsigned, to_integer(operandB_unsigned(4 downto 0)))); -- Insert 0 
               when others             => result := (others => '0');
            end case;
         when others =>
            result   := (others => '0');
            assert false report "Unknown operation for ALU" severity error;
      end case;
      
      carry_out      := result(DATA_WIDTH);
      result_out     := std_logic_vector(result(DATA_WIDTH-1 downto 0));
      zero_out       := is_zero(result_out);
      negative_out   := is_negative(result_out);
      overflow_out   := (not carry_out and result_out(result_out'left)) or (carry_out and not result_out(result_out'left));
      parity_out     := '0'; -- TODO
      
      -- Mapping output
      status_o.carry    <= carry_out;
      status_o.zero     <= zero_out;
      status_o.negative <= negative_out;
      status_o.overflow <= overflow_out;
      status_o.parity   <= parity_out;
      operandD_o        <= result_out;
      
   end process;

end rtl;
      
