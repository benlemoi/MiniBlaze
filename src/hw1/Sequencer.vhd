-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : Sequencer
--   Date             : 07/07/2016
--
--   Description      : Sequencer of the core. Fetch, decode, execute and store.
--
--   --------------------------------------------------------------------------------
--   Modifications
--   --------------------------------------------------------------------------------
--   Date             : Ver. : Author           : Modification comments
--   --------------------------------------------------------------------------------
--                    :      :                  :
--   25/07/2016       : 1.0  : B.Lemoine        : First draft
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

entity sequencer is
   generic(
      D_WIDTH  : natural := 32
   );
   port(
      -- Clock and reset
      clk               : in  std_logic;
      reset_n           : in  std_logic;
      -- Interface memory in
      data_mem_in_i     : in  std_logic_vector(D_WIDTH-1 downto 0);
      addr_mem_in_o     : out std_logic_vector(D_WIDTH-1 downto 0);
      rd_en_mem_in_o    : out std_logic;
      -- Interface memory out
      addr_mem_out_o    : out std_logic_vector(D_WIDTH-1 downto 0);
      data_mem_out_o    : out std_logic_vector(D_WIDTH-1 downto 0);
      wr_en_mem_out_o   : out std_logic
   );
end sequencer;

architecture rtl of sequencer is

-- Components declaration
component general_purpose_register_bank is
   generic(
      D_WIDTH  : natural := 32
   );
   port(
      clk         : in std_logic;
      addr_i      : in std_logic_vector(4 downto 0);
      data_i      : in std_logic_vector(D_WIDTH-1 downto 0);
      wr_i        : in std_logic;
      data_o      : in std_logic_vector(D_WIDTH-1 downto 0);
   );
)end component;

-- Signals declaration
type  fsm_seq is (st_fetch, st_decode, st_execute, st_store)
signal r_fsm_seq           : fsm_seq := st_fetch;

-- Special purpose registers
signal r_ProgramCounter    : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal r_MSR               : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');

-- General purpose registers
type vect_32x32b is array (range natural 0 to 31) of std_logic_vector(D_WIDTH-1);
signal v_GeneralReg        : vect_32x32b := (others => (others => '0'));


-- Mem in
signal r_addr_in           : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal r_rd_en_mem_in      : std_logic                            := '0';
signal s_instruction       : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');

begin

   s_instruction  <= data_mem_in_i;

   p_seq : process(clk)
   begin
      if rising_edge(clk) then
         if reset_n = '0' then
            r_fsm_seq         <= st_fetch;
         else
            -- Default values
            r_rd_en_mem_in       <= '0';
         
            case r_fsm_seq is
               when st_fetch =>
                  -- Fetch instruction based on the Program Counter value
                  r_addr_mem_in     <= r_ProgramCounter;
                  r_rd_en_mem_in    <= '1';
                  r_fsm_seq         <= st_decode;
               
               when st_decode =>
                  
                  --default value
                  s_input_alu_A                    <= v_GeneralReg(to_integer(to_unsigned(s_instruction(20 downto 16))));
                  r_param_alu.ctrl_op.whichCarry   <= CARRY_INPUT;
                  r_param_alu.ctrl_op.ctrlShift    <= LEFT_SHIFT;
                  r_param_alu.ctrl_op.negOperandA  <= '0';
                  r_param_alu.ctrl_op.negOperandB  <= '0';
                  r_param_alu.ctrl_op.multType     <= LSW;
                  r_wr_carry_output                <= '0';
                  r_allow_next_instruction         <= '0';
                  r_last_op_was_imm                <= '0';
                  r_is_branch                      <= '0';
                  r_is_load_instruction            <= '0';
                  r_is_store_instruction           <= '0';
                  r_load_exclusive                 <= '0';
                  
                  r_fsm_seq                        <= st_execute;
                  
                  -- Type A / Type B instruction
                  if s_instruction(29) = '0' then -- rB
                     s_input_alu_B                    <= v_GeneralReg(to_integer(to_unsigned(s_instruction(15 downto 11))));
                  else -- imm
                     s_input_alu_B                    <= std_logic_vector(resize(unsigned(s_instruction(15 downto 0))));
                  end if;

                  -- Store rD address
                  r_rD_address                     <= s_instruction(25 downto 21);                  
               
                  -- add, addc, addk, addkc, addi, addic, addik, addikc
                  -- rsub, rsubi
                  if (s_instruction(31 downto 30) = "00") and (s_instruction(26) = '0') and (s_instruction(10 downto 0) = (others => '0')) then
                     r_param_alu.operation            <= OP_ADD;
                     if s_instruction(27) = '1' then -- C Bit
                        r_param_alu.ctrl_op.whichCarry <= CARRY_INPUT;
                     else
                        if s_instruction(26) = '1' then -- sub
                           r_param_alu.ctrl_op.whichCarry <= CARRY_ONE;
                        else -- add
                           r_param_alu.ctrl_op.whichCarry <= CARRY_ZERO;
                        end if;
                     end if;
                     if s_instruction(26) = '1' then -- Substrate bit
                        r_param_alu.ctrl_op.negOperandA  <= '1';
                     end if;
                     r_wr_carry_output <= not s_instruction(28);
                                        
                     
                  -- and, andi
                  elsif s_instruction(31 downto 30) = "10" and s_instruction(28 downto 26) = "001" then
                     r_param_alu.operation            <= OP_AND;
                  
                  -- andn, andni
                  elsif s_instruction(31 downto 30) = "10" and s_instruction(28 downto 26) = "011" then
                     r_param_alu.operation            <= OP_AND;
                     r_param_alu.ctrl_op.negOperandB  <= s_instruction(27);
                     
                  -- or, ori
                  elsif s_instruction(31 downto 30) = "10" and s_instruction(28 downto 26) = "000" then   
                     r_param_alu.operation            <= OP_OR;
                  
                  -- xor, xori
                  elsif s_instruction(31 downto 30) = "10" and s_instruction(28 downto 26) = "010" then   
                     r_param_alu.operation            <= OP_XOR;
                                     

                  -- conditional branch instructions
                  elsif (s_instruction(31 downto 30) = "10") and (s_instruction(28 downto 26) = "111") then
                     
                     -- D bit, allow following instruction to complete exection
                     r_allow_next_instruction      <= s_instruction(25);
                     r_is_branch_cond              <= '1';
                     r_branch_op                   <= s_instruction(24 downto 21);
                     r_param_alu.operation         <= OP_PTB;               
                     
                  -- unconditional branch instructions
                  elsif (s_instruction(31 downto 30) = "10") and (s_instruction(28 downto 26) = "110") then 
                     
                     r_allow_next_instruction      <= s_instruction(20);
                     r_is_branch_uncond            <= '1';
                     r_branch_op                   <= s_instruction(20 downto 17);
                     r_param_alu.operation         <= OP_PTB; 

                  -- Barrel Shift : bsrl, bsra, bsll
                  elsif (s_instruction(31 downto 30) = "01") and (s_instruction(28 downto 26) = "001") then 
                                      
                     
                     r_param_alu.operation      <= OP_BS;
                     if s_instruction(15) = '1' then -- bit S (Side bit)
                        r_param_alu.ctrl_op.ctrlShift    <= LEFT_SHIFT;
                     else
                        if s_instruction(14) = '1' then -- bit T (Type bit)
                           r_param_alu.ctrl_op.ctrlShift <= RIGHT_SHIFT_ARITH;
                        else
                           r_param_alu.ctrl_op.ctrlShift <= RIGHT_SHIFT_LOGIC;
                        end if;
                     end if;
                     
                  -- Integer Compare : cmp, cmpu
                  elsif s_instruction(31 downto 26) = "000101" then
                     r_param_alu.operation            <= OP_ADD;
                     r_param_alu.ctrl_op.whichCarry   <= CARRY_ONE;
                     r_param_alu.ctrl_op.negOperandA  <= '1';
                  
                  -- Immediate : imm
                  elsif s_instruction(31 downto 26) = "101100" then
                     r_param_alu.operation            <= OP_PTA;
                  
                  -- Load/Store instruction
                  elsif s_instruction(31 downto 30) = "11" then
                      
                     r_is_load_instruction            <= not s_instruction(28);                     
                     r_is_store_instruction           <= s_instruction(28);
                     r_op_load_store                  <= s_instruction(27 downto 26);
                     r_param_alu.operation            <= OP_ADD;
                     r_param_alu.ctrl_op.whichCarry   <= CARRY_ZERO;
                     if s_instruction(29 downto 26) = "0010" or s_instruction(29 downto 26) = "0110" then
                        r_load_exclusive = '1';
                     end if;
                   
                  -- Multipy instruction : only mul, muli (C_USE_HW_MUL = '1')
                  elsif s_instruction(31 downto 26) = "010000" then
                     r_param_alu.operation            <= OP_MULT;  
                  
                  -- Return from Subroutine
                  elsif (s_instruction(31 downto 25) = "1011011") then 
                     r_allow_next_instruction <= '1';
                     r_param_alu.operation            <= OP_ADD;
                     r_param_alu.ctrl_op.whichCarry   <= CARRY_ZERO;
                     
                  -- Sign Extend Halfword / Byte
                  elsif (s_instruction(31 downto 26) = "100100") then
                     if s_instruction(0) = '1' then
                        r_param_alu.operation            <= OP_SEXT16;
                     else
                        r_param_alu.operation            <= OP_SEXT8;
                     end if;
                        
                  -- Shift
                  elsif (s_instruction(31 downto 26) = "100100") then 
                     r_param_alu.operation   <= OP_SHIFT;
                     r_wr_carry_output       <= '1';
                     case s_instruction(6 downto 5) is
                        when "00"   => r_param_alu.ctrl_op.whichCarry   <= CARRY_ARITH;
                        when "01"   => r_param_alu.ctrl_op.whichCarry   <= CARRY_INPUT;
                        when "10"   => r_param_alu.ctrl_op.whichCarry   <= CARRY_ZERO;
                     end case;
                  
                  end if;
                  
               when st_execute =>
               
                  -- Go to fecth step unless we wait for a memory access
                  r_fsm_seq         <= st_fetch;
               
                  -- Increment r_ProgramCounter of 4 to fetch the next instruction
                  -- Overwrite the value after if a branch is requested
                  r_ProgramCounter  <= r_ProgramCounter + 4;
               
                  -- Store ALU output
                  v_GeneralReg(to_integer(to_unsigned(r_rD_address)))   <= s_output_alu;
                  -- Change MSR carry bit if needed
                  if r_wr_carry_output = '1' then
                     r_MSR(MSR_C)                                       <= s_status_alu.carry;
                  end if;
                  
                  -- Branch execution
                  if r_is_branch_cond = '1' then
                     v_GeneralReg(to_integer(to_unsigned(r_rD_address))) <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)));
                     if r_branch_op = "0000" and s_status_alu.zero = '1' then -- Branch if Equal
                        r_ProgramCounter  <= std_logic_vector(unsigned(r_ProgramCounter) + unsigned(s_output_alu));
                     elsif r_branch_op = "0101" and ((s_status_alu.negative = '0') or (s_status_alu.zero = '1') then -- Branch if Greater or Equal
                        r_ProgramCounter  <= std_logic_vector(unsigned(r_ProgramCounter) + unsigned(s_output_alu));
                     elsif r_branch_op = "0100" and s_status_alu.negative = '0' then -- Branch if Greater Than
                        r_ProgramCounter  <= std_logic_vector(unsigned(r_ProgramCounter) + unsigned(s_output_alu));
                     elsif r_branch_op = "0011" and ((s_status_alu.negative = '1') or (s_status_alu.zero = '1') then -- Branch if Less or Equal
                        r_ProgramCounter  <= std_logic_vector(unsigned(r_ProgramCounter) + unsigned(s_output_alu));
                     elsif r_branch_op = "0010" and s_status_alu.negative = '1' then -- Branch if Less Than
                        r_ProgramCounter  <= std_logic_vector(unsigned(r_ProgramCounter) + unsigned(s_output_alu));
                     elsif r_branch_op = "0001" and s_status_alu.zero = '0' then   -- Branch if Not Equal
                        r_ProgramCounter  <= std_logic_vector(unsigned(r_ProgramCounter) + unsigned(s_output_alu));
                     end if;
                  elsif r_is_branch_uncond = '1' then
                     if r_branch_op(3) = '0' then -- L bit
                        v_GeneralReg(to_integer(to_unsigned(r_rD_address))) <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)));
                     end if;
                     if r_branch_op(2) = '1' then -- A bit
                        r_ProgramCounter  <= s_output_alu;
                     else
                        r_ProgramCounter  <= std_logic_vector(unsigned(r_ProgramCounter) + unsigned(s_output_alu));
                     end if;
                  end if;
                  
                  -- Note : Load & Write this way only works for one cycle access memory
                  
                  -- Load execution
                  if r_is_load_instruction = '1' then
                     r_addr_mem_in    <= s_output_alu;
                     r_rd_en_mem_in   <= '1';
                     r_step_load      <= "01";
                     r_fsm_seq     <= st_execute;
                     if r_step_load = "01" then
                        r_step_load   <= "00";
                        r_fsm_seq     <= st_fetch;
                        if r_op_load_store = "00" then
                           v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(7  downto  0) <= data_mem_in_i(7 downto 0);
                           v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(31 downto  8) <= (others => '0');
                        elsif r_op_load_store = "01" then
                           v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(15 downto  0) <= data_mem_in_i(15 downto 0);
                           v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(31 downto 16) <= (others => '0');
                        elsif r_op_load_store = "10" then
                           v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(31 downto  0) <= data_mem_in_i(31 downto 0);
                           if r_load_exclusive = '1' then
                              r_MSR(MSR_C) <= '0';
                           end if;
                        end if;
                     end if;
                  end if;
                  
                  -- Store execution
                  if r_is_store_instruction = '1' then
                     r_addr_mem_out    <= s_output_alu;
                     if r_op_load_store = "00" then
                        r_wr_en_mem_out(integer(unsigned(s_output_alu(1 downto 0)))) <= '1'; -- others are at '0'
                        case s_output_alu is
                           when "00"   =>
                              r_data_mem_out(7  downto  0)  <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(7  downto  0);
                           when "01"   => 
                              r_data_mem_out(15 downto  8)  <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(7  downto  0);
                           when "10"   => 
                              r_data_mem_out(23 downto 16)  <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(7  downto  0);
                           when others =>
                              r_data_mem_out(31 downto 24)  <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(7  downto  0);
                        end case;
                     elsif r_op_load_store = "01" then
                        if s_output_alu(1) = '0' then
                           r_wr_en_mem_out   <= "0011";
                        else
                           r_wr_en_mem_out   <= "1100";
                        end if;
                        
                        case s_output_alu is
                           when "00"   =>
                              r_data_mem_out(15 downto  0)  <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(15 downto  0);
                           when others =>
                              r_data_mem_out(31 downto 16)  <= v_GeneralReg(to_integer(to_unsigned(r_rD_address)))(15 downto  0);
                        end case;                     
                     
                     elsif r_op_load_store = "11" then
                        r_wr_en_mem_out   <= "1111";
                        r_data_mem_out    <= v_GeneralReg(to_integer(to_unsigned(r_rD_address))):
                        
                     end if;
                  end if;
                     
         end if;
      end if;
   end process;
   
   -- Type B instruction, ALU input B depends of the precedent instruction
   s2_input_alu_B <= s_input_alu_B when r_last_op_was_imm = '0' else (s_input_alu_B(15 downto 0) & r_imm);
   
   s_carry_alu_in <= r_MSR(MSR_C);
   
   i_ALU : ALU
   generic map(
      DATA_WIDTH  => D_WIDTH
   )
   port map(
      param_i     => r_param_alu,
      carry_i     => s_carry_alu_in,
      operandA_i  => s_input_alu_A,
      operandB_i  => s2_input_alu_B,
      operandD_o  => s_output_alu,
      status_o    => s_status_alu
   );
      


end rtl;
      
      
      