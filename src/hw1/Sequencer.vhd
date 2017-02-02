-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : Sequencer
--   Date             : 07/25/2016
--
--   Description      : Sequencer of the core. Fetch, decode, execute and store.
--                      This implementation (v1) does not aim to be fast. There is
--                      no pipeline, memory access are slow. The goal is to have
--                      a functionnal core that can be played with.
--
--   --------------------------------------------------------------------------------
--   Modifications
--   --------------------------------------------------------------------------------
--   Date             : Ver. : Author           : Modification comments
--   --------------------------------------------------------------------------------
--                    :      :                  :
--   07/25/2016       : 1.0  : B.Lemoine        : First draft
--                    :      :                  :
-- **********************************************************************************
--   MIT License
--   
--   Copyright (c) 07/25/2016, Benjamin Lemoine
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
use work.spec_reg_pkg.all;

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
      data_mem_in_en_i  : in  std_logic;
      addr_mem_in_o     : out std_logic_vector(D_WIDTH-1 downto 0);
      rd_en_mem_in_o    : out std_logic;
      -- Interface memory out
      addr_mem_out_o    : out std_logic_vector(D_WIDTH-1 downto 0);
      data_mem_out_o    : out std_logic_vector(D_WIDTH-1 downto 0);
      wr_ack_mem_out_o  ; out std_logic;
      wr_en_mem_out_o   : out std_logic_vector(3 downto 0)
   );
end sequencer;

architecture rtl of sequencer is

-- Components declaration
component ALU is
   generic(
      DATA_WIDTH  : natural := D_WIDTH
   );
   port(
      param_i     : in  t_param_alu;
      carry_i     : in  std_logic;
      operandA_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      operandB_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      operandD_o  : out std_logic_vector(DATA_WIDTH - 1 downto 0);
      status_o    : out t_status_alu_out
   );
end component;

-- Signals declaration
type  fsm_seq is (st_fetch, st_decode, st_execute);
signal r_fsm_seq                    : fsm_seq := st_fetch;

-- Special purpose registers
signal r_ProgramCounter             : unsigned(D_WIDTH-1 downto 0)         := (others => '0');
signal r_MSR                        : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal r_imm                        : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');

-- General purpose registers
type vect_32x32b is array (0 to 31) of std_logic_vector(D_WIDTH-1 downto 0);
signal v_GeneralReg                 : vect_32x32b := (others => (others => '0'));


-- Mem in
signal r_addr_mem_in                : unsigned(D_WIDTH-1 downto 0)         := (others => '0');
signal r_rd_en_mem_in               : std_logic                            := '0';
signal s_instruction                : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal r_instruction                : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
         
-- Mem out        
signal r_wr_en_mem_out              : std_logic_vector(3 downto 0)         := (others => '0');
signal r_addr_mem_out               : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal r_data_mem_out               : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');

-- ALU
signal r_input_alu_A                : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal r_input_alu_B                : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal s_input_alu_B                : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal r_param_alu                  : t_param_alu                          := c_param_alu_null;
signal s_carry_alu_in               : std_logic                            := '0';
signal s_output_alu                 : std_logic_vector(D_WIDTH-1 downto 0) := (others => '0');
signal s_status_alu                 : t_status_alu_out                     := c_status_alu_out_null;

-- Sequencer
signal r_wr_carry_output            : std_logic                            := '0';
signal r_allow_next_instruction     : std_logic                            := '0';
signal r_last_op_was_imm            : std_logic                            := '0';
signal r_is_branch_cond             : std_logic                            := '0';
signal r_is_branch_uncond           : std_logic                            := '0';
signal r_is_load_instruction        : std_logic                            := '0';
signal r_is_store_instruction       : std_logic                            := '0';
signal r_load_store_exclusive       : std_logic                            := '0';
signal r_rD_address                 : std_logic_vector(4 downto 0)         := (others => '0');
signal r_return_from_subroutine     : std_logic                            := '0';
signal r_branch_op                  : std_logic_vector(3 downto 0)         := (others => '0');
signal r_op_load_store              : std_logic_vector(1 downto 0)         := (others => '0');
signal r_step_load                  : unsigned(1 downto 0)                 := (others => '0');
signal r_step_fetch                 : std_logic                            := '0';


type fsm_fetch is (st_set_address, st_wait_data);
signal r_fsm_fetch                  : fsm_fetch                            := st_set_address;
signal r_fsm_load                   : fsm_fetch                            := st_set_address;




begin

   s_instruction  <= data_mem_in_i;

   p_seq : process(clk)
   begin
      if rising_edge(clk) then
         if reset_n = '0' then
            r_fsm_seq         <= st_fetch;
            r_ProgramCounter  <= (others => '0');
            r_fsm_fetch       <= st_set_address;
            r_fsm_load        <= st_set_address;
         else
            -- Default values
            r_rd_en_mem_in       <= '0';
            r_wr_en_mem_out      <= (others => '0');
         
            case r_fsm_seq is
               when st_fetch =>
                  -- Fetch instruction
                  case r_fsm_fetch is
                     when st_set_address =>
                        if r_allow_next_instruction = '1' then
                           r_addr_mem_in     <= r_addr_mem_in + 4;
                        else
                           r_addr_mem_in     <= r_ProgramCounter;
                        end if;
                        r_rd_en_mem_in    <= '1';
                        r_fsm_fetch <= st_wait_data;
                     
                     when st_wait_data =>
                        if data_mem_in_en_i = '1' then
                           r_fsm_seq      <= st_decode;
                           r_instruction  <= s_instruction;
                        end if;
                     
                     when others =>
                        r_fsm_fetch <= st_set_address;
                  end case

               when st_decode =>
                  
                  --default value
                  r_input_alu_A                    <= v_GeneralReg(to_integer(unsigned(r_instruction(20 downto 16))));
                  r_param_alu.ctrl_op.whichCarry   <= CARRY_INPUT;
                  r_param_alu.ctrl_op.ctrlShift    <= LEFT_SHIFT;
                  r_param_alu.ctrl_op.negOperandA  <= '0';
                  r_param_alu.ctrl_op.negOperandB  <= '0';
                  r_param_alu.ctrl_op.multType     <= LSW;
                  r_wr_carry_output                <= '0';
                  r_allow_next_instruction         <= '0';
                  r_last_op_was_imm                <= '0';
                  r_is_branch_cond                 <= '0';
                  r_is_branch_uncond               <= '0';
                  r_is_load_instruction            <= '0';
                  r_is_store_instruction           <= '0';
                  r_load_store_exclusive           <= '0';
                  r_return_from_subroutine         <= '0';
                  
                  -- Next stage
                  r_fsm_seq                        <= st_execute;
                  
                  -- Type A / Type B instruction
                  if r_instruction(29) = '0' then -- rB
                     r_input_alu_B                    <= v_GeneralReg(to_integer(unsigned(r_instruction(15 downto 11))));
                  else -- imm
                     r_input_alu_B                    <= std_logic_vector(resize(unsigned(r_instruction(15 downto 0)), D_WIDTH));
                  end if;

                  -- Store rD address
                  r_rD_address                     <= r_instruction(25 downto 21);                  
               
                  -- add, addc, addk, addkc, addi, addic, addik, addikc
                  -- rsub, rsubi
                  if (r_instruction(31 downto 30) = "00") then
                     r_param_alu.operation            <= OP_ADD;
                     if r_instruction(27) = '1' then -- C Bit
                        r_param_alu.ctrl_op.whichCarry <= CARRY_INPUT;
                     else
                        if r_instruction(26) = '1' then -- sub
                           r_param_alu.ctrl_op.whichCarry <= CARRY_ONE;
                        else -- add
                           r_param_alu.ctrl_op.whichCarry <= CARRY_ZERO;
                        end if;
                     end if;
                     if r_instruction(26) = '1' then -- Substrate bit
                        r_param_alu.ctrl_op.negOperandA  <= '1';
                     end if;
                     r_wr_carry_output <= not r_instruction(28);
                                        
                     
                  -- and, andi
                  elsif r_instruction(31 downto 30) = "10" and r_instruction(28 downto 26) = "001" then
                     r_param_alu.operation            <= OP_AND;
                  
                  -- andn, andni
                  elsif r_instruction(31 downto 30) = "10" and r_instruction(28 downto 26) = "011" then
                     r_param_alu.operation            <= OP_AND;
                     r_param_alu.ctrl_op.negOperandB  <= r_instruction(27);
                     
                  -- or, ori
                  elsif r_instruction(31 downto 30) = "10" and r_instruction(28 downto 26) = "000" then   
                     r_param_alu.operation            <= OP_OR;
                  
                  -- xor, xori
                  elsif r_instruction(31 downto 30) = "10" and r_instruction(28 downto 26) = "010" then   
                     r_param_alu.operation            <= OP_XOR;
                                     

                  -- conditional branch instructions
                  elsif (r_instruction(31 downto 30) = "10") and (r_instruction(28 downto 26) = "111") then
                     
                     -- D bit, allow following instruction to complete exection
                     r_allow_next_instruction      <= r_instruction(25);
                     r_is_branch_cond              <= '1';
                     r_branch_op                   <= r_instruction(24 downto 21);
                     r_param_alu.operation         <= OP_PTB;               
                     
                  -- unconditional branch instructions
                  elsif (r_instruction(31 downto 30) = "10") and (r_instruction(28 downto 26) = "110") then 
                     
                     r_allow_next_instruction      <= r_instruction(20);
                     r_is_branch_uncond            <= '1';
                     r_branch_op                   <= r_instruction(20 downto 17);
                     r_param_alu.operation         <= OP_PTB; 

                  -- Barrel Shift : bsrl, bsra, bsll
                  elsif (r_instruction(31 downto 30) = "01") and (r_instruction(28 downto 26) = "001") then 
                                      
                     
                     r_param_alu.operation      <= OP_BS;
                     if r_instruction(15) = '1' then -- bit S (Side bit)
                        r_param_alu.ctrl_op.ctrlShift    <= LEFT_SHIFT;
                     else
                        if r_instruction(14) = '1' then -- bit T (Type bit)
                           r_param_alu.ctrl_op.ctrlShift <= RIGHT_SHIFT_ARITH;
                        else
                           r_param_alu.ctrl_op.ctrlShift <= RIGHT_SHIFT_LOGIC;
                        end if;
                     end if;
                     
                  -- Integer Compare : cmp, cmpu
                  elsif r_instruction(31 downto 26) = "000101" then
                     r_param_alu.operation            <= OP_ADD;
                     r_param_alu.ctrl_op.whichCarry   <= CARRY_ONE;
                     r_param_alu.ctrl_op.negOperandA  <= '1';
                  
                  -- Immediate : imm
                  elsif r_instruction(31 downto 26) = "101100" then
                     r_param_alu.operation            <= OP_PTA;
                  
                  -- Load/Store instruction
                  elsif r_instruction(31 downto 30) = "11" then
                      
                     r_is_load_instruction            <= not r_instruction(28);                     
                     r_is_store_instruction           <= r_instruction(28);
                     r_op_load_store                  <= r_instruction(27 downto 26);
                     r_param_alu.operation            <= OP_ADD;
                     r_param_alu.ctrl_op.whichCarry   <= CARRY_ZERO;
                     if r_instruction(10) = '1' and  r_instruction(29) = '0' then
                        r_load_store_exclusive        <= '1';
                     end if;
                   
                  -- Multipy instruction : only mul, muli (C_USE_HW_MUL = '1')
                  elsif r_instruction(31 downto 30) = "01" and r_instruction(28 downto 26) = "000" then
                     r_param_alu.operation            <= OP_MULT;  
                  
                  -- Return from Subroutine
                  elsif (r_instruction(31 downto 25) = "1011011") then 
                     r_allow_next_instruction         <= '1';
                     r_param_alu.operation            <= OP_ADD;
                     r_param_alu.ctrl_op.whichCarry   <= CARRY_ZERO;
                     r_return_from_subroutine         <= '1';
                     
                  -- Sign Extend Halfword / Byte
                  elsif (r_instruction(31 downto 26) = "100100") then
                     if r_instruction(0) = '1' then
                        r_param_alu.operation            <= OP_SEXT16;
                     else
                        r_param_alu.operation            <= OP_SEXT8;
                     end if;
                        
                  -- Shift
                  elsif (r_instruction(31 downto 26) = "100100") then 
                     r_param_alu.operation   <= OP_SHIFT;
                     r_wr_carry_output       <= '1';
                     case r_instruction(6 downto 5) is
                        when "00"   => r_param_alu.ctrl_op.whichCarry   <= CARRY_ARITH;
                        when "01"   => r_param_alu.ctrl_op.whichCarry   <= CARRY_INPUT;
                        when "10"   => r_param_alu.ctrl_op.whichCarry   <= CARRY_ZERO;
                        when others => r_param_alu.ctrl_op.whichCarry   <= CARRY_ZERO;
                     end case;
                  
                  end if;
                  
               when st_execute =>
               
                  -- Go to fecth step unless we wait for a memory access
                  r_fsm_seq         <= st_fetch;
               
                  -- Increment r_ProgramCounter of 4 to fetch the next instruction
                  -- Overwrite the value after if a branch is requested
                  r_ProgramCounter  <= r_ProgramCounter + 4;
               
                  -- Store ALU output
                  v_GeneralReg(to_integer(unsigned(r_rD_address)))   <= s_output_alu;
                  -- Change MSR carry bit if needed
                  if r_wr_carry_output = '1' then
                     r_MSR(MSR_C)                                       <= s_status_alu.carry;
                  end if;
                  
                  -- Branch execution
                  if r_is_branch_cond = '1' then
                     v_GeneralReg(to_integer(unsigned(r_rD_address))) <= v_GeneralReg(to_integer(unsigned(r_rD_address)));
                     if r_branch_op = "0000" and s_status_alu.zero = '1' then -- Branch if Equal
                        r_ProgramCounter  <= r_ProgramCounter + unsigned(s_output_alu);
                     elsif r_branch_op = "0101" and ((s_status_alu.negative = '0') or (s_status_alu.zero = '1')) then -- Branch if Greater or Equal
                        r_ProgramCounter  <= r_ProgramCounter + unsigned(s_output_alu);
                     elsif r_branch_op = "0100" and s_status_alu.negative = '0' then -- Branch if Greater Than
                        r_ProgramCounter  <= r_ProgramCounter + unsigned(s_output_alu);
                     elsif r_branch_op = "0011" and ((s_status_alu.negative = '1') or (s_status_alu.zero = '1')) then -- Branch if Less or Equal
                        r_ProgramCounter  <= r_ProgramCounter + unsigned(s_output_alu);
                     elsif r_branch_op = "0010" and s_status_alu.negative = '1' then -- Branch if Less Than
                        r_ProgramCounter  <= r_ProgramCounter + unsigned(s_output_alu);
                     elsif r_branch_op = "0001" and s_status_alu.zero = '0' then   -- Branch if Not Equal
                        r_ProgramCounter  <= r_ProgramCounter + unsigned(s_output_alu);
                     end if;
                  -- Return from subroutine
                  elsif r_return_from_subroutine = '1' then
                     r_ProgramCounter     <= unsigned(s_output_alu);
                  elsif r_is_branch_uncond = '1' then
                     if r_branch_op(3) = '0' then -- L bit
                        v_GeneralReg(to_integer(unsigned(r_rD_address))) <= v_GeneralReg(to_integer(unsigned(r_rD_address)));
                     end if;
                     if r_branch_op(2) = '1' then -- A bit
                        r_ProgramCounter  <= unsigned(s_output_alu);
                     else
                        r_ProgramCounter  <= r_ProgramCounter + unsigned(s_output_alu);
                     end if;                                   
                  -- Note : Load & Write this way only works for one cycle access memory
                  -- Load execution
                  elsif r_is_load_instruction = '1' then
                     v_GeneralReg(to_integer(unsigned(r_rD_address))) <= v_GeneralReg(to_integer(unsigned(r_rD_address)));
                     r_fsm_load  <= st_execute;
                     case r_fsm_load is
                        when st_set_address =>
                           r_addr_mem_in     <= unsigned(s_output_alu);
                           r_rd_en_mem_in    <= '1';
                           r_fsm_load        <= st_wait_data;
                        when st_wait_data =>
                           if data_mem_in_en_i = '1' then
                              r_fsm_seq         <= st_fetch;
                              if r_op_load_store = "00" then
                                 case r_addr_mem_in(1 downto 0) is
                                    when "00" =>
                                       v_GeneralReg(to_integer(unsigned(r_rD_address))) <= x"000000" & data_mem_in_i(7 downto 0);
                                    when "01" =>
                                       v_GeneralReg(to_integer(unsigned(r_rD_address))) <= x"000000" & data_mem_in_i(15 downto 8);
                                    when "10" =>
                                       v_GeneralReg(to_integer(unsigned(r_rD_address))) <= x"000000" & data_mem_in_i(23 downto 16);
                                    when others =>
                                       v_GeneralReg(to_integer(unsigned(r_rD_address))) <= x"000000" & data_mem_in_i(31 downto 24);
                                 end case;
                              elsif r_op_load_store = "01" then
                                 case r_addr_mem_in(1 downto 0) is
                                    when "00" =>
                                       v_GeneralReg(to_integer(unsigned(r_rD_address))) <= x"0000" & data_mem_in_i(15 downto 0);
                                    when "10" =>
                                       v_GeneralReg(to_integer(unsigned(r_rD_address))) <= x"0000" & data_mem_in_i(31 downto 16);
                                    when others =>
                                       assert false report "Address non-aligned on 16b access" severity error;
                                 end case;
                              elsif r_op_load_store = "10" then
                                 if r_addr_mem_in(1 downto 0) = "00" then
                                    v_GeneralReg(to_integer(unsigned(r_rD_address)))(31 downto  0) <= data_mem_in_i(31 downto 0);
                                 else
                                    assert false report "Address non-aligned on 32b access" severity error;
                                 if r_load_store_exclusive = '1' then
                                    r_MSR(MSR_C) <= '0';
                                 end if;
                              end if;
                           end if;
                        when others =>
                           r_fsm_load  <= st_set_address;                              
                     end case;
                  -- Store execution
                  elsif r_is_store_instruction = '1' then
                     v_GeneralReg(to_integer(unsigned(r_rD_address))) <= v_GeneralReg(to_integer(unsigned(r_rD_address)));
                     r_addr_mem_out    <= s_output_alu;
                     if r_op_load_store = "00" then
                        r_wr_en_mem_out(to_integer(unsigned(s_output_alu(1 downto 0)))) <= '1'; -- others are at '0'
                        case s_output_alu(1 downto 0) is
                           when "00"   =>
                              r_data_mem_out(7  downto  0)  <= v_GeneralReg(to_integer(unsigned(r_rD_address)))(7  downto  0);
                           when "01"   => 
                              r_data_mem_out(15 downto  8)  <= v_GeneralReg(to_integer(unsigned(r_rD_address)))(7  downto  0);
                           when "10"   => 
                              r_data_mem_out(23 downto 16)  <= v_GeneralReg(to_integer(unsigned(r_rD_address)))(7  downto  0);
                           when others =>
                              r_data_mem_out(31 downto 24)  <= v_GeneralReg(to_integer(unsigned(r_rD_address)))(7  downto  0);
                        end case;
                     elsif r_op_load_store = "01" then
                        if s_output_alu(1) = '0' then
                           r_wr_en_mem_out   <= "0011";
                        else
                           r_wr_en_mem_out   <= "1100";
                        end if;
                        
                        case s_output_alu(1 downto 0) is
                           when "00"   =>
                              r_data_mem_out(15 downto  0)  <= v_GeneralReg(to_integer(unsigned(r_rD_address)))(15 downto  0);
                           when others =>
                              r_data_mem_out(31 downto 16)  <= v_GeneralReg(to_integer(unsigned(r_rD_address)))(15 downto  0);
                        end case;                     
                     
                     elsif r_op_load_store = "10" then
                        r_wr_en_mem_out   <= "1111";
                        r_data_mem_out    <= v_GeneralReg(to_integer(unsigned(r_rD_address)));
                        if r_load_store_exclusive = '1' then
                           r_MSR(MSR_C) <= '0';
                        end if;                        
                        
                     end if;
                  end if;
               
            end case;   
         end if;
      end if;
   end process;
   
   -- Type B instruction, ALU input B depends of the precedent instruction
   s_input_alu_B <= r_input_alu_B when r_last_op_was_imm = '0' else (r_input_alu_B(15 downto 0) & r_imm);
   
   -- Carry input of the ALU depends of the bit C of the MSR
   s_carry_alu_in <= r_MSR(MSR_C);
   
   -- ALU
   i_ALU : ALU
   generic map(
      DATA_WIDTH  => D_WIDTH
   )
   port map(
      param_i     => r_param_alu,
      carry_i     => s_carry_alu_in,
      operandA_i  => r_input_alu_A,
      operandB_i  => s_input_alu_B,
      operandD_o  => s_output_alu,
      status_o    => s_status_alu
   );
   
   -- ===============================   
   -- Mapping output
   -- ===============================
   
   addr_mem_in_o     <= std_logic_vector(r_addr_mem_in);
   rd_en_mem_in_o    <= r_rd_en_mem_in;
   addr_mem_out_o    <= r_addr_mem_out;
   data_mem_out_o    <= r_data_mem_out;
   wr_en_mem_out_o   <= r_wr_en_mem_out;

end rtl;
      
      
      
