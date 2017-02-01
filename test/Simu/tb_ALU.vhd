-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : tb_ALU
--   Date             : 07/07/2016
--
--   Description      : TestBench for Arithmetic Logic Unit
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
use work.data_pkg.all;

library vunit_lib;
context vunit_lib.vunit_context;

entity tb_ALU is
   generic (
      runner_cfg  : string
   );
end tb_ALU;

architecture simu of tb_ALU is

---------------------------
-- Component declaration

component ALU
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
end component;

---------------------------
-- Constant declaration
constant DWIDTH               : integer range 0 to 63 := 32;
constant PERIOD_CLK           : time := 10 ns;

constant input_file           : string := "test2.hex";

---------------------------
-- Signal declaration

signal clk              : std_logic                               := '0';
                              
signal s_param          : t_param_alu                             := c_param_alu_null;
signal s_carry          : std_logic                               := '0';
signal s_operandA       : std_logic_vector(DWIDTH-1 downto 0)     := (others => '0');
signal s_operandB       : std_logic_vector(DWIDTH-1 downto 0)     := (others => '0');
signal s_operandD       : std_logic_vector(DWIDTH-1 downto 0)     := (others => '0');
signal s_status         : t_status_alu_out                        := c_status_alu_out_null;

type fsm_test is (st_start, st_test_1, st_test_2, st_test_3, st_end);
signal r_fsm_test       : fsm_test                                := st_start;    
signal r_count          : unsigned(31 downto 0)                   := (others => '0');

signal r_set            : std_logic := '0';
signal r_error          : std_logic := '0';

signal r_ack_test       : std_logic := '0';
signal r_start_test     : std_logic := '0';
signal r_do_test        : fsm_test := st_start;

begin

   clk   <= not clk after PERIOD_CLK/2;

   i_dut : ALU
   generic map(
      DATA_WIDTH  => DWIDTH
   )
   port map(
      param_i     => s_param,
      carry_i     => s_carry,  
      operandA_i  => s_operandA,
      operandB_i  => s_operandB,
      operandD_o  => s_operandD,
      status_o    => s_status  
   );            
   
   main : process
   begin
      test_runner_setup(runner,runner_cfg);
      while test_suite loop
         if run("test_1") then
            wait until rising_edge(clk);
            r_start_test <= '1';
            r_do_test    <= st_test_1;
            wait until rising_edge(clk);
            r_start_test <= '0';
            wait until r_ack_test = '1' and rising_edge(clk);
         elsif run("test_2") then
            wait until rising_edge(clk);
            r_start_test <= '1';
            r_do_test    <= st_test_2;
            wait until rising_edge(clk);
            r_start_test <= '0';
            wait until r_ack_test = '1' and rising_edge(clk);
         elsif run("test_random") then
            wait until rising_edge(clk);
            r_start_test <= '1';
            r_do_test    <= st_test_3;
            wait until rising_edge(clk);
            r_start_test <= '0';
            wait until r_ack_test = '1' and rising_edge(clk);      
         end if;
      end loop;
      test_runner_cleanup(runner);
   end process;
            
      

    
   p_test : process(clk)
   begin
      if rising_edge(clk) then
         r_ack_test <= '0';
         case r_fsm_test is
            when st_start =>
               if r_start_test = '1' then
                  r_fsm_test <= r_do_test;
               end if;
               r_count     <= (others => '0');
               r_set       <= '1';       
               r_error     <= '0';                  
            when st_test_1 => -- PassThrough
            
               -- Generation control
               if r_count = 16 then
                  r_count     <= (others => '0');
                  r_fsm_test  <= r_fsm_test;
               else
                  r_count                          <= r_count + 1;
                  s_param.operation                <= OP_PTA;
                  s_param.ctrl_op.keepCarry        <= '0';
                  s_param.ctrl_op.negOperandA      <= '0';
                  s_param.ctrl_op.negOperandB      <= '0';
                  s_param.ctrl_op.whichCarry       <= CARRY_INPUT;
                  s_param.ctrl_op.ctrlShift        <= LEFT_SHIFT;
                  s_carry                          <= not s_carry;
                  s_operandA                       <= std_logic_vector(resize(r_count, DWIDTH));
                  s_operandB                       <= std_logic_vector(resize(r_count+4, DWIDTH));
               end if;
                  
               -- Check data
               if is_zero(s_operandA) = '0' then
                  if (unsigned(s_operandD) /= (unsigned(s_operandA))) or (s_status.carry /= '0') then
                     ASSERT false report "Test 1 failure" severity failure;
                  end if;
               end if;
               r_ack_test <= '1';
               
            when st_test_2 => -- Add
               s_param.operation                <= OP_ADD;
               s_param.ctrl_op.keepCarry        <= '0';
               s_param.ctrl_op.negOperandA      <= '0';
               s_param.ctrl_op.negOperandB      <= '0';
               s_param.ctrl_op.whichCarry       <= CARRY_INPUT;
               s_param.ctrl_op.ctrlShift        <= LEFT_SHIFT;
               s_carry                          <= '0';
               s_operandA                       <= x"7fffffff";
               s_operandB                       <= x"0000ffff";
               r_count                          <= r_count + 1;
               
               if r_count /= 0 then
                  r_fsm_test  <= r_fsm_test;
                  r_count     <= (others => '0');
                  if s_operandD /= x"8000FFFE" then
                     assert false report "Test 2 failure" severity failure;
                  end if;
               end if;
               r_ack_test <= '1';
               
            when st_test_3 => -- Random
               if r_count < data_in'length then
                  if r_set = '1' then
                     s_operandA                       <= data_in(to_integer(r_count)).operandA;
                     s_operandB                       <= data_in(to_integer(r_count)).operandB;
                     s_param.operation                <= data_in(to_integer(r_count)).operation; 
                     s_param.ctrl_op.keepCarry        <= data_in(to_integer(r_count)).keepCarry;
                     s_param.ctrl_op.negOperandA      <= data_in(to_integer(r_count)).negOpA;
                     s_param.ctrl_op.negOperandB      <= data_in(to_integer(r_count)).negOpB;
                     s_param.ctrl_op.whichCarry       <= data_in(to_integer(r_count)).carry_type;
                     s_param.ctrl_op.ctrlShift        <= data_in(to_integer(r_count)).shift_type;
                     s_carry                          <= data_in(to_integer(r_count)).carry_in;
                     r_set                            <= '0';
                     r_error                          <= '0';
                  else
                     if data_in(to_integer(r_count)).operandD /= s_operandD then
                        assert false report "Test 3 failure operandD : " & integer'image(to_integer(r_count)) severity failure;
                     end if;
                     if data_in(to_integer(r_count)).carry_out /= s_status.carry then
                        r_error <= '1';
                        assert false report "Test 3 failure Carry out: " & integer'image(to_integer(r_count)) severity failure;
                     end if;
                     if data_in(to_integer(r_count)).zero /= s_status.zero then
                        r_error <= '1';
                        assert false report "Test 3 failure Zero: " & integer'image(to_integer(r_count)) severity failure;
                     end if;
                     if data_in(to_integer(r_count)).negative /= s_status.negative then
                        r_error <= '1';
                        assert false report "Test 3 failure Negative: " & integer'image(to_integer(r_count)) severity failure;
                     end if;
                     if data_in(to_integer(r_count)).overflow /= s_status.overflow then
                        r_error <= '1';
                     end if;
                     
                     r_set                            <= '1';
                     r_count                          <= r_count + 1;                     
                  end if;
               else
                  r_ack_test <= '1';
                  r_fsm_test <= st_start;
               end if;       
               
            when st_end => null;
               
            when others => null;
                  
         end case;
      end if;
   end process;



end simu;
