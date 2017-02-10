-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : tb_sequencer
--   Date             : 08/05/2016
--
--   Description      : Test bench for Sequencer unit
--
--   --------------------------------------------------------------------------------
--   Modifications
--   --------------------------------------------------------------------------------
--   Date             : Ver. : Author           : Modification comments
--   --------------------------------------------------------------------------------
--                    :      :                  :
--   08/05/2016      : 1.0  : B.Lemoine        : First draft
--                    :      :                  :
-- **********************************************************************************
--   MIT License
--   
--   Copyright (c) 2016, Benjamin Lemoine
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
use work.tb_sequencer_pkg.all;

library vunit_lib;
context vunit_lib.vunit_context;

entity tb_sequencer is  
   generic (
      runner_cfg  : string
   );
end entity;


architecture rtl of tb_sequencer is

-- Components declaration
Component sequencer is
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
      wr_en_mem_out_o   : out std_logic_vector(3 downto 0)
   );
end Component;



-- Signals declaration
signal data_o           : std_logic_vector(D_WIDTH-1 downto 0);
signal data_i           : std_logic_vector(D_WIDTH-1 downto 0);
signal s_data_i         : std_logic_vector(D_WIDTH-1 downto 0);
signal addr_in          : std_logic_vector(31 downto 0);
signal addr_out         : std_logic_vector(31 downto 0);
signal addr             : std_logic_vector(SIZE_MEM-1 downto 0);  
signal s_addr           : std_logic_vector(SIZE_MEM-1 downto 0);  
signal wr_en            : std_logic_vector(NB_COL-1 downto 0);
signal s_wr_en          : std_logic_vector(NB_COL-1 downto 0);

signal clk              : std_logic := '0';
signal reset_n          : std_logic := '0';

signal r_init_done      : std_logic := '0';
signal r_cnt            : unsigned(31 downto 0) := (others => '0');
signal addr_init        : std_logic_vector(31 downto 0);
signal data_init        : std_logic_vector(31 downto 0);
signal wr_en_init        : std_logic_vector(3 downto 0);

signal data_o_en : std_logic;
signal r_data_o_en : std_logic;
signal rd_en : std_logic;

signal RAM : ram_type := (others => (others => '0'));

constant c_size_init    : integer  := SIZE;

   type fsm_load_app is (st_wait_top, st_load_app);
   signal r_fsm_load_app : fsm_load_app := st_wait_top;
   signal r_prog_end : std_logic := '0';
   signal r_prog_to_run : ram_type;
   signal r_prog_start : std_logic := '0';

begin

   -- Clock generation (125 MHz)
   clk   <= not clk after C_PERIOD/2; 

   i_sequencer : sequencer 
      generic map (
         D_WIDTH           => 32
      )
      port map(
         -- Clock and reset
         clk               => clk,
         reset_n           => reset_n,
         -- Interface memory in
         data_mem_in_i     => data_o,
         data_mem_in_en_i  => r_data_o_en,
         addr_mem_in_o     => addr_in,
         rd_en_mem_in_o    => rd_en,
         -- Interface memory out
         addr_mem_out_o    => addr_out,
         data_mem_out_o    => s_data_i,
         wr_en_mem_out_o   => s_wr_en
      );
      
   s_addr <= addr_in(SIZE_MEM+1 downto 2) when s_wr_en = x"0" else addr_out(SIZE_MEM+1 downto 2);
   
   process (clk)
   begin
      if rising_edge(clk) then
         if (wr_en = c_zero(NB_COL-1 downto 0)) then
            data_o <= RAM(to_integer(unsigned(addr)));
         end if;
         for i in 0 to NB_COL-1 loop
            if wr_en(i) = '1' then
               RAM(to_integer(unsigned(addr)))(COL_WIDTH*(i+1)-1 downto i*COL_WIDTH)  <= data_i(COL_WIDTH*(i+1)-1 downto i*COL_WIDTH);
            end if;
         end loop;
      end if;
   end process; 
   
   -- Init memory
   process(clk)
   begin
      if rising_edge(clk) then
         r_data_o_en <= rd_en;
      
         case r_fsm_load_app is
            when st_wait_top =>
               r_init_done <= '1';
               if r_prog_start = '1' then
                  r_fsm_load_app <= st_load_app;
                  r_init_done    <= '0';
                  r_prog_end     <= '0';
               end if;
            when st_load_app =>
               if r_cnt < c_size_init then
                  r_cnt       <= r_cnt + 1;
                  addr_init   <= std_logic_vector(r_cnt);
                  data_init   <= r_prog_to_run(to_integer(r_cnt));
                  wr_en_init  <= (others => '1');
               else
                  r_init_done    <= '1';
                  r_prog_end     <= '1';
                  wr_en_init     <= (others => '0');
                  r_fsm_load_app <= st_wait_top;
               end if;
            when others =>
               r_fsm_load_app <= st_wait_top;
         end case;
      end if;
   end process;
   
   addr        <= addr_init(SIZE_MEM-1 downto 0) when r_init_done = '0' else s_addr;
   data_i      <= data_init when r_init_done = '0' else s_data_i;
   wr_en       <= wr_en_init when r_init_done = '0' else s_wr_en;
   
   main : process
      variable filter : log_filter_t;
      variable nb_loop : integer := 0;
      variable v_numero_test : integer;
   begin
      checker_init(  display_format => verbose,
                     file_name      => join(output_path(runner_cfg), "error.cvs"),
                     file_format    => verbose_csv);
      logger_init(   display_format => verbose,
                     file_name      => join(output_path(runner_cfg), "log.csv"),
                     file_format    => verbose_csv);
      stop_level((debug,verbose), display_handler, filter);
      test_runner_setup(runner,runner_cfg);
      enable_pass_msg;
      enable_pass_msg(file_handler);
      enable_pass_msg(display_handler);
      while test_suite loop
         reset_checker_stat;
         reset_n <= '0';
         wait for 10*C_PERIOD;
         if run("test_add") then
            v_numero_test := 0;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));
         elsif run("test_rsub") then
            v_numero_test := 1;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));         
         elsif run("test_addc") then
            v_numero_test := 2;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));        
         elsif run("test_rsubc") then
            v_numero_test := 3;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0))); 
         elsif run("test_addk") then
            v_numero_test := 4;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));             
         elsif run("test_cmp") then
            v_numero_test := 5;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(RAM(13)(31), c_test(v_numero_test).results(0)(31));
         elsif run("test_addi") then
            v_numero_test := 6;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));   
         elsif run("test_mul") then
            v_numero_test := 7;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));       
         elsif run("test_bsra") then
            v_numero_test := 8;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));      
         elsif run("test_bsll") then
            v_numero_test := 9;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));               
         elsif run("test_muli") then
            v_numero_test := 10;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0))); 
         elsif run("test_bsrli") then
            v_numero_test := 11;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));          
         elsif run("test_bsrai") then
            v_numero_test := 12;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));    
         elsif run("test_bslli") then
            v_numero_test := 13;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));    
         elsif run("test_or") then
            v_numero_test := 14;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0))); 
         elsif run("test_and") then
            v_numero_test := 15;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));             
         elsif run("test_xor") then
            v_numero_test := 16;         
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));  
         elsif run("test_andn") then
            v_numero_test := 17;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));                
         elsif run("test_sra") then
            v_numero_test := 18;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));        
         elsif run("test_src") then
            v_numero_test := 19;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1))); 
         elsif run("test_srl") then
            v_numero_test := 20;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));            
            check_equal(unsigned(RAM(11)), unsigned(c_test(v_numero_test).results(2)));   
         elsif run("test_sext8") then
            v_numero_test := 21;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1))); 
         elsif run("test_sext16") then
            v_numero_test := 22;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));     
         elsif run("test_br") then
            v_numero_test := 23;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));              
         elsif run("test_brd") then
            v_numero_test := 24;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));          
         elsif run("test_brld") then
            v_numero_test := 25;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));          
         elsif run("test_bra") then
            v_numero_test := 26;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));             
         elsif run("test_brad") then
            v_numero_test := 27;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1))); 
         elsif run("test_brald") then
            v_numero_test := 28;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));   
         elsif run("test_beq") then
            v_numero_test := 29;
            -- Load test program
            r_prog_to_run  <= c_test(v_numero_test).program;
            r_prog_start   <= '1';
            wait until rising_edge(clk);
            r_prog_start   <= '0';
            wait until r_prog_end = '1' and rising_edge(clk);
            reset_n        <= '1';
            wait for NB_WAIT_CLK*C_PERIOD;
            -- Check output data
            check_equal(unsigned(RAM(13)), unsigned(c_test(v_numero_test).results(0)));              
            check_equal(unsigned(RAM(12)), unsigned(c_test(v_numero_test).results(1)));            
         end if;
         nb_loop := nb_loop+1;
      end loop;
      test_runner_cleanup(runner);
   end process;
   

end rtl;
   
      
