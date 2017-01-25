library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library vunit_lib;
context vunit_lib.vunit_context;

entity tb_generic_hdl_fifo is
   generic (
      runner_cfg  : string
   );
end tb_generic_hdl_fifo;

architecture simu of tb_generic_hdl_fifo is

component generic_hdl_fifo is
   generic (
      G_DEPTH_LOG2   : integer := 4; -- Depth is equal to 2^(G_DEPTH_LOG2)
      G_WIDTH        : integer := 8
   );
   port (
      clk            : in  std_logic;
      rst_n          : in  std_logic;
      -- Data
      data_wr        : in  std_logic_vector(G_WIDTH-1 downto 0);
      wr_en          : in  std_logic;
      rd_en          : in  std_logic;
      data_rd        : out std_logic_vector(G_WIDTH-1 downto 0);
      rd_valid       : out std_logic;
      -- Status
      nb_data        : out std_logic_vector(G_DEPTH_LOG2 downto 0);
      empty          : out std_logic;
      full           : out std_logic
   );
end component;

constant C_DEPTH           : integer := 6;
constant C_WIDTH           : integer := 16;
constant C_PERIOD          : time := 8 ns;

signal s_zero              : std_logic_vector(C_WIDTH-1 downto 0)       := (others => '0');
signal s_one               : std_logic_vector(C_WIDTH-1 downto 0)       := (others => '1');
signal s_test              : std_logic_vector(C_WIDTH-1 downto 0);

signal clk                 : std_logic                                  := '0';   
signal rst_n               : std_logic                                  := '0';
signal s_data_wr           : std_logic_vector(C_WIDTH-1 downto 0)       := (others => '0');
signal s_wr_en             : std_logic                                  := '0';
signal s_rd_en             : std_logic                                  := '0';
signal s_data_rd           : std_logic_vector(C_WIDTH-1 downto 0)       := (others => '0');
signal s_rd_valid          : std_logic                                  := '0';
signal s_nb_data           : std_logic_vector(C_DEPTH downto 0)         := (others => '0');
signal s_empty             : std_logic                                  := '0';
signal s_full              : std_logic                                  := '0';

begin

   clk      <= not clk after C_PERIOD/2;
   s_test   <= x"CAFE";

   i_dut : generic_hdl_fifo
   generic map(
      G_DEPTH_LOG2      => C_DEPTH,
      G_WIDTH           => C_WIDTH
   )
   port map(
      clk               => clk,
      rst_n             => rst_n,
      --
      data_wr           => s_data_wr,
      wr_en             => s_wr_en,
      rd_en             => s_rd_en,
      data_rd           => s_data_rd,
      rd_valid          => s_rd_valid,
      -- 
      nb_data           => s_nb_data,
      empty             => s_empty,
      full              => s_full
   );
   
   main : process
      variable filter : log_filter_t;
   begin
      checker_init(  display_format => verbose,
                     file_name      => join(output_path(runner_cfg), "error.cvs"),
                     file_format    => verbose_csv);
      logger_init(   display_format => verbose,
                     file_name      => join(output_path(runner_cfg), "log.csv"),
                     file_format    => verbose_csv);
      stop_level((debug,verbose), display_handler, filter);
      test_runner_setup(runner,runner_cfg);
      while test_suite loop
         reset_checker_stat;
         wait until rising_edge(clk);
         rst_n <= '0';
         wait for 10*C_PERIOD;
         rst_n <= '1';
         wait until rising_edge(clk);
         if run("test_during_reset") then
            rst_n <= '0';
            wait until rising_edge(clk);
            check_equal(s_data_wr, s_zero(s_data_wr'left downto 0));
            check_equal(s_wr_en, '0');
            check_equal(s_rd_en, '0');
            check_equal(s_data_rd, s_zero(s_data_rd'left downto 0));
            check_equal(s_rd_valid, '0');
            check_equal(s_nb_data, s_zero(s_nb_data'left downto 0));
            check_equal(s_empty, '1');
            check_equal(s_full, '1');
         elsif run("test_write_one_word") then
            wait until rising_edge(clk);
            s_wr_en     <= '1';
            s_data_wr   <= s_test;
            check_equal(s_empty, '1');
            check_equal(s_nb_data, s_zero(s_nb_data'left downto 0));
            wait until rising_edge(clk);
            s_wr_en     <= '0';
            s_rd_en     <= '1';
            wait until rising_edge(clk);
            check_equal(s_nb_data, std_logic_vector(to_unsigned(1,C_DEPTH+1)));
            check_equal(s_empty, '0');            
            s_rd_en     <= '0';            
            wait until rising_edge(clk);
            check_equal(s_empty, '1');
            check_equal(s_nb_data, s_zero(s_nb_data'left downto 0));            
            check_equal(s_data_rd, s_test);
            check_equal(s_rd_valid, '1');
            wait until rising_edge(clk);
            check_equal(s_rd_valid, '0');
         elsif run("test_after_reset") then
            check_equal(s_data_wr, s_zero(s_data_wr'left downto 0));
            check_equal(s_wr_en, '0');
            check_equal(s_rd_en, '0');
            check_equal(s_data_rd, s_zero(s_data_rd'left downto 0));
            check_equal(s_rd_valid, '0');
            check_equal(s_nb_data, s_zero(s_nb_data'left downto 0));
            check_equal(s_empty, '1');
            check_equal(s_full, '0');            
         elsif run("test_fail") then
            assert false;
         end if;
      end loop;
      test_runner_cleanup(runner);
   end process;
   
end simu;
