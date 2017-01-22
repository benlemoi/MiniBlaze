library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_generic_hdl_fifo is
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

constant size_data   : integer := 8;
constant log2_depth  : integer := 4;

signal clk           : std_logic := '0';
signal rst_n         : std_logic := '1';
signal data_wr       : std_logic_vector(size_data-1 downto 0);
signal data_rd       : std_logic_vector(size_data-1 downto 0);
signal wr_en         : std_logic := '0';
signal rd_en         : std_logic := '0';
signal nb_data       : std_logic_vector(log2_depth downto 0);
signal empty         : std_logic;
signal full          : std_logic;
signal rd_valid      : std_logic;
signal cpt           : unsigned(size_data-1 downto 0);

begin

   clk   <= not clk after 4 ns; -- 125 Mhz
   rst_n <= '0', '1' after 80 ns;
   wr_en <= '0', '1' after 88 ns, '1' after 216 ns;
   rd_en <= '0', '1' after 216 ns;

   i_dut : generic_hdl_fifo 
   generic map (
      G_DEPTH_LOG2      => log2_depth,
      G_WIDTH           => size_data
   )
   port map (
      clk      => clk,
      rst_n    => rst_n,
      data_wr  => data_wr,
      wr_en    => wr_en,
      rd_en    => rd_en,
      data_rd  => data_rd,
      rd_valid => rd_valid,
      nb_data  => nb_data,
      empty    => empty,
      full     => full
   );
   
   process(clk)
   begin
      if rising_edge(clk) then
         if rst_n = '0' then
            data_wr  <= (others => '0');
            cpt      <= (others => '0');
         else
            data_wr  <= std_logic_vector(cpt);
            cpt      <= cpt + 1;
         end if;
      end if;
   end process;

end simu;