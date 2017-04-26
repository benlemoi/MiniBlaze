-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : peripheral_uart
--   Date             : 07/25/2016
--
--   Description      : 
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

entity peripheral_uart is
   generic (
      CLK_IN         : integer                  := 10000000;
      BAUDRATE       : integer                  := 115200;
      DATA_BITS      : integer range 7 to 8     := 8;       
      STOP_BITS      : integer range 1 to 2     := 1;       
      USE_PARITY     : integer range 0 to 1     := 0;
      ODD_PARITY     : integer range 0 to 1     := 0;
      USE_FIFO_TX    : integer range 0 to 1     := 1;
      USE_FIFO_RX    : integer range 0 to 1     := 1;
      SIZE_FIFO_TX   : integer range 0 to 10    := 4; -- Log2 of fifo size
      SIZE_FIFO_RX   : integer range 0 to 10    := 4  -- Log2 of fifo size
   );
   port (
      clk            : in  std_logic;
      rst_n          : in  std_logic;
      --
      addr           : in  std_logic_vector(31 downto 0);
      data_wr        : in  std_logic_vector(31 downto 0);
      wr_en          : in  std_logic;
      data_rd        : out std_logic_vector(31 downto 0);
      -- 
      RX             : in  std_logic;
      TX             : out std_logic
   );
end peripheral_uart;

architecture rtl of peripheral_uart is

   -- Component declaration
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
   
   
   -- Signal declaration
   signal r_read_rx_data            : std_logic                               := '0';
   signal r_clear_received_bit      : std_logic                               := '0';
   signal r_clear_rx_sts            : std_logic                               := '0';
   signal r_clear_tx_sts            : std_logic                               := '0';
   signal r_input_data              : std_logic_vector(7 downto 0)            := (others => '0');
   signal r_input_data_en           : std_logic                               := '0';
   signal r_reset_tx_fifo           : std_logic                               := '0';
   signal r_reset_rx_fifo           : std_logic                               := '0';
   signal s_clk_in                  : std_logic_vector(31 downto 0)           := (others => '0');
   signal s_baudrate                : std_logic_vector(31 downto 0)           := (others => '0');
   signal s_data_bits               : std_logic_vector(3 downto 0)            := (others => '0');
   signal s_stop_bits               : std_logic_vector(3 downto 0)            := (others => '0');
   signal s_use_parity              : std_logic                               := '0';                            
   signal s_odd_parity              : std_logic                               := '0';
   signal s_user_fifo_tx            : std_logic                               := '0';
   signal s_user_fifo_rx            : std_logic                               := '0';
   signal s_size_fifo_tx            : std_logic_vector(15 downto 0)           := (others => '0');
   signal s_size_fifo_rx            : std_logic_vector(15 downto 0)           := (others => '0');
   signal s2_nb_data_tx             : std_logic_vector(15 downto 0)           := (others => '0');
   signal s2_nb_data_rx             : std_logic_vector(15 downto 0)           := (others => '0');
   signal r_data_rd                 : std_logic_vector(7 downto 0)            := (others => '0');
   signal r_rd_fifo_tx              : std_logic                               := '0';
   signal s_data_rd_tx              : std_logic_vector(7 downto 0)            := (others => '0');
   signal s_rd_valid_tx             : std_logic                               := '0';
   signal s_nb_data_tx              : std_logic_vector(SIZE_FIFO_TX downto 0) := (others => '0');
   signal s_empty_tx                : std_logic                               := '0';
   signal s_full_tx                 : std_logic                               := '0';
   signal r_last_word_sent          : std_logic                               := '1';
   signal s_data_tx                 : std_logic_vector(7 downto 0)            := (others => '0');
   signal s_data_tx_en              : std_logic                               := '0';
   signal s_ack_tx                  : std_logic                               := '0';
   signal s_data_rx                 : std_logic_vector(7 downto 0)            := (others => '0');
   signal s_data_rx_en              : std_logic                               := '0';
   signal s_data_rd_rx              : std_logic_vector(7 downto 0)            := (others => '0');
   signal s_rd_valid_rx             : std_logic                               := '0';
   signal s_nb_data_rx              : std_logic_vector(SIZE_FIFO_RX downto 0) := (others => '0');
   signal s_empty_rx                : std_logic                               := '0';
   signal s_full_rx                 : std_logic                               := '0';
   signal r_data_received           : std_logic_vector(7 downto 0)            := (others => '0');
   signal r_data_received_bit       : std_logic                               := '0';
   signal r_rx_underflow            : std_logic                               := '0';
   signal r_rx_overflow             : std_logic                               := '0';
   signal r_tx_overflow             : std_logic                               := '0';
   signal r_tx_underflow            : std_logic                               := '0';
   

begin

   -- ====================================
   -- Configuration registry
   -- ====================================   
   process(clk)
   begin
      if rising_edge(clk) then
         -- Default values
         r_read_rx_data          <= '0';
         r_clear_received_bit    <= '0';
         r_clear_rx_sts          <= '0';
         r_clear_tx_sts          <= '0';
         r_input_data_en         <= '0';
         
         if wr_en = '1' then
            case addr(7 downto 0) is
               when x"00" =>  
                  r_input_data         <= data_wr(DATA_BITS-1 downto 0);
               when x"01" =>  
                  r_input_data_en      <= data_wr(0);
               when x"02" =>  
                  r_reset_tx_fifo      <= not data_wr(0);
                  r_reset_rx_fifo      <= not data_wr(1);
               when x"03" =>  
                  r_read_rx_data       <= data_wr(0);
                  r_clear_received_bit <= data_wr(1);
                  r_clear_rx_sts       <= data_wr(2);
                  r_clear_tx_sts       <= data_wr(3);
               when x"11" =>
                  
               when others =>
                  null;
            end case;
         end if;
      end if;
   end process;
   
   -- ====================================
   -- Status registry
   -- ==================================== 
   
   s_clk_in       <= std_logic_vector(to_unsigned(CLK_IN,32));
   s_baudrate     <= std_logic_vector(to_unsigned(BAUDRATE,32));
   s_data_bits    <= std_logic_vector(to_unsigned(DATA_BITS,4));
   s_stop_bits    <= std_logic_vector(to_unsigned(STOP_BITS,4));
   s_use_parity   <= '1' when USE_PARITY = 1 else '0';
   s_odd_parity   <= '1' when ODD_PARITY = 1 else '0';
   s_user_fifo_tx <= '1' when USE_FIFO_TX = 1 else '0';
   s_user_fifo_rx <= '1' when USE_FIFO_RX = 1 else '0';
   s_size_fifo_tx <= std_logic_vector(to_unsigned(2**SIZE_FIFO_TX,16));
   s_size_fifo_rx <= std_logic_vector(to_unsigned(2**SIZE_FIFO_RX,16));
   s2_nb_data_tx  <= std_logic_vector(resize(unsigned(s_nb_data_tx),16));
   s2_nb_data_rx  <= std_logic_vector(resize(unsigned(s_nb_data_rx),16));
   
   process(clk)
   begin
      if rising_edge(clk) then
         case addr(7 downto 0) is
            when x"80" =>
               r_data_rd   <= "000000" & s_empty_rx & s_full_rx;
            when x"81" =>
               r_data_rd   <= s2_nb_data_rx(7 downto 0);
            when x"82" =>
               r_data_rd   <= s2_nb_data_rx(15 downto 8);
            when x"83" =>
               r_data_rd   <= "00000" & r_rx_underflow & r_rx_overflow & r_data_received_bit;
            when x"84" =>
               r_data_rd   <= r_data_received;
            when x"90" =>
               r_data_rd   <= "000000" & s_empty_tx & s_full_tx;
            when x"91" =>
               r_data_rd   <= s2_nb_data_tx(7 downto 0);
            when x"92" =>
               r_data_rd   <= s2_nb_data_tx(15 downto 8);
            when x"B0" =>
               r_data_rd   <= s_clk_in(7 downto 0);
            when x"B1" =>
               r_data_rd   <= s_clk_in(15 downto 8);
            when x"B2" =>
               r_data_rd   <= s_clk_in(23 downto 16);
            when x"B3" =>
               r_data_rd   <= s_clk_in(31 downto 24);
            when x"B4" =>
               r_data_rd   <= s_baudrate(7 downto 0);
            when x"B5" =>
               r_data_rd   <= s_baudrate(15 downto 8);
            when x"B6" => -- Reserved for baudrate if 16b not enough
               r_data_rd   <= (others => '0');
            when x"B7" => -- Reserved for baudrate if 16b not enough
               r_data_rd   <= (others => '0');
            when x"B8" =>
               r_data_rd   <= s_stop_bits & s_data_bits;
            when x"B9" =>
               r_data_rd   <= "0000" & s_use_parity & s_odd_parity & s_user_fifo_tx & s_user_fifo_rx;
            when x"BA" =>
               r_data_rd   <= s_size_fifo_tx(7 downto 0);
            when x"BB" =>
               r_data_rd   <= s_size_fifo_tx(15 downto 8);
            when x"BC" =>
               r_data_rd   <= s_size_fifo_rx(7 downto 0);
            when x"BD" =>
               r_data_rd   <= s_size_fifo_rx(15 downto 8);     
            
            when others =>
               r_data_rd   <= (others => '0');
         end case;
      end if;
   end process;
   
   data_rd(7 downto 0)  <= r_data_rd;
   data_rd(31 downto 8) <= (others => '0');
   
   -- ====================================
   -- TX fifo
   -- ====================================
   g_fifo_tx : if USE_FIFO_TX = 1 generate
   
      i_fifo_tx : generic_hdl_fifo 
      generic map (
         G_DEPTH_LOG2      => SIZE_FIFO_TX,
         G_WIDTH           => DATA_BITS
      )
      port map (
         clk      => clk,
         rst_n    => rst_n,
         data_wr  => r_input_data,
         wr_en    => r_input_data_en,
         rd_en    => r_rd_fifo_tx,
         data_rd  => s_data_rd_tx,
         rd_valid => s_rd_valid_tx,
         nb_data  => s_nb_data_tx,
         empty    => s_empty_tx,
         full     => s_full_tx
      );

      process(clk)
      begin
         if rising_edge(clk) then
            -- Consume words if fifo is not empty
            if rst_n = '0' then
               r_last_word_sent  <= '1';
               r_rd_fifo_tx      <= '0';
            else
               if s_empty_tx = '0' and r_last_word_sent = '1' then
                  r_rd_fifo_tx      <= '1';
                  r_last_word_sent  <= '0';
               elsif s_ack_tx = '1' then
                  r_last_word_sent  <= '1';
                  r_rd_fifo_tx      <= '0';
               else
                  r_rd_fifo_tx      <= '0';
               end if;
            end if;
         end if;
      end process;
      
      s_data_tx      <= s_data_rd_tx;
      s_data_tx_en   <= s_rd_valid_tx;
      
   end generate;
   
   g_no_fifo_tx : if USE_FIFO_TX = 0 generate
      process(clk)
      begin
         if rising_edge(clk) then
            if rst_n = '0' then
               s_empty_tx  <= '1';
               s_full_tx   <= '0';
            else
               if r_input_data_en = '1' then
                  s_empty_tx  <= '0';
                  s_full_tx   <= '1';
               elsif s_ack_tx = '1' then
                  s_empty_tx  <= '1';
                  s_full_tx   <= '0';
               end if;
            end if;
         end if;
      end process;
   
      s_data_tx      <= r_input_data;
      s_data_tx_en   <= r_input_data_en;
   end generate;
   
   
   process(clk)
   begin
      if rising_edge(clk) then
         if r_clear_tx_sts = '1' then
            r_tx_overflow  <= '0';
            r_tx_underflow <= '0';
         end if;
         
         if r_input_data_en = '1' and s_full_tx = '1' then
            r_tx_overflow  <= '1';
         end if;
         if r_rd_fifo_tx = '1' and s_empty_tx = '1' then
            r_tx_underflow <= '1';
         end if;
      end if;
   end process;

   -- ====================================
   -- UART module
   -- ====================================   
   i_UART : entity work.UART
   generic map(
      CLK_IN      => CLK_IN,
      BAUDRATE    => BAUDRATE,
      DATA_BITS   => DATA_BITS,
      STOP_BITS   => STOP_BITS,
      USE_PARITY  => USE_PARITY,
      ODD_PARITY  => ODD_PARITY
   )
   port map(
      clk            => clk,
      rst_n          => rst_n,
      -- User intf
      data_in        => s_data_tx,
      data_in_en     => s_data_tx_en,
      data_in_ack    => s_ack_tx,
      data_out       => s_data_rx, 
      data_out_en    => s_data_rx_en,
      -- TX/RX
      RX             => RX,
      TX             => TX
   );   
   
   -- ====================================
   -- RX fifo
   -- ====================================

   g_fifo_rx : if USE_FIFO_RX = 1 generate
   
      i_fifo_tx : generic_hdl_fifo 
      generic map (
         G_DEPTH_LOG2      => SIZE_FIFO_RX,
         G_WIDTH           => DATA_BITS
      )
      port map (
         clk      => clk,
         rst_n    => rst_n,
         data_wr  => s_data_rx,
         wr_en    => s_data_rx_en,
         rd_en    => r_read_rx_data,
         data_rd  => s_data_rd_rx,
         rd_valid => s_rd_valid_rx,
         nb_data  => s_nb_data_rx,
         empty    => s_empty_rx,
         full     => s_full_rx
      );
   
   end generate;
   
   g_no_fifo_rx : if USE_FIFO_RX = 0 generate
      
      s_data_rd_rx   <= s_data_rx;
      s_rd_valid_rx  <= s_data_rx_en;
      
      process(clk)
      begin
         if rising_edge(clk) then
            if s_data_rx_en = '1' then
               s_full_rx            <= '1';
               s_empty_rx           <= '0';
            elsif r_read_rx_data = '1' then
               s_full_rx            <= '0';
               s_empty_rx           <= '1';
            end if;
         end if;
      end process;
   
   end generate;

   process(clk)
   begin
      if rising_edge(clk) then
         if r_clear_received_bit = '1' then
            r_data_received_bit  <= '0';
         end if;
         
         if r_clear_rx_sts = '1' then
            r_data_received_bit  <= '0';
            r_rx_underflow       <= '0';
            r_rx_overflow        <= '0';
         end if;
      
         if s_rd_valid_rx = '1' then
            r_data_received_bit  <= '1';
            r_data_received      <= s_data_rd_rx;
         end if;
         
         if r_read_rx_data = '1' and s_empty_rx = '1' then
            r_rx_underflow <= '1';
         end if;
         
         if s_data_rx_en = '1' and s_full_rx = '1' then
            r_rx_overflow  <= '1';
         end if;
      end if;
   end process;
           
      
   
end rtl;
      
