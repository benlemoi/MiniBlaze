-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : generic_hdl_fifo.vhd
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

entity generic_hdl_fifo is
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
end generic_hdl_fifo;

architecture rtl of generic_hdl_fifo is

signal c_one_loop  : unsigned(G_DEPTH_LOG2 downto 0)   ;--:= (G_DEPTH_LOG2 => '1', others => '0');

type ram_type is array (2**G_DEPTH_LOG2-1 downto 0) of std_logic_vector (G_WIDTH-1 downto 0);
signal RAM     : ram_type := (others => (others => '0'));

signal cur_ptr_wr          : unsigned(G_DEPTH_LOG2 downto 0)         := (others => '0');
signal cur_ptr_wr_lsb      : unsigned(G_DEPTH_LOG2-1 downto 0)       := (others => '0');
signal s_full              : std_logic                               := '0';
      
signal cur_ptr_rd          : unsigned(G_DEPTH_LOG2 downto 0)         := (others => '0');
signal cur_ptr_rd_lsb      : unsigned(G_DEPTH_LOG2-1 downto 0)        := (others => '0');
signal s_empty             : std_logic                               := '0';
signal r_data_rd           : std_logic_vector(G_WIDTH-1 downto 0)    := (others => '0');
signal r_rd_valid          : std_logic                               := '0';

signal s_nb_data           : unsigned(G_DEPTH_LOG2 downto 0)         := (others => '0');

begin

   c_one_loop(G_DEPTH_LOG2)            <= '1';
   c_one_loop(G_DEPTH_LOG2-1 downto 0) <= (others => '0');

   cur_ptr_wr_lsb <= cur_ptr_wr(G_DEPTH_LOG2-1 downto 0);
   cur_ptr_rd_lsb <= cur_ptr_rd(G_DEPTH_LOG2-1 downto 0);

   -- Write process
   p_write : process(clk)
   begin
      if rising_edge(clk) then
         if rst_n = '0' then
            cur_ptr_wr  <= (others => '0');
         else
            if wr_en = '1' and s_full = '0' then
               cur_ptr_wr                       <= cur_ptr_wr + 1;
               RAM(to_integer(cur_ptr_wr_lsb))  <= data_wr;
            end if;
         end if;
      end if;
   end process;   
   
   -- Read process
   p_read : process(clk)
   begin
      if rising_edge(clk) then
         if rst_n = '0' then
            r_rd_valid  <= '0';
            cur_ptr_rd  <= (others => '0');
            r_data_rd   <= (others => '0');
         else
            r_rd_valid <= '0';
            if rd_en = '1' and s_empty = '0' then
               cur_ptr_rd        <= cur_ptr_rd + 1;
               r_data_rd         <= RAM(to_integer(cur_ptr_rd_lsb));
               r_rd_valid        <= '1';
            end if;
         end if;
      end if;
   end process;
  
   -- Status
   s_full      <= '1' when cur_ptr_wr = cur_ptr_rd + c_one_loop else 
                  '1' when rst_n = '0' else
                  '0';
   s_empty     <= '1' when cur_ptr_wr = cur_ptr_rd else '0';
   s_nb_data   <= resize(cur_ptr_wr,G_DEPTH_LOG2+1)-resize(cur_ptr_rd,G_DEPTH_LOG2+1);
   
   -- Mapping output
   full     <= s_full;
   empty    <= s_empty;
   nb_data  <= std_logic_vector(s_nb_data);
   data_rd  <= r_data_rd;
   rd_valid <= r_rd_valid;

end rtl;
