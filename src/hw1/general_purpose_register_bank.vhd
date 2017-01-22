-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : general_purpose_register_bank
--   Date             : 07/07/2016
--
--   Description      : Bank of 32 general purpose registers
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

entity general_purpose_register_bank is
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
)end general_purpose_register_bank;

architecture rtl of general_purpose_register_bank is

-- Component declaration
-- -----------------------
component ram_single_port is
   generic (
      ADDR_WIDTH : integer := 15;
      DATA_WIDTH : integer := 32
   );
   port (
      clk  : in  std_logic;
      we   : in  std_logic;
      addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
      di   : in  std_logic_vector(NB_COL*COL_WIDTH-1 downto 0);
      do   : out std_logic_vector(NB_COL*COL_WIDTH-1 downto 0)
   );
end component;

-- Signal declaration
-- -----------------------
signal s_wr_en_filt     : std_logic := '0';

begin

   -- R0 : Always has a value of zero. Anything written to R0 is discarded
   s_wr_en_filt   <= '0' when addr_i = (others => '0') else wr_i;

   i_reg32 : ram_single_port 
   generic map(
      ADDR_WIDTH  => 4,
      DATA_WIDTH  => D_WIDTH,
   )
   port map(
      clk         => clk,
      we          => s_wr_en_filt,
      addr        => addr_i,
      di          => data_i,
      do          => data_o
   );

end rtl;


      