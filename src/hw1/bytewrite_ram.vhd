-- ************************************************************************************************
--   Project          : MiniBlaze
--   Author           : B.Lemoine
--   Module           : bytewrite_ram.vhd
--   Date             : 07/07/2016
--
--   Description      : Single-Port BRAM
--                      No-change mode : Data output does not change while new contents are loaded into RAM
--                      Byte-wide Write Enable
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
use ieee.std_logic_unsigned.all;

entity bytewrite_ram is
   generic (
      ADDR_WIDTH : integer := 15;
      COL_WIDTH  : integer := 16;
      NB_COL     : integer := 4
   );
   port (
      clk  : in  std_logic;
      we   : in  std_logic_vector(NB_COL-1 downto 0);
      addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
      di   : in  std_logic_vector(NB_COL*COL_WIDTH-1 downto 0);
      do   : out std_logic_vector(NB_COL*COL_WIDTH-1 downto 0)
   );
end bytewrite_ram;

architecture behavioral of bytewrite_ram is

constant SIZE     : natural := 2**ADDR_WIDTH;
constant c_zero   : std_logic_vector(NB_COL*COL_WIDTH-1 downto 0) := (others => '0');

type ram_type is array (SIZE-1 downto 0) of std_logic_vector (NB_COL*COL_WIDTH-1 downto 0);
signal RAM : ram_type := (others => (others => '0'));
   
begin

   process (clk)
   begin
      if rising_edge(clk) then
         if (we = c_zero(NB_COL-1 downto 0)) then
            do <= RAM(conv_integer(addr));
         end if;
         for i in 0 to NB_COL-1 loop
            if we(i) = '1' then
               RAM(conv_integer(addr))(COL_WIDTH*(i+1)-1 downto i*COL_WIDTH)  <= di(COL_WIDTH*(i+1)-1 downto i*COL_WIDTH);
            end if;
         end loop;
      end if;
   end process;
   
end behavioral;
