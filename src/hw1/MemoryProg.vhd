-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : MemoryProg
--   Date             : 07/25/2016
--
--   Description      : Memory block for the Miniblaze on Spartan3. Use of RAMB16_S4.
--                      Use of 4 block RAM configured in 2Kx9 for a total size of
--                      64Kbits. (Parity Byte not used)
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

entity MemoryProg is
   port (
      clk         : in  std_logic;
      addr_i      : in  std_logic_vector(10 downto 0);
      data_in_i   : in  std_logic_vector(31 downto 0);
      wr_en_i     : in  std_logic_vector(3  downto 0);
      data_out_o  : out std_logic_vector(31 downto 0)
   );
end MemoryProg;

architecture rtl of MemoryProg is

component RAMB16_S9
   generic (
      WRITE_MODE     : string := "WRITE_FIRST";
      INIT           : bit_vector(8 downto 0) := "000000000"
   );
   port (
      CLK            : in  std_logic;   
      DI             : in  std_logic_vector(7  downto 0);
      DIP            : in  std_logic_vector(0  downto 0);
      ADDR           : in  std_logic_vector(10 downto 0);
      EN             : in  std_logic;
      WE             : in  std_logic;
      SSR            : in  std_logic;
      DO             : out std_logic_vector(8  downto 0);
      DOP            : out std_logic_vector(0  downto 0)
   );
   
   
begin

   g_RAMB16 : for i in 0 to 3 generate
      i_RAMB16_S9 : RAMB6_S9
         port map(
            CLK         => clk,
            DI          => data_in_i((i+1)*8-1 downto i),
            DIP         => (others => '0'),
            ADDR        => addr_i,
            EN          => '1',
            WE          => wr_en_i(i),
            SSR         => '0', -- reset active high
            DO          => data_out_o,
            DOP         => open
         );
      );
   end generate;

end rtl;
            
            
      
   
