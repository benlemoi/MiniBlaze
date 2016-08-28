-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : UART
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

entity UART is
   generic (
      CLK_IN         : integer   := 10000000;
      BAUDRATE       : integer   := 115200;
      DATA_BITS      : integer   := 8;       
      STOP_BITS      : integer   := 1;       
      USE_PARITY     : integer   := 0;
      ODD_PARITY     : integer   := 0
   );
   port (
      clk            : in  std_logic;
      rst_n          : in  std_logic;
      -- User intf
      data_in        : in  std_logic_vector(DATA_BITS-1 downto 0);
      data_in_en     : in  std_logic;
      data_in_ack    : out std_logic;
      data_out       : out std_logic_vector(DATA_BITS-1 downto 0);
      data_out_en    : out std_logic;
      frame_error    : out std_logic;
      parity_error   : out std_logic;
      -- TX/RX
      RX             : in  std_logic;
      TX             : out std_logic
   );
   
end UART;

architecture rtl of UART is

function CALC_RATIO ( C_BAUDRATE : integer; C_CLKIN : integer) return integer is
   constant C_RATIO  : integer := C_CLKIN/C_BAUDRATE;
   constant C_REMAIN : integer := C_CLKIN rem C_BAUDRATE;   
begin
   if C_BAUDRATE/2 < C_REMAIN then
      return C_RATIO;
   else
      return C_RATIO + 1;
   end if;
end function CALC_RATIO;
   

constant c_nb_clk_per_bit        : integer := CALC_RATIO(BAUDRATE, CLK_IN);
constant c_nb_clk_per_bit_div2   : integer := c_nb_clk_per_bit/2;

-- RX
type fsm_rx is (st_wait_start_bit, st_get_data, st_get_stop_bit);
signal r_fsm_rx                  : fsm_rx                                  := st_wait_start_bit;
signal r_RX                      : std_logic                               := '0';
signal r2_RX                     : std_logic                               := '0';
signal r_data_rx                 : std_logic_vector(DATA_BITS-1 downto 0)  := (others => '0'); 
signal r_data_rx_en              : std_logic                               := '0';
signal r_cnt_bit_uart            : unsigned(31 downto 0)                   := (others => '0'); 
signal r_cnt_data                : unsigned(31 downto 0)                   := (others => '0'); 
signal r_cnt_stop                : unsigned(31 downto 0)                   := (others => '0'); 
signal r_frame_error             : std_logic                               := '0';

-- TX
type fsm_tx is (st_wait_data, st_send_start, st_send_data, st_send_stop, st_wait_1b_for_ack);
signal r_fsm_tx                  : fsm_tx                                  := st_wait_data;
signal r_cnt_bit_uart_tx         : unsigned(31 downto 0)                   := (others => '0');
signal r_cnt_data_tx             : unsigned(31 downto 0)                   := (others => '0');
signal r_cnt_stop_tx             : unsigned(31 downto 0)                   := (others => '0');
signal r_data_in                 : std_logic_vector(DATA_BITS-1 downto 0)  := (others => '0');
signal r_data_in_en              : std_logic                               := '0';
signal r_TX                      : std_logic                               := '1';
signal r_data_tx                 : std_logic_vector(DATA_BITS-1 downto 0)  := (others => '0');
signal r_tx_ack                  : std_logic                               := '0';

begin

   -- ---------------------------------------------
   -- RX side
   -- ---------------------------------------------
   
   p_pipe_in : process(clk)   -- Two pipes to avoid metastability
   begin
      if rising_edge(clk) then
         r_RX  <= RX;
         r2_RX <= r_RX;
      end if;
   end process;   
   
   p_RX : process(clk)
   begin
      if rising_edge(clk) then
         if rst_n = '0' then
            r_fsm_rx       <= st_wait_start_bit;
            r_data_rx_en   <= '0';
         else
            -- default values
            r_data_rx_en   <= '0';
            
            case r_fsm_rx is
               when st_wait_start_bit =>
                  if r2_RX = '0' then
                     r_cnt_bit_uart    <= r_cnt_bit_uart + 1;
                  else
                     r_cnt_bit_uart    <= (others => '0');
                  end if;
                  if r_cnt_bit_uart = c_nb_clk_per_bit_div2 - 1 then
                     r_fsm_rx       <= st_get_data;
                     r_cnt_bit_uart <= (others => '0');
                     r_cnt_data     <= (others => '0');
                     r_cnt_stop     <= (others => '0');
                     r_frame_error  <= '0';
                  end if;
                  
               when st_get_data => -- Sample at the center of each bit                  
                  if r_cnt_bit_uart = c_nb_clk_per_bit - 1 then
                     r_data_rx(to_integer(r_cnt_data))   <= r2_RX;
                     r_cnt_bit_uart                      <= (others => '0');
                     if r_cnt_data = DATA_BITS - 1 then
                        r_fsm_rx <= st_get_stop_bit;
                     else
                        r_cnt_data     <= r_cnt_data + 1;
                     end if;
                  else
                     r_cnt_bit_uart    <= r_cnt_bit_uart + 1;
                  end if;
                  
               when st_get_stop_bit =>
                  if r_cnt_bit_uart = c_nb_clk_per_bit - 1 then
                     if r2_RX = '0' then -- NOK
                        r_cnt_bit_uart <= (others => '0');
                        r_fsm_rx       <= st_wait_start_bit;
                        r_frame_error  <= '1';
                     else -- OK
                        if r_cnt_stop = STOP_BITS - 1 then
                           r_fsm_rx       <= st_wait_start_bit;
                           r_data_rx_en   <= '1';
                        else
                           r_cnt_stop     <= r_cnt_stop + 1;
                           r_cnt_bit_uart <= (others => '0');
                        end if;
                     end if;
                  else
                     r_cnt_bit_uart <= r_cnt_bit_uart + 1;
                  end if;
               
               when others =>
                  r_fsm_rx    <= st_wait_start_bit;
            
            end case;
         end if;
      end if;
   end process;
   
   data_out <= r_data_rx;
   data_out_en <= r_data_rx_en;
   
   -- ---------------------------------------------
   -- TX side
   -- ---------------------------------------------
   
   p_pipe_tx_in : process(clk)
   begin
      if rising_edge(clk) then
         r_data_in      <= data_in;
         r_data_in_en   <= data_in_en;
      end if;
   end process;

   p_TX : process(clk)
   begin
      if rising_edge(clk) then
         if rst_n = '0' then
            r_cnt_bit_uart_tx <= (others => '0');
            r_cnt_data_tx     <= (others => '0');
            r_cnt_stop_tx     <= (others => '0');
            r_fsm_tx          <= st_wait_data;
            r_TX              <= '1';
         else
            -- Default values
            r_tx_ack    <= '0';
         
            case r_fsm_tx is
               when st_wait_data =>
                  r_TX  <= '1';
                  if r_data_in_en = '1' then
                     r_data_tx         <= r_data_in;
                     r_fsm_tx          <= st_send_start;
                     r_cnt_bit_uart_tx <= (others => '0');
                     r_cnt_data_tx     <= (others => '0');
                     r_cnt_stop_tx     <= (others => '0');
                  end if;
               
               when st_send_start =>
                  r_TX  <= '0';
                  if r_cnt_bit_uart_tx = c_nb_clk_per_bit - 1 then
                     r_cnt_bit_uart_tx <= (others => '0');
                     r_fsm_tx          <= st_send_data;
                  else
                     r_cnt_bit_uart_tx <= r_cnt_bit_uart_tx + 1;
                  end if;
               
               when st_send_data =>
                  if r_cnt_data_tx  = DATA_BITS then
                     r_fsm_tx <= st_send_stop;
                     r_TX     <= '1';
                  else
                     if r_cnt_bit_uart_tx = c_nb_clk_per_bit - 1 then
                        r_cnt_data_tx        <= r_cnt_data_tx + 1;
                        r_cnt_bit_uart_tx    <= (others => '0');
                        r_data_tx            <= r_data_tx(0) & r_data_tx(DATA_BITS - 1 downto 1);
                     else
                        r_cnt_bit_uart_tx    <= r_cnt_bit_uart_tx + 1;
                        r_TX                 <= r_data_tx(0);
                     end if;
                  end if;
               
               when st_send_stop =>
                  r_TX        <= '1';  
                  if r_cnt_stop_tx = STOP_BITS -1 then
                     r_fsm_tx             <= st_wait_1b_for_ack;
                     r_cnt_bit_uart_tx    <= (others => '0');
                  else
                     if r_cnt_bit_uart_tx = c_nb_clk_per_bit - 1 then
                        r_cnt_bit_uart_tx <= (others => '0');
                        r_cnt_stop_tx     <= r_cnt_stop_tx + 1;
                     else
                        r_cnt_bit_uart_tx <= r_cnt_bit_uart_tx + 1;
                     end if;
                  end if;
               
               when st_wait_1b_for_ack =>
                  r_TX        <= '1';
                  if r_cnt_bit_uart_tx = c_nb_clk_per_bit - 1 then
                     r_fsm_tx <= st_wait_data;
                     r_tx_ack <= '1';
                  else  
                     r_cnt_bit_uart_tx <= r_cnt_bit_uart_tx + 1;
                  end if;

               when others =>
                  r_fsm_tx <= st_wait_data;
            
            end case;
         
         end if;
      end if;
   end process;
   
   TX             <= r_TX;
   data_in_ack    <= r_tx_ack;
                     

end;
      