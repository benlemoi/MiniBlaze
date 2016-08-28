-- **********************************************************************************
--   Project          : MiniBlaze
--   Author           : Benjamin Lemoine
--   Module           : top_test_uart
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

library unisim;
use unisim.vcomponents.all;

library work;
use work.pkg_utils.all;

entity top_test_uart is
   port (
      -- CLK & RST
      clk            : in  std_logic; -- 50 MHz
      rst            : in  std_logic; -- Button 0
      -- Liaison serie
      RsTx           : out std_logic;
      RsRx           : in  std_logic;
      -- 7 segment display
      seg            : out std_logic_vector(6 downto 0);
      dp             : out std_logic;
      an             : out std_logic_vector(3 downto 0);
      -- Switch
      sw             : in  std_logic_vector(7 downto 0);
      -- Leds
      Led            : out std_logic_vector(7 downto 0)      
   );

end top_test_uart;

architecture rtl of top_test_uart is

-- Reset
signal r_rsl_reset                  : std_logic_vector(4 downto 0)         := (others => '0');
signal reset_global                 : std_logic                            := '0';
signal reset_global_n               : std_logic                            := '1';

-- Clocks
signal clk_50M                    : std_logic                            := '0';
signal clk_10M_dcm                  : std_logic                            := '0';
signal dcm_locked                   : std_logic                            := '0';

-- Main FSM
type main_fsm is (RX_STATE, TX_STATE, LOOPBACK_STATE);
signal r_state                      : main_fsm                             := RX_STATE;
signal r_last_state                 : main_fsm                             := RX_STATE;

-- Send FSM
type fsm_tx is (st_wait_start, st_send_byte, st_wait_ack);
signal r_fsm_tx                     : fsm_tx                               := st_wait_start;
signal r_start_sending              : std_logic                            := '0';
signal r_last_sent_done             : std_logic                            := '0';
signal r_cnt_sent                   : unsigned(3 downto 0)                 := (others => '0');
signal r_data_to_send               : std_logic_vector(15 downto 0)        := (others => '0');

-- Enable
constant c_1second_50M              : integer                              := 50000000;
signal r_cnt_1s                     : unsigned(31 downto 0)                := (others => '0');
signal r_cnt_10s                    : unsigned(31 downto 0)                := (others => '0');
signal r_cnt_1min                   : unsigned(31 downto 0)                := (others => '0');
signal r_cnt_10min                  : unsigned(31 downto 0)                := (others => '0');
signal r_enable_1s                  : std_logic                            := '0';
signal r_enable_10s                 : std_logic                            := '0';
signal r_enable_1min                : std_logic                            := '0';
signal r_enable_10min               : std_logic                            := '0';
signal r_srl_enable_1s              : std_logic_vector(2 downto 0)         := (others => '0');
signal r_srl_enable_10s             : std_logic_vector(1 downto 0)         := (others => '0');
signal r_srl_enable_1min            : std_logic                            := '0';                                                
signal enable_1s                    : std_logic                            := '0';
signal enable_10s                   : std_logic                            := '0';
signal enable_1min                  : std_logic                            := '0';
signal enable_10min                 : std_logic                            := '0';

-- Time
signal r_display_1s                 : unsigned(3 downto 0)         := (others => '0');
signal r_display_10s                : unsigned(3 downto 0)         := (others => '0');
signal r_display_1min               : unsigned(3 downto 0)         := (others => '0');
signal r_display_10min              : unsigned(3 downto 0)         := (others => '0');

-- 7 segments display
signal r_display_7_seg_tx           : std_logic_vector(15 downto 0)        := (others => '0');
signal r_display_7_seg_rx           : std_logic_vector(15 downto 0)        := (others => '0');
signal r_display_7_seg              : std_logic_vector(15 downto 0)        := (others => '0');

-- UART
signal r_data_tx                    : std_logic_vector(7 downto 0)         := (others => '0');
signal r_data_tx_en                 : std_logic                            := '0';
signal data_rx                      : std_logic_vector(7 downto 0)         := (others => '0');
signal data_rx_en                   : std_logic                            := '0';
signal r_data_tx_loopback           : std_logic_vector(7 downto 0)         := (others => '0');
signal r_data_tx_loopback_en        : std_logic                            := '0';
signal data_tx_ack                  : std_logic                            := '0';
signal s_data_tx                    : std_logic_vector(7 downto 0)         := (others => '0');
signal s_data_tx_en                 : std_logic                            := '0';

-- Leds
signal led_vect                     : std_logic_vector(7 downto 0)         := (others => '0');
signal led_enable_1s                : std_logic                            := '0';

begin
   
   ------------------------------------------
   -- Debounce input reset
   ------------------------------------------
   p_debounce_reset : process(clk)
   begin
      if rising_edge(clk) then
          r_rsl_reset <= r_rsl_reset(r_rsl_reset'left-1 downto 0) & rst;
      end if;
   end process;

   reset_global      <= AND_VECT(r_rsl_reset);
   reset_global_n    <= not reset_global;

   ------------------------------------------
   -- DCM to generate 10 MHz clock
   ------------------------------------------

   i_feedback_dcm : BUFG
   port map(
      I  => clk,
      O  => clk_50M
   );
   
   ------------------------------------------
   -- Input switchs
   -- S3  S2  S1  S0   State
   -- 0   0   0   0    Receive data on RS232 and display it on 7 segments display
   -- 0   0   0   1    Send data every seconds according to the data displayed on the 7 segments
   -- 0   0   1   1    Loopback RX to TX
   ------------------------------------------
   
   p_state : process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         case sw is
            when x"00"   => r_state     <= RX_STATE;  
            when x"01"   => r_state     <= TX_STATE;
            when x"03"   => r_state     <= LOOPBACK_STATE;
            when others  => r_state     <= LOOPBACK_STATE;
         end case;
      end if;
   end process;
   
   ------------------------------------------
   -- Main FSM that handle every test cases
   ------------------------------------------
   p_main_fsm : process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         r_last_state <= r_state;
         case r_state is
            when RX_STATE =>
               if data_rx_en = '1' then
                  r_display_7_seg_rx <= r_display_7_seg_rx(11 downto 0) & data_rx(3 downto 0);
               end if;
            when TX_STATE =>
               -- Send the 4 bytes displayed every 1 second
               r_start_sending <= r_enable_1s and r_last_sent_done;
            when LOOPBACK_STATE  =>
               r_data_tx_loopback      <= data_rx;
               r_data_tx_loopback_en   <= data_rx_en;
            when others =>
               null;
         end case;
         
         if r_last_state /= RX_STATE then
            r_display_7_seg_rx <= (others => '0');
         end if;
         
      end if;
   end process;
   
   ------------------------------------------   
   -- FSM in TX_STATE to send the 4 bytes
   ------------------------------------------   
   process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         -- default values
         r_data_tx_en   <= '0';
         
         case r_fsm_tx is
            when st_wait_start =>
               if r_start_sending = '1' then
                  r_fsm_tx          <= st_send_byte;
                  r_last_sent_done  <= '0';
                  r_data_to_send    <= r_display_7_seg_tx;
                  r_cnt_sent        <= x"5";
               else
                  r_last_sent_done  <= '1';
               end if;
            when st_send_byte =>
               if r_cnt_sent = 1 then  
                  r_data_tx         <= x"0D";
               else
                  r_data_tx         <= x"3" & r_data_to_send(15 downto 12);
               end if;
               r_data_to_send       <= r_data_to_send(11 downto 0) & r_data_to_send(15 downto 12);
               r_data_tx_en         <= '1';
               r_cnt_sent           <= r_cnt_sent - 1;
               r_fsm_tx             <= st_wait_ack;
            when st_wait_ack =>
               if data_tx_ack = '1' then
                  if r_cnt_sent = 0 then
                     r_fsm_tx <= st_wait_start;
                  else
                     r_fsm_tx <= st_send_byte;
                  end if;
               end if;
            when others =>
               r_fsm_tx <= st_wait_start;
         end case;
      end if;
   end process;
   
   ------------------------------------------
   -- MUX for input of UART module
   ------------------------------------------
   s_data_tx      <= r_data_tx               when r_state = TX_STATE else
                     r_data_tx_loopback      when r_state = LOOPBACK_STATE else
                     (others => '0');
   s_data_tx_en   <= r_data_tx_en            when r_state = TX_STATE else
                     r_data_tx_loopback_en   when r_state = LOOPBACK_STATE else
                     '0';
   
   ------------------------------------------     
   -- UART module
   ------------------------------------------  
   i_UART : entity work.UART
   generic map(
      CLK_IN      => 50000000,
      BAUDRATE    => 9600,
      DATA_BITS   => 8,
      STOP_BITS   => 1,
      USE_PARITY  => 0,
      ODD_PARITY  => 0
   )
   port map(
      clk            => clk_50M,
      rst_n          => reset_global_n,
      -- User intf
      data_in        => s_data_tx,
      data_in_en     => s_data_tx_en,
      data_in_ack    => data_tx_ack,
      data_out       => data_rx, 
      data_out_en    => data_rx_en,
      -- TX/RX
      RX             => RsRx,
      TX             => RsTx
   );

   -- RsTx <= RsRx;

   ------------------------------------------   
   -- Mux to 7 segments display
   ------------------------------------------
   process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         if enable_1s = '1' then
            case r_state is
               when RX_STATE        => r_display_7_seg   <= r_display_7_seg_rx;
               when TX_STATE        => r_display_7_seg   <= r_display_7_seg_tx;
               when LOOPBACK_STATE  => r_display_7_seg   <= r_display_7_seg_tx;
               when others          => r_display_7_seg   <= r_display_7_seg_tx;
            end case;
         end if;
      end if;
   end process;
   
   ------------------------------------------
   -- 7 segments module 
   ------------------------------------------
   i_7segment : entity work.decode_7seg
   port map(
      clk            => clk_50M,
      reset          => reset_global,
      data_in        => r_display_7_seg,
      segments       => seg,
      anode_selected => an
   );

   --seg               <= "0100100";
   --an                <= "1011";

   dp <= '1';
   
   ------------------------------------------
   -- Enable 1s, 10s, 1 min, 10 min
   ------------------------------------------
   process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         if r_cnt_1s = c_1second_50M - 1 then
            r_enable_1s <= '1';
            r_cnt_1s    <= (others => '0');
         else
            r_enable_1s <= '0';
            r_cnt_1s    <= r_cnt_1s + 1;
         end if;
         
         r_enable_10s   <= '0';
         if r_enable_1s = '1' then
            if r_cnt_10s = 9 then
               r_enable_10s   <= '1';
               r_cnt_10s      <= (others => '0');
            else               
               r_cnt_10s      <= r_cnt_10s + 1;
            end if;
         end if;
         
         r_enable_1min  <= '0';
         if r_enable_10s = '1' then
            if r_cnt_1min = 5 then
               r_enable_1min  <= '1';
               r_cnt_1min     <= (others => '0');
            else               
               r_cnt_1min     <= r_cnt_1min + 1;
            end if;
         end if;

         r_enable_10min <= '0';
         if r_enable_1min = '1' then
            if r_cnt_10min = 9 then
               r_enable_10min <= '1';
               r_cnt_10min    <= (others => '0');
            else               
               r_cnt_10min    <= r_cnt_10min + 1;
            end if;
         end if; 

         -- Delay to have the enables synchrone
         r_srl_enable_1s   <= r_srl_enable_1s(1 downto 0) & r_enable_1s;
         r_srl_enable_10s  <= r_srl_enable_10s(0 downto 0) & r_enable_10s;
         r_srl_enable_1min <= r_enable_1min;

         
      end if;
   end process;
         
   enable_1s      <= r_srl_enable_1s(2);
   enable_10s     <= r_srl_enable_10s(1);
   enable_1min    <= r_srl_enable_1min;
   enable_10min   <= r_enable_10min;

   ------------------------------------------
   -- Time since start
   -- 4 register of 4 bits to display the time
   -- in the following manner : MM-SS
   ------------------------------------------
   process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         if reset_global = '1' then 
            r_display_1s     <= (others => '0');   
            r_display_10s    <= (others => '0');   
            r_display_10s    <= (others => '0');   
            r_display_10min  <= (others => '0');  
         else 
            if enable_1s = '1' then
               if r_display_1s = 9 then
                  r_display_1s   <= (others => '0');
               else
                  r_display_1s   <= r_display_1s + 1;
               end if;
            end if;
            if enable_10s = '1' then
               if r_display_10s = 5 then
                  r_display_10s   <= (others => '0');
               else
                  r_display_10s   <= r_display_10s + 1;
               end if;
            end if;
            if enable_1min = '1' then
               if r_display_1min = 9 then
                  r_display_1min   <= (others => '0');
               else
                  r_display_1min   <= r_display_1min + 1;
               end if;
            end if;
            if enable_10min = '1' then
               if r_display_10min = 9 then
                  r_display_10min   <= (others => '0');
               else
                  r_display_10min   <= r_display_10min + 1;
               end if;
            end if;
         end if;
         
         r_display_7_seg_tx <= std_logic_vector(r_display_10min)
                               & std_logic_vector(r_display_1min)
                               & std_logic_vector(r_display_10s)
                               & std_logic_vector(r_display_1s);
         
      end if;
   end process;
  
   ------------------------------------------
   -- LEDs display
   -- Led 0 : ON  => DCM locked
   --         OFF => DCM unlocked
   -- Led 1 : ON  => FSM main in RX_STATE
   --         OFF => FSM main not in RX_STATE
   -- Led 2 : ON  => FSM main in TX_STATE
   --         OFF => FSM main not in TX_STATE
   -- Led 3 : ON  => FSM main in LOOPBACK_STATE
   --         OFF => FSM main not in LOOPBACK_STATE   
   ------------------------------------------
   
   led_vect(0)    <= dcm_locked;
   led_vect(1)    <= '1' when r_state = RX_STATE else '0';
   led_vect(2)    <= '1' when r_state = TX_STATE else '0';
   led_vect(3)    <= '1' when r_state = LOOPBACK_STATE else '0';   
   led_vect(4)    <= led_enable_1s;
   led_vect(5)    <= reset_global;
   led_vect(6)    <= '0';
   led_vect(7)    <= '0';

   process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         if enable_1s = '1' then
            led_enable_1s <= not led_enable_1s;
         end if;
      end if;
   end process;
   
   -- Output buffer
   p_leds : process(clk_50M)
   begin
      if rising_edge(clk_50M) then
         Led   <= led_vect;
      end if;
   end process;

end rtl;