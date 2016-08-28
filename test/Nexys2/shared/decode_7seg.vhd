library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity decode_7seg is
port(
		clk				: in	std_logic;							-- Clock
		reset				: in 	std_logic;							-- Reset
		data_in			: in 	std_logic_vector(15 downto 0);-- Number to be displayed   4 bits for each segements
		segments			: out	std_logic_vector(6 downto 0); -- Seven segments
		anode_selected : out std_logic_vector(3 downto 0)	-- Selecting one of the four seven segments
		);
end decode_7seg;

architecture rtl of decode_7seg is

constant c_1_ms				: integer	                     := 50000;
signal r_cnt 					: unsigned(31 downto 0)          := (others => '0');
signal r_segments				: std_logic_vector(6 downto 0)	:= (others => '1');
signal r_data_4b				: std_logic_vector(3 downto 0)	:= (others => '0');
signal r_anode_selected		: std_logic_vector(3 downto 0)	:= "1110";

begin

	process(clk)
	begin
		if rising_edge(clk) then
			if reset = '1' then
				r_segments			<= "1111111";
				r_cnt					<= (others => '0');
				r_anode_selected	<= "1110";
		 	else	
				-- Mux data_in
				for i in 0 to r_anode_selected'length-1 loop
					if r_anode_selected(i) = '0' then	
						r_data_4b	<= data_in(4*(i+1)-1 downto 4*i);
					end if;
				end loop;

				-- Display
				case r_data_4b is
					when "0000" => -- Display 0
						r_segments <= "1000000";
					when "0001" => -- Display 1
						r_segments <= "1111001";
					when "0010" => -- Display 2
						r_segments <= "0100100";
					when "0011" => -- Display 3
						r_segments <= "0110000";
					when "0100" => -- Display 4
						r_segments <= "0011001";
					when "0101" => -- Display 5
						r_segments <= "0010010";
					when "0110" => -- Display 6
						r_segments <= "0000010";
					when "0111" => -- Display 7
						r_segments <= "1111000";
					when "1000" => -- Display 8
						r_segments <= "0000000";
					when "1001" => -- Display 9
						r_segments <= "0010000";
					when others =>
						r_segments <= "0000000";	
				end case;

				-- Switch anode every 10 ms
				if r_cnt = c_1_ms then	
					r_cnt						<= (others => '0');
					r_anode_selected		<= r_anode_selected(2 downto 0) & r_anode_selected(3);
			 	else	
				 	r_cnt						<= r_cnt + 1; 			
				end if;
			end if;
		end if;
	end process;

   segments          <= r_segments;
   anode_selected    <= r_anode_selected;

end rtl;

