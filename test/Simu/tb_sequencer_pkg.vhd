library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package tb_sequencer_pkg is


   function instr_A(rD,rA,rB : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector;
   function instr_B(rD,rA,imm : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector;
   
end tb_sequencer_pkg;

package body tb_sequencer_pkg is

   function instr_A(rD,rA,rB : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(15 downto 11) := std_logic_vector(to_unsigned(rB,5));
      
      return result;
   end function;
   
   function instr_B(rD,rA,imm : integer; opcode : std_logic_vector(5 downto 0)) return std_logic_vector is
   variable result : std_logic_vector(31 downto 0) := (others => '0');
   begin
      result(31 downto 26) := opcode;
      result(25 downto 21) := std_logic_vector(to_unsigned(rD,5));
      result(20 downto 16) := std_logic_vector(to_unsigned(rA,5));
      result(15 downto  0) := std_logic_vector(to_unsigned(imm,16));
      
      return result;
   end function;   
      
   

end;