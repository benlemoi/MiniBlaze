library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.ALU_pkg;


entity ALU
   generic(
      DATA_WIDTH  : natural := 32
   );
   port(
      param_i     : in  param_alu;
      carry_i     : in  std_logic;
      operandA_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      operandB_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      operandD_o  : out std_logic_vector(DATA_WIDTH - 1 downto 0);
      status_o    : out status_alu
   );
end ALU;

architecture rtl of ALU is

signal operandA_signed     : signed(DATA_WIDTH - 1 downto 0);
signal operandB_signed     : signed(DATA_WIDTH - 1 downto 0);

signal operandA_unsigned   : unsigned(DATA_WIDTH - 1 downto 0);
signal operandB_unsigned   : unsigned(DATA_WIDTH - 1 downto 0);

begin

operandA_signed   <= signed(operandA_i);
operandB_signed   <= signed(operandB_i);

operandA_unsigned <= unsigned(operandA_i);
operandB_unsigned <= unsigned(operandB_i);



   process(param_i, carry_i, operandA_i, operandB_i)
   
   variable result      : signed(DATA_WIDTH downto 0) := (others => '0');
   variable result_add  : signed(DATA_WIDTH downto 0) := (others => '0');
   variable carry_in    : std_logic                   := '0';
   variable carry_out   : std_logic                   := '0';
   
   begin
   
      -- Chose carry in
      case param_i.ctrl_op.whichCarry is
         when CARRY_IN =>
            carry_in := carry_i;
         when CARRY_ONE =>
            carry_in := '1';
         when CARRY_ZERO =>
            carry_in := '0';
         when CARRY_ARITH =>
            carry_in := operandA_i(operandA_i'left);
      end case;
      
      result_add := signed(add(operandA_i, operandB_i, carry_in));
   
      case param_i.operation is
         when OP_PT =>
            result   := resize(operandA_signed, DATA_WIDTH + 1);
         when OP_ADD =>
            result   := resultat_add;
         when OP_AND =>
            result   := operandA_signed and operandB_signed;
         when OP_OR =>
            result   := operandA_signed or operandB_signed;
         when OP_SHIFT => 
            result   := operandA_signed(0) & carry_in & operandA_signed(operandA_signed'left downto 1);
      
      end case;
      
      carry_out <= result(DATA_WIDTH);
      
      
            
         
            
   
   end process;






-- -------------------------------------
-- |   code   |  operation                          
-- +------------------------------------
-- | 00000000 | Add
-- | 00000001 | Add with Carry
-- | 00000010 | Add with Keep Carry
-- | 00000011 | Add with Carry and Keep Carry
-- | 00000100 | Logical AND
-- | 00000101 | Logical AND NOT
-- |          |                        
-- |          |                        


   process(code_i, status_i, operandA_i, operandB_i)
   
   variable v_operandD  : std_logic_vector(32 downto 0);
   
   begin
      case code_i is
         -- Add subset
         when "00000000" => -- Add
            v_operandD     := operandA_signed + operandB_signed;
            s_status.carry <= v_operandD(32);
            s_status.overflow <= 
            s_operandD     <= v_operandD(31 downto 0);
         when "0000001" => -- Add with Carry
            v_operandD     := operandA_signed + operandB_signed + resize(signed('0' & status_i.carry), operandA_signed'range);
            s_status.carry <= v_operandD(32);
            s_operandD     <= v_operandD(31 downto 0);
         when "00000010" => -- Add with Keep Carry
            v_operandD     := operandA_signed + operandB_signed;
            s_status.carry <= status_i.carry;
            s_operandD     <= v_operandD(31 downto 0);
         when "00000011" => -- Add with Carry and Keep Carry
            v_operandD     := operandA_signed + operandB_signed + resize(signed('0' & status_i.carry), operandA_signed'range);
            s_status.carry <= status_i.carry;
            s_operandD     <= v_operandD(31 downto 0);         
            
         when "00000100" => -- Logical AND
            s_operandD     <= std_logic_vector(operandA_signed and operandB_signed);
            s_status.carry <= '0';
         when "00000101" => -- Logical AND NOT
            s_operandD     <= std_logic_vector(operandA_signed and not(operandB_signed));
            s_status.carry <= '0';         
            
         when "00000110" => -- Logical OR
            s_operandD     <= std_logic_vector(operandA_signed or operandB_signed);
            s_status.carry <= '0';
         
         -- Subtract subset
         when "00000111" => -- Substract
            v_operandD     := operandA_signed - operandB_signed;
            s_status.carry <= v_operandD(32);
            s_operandD     <= v_operandD(31 downto 0);
         when "00001000" => -- Substract with Carry
            v_operandD     := operandA_signed - operandB_signed - resize(signed('0' & status_i.carry), operandA_signed'range);
            s_status.carry <= v_operandD(32);
            s_operandD     <= v_operandD(31 downto 0);
         when "00001001" => -- Substract and keep Carry
            v_operandD     := operandA_signed - operandB_signed;
            s_status.carry <= status_i.carry;
            s_operandD     <= v_operandD(31 downto 0);
         when "00001010" => -- Substract with Carry and Keep Carry
            v_operandD     := operandA_signed - operandB_signed - resize(signed('0' & status_i.carry), operandA_signed'range);
            s_status.carry <= status_i.carry;
            s_operandD     <= v_operandD(31 downto 0);
            
         when "00001011" => -- Sign Extend Halfword
            s_operandD     <= std_logic_vector(resize(operandA_signed(15 downto 0), 32));
            s_status.carry <= '0';
         when "00001011" => -- Sign Extend Byte
            s_operandD     <= std_logic_vector(resize(operandA_signed(7 downto 0), 32));
            s_status.carry <= '0';
         
         when "00001100" => -- Shift Right Arithmetic
            s_operandD(31)          <= operandA_i(31);
            s_operandD(30 downto 0) <= operandA_i(31 downto 1);
            s_status.carry          <= operandA_i(0);
         when "00001101" => -- Shift Right with Carry
            s_operandD(31)          <= status_i.carry;
            s_operandD(30 downto 0) <= operandA_i(31 downto 1);
            s_status.carry          <= operandA_i(0);            
         when "00001110" => -- Shift Right Logical
            s_operandD(31)          <= '0';
            s_operandD(30 downto 0) <= operandA_i(31 downto 1);
            s_status.carry          <= operandA_i(0);            

         when "00001111" => -- Logical Excluse OR
            s_operandD     <= std_logic_vector(operandA_signed xor operandB_signed);
            s_status.carry          <= '0';
         
         when "00010000" => -- Barrel Shift Right Logical
            s_operandD     <= std_logic_vector(sll(operandA_signed, to_integer(operandB_signed(4 downto 0)));
            s_status.carry <= '0';
         when "00010001" => -- Barrel Shift Right Arithmetical
            s_operandD     <= std_logic_vector(srl(operandA_unsigned, to_integer(operandB_signed(4 downto 0)));
            s_status.carry <= '0';       
         when "00010001" => -- Barrel Shift Left Logical (srl shift right)
            s_operandD     <= std_logic_vector(srl(operandA_signed, to_integer(operandB_signed(4 downto 0)));
            s_status.carry <= '0';      

         when others =>
            s_operandD     <= (others => '0');
            s_status.carry <= '0';
      
      end case;
   
   end process;
   
   
   process(s_operandD, s_status.carry)
   begin
      s_status.zero        <= is_zero(s_operandD);
      s_status.negative    <= is_negative(s_operandD);
      s_status.overflow    <= (not s_status.carry and s_operandD(s_operandD'left)) or (s_status.carry and not s_operandD(s_operandD'left));
      s_status.parity      <= '0'; -- TODO
   end process;

end rtl;
      