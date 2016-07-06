library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ALU_pkg is

   type t_status_alu_out is
      record
         carry    : std_logic;   -- Carry resulting from an addition or a subtraction operation
         zero     : std_logic;   -- All bits of the result (operandC) are logic zero
         negative : std_logic;   -- The result of the arithmetic operation if negative
         overflow : std_logic;   -- 
         parity   : std_logic;   -- Indicates whether an even or odd number of bits on the Y bus are logic one
      end record;
      
   type t_status_alu_in is
      record
         carry    : std_logic;   -- Carry from last operation (Contained in MSR)
      end record;
      
   type t_opcode_alu is (
      OP_PT,      -- Pass through
      OP_ADD,     -- Add
      OP_AND,     -- And
      OP_OR,      -- Or
      OP_SHIFT,   -- Right shift
      OP_XOR,     -- Xor
      OP_SEXT8,   -- Sign extend Byte
      OP_SEXT16,  -- Sign extend Word
      OP_MULT,    -- Multiplication
      OP_BS       -- Barrel Shift
   );
   
   type t_type_carry is (
      CARRY_IN,
      CARRY_ONE,
      CARRY_ZERO,
      CARRY_ARITH
   );
      
   type t_ctrl_op_alu is
      record
         keepCarry   : std_logic;     
         negOperandB : std_logic;
         negOperandA : std_logic;
         whichCarry  : t_type_carry;
      end record;
   
   type param_alu is
      record
         operation   : t_opcode_alu;
         ctrl_op     : t_ctrl_op_alu;
      end record;
    
   function is_zero(a : std_logic_vector) return std_logic;
   function is_negative(a : signed) return std_logic;
   function add(a,b : std_logic_vector; c : std_logic) return std_logic_vector;
   
end ALU_pkg;

package body ALU_pkg is
   
   function is_zero(a : std_logic_vector) return std_logic is
      variable tmp : std_logic_vector(a'range);
   begin
      tmp := (others => '0');
      if tmp = a then
         return '1';
      else
         return '0';
      end if;
   end function;
   
   function is_negative(a : signed) return std_logic is
   begin
      if a < 0 then
         return '1';
      else
         return '0';
      end if;
   end function;
   
   function add(a,b : std_logic_vector; c : std_logic) return std_logic_vector is
      variable CBIT     : std_logic                   := c;
      variable RESULT   : std_logic_vector(a'range)   := (others => '0');
      alias XA : std_logic_vector(a'range) is a;
      alias XB : std_logic_vector(a'range) is b;
   begin
      for i in 0 to a'left loop
         RESULT(i)   <= CBIT xor XA(i) xor XB(i);
         CBIT        <= (CBIT and XA(i)) or (CBIT and XB(i)) or (XA(i) and XB(i));
      end loop;
      return RESULT;
   end add;
      
      

end;
      