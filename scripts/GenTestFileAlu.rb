module TypeVHDL

    T_opcode_alu = [ "OP_PTA",
                     "OP_PTB",    
                     "OP_ADD",  
                     "OP_AND", 
                     "OP_OR",  
                     "OP_SHIFT",
                     "OP_XOR",
                     "OP_SEXT8",
                     "OP_SEXT16",
                     "OP_MULT",
                     "OP_BS"    ]
                     
    T_type_carry = ["CARRY_INPUT",
                    "CARRY_ONE",
                    "CARRY_ZERO",
                    "CARRY_ARITH"   ]
                    
    T_type_shift = ["LEFT_SHIFT",
                    "RIGHT_SHIFT_ARITH",
                    "RIGHT_SHIFT_LOGIC" ]

end

class TestFileAlu

    SIZE_MAX  = 1024
    STR_START = "library ieee;\n" \
                "use ieee.std_logic_1164.all;\n" \
                "library work;\n" \
                "use work.ALU_pkg.all;\n" \
                "package data_pkg is\n" \
                "type t_test_data is\n" \
                "   record\n" \
                "       operandA    : std_logic_vector(31 downto 0);\n" \
                "       operandB    : std_logic_vector(31 downto 0);\n" \
                "       carry_in    : std_logic;\n" \
                "       operation   : t_opcode_alu;\n" \
                "       keepCarry   : std_logic;\n" \
                "       negOpA      : std_logic;\n" \
                "       negOpB      : std_logic;\n" \
                "       carry_type  : t_type_carry;\n" \
                "       shift_type  : t_type_shift;\n" \
                "       operandD    : std_logic_vector(31 downto 0);\n" \
                "       carry_out   : std_logic;\n" \
                "       zero        : std_logic;\n" \
                "       negative    : std_logic;\n" \
                "       overflow    : std_logic;\n" \
                "       parity      : std_logic;\n" \
                "   end record;\n" \
                "type t_vect_test_data is array(natural range<>) of t_test_data;\n"
                "\n"
                

    def initialize(file_path, size)    
        @file       = File.new(file_path, "wb")
        @file.write(STR_START)
        @last_carry = 0
        @first      = true
        @str_to_wr = ""
        @nb_data    = 0
        @file.write("constant data_in : t_vect_test_data(0 to #{size-1}) :=\n")        
    end
    
    def write(op1, op2, carry_in, operation, keepCarry, negOpA, negOpB, carry_in_type, ctrlShift)
    
        result, carry_out, zero, negative, overflow, parity = compute(op1, op2, carry_in, operation, keepCarry, negOpA, negOpB, carry_in_type, ctrlShift)
        @last_carry = carry_out
        tab = [op1, op2, carry_in, operation, keepCarry, negOpA, negOpB, carry_in_type, ctrlShift, result, carry_out, zero, negative, overflow, parity]
        # Format d'ecriture
        str = ""
        if(@first)
            @first = false
            str += "("
        else
            str += ","
        end
        str += "#{@nb_data} => (#{format32b(op1)}, #{format32b(op2)}, #{format1b(carry_in)}, #{T_opcode_alu[operation]}, #{format1b(keepCarry)}, #{format1b(negOpA)}, #{format1b(negOpB)}, "\
              "#{T_type_carry[carry_in_type]}, #{T_type_shift[ctrlShift]}, #{format32b(result)}, #{format1b(carry_out)}, #{format1b(zero)}, #{format1b(negative)}, #{format1b(overflow)}, #{format1b(parity)})\r\n"
        @file.write(str)
        @nb_data += 1
    
    end
    
    def format32b(vect_32b)
        return_str = "x\"#{"%08.8x"%(vect_32b&0xffffffff)}\""
    end
    
    def format4b(vect_4b)
        return_str = "x\"#{"%1x"%(vect_4b&0xf)}\""
    end    
    
    def format1b(bit)
        return_str = "'#{"%1d"%(bit&0x1)}'"
    end
    
    def compute(op1, op2, carry_in, operation, keepCarry, negOpA, negOpB, carry_in_type, ctrlShift)
        result      = 0
        carry_out   = 0
        zero        = 0
        negative    = 0
        overflow    = 0
        parity      = 0
        
        op1 = op1&0xffffffff
        op2 = op2&0xffffffff
        
        if negOpA == 1 
            op1 = ~op1
        end
        if negOpB == 1
            op2 = ~op2
        end
        
        op1 = op1&0xffffffff
        op2 = op2&0xffffffff        
        
        case carry_in_type
            when 0
                carry = carry_in
            when 1
                carry = 1
            when 2
                carry = 0
            when 3
                carry = (op1 >> 31)&0x1
            else
                carry = 0
        end
        
        case operation
            when 0 # Pass Through A
                result = op1
            when 1 # Pass Through A
                result = op2              
            when 2 # Add
                result = op1 + op2 + carry
            when 3 # And
                result = op1 & op2             
            when 4 # Or
                result = op1 | op2
            when 5 # Right Shift
                result = ((op1 >> 1)&0x7fffffff) | ((carry<<31)&0x80000000) | ((op1&0x1)<<32)
            when 6 # Xor
                result = op1 ^ op2
            when 7 # Sign extend Byte
                if (op1>>7)&0x1 == 1
                    result = (op1&0x000000ff) | (0xffffff00)
                else
                    result = (op1&0x000000ff)
                end
            when 8 # Sign extend Word
                if (op1>>15)&0x1 == 1
                    result = (op1&0x0000ffff) | (0xffff0000)
                else
                    result = (op1&0x0000ffff)
                end
            when 9 # Multiplication
                result = op1*op2
            when 10 # Barrel Shift
                case ctrlShift 
                    when 0
                        result = op1 << (op2&0x1F)
                    when 1
                        result = op1 >> (op2&0x1F)
                    when 2
                        result = op1.abs >> (op2&0x1F)
                    else
                        result = 0
                end
            else
                result = 0
        end
        
        carry_out   = 0
        carry_out   = (result>>32)&0x1 if [0,1,2,5,7,8,9].include?(operation)
        zero        = 1 if 0 == (result&0xFFFFFFFF)
        negative    = 1 if (1 == (result>>31)&0x1)
        # overflow    = ( ((~carry_out)&0x1) & ((result>>31)&0x1) ) | ( ((carry_out)&0x1) & ((~(result>>31))&0x1) )
        overflow    = ( ((carry_out==0) ? 1 : 0) & ((result>>31)&0x1) ) | ( ((carry_out)&0x1) & ((((result>>31)&0x1)==0) ? 1 : 0) )
        parity      = 0
        result      = result&0xffffffff
        
        return result, carry_out, zero, negative, overflow, parity
        
    end
    
    def close
        @file.write(");\n")
        @file.write("end data_pkg;")
        @file.close
    end
    
    def test
        puts format32b(0x01234567)
        puts format1b(0)
        puts format1b(1)
        puts format1b(1345)
    end
        
end


if __FILE__ == $0
    include TypeVHDL
    N = 8192;
    testAlu = TestFileAlu.new('test/Simu/data_pkg.vhd', N)
    puts("Starting")
    (0...N).each do |i|
        op1 = rand((2**31)-1)
        op2 = rand((2**31)-1)
        carry = rand(3)
        operation = rand(T_opcode_alu.size)
        negOpA = rand(2)
        negOpB = rand(2)
        keepCarry = rand(2)
        carry_in_type = rand(4)
        ctrlShift = rand(3)
        print("#{100*i/N}%         \r")
        testAlu.write(op1,op2,carry,operation, keepCarry, negOpA, negOpB, carry_in_type,ctrlShift)
    end
    print("\n")
    puts("Done")    
    # testAlu = TestFileAlu.new('data_pkg1.vhd', 1)
    # op1 = 0x75ee090a
    # op2 = 0x2d015b30
    # carry = 1
    # operation = 3
    # negOpA = 0
    # negOpB = 0
    # keepCarry = 1
    # carry_in_type = 3
    # ctrlShift = 0
    # testAlu.write(op1,op2,carry,operation, keepCarry, negOpA, negOpB, carry_in_type,ctrlShift)
    # testAlu.test
    testAlu.close
end

    
