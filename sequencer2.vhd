library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use work.constants.all;

entity sequencer2 is
    port(
		rst                : in  std_logic;
		clk              	 : in  std_logic;
		ale		  	 : out std_logic; --Address Latch Enable
		psen		 	 : out std_logic; --Program Store Enable

		alu_op_code	 	 : out  std_logic_vector (3 downto 0);
		alu_src_1L		 : out  std_logic_vector (7 downto 0);
		alu_src_1H		 : out  std_logic_vector (7 downto 0);
		alu_src_2L		 : out  std_logic_vector (7 downto 0);
		alu_src_2H		 : out  std_logic_vector (7 downto 0);
		alu_by_wd		 : out  std_logic;             -- byte(0)/word(1) instruction
		alu_cy_bw		 : out  std_logic;             -- carry/borrow bit
		alu_ans_L		 : in std_logic_vector (7 downto 0);
		alu_ans_H		 : in std_logic_vector (7 downto 0);
		alu_cy		 	 : in std_logic;             -- carry out of bit 7/15
		alu_ac		 	 : in std_logic;		    -- carry out of bit 3/7
		alu_ov		 	 : in std_logic;		    -- overflow

		dividend_i		 : out  std_logic_vector(15 downto 0);
		divisor_i		 : out  std_logic_vector(15 downto 0);
		quotient_o		 : in std_logic_vector(15 downto 0); 
		remainder_o	 	 : in std_logic_vector(15 downto 0);
		div_done		 : in std_logic ;

		mul_a_i		 	 : out  std_logic_vector(15 downto 0);  -- Multiplicand
		mul_b_i		 	 : out  std_logic_vector(15 downto 0);  -- Multiplicator
		mul_prod_o 	 	 : in std_logic_vector(31 downto 0) ;-- Product

		i_ram_wrByte   	 : out std_logic; 
		i_ram_wrBit   	 : out std_logic; 
		i_ram_rdByte   	 : out std_logic; 
		i_ram_rdBit   	 : out std_logic; 
		i_ram_addr 	 	 : out std_logic_vector(7 downto 0); 
		i_ram_diByte  	 : out std_logic_vector(7 downto 0); 
		i_ram_diBit   	 : out std_logic; 
		i_ram_doByte   	 : in std_logic_vector(7 downto 0); 
		i_ram_doBit   	 : in std_logic; 
		
		i_rom_addr       : out std_logic_vector (15 downto 0);
		i_rom_data       : in  std_logic_vector (7 downto 0);
		i_rom_rd         : out std_logic;
		
		pc_debug	 	 : out std_logic_vector (15 downto 0);
		interrupt_flag	 : in  std_logic_vector (2 downto 0);
		erase_flag	 : out std_logic);

end sequencer2;

-------------------------------------------------------------------------------

architecture seq_arch of sequencer2 is

    type t_cpu_state is (T0, T1, I0); --these determine whether you are in initialisation, state, normal execution state, etc. T0 is fetching, T1 is execution
    type t_exe_state is (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10); --these are the equivalence T0, T1 in the lecture
    
	signal cpu_state 		: t_cpu_state;
    signal exe_state 		: t_exe_state;
    signal IR_1				: std_logic_vector(7 downto 0);		-- Instruction Registers
    signal IR_2				: std_logic_vector(7 downto 0);	
    signal IR_3				: std_logic_vector(7 downto 0);	
    signal N_BYTE		: std_logic_vector(1 downto 0);     -- number of instruction byte
	signal PC				: std_logic_vector(15 downto 0);	-- Program Counter
	signal AR				: std_logic_vector(7 downto 0);		-- Address Register
	signal DR				: std_logic_vector(7 downto 0);		-- Data Register
	signal int_hold			: std_logic;
	signal reg_pc_15_11 : std_logic_vector (4 downto 0);
    signal reg_pc_10_8  : std_logic_vector (2 downto 0);
    signal reg_pc_7_0   : std_logic_vector (7 downto 0);
    signal reg_op1      : std_logic_vector (7 downto 0);
    signal reg_op2      : std_logic_vector (7 downto 0);
    signal reg_op3      : std_logic_vector (7 downto 0);
    signal reg_acc      : std_logic_vector (7 downto 0);
    signal reg_cy       : STD_LOGIC;
    signal reg_ac       : STD_LOGIC;
    signal reg_f0       : STD_LOGIC;
    signal reg_rs1      : STD_LOGIC;
    signal reg_rs0      : STD_LOGIC;
    signal reg_ov       : STD_LOGIC;
    signal reg_nu       : STD_LOGIC;
    signal reg_p        : STD_LOGIC;
    signal psw_temp: std_logic_vector(7 downto 0); -- PSW
	 signal ACC: std_logic_vector(7 downto 0); -- ACC register
	 signal temp_sig8: std_logic_vector(7 downto 0); -- ACC register

begin

    process(rst, clk)

-------------------------------------------------------------------------------
        
        procedure GET_RAM_ADDR_1 (a : out std_logic_vector (7 downto 0)) is
        begin
            a := "000" & reg_rs1 & reg_rs0 & IR_1(2 downto 0);
        end GET_RAM_ADDR_1;

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

		  procedure START_RD_RAM (a : std_logic_vector(7 downto 0); read_byte: std_logic) is
			begin
			i_ram_addr		<= a;
			i_ram_rdBit		<= not read_byte;
			i_ram_rdByte	<= read_byte;
			i_ram_wrBit		<= '0';
			i_ram_wrByte	<= '0';
			end START_RD_RAM;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
        
        procedure START_WR_RAM (a : std_logic_vector (7 downto 0)) is
        begin
            i_ram_addr <= a;
            i_ram_rdBit <= '0';
            i_ram_wrBit <= '1';
				i_ram_rdByte <= '0';
				i_ram_wrByte <= '1';
        end START_WR_RAM;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
        
        procedure STOP_RD_WR_RAM is
        begin
            i_ram_addr <= "--------";
            i_ram_diByte <= "--------";
            i_ram_diBit <= '-';
            i_ram_rdBit <= '0';
            i_ram_wrBit <= '0';
        end STOP_RD_WR_RAM;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

        procedure START_RD_ROM (addr: std_logic_vector (15 downto 0)) is
        begin
            i_rom_addr <= addr;
            i_rom_rd <= '1';
        end START_RD_ROM;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

        procedure SET_PSW (p : std_logic_vector (7 downto 0)) is
        begin
            reg_cy <= p(7);
            reg_ac <= p(6);
            reg_f0 <= p(5);
            reg_rs1 <= p(4);
            reg_rs0 <= p(3);
            reg_ov <= p(2);
            reg_nu <= p(1);
            reg_p <= p(0);
        end SET_PSW;
        
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
	 
	 procedure INC_PC is
		begin
			alu_src_1L 	<= PC(7 downto 0);
			alu_src_1H	<= PC(15 downto 8);
			alu_by_wd	<= WORD;             -- byte(0)/word(1) instruction
			alu_op_code	<= ALU_OPC_INC;
		end INC_PC;
		
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
	 procedure SHUT_DOWN_ALU is
		begin
			alu_op_code <= ALU_OPC_NONE;
		--	alu_src_1L	<= "--------";  --every alu operation always deals with src_1L and src_2L so we don't need to reset here
			alu_src_1H	<= "--------";--will be set if 2 bytes operation
		--	alu_src_2L	<= "--------";
			alu_src_2H	<= "--------";--will be set if 2 bytes operation
			alu_cy_bw	<= '0';--CARRY/BORROW = 0 by defaut
			alu_by_wd	<= '0';--BYTE: defaut
		end SHUT_DOWN_ALU;
-------------------------------------------------------------------------------

    begin
    if( rst = '1' ) then
   	cpu_state <= T0;
    exe_state <= E0;
	ale <= '0'; psen <= '0';
	mul_a_i <= (others => '0'); mul_b_i <= (others => '0');
	dividend_i <= (others => '0'); divisor_i <= (others => '1');
	i_ram_wrByte <= '0'; i_ram_rdByte <= '0'; i_ram_wrBit <= '0'; i_ram_rdBit <= '0';
	IR_1 <= (others => '0');
	IR_2 <= (others => '0');
	IR_3 <= (others => '0');
	PC <= (others => '0');
--	PC <= "0000000000100111";
	AR <= (others => '0');
	DR <= (others => '0');
	pc_debug <= (others => '1');
	int_hold <= '0';
	erase_flag <= '0';	
    elsif (clk'event and clk = '1') then

    --processing instructions
    case cpu_state is
		when T0 =>
			case exe_state is
				when E0	=>--fetch instruction
					START_RD_ROM(PC); --load PC address of instruction to ROM, 16 bits
					START_RD_RAM(xE0,'1');--read ACC
					INC_PC;			--increase PC to next instruction
					exe_state<= E1;		
					
				when E1	=> --decode instruction
					PC (15 downto 8) <= alu_ans_H; 
					PC (7 downto 0) <=alu_ans_L;
					IR_1<=i_rom_data;	--read instruction from ROM to IR_1 , 1 byte	
					ACC<= i_ram_doByte;--preload data of ACC
					i_ram_addr<= xD0; --read PSW
					exe_state <= E2;
					
				when E2 =>
					i_rom_addr <= PC;
					psw_temp <= i_ram_doByte;--preload data of PSW
					SET_PSW(psw_temp);
					exe_state <=E3;
					
				when E3 =>
					IR_2<= i_rom_data;--preload IR_2
					exe_state<= E0;
					cpu_state<=T1;
				
				when others =>	 
			end case;  
		when T1 =>
			case IR_1 is 
				
				-- NOP
				when "00000000"  =>
					case exe_state is
						when E0	=>  
							exe_state <= E1;
						
						when E1	=>
							exe_state <= E2;
							
						when E2	=>						
							exe_state <= E0;
							cpu_state <= T0;
						when others => null;
					end case;  
				


-------------------------------------------------------------------------------------------------------------------------
--------------- MOV A,Rn
				when "11101000" 
					| "11101001"
					| "11101010"
					| "11101011"
					| "11101100"
					| "11101101"
					| "11101110"
					| "11101111"  =>
					case exe_state is
						when E0 => -- initialize to read PSW
							START_RD_RAM("000" & reg_rs1 & reg_rs0 & IR_1(2 downto 0),'1'); -- fetch address of Rn into RAM
							exe_state <= E1;
							
						when E1 => -- write data of Rn to ACC
							START_WR_RAM(xE0); --set ACC address to be written
							i_ram_diByte <= i_ram_doByte;
							exe_state <= E2;				
						
						when E2 =>
							STOP_RD_WR_RAM;
							exe_state <= E0;
							cpu_state <= T0;
							
						when others =>  null;
					end case; 
-------------------------------------------------------------------------------------------------------------------------
----------------ACALL
					-- pc       <- pc + 2
			        -- sp       <- sp + 1
                    -- mem(sp)  <- pc(7-0)
                    -- sp       <- sp + 1
                    -- mem(sp)  <- pc(15-8)
                    -- pc(10-0) <- page address
                    --The page address is obtained by successively concatenating the five high-order bits of the incremented PC, opcode bits 7-5, and the second byte of the instruction.

                when "---10001" =>
                	case exe_state is
                		when E0 =>
                			START_RD_RAM(x81,'1'); -- read SP address
                			PC <= PC+2;	--increment PC by 2
                			exe_state <= E1;

                		when E1 =>
                			temp_sig8 <= i_ram_doByte+2;
                			START_WR_RAM(i_ram_doByte+1);	--write to SP+1 address
                			i_ram_diByte <= PC(7 downto 0);	--write in PC(7-0)
                			exe_state <= E2;

                		when E2 =>
                			START_WR_RAM(temp_sig8);	--write to SP+2 address
                			i_ram_diByte <= PC(15 downto 8); --write in PC(15-8)
                			PC <= PC(15 downto 11) & IR_1(7 downto 5) & IR_2; --Set PC. 
                			exe_state <= E3;

                		when E3 =>
                			STOP_RD_WR_RAM;
                			exe_state <= E0;
							cpu_state <= T0;
						when others =>  null;

                	end case;
						
						
-------------------------------------------------------------------------------------------------------------------------
----------------AJMP
					 when "---00001" =>
						case exe_state is
							when E0 =>
								PC <= PC+1; --should we use the ALU here (would tht be more 'pure' ?
								exe_state <= E1;
								
							when E1 =>
								PC <= PC(15 downto 11)&IR_1(7 downto 5)&IR_2;
								exe_state <= E2;
								
							when E2 =>
								exe_state <= E0;
								cpu_state <= T0;
								
							when others => null;
						end case;

----------------JMP
                when "01110011" =>
                	case exe_state is
                		when E0 =>
                			START_RD_RAM(x82,'1'); --read DPL
                			exe_state <= E1;
                		when E1 =>
                			alu_src_1L <= i_ram_doByte; -- put DPL in lower byte of ALU source1
                			i_ram_addr <= x83; --read DPH
                			exe_state <= E2;
                		when E2 =>
                			alu_src_1H <= i_ram_doByte; -- put DPH into higher byte of ALU source1
                			alu_src_2L <= ACC; -- put ACC into lower byte of ALU source2
                			alu_src_2H <= "00000000";
                			alu_op_code <= ALU_OPC_ADD;
                			alu_by_wd <= WORD;
                			exe_state <= E3;

                		when E3 =>
                			PC(7 downto 0) <= alu_ans_L;
                			PC(15 downto 8) <= alu_ans_H;
                			exe_state <= E4;

                		when E4 =>
                			STOP_RD_WR_RAM;
                			SHUT_DOWN_ALU;
                			exe_state <= E0;
                			cpu_state <= T0;
                		when others => null;
                	end case;
-------------------------------------------------------------------------------------------------------------------------
----------------INC
-----------------------INC A
					when "00000100" =>
						case exe_state is
							when E0 =>
								START_RD_RAM(xE0,'1'); --read ACC
								exe_state <= E1;
								
							when E1 =>
								alu_src_1L <= i_ram_doByte; --load ACC contents in src 1
								--alu_src_1H <= "00000000";
								--alu_src_2L <= "00000001";
								--alu_src_2H <= "00000000";
								alu_op_code <= ALU_OPC_INC;
								alu_by_wd <= BYTE;
								exe_state <= E2;
								
							when E2 =>
							START_WR_RAM(xE0);
								i_ram_diByte <= alu_ans_L;

								exe_state <= E3;
								
							when E3 =>
								STOP_RD_WR_RAM;
                			SHUT_DOWN_ALU;
                			exe_state <= E0;
                			cpu_state <= T0;
								
                		    when others => null;
						end case;		
----------------------------------------------------------------------------------------------------------------------------------------
--------------------------INC Rn
					when "00001---" =>
						case exe_state is
							when E0 =>
								START_RD_RAM("000" & reg_rs1 & reg_rs0 & IR_1(2 downto 0),'1'); -- fetch address of Rn into RAM
								exe_state <= E1;
								
							when E1 =>
								alu_src_1L <= i_Ram_doByte;
								alu_op_code <= ALU_OPC_INC;
								alu_by_wd <= BYTE;
								exe_state <= E2;
								
							when E2 =>
								START_WR_RAM ("000" & reg_rs1 & reg_rs0 & IR_1(2 downto 0)); --Write inc value to Reg
								i_Ram_diByte <= alu_ans_L;
								exe_state <= E3;
							
							when E3 =>
								STOP_RD_WR_RAM;
                			SHUT_DOWN_ALU;
                			exe_state <= E0;
                			cpu_state <= T0;
								
							when others => null;
						end case;
--------------------------------------------------------------------------------------------------------------------
---------------------------- INC direct
					when "00000101" => 
						case exe_state is
							when E0 =>
								PC <= PC+1; --2 byte instruction
								START_RD_RAM (IR_2,'1');
								exe_state <= E1;
								
							when E1 =>
								alu_src_1L <= i_ram_doByte;
								alu_op_code <= ALU_OPC_INC;
								alu_by_wd <= BYTE;
								exe_state <= E2;
								
							when E2 =>
								START_WR_RAM (IR_2);
								i_ram_diByte <= alu_ans_L;
								exe_state <= E3;
								
							when E3 =>
								STOP_RD_WR_RAM;
                			SHUT_DOWN_ALU;
                			exe_state <= E0;
                			cpu_state <= T0;
								
							when others => null;
						end case;
--------------------------------------------------------------------------------------------------------------------------
------------------- INC @Ri
					when "0000011-" =>
						case exe_state is 
							when E0 =>
								
								START_RD_RAM("000" & reg_rs1 & reg_rs0 & IR_1(2 downto 0),'1');
								exe_state <= E1;
								
							when E1 =>
								temp_sig8 <= i_ram_doByte;
								START_RD_RAM(temp_sig8,'1'); --faster if replace with ram_addr
								exe_state <= E2;
								
							when E2 =>
								alu_src_1L <= i_ram_doByte;
								alu_op_code <= ALU_OPC_INC;
								alu_by_wd <= BYTE;
								exe_state <= E3;
								
							when E3 =>
								START_WR_RAM (temp_sig8);
								i_ram_diByte <= alu_ans_L;
								exe_state <= E4;
								
							when E4=>
								STOP_RD_WR_RAM;
                			SHUT_DOWN_ALU;
                			exe_state <= E0;
                			cpu_state <= T0;
								
							when others  => null;
						end case;
--------------------------------------------------------------------------------------------------------------------------
				when others => 				
					exe_state <= E0;	
					cpu_state <= T0;
			end case;
		when others => 
			cpu_state <= T0;
	end case; --cpu_state
	
end if;
end process;
end seq_arch;

-------------------------------------------------------------------------------

-- end of file --
