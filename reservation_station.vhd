-- ----------------------------------------------------------------------------
-- reservation_station.vhd --  reservation_station of is stage
-- Original Zhe Li, Master thesis, IMS, Uni Hannover, 2020
-- ----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library core_superscalar_mips;
use core_superscalar_mips.superscalar_mips_package.all;

entity reservation_station is
generic (
      ROB_log2_Size : natural := ROB_log2_Size_c;
      RS_DEPTH : natural := RS_DEPTH_c;
      RS_log2_Size : natural := RS_log2_Size_c
    );
   port (
      clock                    	: in std_ulogic;
      reset                    	: in std_ulogic;
	  clk_en                   	: in std_ulogic;
    -- access --
    -- input        
      inst1_rega_value_in     	: in std_ulogic_vector(31 downto 0);   
      inst1_regb_value_in      	: in std_ulogic_vector(31 downto 0);   
	  inst1_imm_value_in        : in std_ulogic_vector(16 downto 0);   
      inst1_rega_tag_in        	: in std_ulogic_vector(ROB_log2_Size downto 0);    --from RAT
      inst1_regb_tag_in        	: in std_ulogic_vector(ROB_log2_Size downto 0);    --from RAT
      inst1_dest_tag_in        	: in std_ulogic_vector(ROB_log2_Size downto 0);    --from ROB
	  inst1_dest_reg_in        	: in std_ulogic_vector(04 downto 0);   
	  inst1_Movn_in           	: in std_ulogic; 
	  inst1_Movz_in            	: in std_ulogic; 	  
      inst1_ALUOp_in           	: in std_ulogic_vector(04 downto 0);    
--	  inst1_alu_shamt_in       	: in std_ulogic_vector(04 downto 0);    
	  inst1_Regwrite_in         : in std_ulogic; 
	  inst1_Link_in 			: in std_ulogic; 
	  inst1_ALUSrcImm_in		: in std_ulogic; 
	  inst1_LLSC_in				: in std_ulogic; 
	  inst1_Memread_in			: in std_ulogic; 
	  inst1_Memwrite_in			: in std_ulogic; 
	  inst1_Membyte_in			: in std_ulogic; 
	  inst1_Memhalf_in			: in std_ulogic; 
	  inst1_MemSignExtend_in	: in std_ulogic; 
	  inst1_Left_in				: in std_ulogic; 
	  inst1_Right_in			: in std_ulogic; 
	  inst1_MemtoReg_in			: in std_ulogic;  
	  inst1_RestartPC_in		: in std_ulogic_vector(31 downto 0);
	  inst1_IsBDS_in			: in std_ulogic;
	  inst1_Trap_in				: in std_ulogic; 
	  inst1_TrapCond_in 		: in std_ulogic; 
	  inst1_EX_CanErr_in		: in std_ulogic; 
	  inst1_M_CanErr_in			: in std_ulogic; 
	  inst1_valid_in           	: in std_ulogic;                       
	  
	  inst2_rega_value_in      	: in std_ulogic_vector(31 downto 0);    
      inst2_regb_value_in      	: in std_ulogic_vector(31 downto 0);    
	  inst2_imm_value_in        : in std_ulogic_vector(16 downto 0);    
      inst2_rega_tag_in        	: in std_ulogic_vector(ROB_log2_Size downto 0);    -- RAT
      inst2_regb_tag_in       	: in std_ulogic_vector(ROB_log2_Size downto 0);    -- RAT
      inst2_dest_tag_in        	: in std_ulogic_vector(ROB_log2_Size downto 0);    -- ROB
	  inst2_dest_reg_in        	: in std_ulogic_vector(04 downto 0);    
	  inst2_Movn_in            	: in std_ulogic;
	  inst2_Movz_in            	: in std_ulogic; 
      inst2_ALUOp_in           	: in std_ulogic_vector(04 downto 0);    
--	  inst2_alu_shamt_in       	: in std_ulogic_vector(04 downto 0);  
	  inst2_Regwrite_in         : in std_ulogic; 
	  inst2_Link_in 			: in std_ulogic; 
	  inst2_ALUSrcImm_in		: in std_ulogic; 
	  inst2_LLSC_in				: in std_ulogic; 
	  inst2_Memread_in			: in std_ulogic; 
	  inst2_Memwrite_in			: in std_ulogic; 
	  inst2_Membyte_in			: in std_ulogic; 
	  inst2_Memhalf_in			: in std_ulogic; 
	  inst2_MemSignExtend_in	: in std_ulogic; 
	  inst2_Left_in				: in std_ulogic; 
	  inst2_Right_in			: in std_ulogic; 
	  inst2_MemtoReg_in			: in std_ulogic; 
	  inst2_RestartPC_in		: in std_ulogic_vector(31 downto 0);
	  inst2_IsBDS_in			: in std_ulogic;	  
	  inst2_Trap_in				: in std_ulogic; 
	  inst2_TrapCond_in 		: in std_ulogic; 
	  inst2_EX_CanErr_in		: in std_ulogic; 
	  inst2_M_CanErr_in			: in std_ulogic; 
	  inst2_valid_in           	: in std_ulogic;                       
    -- CDB       
      cdb1_tag_in              	: in std_ulogic_vector(ROB_log2_Size downto 0);    --from CDB
      cdb1_value_in            	: in std_ulogic_vector(31 downto 0);   --from CDB
	  cdb2_tag_in              	: in std_ulogic_vector(ROB_log2_Size downto 0);    --from CDB
      cdb2_value_in            	: in std_ulogic_vector(31 downto 0);   --from CDB
	  cdb2_regwrite_in     	    : in std_ulogic;
    -- EXCEPTION	  
	  IS_Stall					: in std_ulogic;
	  EX_Stall                  : in std_ulogic;
	  flush_exc                	: in std_ulogic;
	  flush_tag_in             	: in std_ulogic_vector(ROB_log2_Size downto 0);
	  ROB_head_tag_in          	: in std_ulogic_vector(ROB_log2_Size downto 0);      --from ROB
    -- outPUT      
      inst1_rega_value_out    	: out std_ulogic_vector(31 downto 0);    --to EX
      inst1_regb_value_out     	: out std_ulogic_vector(31 downto 0);    --to EX
	  inst1_imm_value_out       : out std_ulogic_vector(31 downto 0);
	  inst1_dest_reg_out       	: out std_ulogic_vector(04 downto 0);    --to EX
      inst1_dest_tag_out       	: out std_ulogic_vector(ROB_log2_Size downto 0);    --to EX
	  inst1_Movn_out       	   	: out std_ulogic;
	  inst1_Movz_out       	   	: out std_ulogic; 	  
      inst1_ALUOp_out          	: out std_ulogic_vector(04 downto 0);    --to EX
	  inst1_alu_shamt_out      	: out std_ulogic_vector(04 downto 0); 
	  inst1_Regwrite_out        : out std_ulogic; 
	  inst1_link_out 			: out std_ulogic; 
	  inst1_ALUSrcImm_out		: out std_ulogic; 
	  inst1_LLSC_out			: out std_ulogic; 
	  inst1_Memread_out			: out std_ulogic; 
	  inst1_Memwrite_out		: out std_ulogic; 
	  inst1_Membyte_out			: out std_ulogic; 
	  inst1_Memhalf_out			: out std_ulogic; 
	  inst1_MemSignExtend_out	: out std_ulogic; 
	  inst1_Left_out			: out std_ulogic; 
	  inst1_Right_out			: out std_ulogic; 
	  inst1_MemtoReg_out		: out std_ulogic; 
	  inst1_RestartPC_out		: out std_ulogic_vector(31 downto 0);
	  inst1_IsBDS_out			: out std_ulogic;	  
	  inst1_Trap_out			: out std_ulogic; 
	  inst1_TrapCond_out 		: out std_ulogic; 
	  inst1_EX_CanErr_out		: out std_ulogic; 
	  inst1_M_CanErr_out		: out std_ulogic; 
      RtRds                     : out RS_array_register;
	  Movs                      : out std_ulogic_vector(RS_DEPTH-1 downto 0);
	  RS_empty_o                : out std_ulogic_vector(RS_log2_Size downto 0);
	  RS_Full                  	: out std_ulogic;
	  EX_valid_o           		: out std_ulogic
   );
end reservation_station;

architecture rtl of reservation_station is
   component reservation_station_entry is
      port (
	  clock                    : in std_ulogic;
      reset                    : in std_ulogic;
	  clk_en      			   : in std_ulogic;
	  clear                    : in std_ulogic;
      fill                     : in std_ulogic;
      inst_inorder             : in std_ulogic;
	  inorder             	   : in std_ulogic;
      rega_value_in            : in std_ulogic_vector(31 downto 0);
      regb_value_in            : in std_ulogic_vector(31 downto 0);
      imm_value_in             : in std_ulogic_vector(16 downto 0);
      dest_reg_in              : in std_ulogic_vector(04 downto 0);
      dest_tag_in              : in std_ulogic_vector(ROB_log2_Size downto 0);
      waiting_taga_in          : in std_ulogic_vector(ROB_log2_Size downto 0);
      waiting_tagb_in          : in std_ulogic_vector(ROB_log2_Size downto 0);
      ALUOp_in                 : in std_ulogic_vector(04 downto 0);
--	  alu_shamt_in             : in std_ulogic_vector(04 downto 0);
      Movn_in              	   : in std_ulogic;
      Movz_in              	   : in std_ulogic;
      Regwrite_in              : in std_ulogic; 
      Link_in 				   : in std_ulogic; 
      ALUSrcImm_in			   : in std_ulogic; 
      LLSC_in				   : in std_ulogic; 
      Memread_in			   : in std_ulogic; 
      Memwrite_in			   : in std_ulogic; 
      Membyte_in			   : in std_ulogic; 
      Memhalf_in			   : in std_ulogic; 
      MemSignExtend_in		   : in std_ulogic; 
      Left_in				   : in std_ulogic; 
      Right_in				   : in std_ulogic; 
      MemtoReg_in			   : in std_ulogic; 
      RestartPC_in			   : in std_ulogic_vector(31 downto 0);
      IsBDS_in				   : in std_ulogic;	  
      Trap_in				   : in std_ulogic; 
      TrapCond_in 			   : in std_ulogic; 
      EX_CanErr_in			   : in std_ulogic; 
      M_CanErr_in			   : in std_ulogic; 

      cdb1_tag_in              : in std_ulogic_vector(ROB_log2_Size downto 0);
      cdb1_value_in            : in std_ulogic_vector(31 downto 0);
      cdb2_tag_in              : in std_ulogic_vector(ROB_log2_Size downto 0);
      cdb2_value_in            : in std_ulogic_vector(31 downto 0);
	  cdb2_regwrite_in     	   : in std_ulogic;
	  IS_Stall				   : in std_ulogic;
	  EX_Stall                 : in std_ulogic;
      flush_exc                : in std_ulogic;
      flush_tag_in             : in std_ulogic_vector(ROB_log2_Size downto 0);
      ROB_head_tag_in          : in std_ulogic_vector(ROB_log2_Size downto 0); 

      dest_tag_out             : out std_ulogic_vector(ROB_log2_Size downto 0);
	  dest_reg_out             : out std_ulogic_vector(04 downto 0);
      rega_value_out           : out std_ulogic_vector(31 downto 0);
      regb_value_out           : out std_ulogic_vector(31 downto 0);
	  imm_value_out            : out std_ulogic_vector(16 downto 0);
      ALUOp_out                : out std_ulogic_vector(04 downto 0);
--	  alu_shamt_out            : out std_ulogic_vector(04 downto 0);
      Movn_out                 : out std_ulogic;
      Movz_out             	   : out std_ulogic;
      link_out 				   : out std_ulogic;
      Regwrite_out             : out std_ulogic;
      ALUSrcImm_out			   : out std_ulogic; 
      LLSC_out				   : out std_ulogic; 
      Memread_out			   : out std_ulogic; 
      Memwrite_out			   : out std_ulogic; 
      Membyte_out			   : out std_ulogic; 
      Memhalf_out			   : out std_ulogic; 
      MemSignExtend_out		   : out std_ulogic; 
      Left_out				   : out std_ulogic; 
      Right_out				   : out std_ulogic; 
      MemtoReg_out			   : out std_ulogic; 
      RestartPC_out			   : out std_ulogic_vector(31 downto 0);
      IsBDS_out				   : out std_ulogic;	  
      Trap_out				   : out std_ulogic; 
      TrapCond_out 			   : out std_ulogic; 
      EX_CanErr_out			   : out std_ulogic; 
      M_CanErr_out			   : out std_ulogic; 
	  inorder_out              : out std_ulogic; 
      counter            : out std_ulogic_vector(RS_log2_Size+1 downto 0);
--      counter                  : out std_ulogic_vector(RS_log2_Size downto 0);
      status_out               : out std_ulogic_vector(02 downto 0);
      RtRd_out                 : out std_ulogic_vector(04 downto 0)   
   );
   end component;
   
   type type_status is ARRAY (RS_DEPTH downto 0) of std_ulogic_vector(02 downto 0);
   type type_register is ARRAY (RS_DEPTH-1 downto 0) of std_ulogic_vector(04 downto 0);
   type type_imm is ARRAY (RS_DEPTH-1 downto 0) of std_ulogic_vector(16 downto 0);
   type type_value is ARRAY (RS_DEPTH-1 downto 0) of std_ulogic_vector(31 downto 0);
   type type_tag is ARRAY (RS_DEPTH-1 downto 0) of std_ulogic_vector(ROB_log2_Size downto 0);
   type type_counter is ARRAY (RS_DEPTH-1 downto 0) of std_ulogic_vector(RS_log2_Size+1 downto 0);
   
   signal clears_wire      	            : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal dest_regs_in                  : type_register;
   signal dest_tags_in                  : type_tag;
   signal rega_values_in           		: type_value;
   signal regb_values_in           		: type_value;
   signal waiting_tagas_in         		: type_tag;
   signal waiting_tagbs_in         	 	: type_tag;
   signal ALUOps_in             		: type_register;
   signal Imms_in                       : type_imm;
--   signal alu_shamts_in                 : type_register;
   signal Movns_in               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Movzs_in               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Regwrites_in                  : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Links_in                      : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal ALUSrcImms_in               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal LLSCs_in                      : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Memreads_in               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Memwrites_in               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Membytes_in                   : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Memhalfs_in               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal MemSignExtends_in             : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Lefts_in                      : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Rights_in               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal MemtoRegs_in               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal RestartPCs_in                 : type_value;
   signal IsBDSs_in               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Traps_in               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal TrapConds_in            		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal EX_CanErrs_in               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal M_CanErrs_in             		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal inorders                      : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal inst_inorders                 : std_ulogic_vector(RS_DEPTH-1 downto 0);
--   signal order_counters_out            : type_inorder_out;
   signal statuses                 		: type_status;
   signal dest_regs_out            	 	: type_register;
   signal RtRds_out                     : type_register;
   signal dest_tags_out            		: type_tag;
   signal rega_values_out          		: type_value;
   signal regb_values_out           	: type_value;
   signal ALUOps_out            		: type_register;
   signal Imms_out                      : type_imm;
   signal counters                      : type_counter;
--   signal alu_shamts_out                : type_register;
   signal Movns_out        		   		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Movzs_out        		   		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Regwrites_out                 : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Links_out                     : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal ALUSrcImms_out               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal LLSCs_out                     : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Memreads_out               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Memwrites_out               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Membytes_out                  : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Memhalfs_out               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal MemSignExtends_out            : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Lefts_out                     : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Rights_out               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal MemtoRegs_out               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal RestartPCs_out                : type_value;
   signal IsBDSs_out               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal Traps_out               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal TrapConds_out            		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal EX_CanErrs_out               	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal M_CanErrs_out             	: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal inorders_out                  : std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal fills_wire               		: std_ulogic_vector(RS_DEPTH-1 downto 0);
   signal inst1_inorder, inst2_inorder  : std_ulogic;
   signal inst1_imm_value               : std_ulogic_vector(16 downto 0);
   signal empty_num                    	: integer range 0 to RS_DEPTH;
   signal first_empty                   : integer range 0 to RS_DEPTH-1;
   signal last_empty                    : integer range 0 to RS_DEPTH-1;
   signal ready_num                    	: integer range 0 to RS_DEPTH;
   signal MAX                        	: integer range 0 to RS_DEPTH-1;
   signal Order_MAX                     : integer range 0 to RS_DEPTH-1;
   signal MAX_End                       : integer range 0 to RS_DEPTH; 
   signal counter1                      : integer range 0 to 4*RS_DEPTH-1; 
   signal counter2                      : integer range 0 to 4*RS_DEPTH-1;  
   
begin  
	-- First determine whether the RS is full, and find two empty entries, 
	-- and store the information contained in the input intructions in the empty entry to wait.
	--	If the RS is full, the IF ID stall will wait for enough empty entries.
    empty_ready:process(statuses)
	variable empty_num_pre       	   : integer;
	variable ready_num_pre       	   : integer;
	variable first_empty_pre       	   : integer;
	variable last_empty_pre       	   : integer;
	begin
		first_empty_pre := 0;
		last_empty_pre := 0;
		empty_num_pre :=0;
		ready_num_pre :=0;
		for i in RS_DEPTH-1 downto 0 loop
			if (statuses(i) = "000") then
				empty_num_pre := empty_num_pre + 1;
				first_empty_pre   := i;
			end if;
			if (statuses(i)(2) = '1') then
				ready_num_pre := ready_num_pre + 1;
			end if;
		end loop;
		for i in 0 TO RS_DEPTH-1 loop
			if (statuses(i) = "000") then
				last_empty_pre   := i;
			end if;
		end loop;
		empty_num <= empty_num_pre;
		ready_num <= ready_num_pre;
		first_empty   <= first_empty_pre;
		last_empty   <= last_empty_pre;
	end process;

	RS_empty_o <= std_ulogic_vector(to_unsigned(empty_num, RS_log2_Size+1));
	RS_Full <= '1' when ((empty_num <= 3 And inst1_valid_in = '1' And inst2_valid_in = '1') or
				   (empty_num <= 2 and (inst1_valid_in = '1' or inst2_valid_in = '1')) or empty_num <= 1) else
			   	'0';
	-- some instructions should execute in order,like store and HILO instructions--
	inst1_inorder <= '1' when (inst1_ALUOp_in = "00101" or inst1_ALUOp_in = "00110" or inst1_ALUOp_in = "00111" or inst1_ALUOp_in = "10001" or inst1_Memwrite_in = '1' or
								inst1_ALUOp_in = "01000" or inst1_ALUOp_in = "01001" or inst1_ALUOp_in = "01010" or inst1_ALUOp_in = "01101" or 
								inst1_ALUOp_in = "01110" or inst1_ALUOp_in = "01011" or inst1_ALUOp_in = "01100" or inst1_ALUOp_in = "10000" ) else 
					 '0';
	inst2_inorder <= '1' when (inst2_ALUOp_in = "00101" or inst2_ALUOp_in = "00110" or inst2_ALUOp_in = "00111" or inst2_ALUOp_in = "10001" or inst2_Memwrite_in = '1' or
								inst2_ALUOp_in = "01000" or inst2_ALUOp_in = "01001" or inst2_ALUOp_in = "01010" or inst2_ALUOp_in = "01101" or 
								inst2_ALUOp_in = "01110" or inst2_ALUOp_in = "01011" or inst2_ALUOp_in = "01100" or inst2_ALUOp_in = "10000" ) else 
					 '0';	
	-- write in --	
	assigninputs: for i in 0 TO RS_DEPTH-1 generate
	process (empty_num, first_empty, last_empty, 
	inst1_imm_value_in, inst1_dest_reg_in, inst1_ALUOp_in, inst1_Movz_in, inst1_Regwrite_in, inst1_Link_in, inst1_ALUSrcImm_in, inst1_LLSC_in, inst1_Memread_in,
	inst1_Memwrite_in, inst1_Membyte_in, inst1_Memhalf_in, inst1_MemSignExtend_in, inst1_Left_in, inst1_Right_in, inst1_MemtoReg_in,
	inst1_RestartPC_in, inst1_IsBDS_in, inst1_Trap_in, inst1_TrapCond_in, inst1_EX_CanErr_in, inst1_M_CanErr_in, inst1_valid_in, inst2_valid_in, 
	inst1_rega_tag_in, inst1_rega_value_in, inst1_regb_tag_in, inst1_regb_value_in, inst1_dest_tag_in, inst1_Movn_in, inst2_rega_tag_in, inst2_regb_tag_in,inst2_imm_value_in, inst2_Movn_in,
	inst2_dest_reg_in, inst2_ALUOp_in, inst2_Movz_in, inst2_Regwrite_in, inst2_Link_in, inst2_ALUSrcImm_in, inst2_LLSC_in, inst2_Memread_in, inst2_dest_tag_in,
	inst2_Memwrite_in, inst2_Membyte_in, inst2_Memhalf_in, inst2_MemSignExtend_in, inst2_Left_in, inst2_Right_in, inst2_MemtoReg_in,
	inst2_RestartPC_in, inst2_IsBDS_in, inst2_Trap_in, inst2_TrapCond_in, inst2_EX_CanErr_in, inst2_M_CanErr_in, inst2_rega_value_in, inst2_regb_value_in,
	MAX_End, statuses, inst1_inorder, inst2_inorder)
    begin

			if (inst1_valid_in = '1' and inst2_valid_in = '1' and empty_num > 1) then
				if (i = first_empty) then
					rega_values_in(i) <= inst1_rega_value_in;          
					regb_values_in(i) <= inst1_regb_value_in;
					waiting_tagas_in(i)  <= inst1_rega_tag_in;
					waiting_tagbs_in(i)  <= inst1_regb_tag_in;
					Imms_in(i)           <= inst1_imm_value_in;
					dest_tags_in(i) 	 <= inst1_dest_tag_in;
					dest_regs_in(i)		 <= inst1_dest_reg_in;
					ALUOps_in(i)		 <= inst1_ALUOp_in;
--					alu_shamts_in(i)	 <= inst1_alu_shamt_in;
					Movns_in(i) 		 <= inst1_Movn_in;
					Movzs_in(i) 		 <= inst1_Movz_in;
					Regwrites_in(i) 	 <= inst1_Regwrite_in;
					Links_in(i)    		 <= inst1_Link_in;
					ALUSrcImms_in(i)  	 <= inst1_ALUSrcImm_in;              	
					LLSCs_in(i)          <= inst1_LLSC_in;
					Memreads_in(i)    	 <= inst1_Memread_in;          	
					Memwrites_in(i)   	 <= inst1_Memwrite_in;          	
					Membytes_in(i)    	 <= inst1_Membyte_in;             
					Memhalfs_in(i)    	 <= inst1_Memhalf_in;         	
					MemSignExtends_in(i) <= inst1_MemSignExtend_in;            
					Lefts_in(i)       	 <= inst1_Left_in;            
					Rights_in(i)      	 <= inst1_Right_in;       		
					MemtoRegs_in(i)   	 <= inst1_MemtoReg_in;          	                         	
					RestartPCs_in(i)  	 <= inst1_RestartPC_in;             
					IsBDSs_in(i)      	 <= inst1_IsBDS_in;      		
					Traps_in(i)       	 <= inst1_Trap_in;       		
					TrapConds_in(i)   	 <= inst1_TrapCond_in;       		
					EX_CanErrs_in(i)  	 <= inst1_EX_CanErr_in;          	
					M_CanErrs_in(i)   	 <= inst1_M_CanErr_in;       
					inst_inorders(i)	 <= inst1_inorder;			
					fills_wire(i)   	 <= '1';
					inorders(i)          <= '1';
				elsif (i = last_empty) then
					rega_values_in(i) <= inst2_rega_value_in;          
					regb_values_in(i) <= inst2_regb_value_in;
					waiting_tagas_in(i)  <= inst2_rega_tag_in;
					waiting_tagbs_in(i)  <= inst2_regb_tag_in;
					dest_tags_in(i)	 	 <= inst2_dest_tag_in;
					Imms_in(i)           <= inst2_imm_value_in;
					dest_regs_in(i)		 <= inst2_dest_reg_in;
					ALUOps_in(i)	 	 <= inst2_ALUOp_in;
--					alu_shamts_in(i)	 <= inst2_alu_shamt_in;
					Movns_in(i) 		 <= inst2_Movn_in;
					Movzs_in(i) 		 <= inst2_Movz_in;
					Regwrites_in(i)		 <= inst2_Regwrite_in;
					Links_in(i)    		 <= inst2_Link_in;
					ALUSrcImms_in(i)     <= inst2_ALUSrcImm_in;              	
					LLSCs_in(i)       	 <= inst2_LLSC_in;
					Memreads_in(i)    	 <= inst2_Memread_in;          	
					Memwrites_in(i)   	 <= inst2_Memwrite_in;          	
					Membytes_in(i)    	 <= inst2_Membyte_in;             
					Memhalfs_in(i)    	 <= inst2_Memhalf_in;         	
					MemSignExtends_in(i) <= inst2_MemSignExtend_in;            
					Lefts_in(i)       	 <= inst2_Left_in;            
					Rights_in(i)      	 <= inst2_Right_in;       		
					MemtoRegs_in(i)   	 <= inst2_MemtoReg_in;          	         	                     	
					RestartPCs_in(i)  	 <= inst2_RestartPC_in;             
					IsBDSs_in(i)      	 <= inst2_IsBDS_in;         		
					Traps_in(i)       	 <= inst2_Trap_in;       		
					TrapConds_in(i)   	 <= inst2_TrapCond_in;       		
					EX_CanErrs_in(i)  	 <= inst2_EX_CanErr_in;           	
					M_CanErrs_in(i)   	 <= inst2_M_CanErr_in;   
					inst_inorders(i)	 <= inst2_inorder;						
					fills_wire(i)   	 <= '1';
					inorders(i)          <= '0';
				else  
					dest_tags_in(i)		 <= (others=> '1');
					dest_regs_in(i)		 <= "00000";
					rega_values_in(i) 	 <= (others => '0');
					regb_values_in(i)	 <= (others => '0');
					waiting_tagas_in(i)	 <= (others=> '1');
					waiting_tagbs_in(i)	 <= (others=> '1');
					Imms_in(i)         	 <= (others => '0');
					ALUOps_in(i) 		 <= "00000";
--					alu_shamts_in(i)	 <= "00000";
					Movns_in(i)			 <= '0';
					Movzs_in(i)			 <= '0';
					Regwrites_in(i)		 <= '0';
					Links_in(i)    		 <= '0';
					ALUSrcImms_in(i)  	 <= '0';              	
					LLSCs_in(i)       	 <= '0';
					Memreads_in(i)    	 <= '0';          	
					Memwrites_in(i)   	 <= '0';          	
					Membytes_in(i)    	 <= '0';             
					Memhalfs_in(i)    	 <= '0';         	
					MemSignExtends_in(i) <= '0';            
					Lefts_in(i)       	 <= '0';            
					Rights_in(i)      	 <= '0';       		
					MemtoRegs_in(i)   	 <= '0';          	      		                   	
					RestartPCs_in(i)  	 <= (others => '0');         
					IsBDSs_in(i)      	 <= '0'; 
					Traps_in(i)       	 <= '0';       		
					TrapConds_in(i)   	 <= '0';       		
					EX_CanErrs_in(i)  	 <= '0';           	
					M_CanErrs_in(i)   	 <= '0';   
					fills_wire(i) 		 <= '0';
					inst_inorders(i)	 <= '0';	
					inorders(i)        	 <= '0';
				end if;
			elsif (inst1_valid_in = '1' and inst2_valid_in = '0' and empty_num > 0) then
				if (i = first_empty) then
					rega_values_in(i) <= inst1_rega_value_in;          
					regb_values_in(i) <= inst1_regb_value_in;
					waiting_tagas_in(i)	 <= inst1_rega_tag_in;
					waiting_tagbs_in(i)	 <= inst1_regb_tag_in;
					dest_tags_in(i)		 <= inst1_dest_tag_in;
					Imms_in(i)           <= inst1_imm_value_in;
					dest_regs_in(i)		 <= inst1_dest_reg_in;
					ALUOps_in(i)		 <= inst1_ALUOp_in;
--					alu_shamts_in(i)	 <= inst1_alu_shamt_in;
					Movns_in(i)		     <= inst1_Movn_in;
					Movzs_in(i) 		 <= inst1_Movz_in;
					Regwrites_in(i)		 <= inst1_Regwrite_in;
					Links_in(i)    		 <= inst1_Link_in;
					ALUSrcImms_in(i)     <= inst1_ALUSrcImm_in;              	
					LLSCs_in(i)          <= inst1_LLSC_in;
					Memreads_in(i)       <= inst1_Memread_in;          	
					Memwrites_in(i)      <= inst1_Memwrite_in;          	
					Membytes_in(i)       <= inst1_Membyte_in;             
					Memhalfs_in(i)       <= inst1_Memhalf_in;         	
					MemSignExtends_in(i) <= inst1_MemSignExtend_in;            
					Lefts_in(i)       	 <= inst1_Left_in;            
					Rights_in(i)     	 <= inst1_Right_in;       		
					MemtoRegs_in(i)    	 <= inst1_MemtoReg_in;          	          	                    	
					RestartPCs_in(i)  	 <= inst1_RestartPC_in;             
					IsBDSs_in(i)      	 <= inst1_IsBDS_in; 
					Traps_in(i)       	 <= inst1_Trap_in;       		
					TrapConds_in(i)   	 <= inst1_TrapCond_in;       		
					EX_CanErrs_in(i)  	 <= inst1_EX_CanErr_in;           	
					M_CanErrs_in(i)   	 <= inst1_M_CanErr_in;
					inst_inorders(i)	 <= inst1_inorder;	
					fills_wire(i)    	 <= '1';
					inorders(i)          <= '1';
				else 
					dest_tags_in(i)		 <= (others=> '1');
					dest_regs_in(i)		 <= "00000";
					rega_values_in(i) 	 <= (others => '0');
					regb_values_in(i)	 <= (others => '0');
					waiting_tagas_in(i)	 <= (others=> '1');
					waiting_tagbs_in(i)	 <= (others=> '1');
					Imms_in(i)         	 <= (others => '0');
					ALUOps_in(i) 		 <= "00000";
--					alu_shamts_in(i)	 <= "00000";
					Movns_in(i)			 <= '0';
					Movzs_in(i)			 <= '0';
					Regwrites_in(i)		 <= '0';
					Links_in(i)    		 <= '0';
					ALUSrcImms_in(i)  	 <= '0';              	
					LLSCs_in(i)       	 <= '0';
					Memreads_in(i)    	 <= '0';          	
					Memwrites_in(i)   	 <= '0';          	
					Membytes_in(i)    	 <= '0';             
					Memhalfs_in(i)    	 <= '0';         	
					MemSignExtends_in(i) <= '0';            
					Lefts_in(i)       	 <= '0';            
					Rights_in(i)      	 <= '0';       		
					MemtoRegs_in(i)   	 <= '0';          	      		                   	
					RestartPCs_in(i)  	 <= (others => '0');         
					IsBDSs_in(i)      	 <= '0'; 
					Traps_in(i)       	 <= '0';       		
					TrapConds_in(i)   	 <= '0';       		
					EX_CanErrs_in(i)  	 <= '0';           	
					M_CanErrs_in(i)   	 <= '0';   
					fills_wire(i) 		 <= '0';
					inst_inorders(i)	 <= '0';	
					inorders(i)        	 <= '0';
				end if;
			elsif (inst1_valid_in = '0' and inst2_valid_in = '1' and empty_num > 0) then
				if (i = first_empty) then
					rega_values_in(i) <= inst2_rega_value_in;          
					regb_values_in(i) <= inst2_regb_value_in;
					waiting_tagas_in(i)  <= inst2_rega_tag_in;
					waiting_tagbs_in(i)  <= inst2_regb_tag_in;
					dest_tags_in(i)		 <= inst2_dest_tag_in;
					Imms_in(i)           <= inst2_imm_value_in;
					dest_regs_in(i)		 <= inst2_dest_reg_in;
					ALUOps_in(i)		 <= inst2_ALUOp_in;
--					alu_shamts_in(i)	 <= inst2_alu_shamt_in;
					Movns_in(i) 		 <= inst2_Movn_in;
					Movzs_in(i) 		 <= inst2_Movz_in;
					Regwrites_in(i)		 <= inst2_Regwrite_in;
					Links_in(i)    		 <= inst2_Link_in;
					ALUSrcImms_in(i)  	 <= inst2_ALUSrcImm_in;              	
					LLSCs_in(i)       	 <= inst2_LLSC_in;
					Memreads_in(i)    	 <= inst2_Memread_in;          	
					Memwrites_in(i)   	 <= inst2_Memwrite_in;          	
					Membytes_in(i)    	 <= inst2_Membyte_in;             
					Memhalfs_in(i)    	 <= inst2_Memhalf_in;         	
					MemSignExtends_in(i) <= inst2_MemSignExtend_in;            
					Lefts_in(i)       	 <= inst2_Left_in;            
					Rights_in(i)      	 <= inst2_Right_in;       		
					MemtoRegs_in(i)   	 <= inst2_MemtoReg_in;          	         		                    	
					RestartPCs_in(i)  	 <= inst2_RestartPC_in;             
					IsBDSs_in(i)      	 <= inst2_IsBDS_in; 
					Traps_in(i)       	 <= inst2_Trap_in;       		
					TrapConds_in(i)   	 <= inst2_TrapCond_in;       		
					EX_CanErrs_in(i)  	 <= inst2_EX_CanErr_in;           	
					M_CanErrs_in(i)   	 <= inst2_M_CanErr_in;
					inst_inorders(i)	 <= inst2_inorder;	
					fills_wire(i)    	 <= '1';
					inorders(i)          <= '1';
				else
					dest_tags_in(i)		 <= (others=> '1');
					dest_regs_in(i)		 <= "00000";
					rega_values_in(i) 	 <= (others => '0');
					regb_values_in(i)	 <= (others => '0');
					waiting_tagas_in(i)	 <= (others=> '1');
					waiting_tagbs_in(i)	 <= (others=> '1');
					Imms_in(i)         	 <= (others => '0');
					ALUOps_in(i) 		 <= "00000";
--					alu_shamts_in(i)	 <= "00000";
					Movns_in(i)			 <= '0';
					Movzs_in(i)			 <= '0';
					Regwrites_in(i)		 <= '0';
					Links_in(i)    		 <= '0';
					ALUSrcImms_in(i)  	 <= '0';              	
					LLSCs_in(i)       	 <= '0';
					Memreads_in(i)    	 <= '0';          	
					Memwrites_in(i)   	 <= '0';          	
					Membytes_in(i)    	 <= '0';             
					Memhalfs_in(i)    	 <= '0';         	
					MemSignExtends_in(i) <= '0';            
					Lefts_in(i)       	 <= '0';            
					Rights_in(i)      	 <= '0';       		
					MemtoRegs_in(i)   	 <= '0';          	      		                   	
					RestartPCs_in(i)  	 <= (others => '0');         
					IsBDSs_in(i)      	 <= '0'; 
					Traps_in(i)       	 <= '0';       		
					TrapConds_in(i)   	 <= '0';       		
					EX_CanErrs_in(i)  	 <= '0';           	
					M_CanErrs_in(i)   	 <= '0';   
					fills_wire(i) 		 <= '0';
					inst_inorders(i)	 <= '0';	
					inorders(i)        	 <= '0';
				end if;
			else		
				dest_tags_in(i)		 <= (others=> '1');
				dest_regs_in(i)		 <= "00000";
				rega_values_in(i) 	 <= (others => '0');
				regb_values_in(i)	 <= (others => '0');
				waiting_tagas_in(i)	 <= (others=> '1');
				waiting_tagbs_in(i)	 <= (others=> '1');
				Imms_in(i)         	 <= (others => '0');
				ALUOps_in(i) 		 <= "00000";
--				alu_shamts_in(i)	 <= "00000";
				Movns_in(i)			 <= '0';
				Movzs_in(i)			 <= '0';
				Regwrites_in(i)		 <= '0';
				Links_in(i)    		 <= '0';
				ALUSrcImms_in(i)  	 <= '0';              	
				LLSCs_in(i)       	 <= '0';
				Memreads_in(i)    	 <= '0';          	
				Memwrites_in(i)   	 <= '0';          	
				Membytes_in(i)    	 <= '0';             
				Memhalfs_in(i)    	 <= '0';         	
				MemSignExtends_in(i) <= '0';            
				Lefts_in(i)       	 <= '0';            
				Rights_in(i)      	 <= '0';       		
				MemtoRegs_in(i)   	 <= '0';          	      		                   	
				RestartPCs_in(i)  	 <= (others => '0');         
				IsBDSs_in(i)      	 <= '0'; 
				Traps_in(i)       	 <= '0';       		
				TrapConds_in(i)   	 <= '0';       		
				EX_CanErrs_in(i)  	 <= '0';           	
				M_CanErrs_in(i)   	 <= '0';   
				fills_wire(i) 		 <= '0';
				inst_inorders(i)	 <= '0';	
				inorders(i)        	 <= '0';
			end if;
			if (i = MAX_End and statuses(i)(2) = '1') then
				clears_wire(i) <= '1';
			else
				clears_wire(i) <= '0';
			end if;
    end process;
	end generate;


   RSEMODULES : for i in 0 TO RS_DEPTH-1 generate

      entries : reservation_station_entry
         port MAP (
            clock                    => clock,
            reset                    => reset,
			clk_en                   => clk_en,
			clear                    => clears_wire(i),
            fill                     => fills_wire(i),
            inst_inorder             => inst_inorders(i),
            inorder                  => inorders(i), 
            rega_value_in            => rega_values_in(i),
            regb_value_in            => regb_values_in(i),
			imm_value_in             => Imms_in(i),
            dest_tag_in              => dest_tags_in(i),
			dest_reg_in              => dest_regs_in(i),			
            waiting_taga_in          => waiting_tagas_in(i),
            waiting_tagb_in          => waiting_tagbs_in(i),
            ALUOp_in                 => ALUOps_in(i),
--            alu_shamt_in             => alu_shamts_in(i),
			Movn_in             	 => Movns_in(i),
			Movz_in             	 => Movzs_in(i),
			Regwrite_in              => Regwrites_in(i),
			Link_in                  => Links_in(i),
			ALUSrcImm_in   			 => ALUSrcImms_in(i),
			LLSC_in        			 => LLSCs_in(i),
			Memread_in     			 => Memreads_in(i),
			Memwrite_in    			 => Memwrites_in(i),
			Membyte_in    			 => Membytes_in(i),
			Memhalf_in    			 => Memhalfs_in(i),
			MemSignExtend_in		 => MemSignExtends_in(i),
			Left_in        			 => Lefts_in(i),
			Right_in       			 => Rights_in(i),
			MemtoReg_in    			 => MemtoRegs_in(i),
			RestartPC_in   			 => RestartPCs_in(i),
			IsBDS_in       			 => IsBDSs_in(i),
			Trap_in        			 => Traps_in(i),
			TrapCond_in    			 => TrapConds_in(i),
			EX_CanErr_in   			 => EX_CanErrs_in(i),
			M_CanErr_in   			 => M_CanErrs_in(i),
			
            cdb1_tag_in              => cdb1_tag_in,
            cdb1_value_in            => cdb1_value_in,
            cdb2_tag_in              => cdb2_tag_in,
            cdb2_value_in            => cdb2_value_in,
			cdb2_regwrite_in         => cdb2_regwrite_in,
			IS_Stall            	 => IS_Stall,
   			EX_Stall            	 => EX_Stall,
			flush_exc                => flush_exc,
			flush_tag_in             => flush_tag_in,
			ROB_head_tag_in          => ROB_head_tag_in,

            dest_tag_out             => dest_tags_out(i),
			dest_reg_out             => dest_regs_out(i),
            rega_value_out           => rega_values_out(i),
            regb_value_out           => regb_values_out(i),
			imm_value_out            => Imms_out(i),
            ALUOp_out                => ALUOps_out(i),
--            alu_shamt_out            => alu_shamts_out(i),
			Movn_out              	 => Movns_out(i),
			Movz_out              	 => Movzs_out(i),
			Regwrite_out             => Regwrites_out(i),
			Link_out                 => Links_out(i),
			ALUSrcImm_out   		 => ALUSrcImms_out(i),
			LLSC_out        		 => LLSCs_out(i),
			Memread_out     		 => Memreads_out(i),
			Memwrite_out    		 => Memwrites_out(i),
			Membyte_out    			 => Membytes_out(i),
			Memhalf_out    			 => Memhalfs_out(i),
			MemSignExtend_out		 => MemSignExtends_out(i),
			Left_out        		 => Lefts_out(i),
			Right_out       		 => Rights_out(i),
			MemtoReg_out    		 => MemtoRegs_out(i),
			RestartPC_out   		 => RestartPCs_out(i),
			IsBDS_out       		 => IsBDSs_out(i),
			Trap_out        		 => Traps_out(i),
			TrapCond_out    		 => TrapConds_out(i),
			EX_CanErr_out   		 => EX_CanErrs_out(i),
			M_CanErr_out   			 => M_CanErrs_out(i),
			inorder_out              => inorders_out(i),
--			order_counter            => order_counters_out(i),
			counter                  => counters(i),
            status_out               => statuses(i),
			RtRd_out                 => RtRds_out(i)
         );
   end generate;
    -- The longest waiting is sent out first 
	counters_compare: process(counters, inorders_out, statuses)
	variable MAX_pre1       	: integer := 0;
	variable MAX_counter1       : std_ulogic_vector(RS_log2_Size+1 downto 0);
	variable MAX_pre2       	: integer := 0;
	variable MAX_counter2 		: std_ulogic_vector(RS_log2_Size+1 downto 0);
	begin
		MAX_pre1 := 0;
		MAX_counter1 := (others => '0');
		MAX_pre2 := 0;
		MAX_counter2 := (others => '0');
		for i in 0 to RS_DEPTH-1 loop
			if (MAX_counter1 < counters(i) and inorders_out(i) = '0' and statuses(i)(2) = '1') then
				MAX_counter1 := counters(i);
				MAX_pre1 := i;
			else
			    MAX_counter1 := MAX_counter1;
				MAX_pre1 := MAX_pre1;
			end if;
			if (MAX_counter2 < counters(i) and inorders_out(i) = '1' ) then
				MAX_counter2 := counters(i);
				MAX_pre2 := i;
			else
			    MAX_counter2 := MAX_counter2;
				MAX_pre2 := MAX_pre2;
			end if;
		end loop;
		counter1 <= to_integer(unsigned(MAX_counter1));
		MAX <= MAX_pre1;	
		counter2 <= to_integer(unsigned(MAX_counter2));
		Order_MAX <= MAX_pre2;
	end process;

	MAX_End <= MAX when (counter1 >= counter2 or (counter1 < counter2 and statuses(Order_MAX)(2) = '0' and inorders_out(MAX) = '0')) else
			   Order_MAX when (counter1 < counter2 and statuses(Order_MAX)(2) = '1') else
			   RS_DEPTH;
   	-- read out --

	OUTPUT: process(MAX_End, statuses, rega_values_out, regb_values_out, Imms_out, dest_tags_out, dest_regs_out, Movns_out, Movzs_out, ALUOps_out, Regwrites_out, Links_out, ALUSrcImms_out, LLSCs_out, Memreads_out,
	Memwrites_out, Membytes_out, Memhalfs_out, MemSignExtends_out, Lefts_out, Rights_out, MemtoRegs_out, RestartPCs_out, IsBDSs_out, Traps_out, TrapConds_out, EX_CanErrs_out, M_CanErrs_out)
	begin
		if (statuses(MAX_End)(2) = '1' and MAX_End /= RS_DEPTH) then
			inst1_rega_value_out <= rega_values_out(MAX_End);
			inst1_regb_value_out <= regb_values_out(MAX_End);
			inst1_imm_value      <= Imms_out(MAX_End);
			inst1_dest_tag_out   <= dest_tags_out(MAX_End);
			inst1_dest_reg_out   <= dest_regs_out(MAX_End);
			inst1_Movn_out       <= Movns_out(MAX_End);
			inst1_Movz_out       <= Movzs_out(MAX_End);
			inst1_ALUOp_out      <= ALUOps_out(MAX_End);	
			inst1_Regwrite_out   <= Regwrites_out(MAX_End);
			inst1_Link_out       <= Links_out(MAX_End);
			inst1_ALUSrcImm_out  <= ALUSrcImms_out(MAX_End);
			inst1_LLSC_out       <= LLSCs_out(MAX_End);
			inst1_Memread_out    <= Memreads_out(MAX_End);
			inst1_Memwrite_out   <= Memwrites_out(MAX_End);
			inst1_Membyte_out    <= Membytes_out(MAX_End);
			inst1_Memhalf_out    <= Memhalfs_out(MAX_End);
			inst1_MemSignExtend_out <= MemSignExtends_out(MAX_End);
			inst1_Left_out       <= Lefts_out(MAX_End);
			inst1_Right_out      <= Rights_out(MAX_End);
			inst1_MemtoReg_out   <= MemtoRegs_out(MAX_End);
			inst1_RestartPC_out  <= RestartPCs_out(MAX_End);
			inst1_IsBDS_out      <= IsBDSs_out(MAX_End);
			inst1_Trap_out       <= Traps_out(MAX_End);
			inst1_TrapCond_out   <= TrapConds_out(MAX_End);
			inst1_EX_CanErr_out  <= EX_CanErrs_out(MAX_End);
			inst1_M_CanErr_out   <= M_CanErrs_out(MAX_End);
		else
			inst1_rega_value_out <= (others => '0');
			inst1_regb_value_out <= (others => '0');
			inst1_imm_value		 <= (others => '0');
			inst1_dest_tag_out   <= (others => '1');
			inst1_dest_reg_out   <= (others => '0');
			inst1_Movn_out       <= '0';
			inst1_Movz_out       <= '0';
			inst1_ALUOp_out      <= (others => '0');	
			inst1_Regwrite_out   <= '0';
			inst1_Link_out       <= '0';
			inst1_ALUSrcImm_out  <= '0';
			inst1_LLSC_out       <= '0';
			inst1_Memread_out    <= '0';
			inst1_Memwrite_out   <= '0';
			inst1_Membyte_out    <= '0';
			inst1_Memhalf_out    <= '0';
			inst1_MemSignExtend_out <= '0';
			inst1_Left_out       <= '0';
			inst1_Right_out      <= '0';
			inst1_MemtoReg_out   <= '0';
			inst1_RestartPC_out  <= (others => '0');
			inst1_IsBDS_out      <= '0';
			inst1_Trap_out       <= '0';
			inst1_TrapCond_out   <= '0';
			inst1_EX_CanErr_out  <= '0';
			inst1_M_CanErr_out   <= '0';
		end if;
	end process;
	
	RtRd : for i in 0 TO RS_DEPTH-1 generate
		RtRds(i) <=  RtRds_out(i);
		Movs(i)  <= Movns_out(i) or Movzs_out(i);
	end generate;
	inst1_imm_value_out <= ("111111111111111" & inst1_imm_value(16 downto 0)) when (inst1_imm_value(16) = '1') else ("000000000000000" & inst1_imm_value(16 downto 0));
	inst1_alu_shamt_out <= inst1_imm_value(10 downto 6);
	EX_valid_o <= '1' when (statuses(MAX_End)(2) = '1') else '0';
end rtl;

   
   
   
   
   
   
