-- ----------------------------------------------------------------------------
-- reservation_station_entry.vhd -  the entries of reservation station. It stores all 
-- instructions waiting to be executed. When the stored rs and rt have been updated, 
-- then they are ready to be sent to the ex stage. At the same time, it is equipped 
-- with a counter, which is ready and waits the longest to be sent first, similar to 
-- Least recently used (LRU). If it is not ready, then continue to wait until it is updated.
-- Each time CDB (commom data bus) sends two tags and the corresponding updated value, 
-- if the tag stored in RS matches it, the value will be overwritten and enter the ready state 
-- to wait for sending.
-- Original Zhe Li, Master thesis, IMS, Uni Hannover, 2020
-- ----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library core_superscalar_mips;
use core_superscalar_mips.superscalar_mips_package.all;

entity reservation_station_entry is
   generic (
      ROB_log2_Size : natural := ROB_log2_Size_c;
      ROB_DEPTH : natural := ROB_DEPTH_c;
      RS_DEPTH : natural := RS_DEPTH_c;
      RS_log2_Size : natural := RS_log2_Size_c
    );
    port (
	   clock                    : in std_ulogic;
      reset                    : in std_ulogic;
	   clk_en      			    : in std_ulogic;
      clear                    : in std_ulogic;
      fill                     : in std_ulogic;
      inst_inorder             : in std_ulogic;  
      inorder                  : in std_ulogic;
      rega_value_in            : in std_ulogic_vector(31 downto 0);
      regb_value_in            : in std_ulogic_vector(31 downto 0);
      imm_value_in             : in std_ulogic_vector(16 downto 0);
      dest_reg_in              : in std_ulogic_vector(04 downto 0);
      dest_tag_in              : in std_ulogic_vector(ROB_log2_Size downto 0);
      waiting_taga_in          : in std_ulogic_vector(ROB_log2_Size downto 0);
      waiting_tagb_in          : in std_ulogic_vector(ROB_log2_Size downto 0);
      ALUOp_in                 : in std_ulogic_vector(04 downto 0);
--	   alu_shamt_in             : in std_ulogic_vector(04 downto 0);
      Movn_in              	 : in std_ulogic;
      Movz_in              	 : in std_ulogic;
      Regwrite_in              : in std_ulogic; 
      Link_in 				       : in std_ulogic; 
      ALUSrcImm_in			    : in std_ulogic; 
      LLSC_in				       : in std_ulogic; 
      Memread_in			       : in std_ulogic; 
      Memwrite_in			       : in std_ulogic; 
      Membyte_in			       : in std_ulogic; 
      Memhalf_in			       : in std_ulogic; 
      MemSignExtend_in		    : in std_ulogic; 
      Left_in				       : in std_ulogic; 
      Right_in				       : in std_ulogic; 
      MemtoReg_in			       : in std_ulogic; 
      RestartPC_in			    : in std_ulogic_vector(31 downto 0);
      IsBDS_in				       : in std_ulogic;	  
      Trap_in				       : in std_ulogic; 
      TrapCond_in 			    : in std_ulogic; 
      EX_CanErr_in			    : in std_ulogic; 
      M_CanErr_in			       : in std_ulogic; 

      cdb1_tag_in              : in std_ulogic_vector(ROB_log2_Size downto 0);
      cdb1_value_in            : in std_ulogic_vector(31 downto 0);
      cdb2_tag_in              : in std_ulogic_vector(ROB_log2_Size downto 0);
      cdb2_value_in            : in std_ulogic_vector(31 downto 0);
      cdb2_regwrite_in     	 : in std_ulogic;
      IS_Stall					    : in std_ulogic;
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
--	   alu_shamt_out            : out std_ulogic_vector(04 downto 0);
      Movn_out                 : out std_ulogic;
      Movz_out             	 : out std_ulogic;
      link_out 				    : out std_ulogic;
      Regwrite_out             : out std_ulogic;
      ALUSrcImm_out			    : out std_ulogic; 
      LLSC_out				       : out std_ulogic; 
      Memread_out			       : out std_ulogic; 
      Memwrite_out			    : out std_ulogic; 
      Membyte_out			       : out std_ulogic; 
      Memhalf_out			       : out std_ulogic; 
      MemSignExtend_out		    : out std_ulogic; 
      Left_out				       : out std_ulogic; 
      Right_out				    : out std_ulogic; 
      MemtoReg_out			    : out std_ulogic; 
      RestartPC_out			    : out std_ulogic_vector(31 downto 0);
      IsBDS_out				    : out std_ulogic;	  
      Trap_out				       : out std_ulogic; 
      TrapCond_out 			    : out std_ulogic; 
      EX_CanErr_out			    : out std_ulogic; 
      M_CanErr_out			    : out std_ulogic; 
	   inorder_out              : out std_ulogic; 
      counter            : out std_ulogic_vector(RS_log2_Size+1 downto 0);
--      counter                  : out std_ulogic_vector(RS_log2_Size downto 0);
      status_out               : out std_ulogic_vector(02 downto 0);
      RtRd_out                 : out std_ulogic_vector(04 downto 0)   
   );
end reservation_station_entry;

architecture rtl of reservation_station_entry is
   signal n_dest_tag                : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_dest_reg                : std_ulogic_vector(04 downto 0);
   signal n_Imm_value               : std_ulogic_vector(16 downto 0);
   signal waiting_taga              : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_waiting_taga            : std_ulogic_vector(ROB_log2_Size downto 0);
   signal waiting_tagb              : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_waiting_tagb            : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_rega_value              : std_ulogic_vector(31 downto 0);
   signal n_regb_value              : std_ulogic_vector(31 downto 0);
   signal n_ALUOp             	   : std_ulogic_vector(04 downto 0);
--   signal n_alu_shamt           	   : std_ulogic_vector(04 downto 0);
   signal n_Movn             		   : std_ulogic;
   signal n_Movz             		   : std_ulogic;
   signal n_Regwrite             	: std_ulogic;
   signal n_Link             		   : std_ulogic;
   signal n_ALUSrcImm             	: std_ulogic;
   signal n_LLSC             	 	   : std_ulogic;
   signal n_Memread             	   : std_ulogic;
   signal n_Memwrite             	: std_ulogic;
   signal n_Membyte             	   : std_ulogic;
   signal n_Memhalf             	   : std_ulogic;
   signal n_MemSignExtend           : std_ulogic;
   signal n_Left             	 	   : std_ulogic;
   signal n_Right             		: std_ulogic;
   signal n_MemtoReg             	: std_ulogic;
   signal n_RestartPC				   : std_ulogic_vector(31 downto 0);
   signal n_IsBDS					      : std_ulogic; 
   signal n_Trap					      : std_ulogic; 
   signal n_TrapCond 				   : std_ulogic; 
   signal n_EX_CanErr				   : std_ulogic; 
   signal n_M_CanErr				      : std_ulogic; 
   signal n_inst_inorder				: std_ulogic;

   signal taga_in_nonnull           : std_ulogic;
   signal tagb_in_nonnull           : std_ulogic;
   signal taga_cur_nonnull          : std_ulogic;
   signal tagb_cur_nonnull          : std_ulogic;
   signal taga_in_match_cdb1_in     : std_ulogic;
   signal taga_in_match_cdb2_in     : std_ulogic;
   signal tagb_in_match_cdb1_in     : std_ulogic;
   signal tagb_in_match_cdb2_in     : std_ulogic;
   signal taga_cur_match_cdb1_cur   : std_ulogic;
   signal taga_cur_match_cdb2_cur   : std_ulogic;
   signal tagb_cur_match_cdb1_cur   : std_ulogic;
   signal tagb_cur_match_cdb2_cur   : std_ulogic;
   signal status_currently_empty    : std_ulogic;
   signal taga_cur_match_cdb1_in    : std_ulogic;
   signal taga_cur_match_cdb2_in    : std_ulogic;
   signal tagb_cur_match_cdb1_in    : std_ulogic;
   signal tagb_cur_match_cdb2_in    : std_ulogic;
  

   signal n_waiting_taga_pre        : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_waiting_tagb_pre        : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_rega_value_pre          : std_ulogic_vector(31 downto 0);
   signal n_regb_value_pre          : std_ulogic_vector(31 downto 0);
   signal waiting_flag              : std_ulogic_vector(01 downto 0);
   signal waiting_flag_l            : std_ulogic_vector(01 downto 0);
   signal n_waiting_taga_pre_l      : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_waiting_tagb_pre_l      : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_rega_value_pre_l        : std_ulogic_vector(31 downto 0);
   signal n_regb_value_pre_l        : std_ulogic_vector(31 downto 0);
  
   subtype state_t is std_ulogic_vector(02 downto 0);

   constant READY                   : state_t := "100";
   constant WAITING_A               : state_t := "001";
   constant WAITING_B               : state_t := "010";
   constant WAITING_BOTH            : state_t := "011";
   constant EMPTY                   : state_t := "000";

   signal status_out_wire           : state_t;
   signal n_status                  : state_t;

   signal dest_tag_out_wire       : std_ulogic_vector(ROB_log2_Size downto 0);
   signal dest_reg_out_wire       : std_ulogic_vector(04 downto 0);
   signal rega_value_out_wire     : std_ulogic_vector(31 downto 0);
   signal regb_value_out_wire     : std_ulogic_vector(31 downto 0);
   signal Imm_value_out_wire      : std_ulogic_vector(16 downto 0);
   signal ALUOp_out_wire          : std_ulogic_vector(04 downto 0);
--   signal alu_shamt_out_wire      : std_ulogic_vector(04 downto 0);
   signal Movn_out_wire        	 : std_ulogic;
   signal Movz_out_wire        	 : std_ulogic;
   signal Regwrite_out_wire       : std_ulogic;
   signal Link_out_wire           : std_ulogic;
   signal ALUSrcImm_out_wire      : std_ulogic;
   signal LLSC_out_wire           : std_ulogic;
   signal Memread_out_wire        : std_ulogic;
   signal Memwrite_out_wire       : std_ulogic;
   signal Membyte_out_wire        : std_ulogic;
   signal Memhalf_out_wire        : std_ulogic;
   signal MemSignExtend_out_wire  : std_ulogic;
   signal Left_out_wire           : std_ulogic;
   signal Right_out_wire          : std_ulogic;
   signal MemtoReg_out_wire       : std_ulogic;
   signal RestartPC_out_wire	    : std_ulogic_vector(31 downto 0);
   signal IsBDS_out_wire		    : std_ulogic;
   signal Trap_out_wire			    : std_ulogic; 
   signal TrapCond_out_wire 	    : std_ulogic; 
   signal EX_CanErr_out_wire	    : std_ulogic; 
   signal M_CanErr_out_wire		 : std_ulogic; 
   signal inorder_out_wire        : std_ulogic; 
   signal fill_wire               : std_ulogic; 
   signal reset_flush             : std_ulogic;
   signal flush_tag_num           : integer range 0 to 2*ROB_DEPTH-1;
   signal ROB_head_num            : integer range 0 to 2*ROB_DEPTH-1;
   signal dest_tag_num            : integer range 0 to 2*ROB_DEPTH-1;
   signal counter_wire            : integer range 0 to 4*RS_DEPTH-1;
   signal n_counter               : integer range 0 to 4*RS_DEPTH-1;
   signal counter_plus_one        : integer range 0 to 4*RS_DEPTH-1;
--   signal order_counter_plus_one     : integer range 0 to 4*RS_DEPTH-1;
--   signal order_counter_wire         : integer range 0 to 4*RS_DEPTH-1;
--   signal n_order_counter            : integer range 0 to 4*RS_DEPTH-1;
begin
   flush_tag_num <= to_integer(unsigned(flush_tag_in));
	ROB_head_num <= to_integer(unsigned(ROB_head_tag_in));
	dest_tag_num <= to_integer(unsigned(dest_tag_out_wire));
   fill_wire <= fill when (IS_Stall = '0') else '0';
	-- When an exception occurs, in order to achieve an precise exception, it needs 
	-- the head of the ROB and the tag where the exception occurred. it need to flush 
	-- the intructions except between head and flush tag.
	flush_or_not: process(flush_exc, ROB_head_num, flush_tag_num, dest_tag_num)
	begin
		if (flush_exc = '1') then
			if (ROB_head_num > flush_tag_num) then
				if (dest_tag_num >= flush_tag_num and dest_tag_num < ROB_head_num) then
					reset_flush <= '1';
				else 
					reset_flush <= '0';
				end if;
			elsif (ROB_head_num < flush_tag_num) then
				if (dest_tag_num < ROB_head_num or dest_tag_num >= flush_tag_num) then
					reset_flush <= '1';
				else	
					reset_flush <= '0';
				end if;
			end if;
      else
         reset_flush <= '0';
		end if;
	end process;
	
    RES_entry_seq:process (clock)
   begin
      if rising_edge(clock) then
         if (reset = '1') then
            status_out_wire      <= "000";
            dest_tag_out_wire    <= (others => '1');
			   dest_reg_out_wire    <= (others => '0');
            waiting_taga         <= (others => '1');
            waiting_tagb         <= (others => '1');
            rega_value_out_wire  <= (others => '0');
            regb_value_out_wire  <= (others => '0');
			   Imm_value_out_wire   <= (others => '0');
            ALUOp_out_wire       <= "00000";
--			   alu_shamt_out_wire   <= "00000";
            Movn_out_wire        <= '0';
            Movz_out_wire        <= '0';
            Regwrite_out_wire    <= '0';
            Link_out_wire        <= '0';         
            ALUSrcImm_out_wire   <= '0';   
            LLSC_out_wire        <= '0';    
            Memread_out_wire     <= '0';    
            Memwrite_out_wire    <= '0';    
            Membyte_out_wire     <= '0';   
            Memhalf_out_wire     <= '0';   
            MemSignExtend_out_wire <= '0'; 
            Left_out_wire        <= '0';   
            Right_out_wire       <= '0';   
            MemtoReg_out_wire    <= '0';   
            RestartPC_out_wire	<= (others => '0');
            IsBDS_out_wire		   <= '0';
            Trap_out_wire		   <= '0';	   
            TrapCond_out_wire 	<= '0';  
            EX_CanErr_out_wire	<= '0';  
            M_CanErr_out_wire	   <= '0';	   
            inorder_out_wire     <= '0';
            counter_wire         <= 0;
--            order_counter_wire   <= 0;
         else
            if (clk_en = '1') then
               if (EX_Stall = '0') then
                  if (clear = '1') then
                     status_out_wire      <= "000";
                     dest_reg_out_wire    <= (others => '0');
                     Movn_out_wire        <= '0';
                     Movz_out_wire        <= '0';
                     inorder_out_wire     <= '0';
                     counter_wire         <= 0;
--                     order_counter_wire   <= 0;
                  elsif (IS_Stall = '0') then
                     status_out_wire      <= n_status;
                     dest_tag_out_wire    <= n_dest_tag;
                     dest_reg_out_wire    <= n_dest_reg;
                     waiting_taga         <= n_waiting_taga;
                     waiting_tagb         <= n_waiting_tagb;
                     rega_value_out_wire  <= n_rega_value;
                     regb_value_out_wire  <= n_regb_value;
                     Imm_value_out_wire   <= n_Imm_value;
                     ALUOp_out_wire       <= n_ALUOp;
--                     alu_shamt_out_wire   <= n_alu_shamt;
                     Movn_out_wire        <= n_Movn;
                     Movz_out_wire        <= n_Movz;
                     Regwrite_out_wire    <= n_Regwrite;
                     Link_out_wire        <= n_Link;         
                     ALUSrcImm_out_wire   <= n_ALUSrcImm;   
                     LLSC_out_wire        <= n_LLSC;    
                     Memread_out_wire     <= n_Memread;    
                     Memwrite_out_wire    <= n_Memwrite;    
                     Membyte_out_wire     <= n_Membyte;   
                     Memhalf_out_wire     <= n_Memhalf;   
                     MemSignExtend_out_wire <= n_MemSignExtend; 
                     Left_out_wire        <= n_Left;   
                     Right_out_wire       <= n_Right;   
                     MemtoReg_out_wire    <= n_MemtoReg;    
                     RestartPC_out_wire	<= n_RestartPC;  
                     IsBDS_out_wire		   <= n_IsBDS;				
                     Trap_out_wire		   <= n_Trap;	   
                     TrapCond_out_wire 	<= n_TrapCond;  
                     EX_CanErr_out_wire	<= n_EX_CanErr;  
                     M_CanErr_out_wire	   <= n_M_CanErr;	   
                     inorder_out_wire     <= n_inst_inorder;
                     counter_wire         <= n_counter;
--                     order_counter_wire   <= n_order_counter;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end process;
   
   taga_in_nonnull <= '1' when (waiting_taga_in/= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
   tagb_in_nonnull <= '1' when(waiting_tagb_in/= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
   taga_cur_nonnull <= '1' when(waiting_taga/= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
   tagb_cur_nonnull <= '1' when(waiting_tagb/= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
   taga_in_match_cdb1_in <= '1' when(waiting_taga_in = cdb1_tag_in) else '0';   --compare the input tag and cdb tag
   taga_in_match_cdb2_in <= '1' when(waiting_taga_in = cdb2_tag_in) else '0';
   tagb_in_match_cdb1_in <= '1' when(waiting_tagb_in = cdb1_tag_in) else '0';
   tagb_in_match_cdb2_in <= '1' when(waiting_tagb_in = cdb2_tag_in) else '0';
   taga_cur_match_cdb1_in <= '1' when(waiting_taga = cdb1_tag_in) else '0';     --compare the stored tag and cdb tag
   taga_cur_match_cdb2_in <= '1' when(waiting_taga = cdb2_tag_in) else '0';
   tagb_cur_match_cdb1_in <= '1' when(waiting_tagb = cdb1_tag_in) else '0';
   tagb_cur_match_cdb2_in <= '1' when(waiting_tagb = cdb2_tag_in) else '0';
   status_currently_empty <= '1' when(status_out_wire = "000") else '0';

   
   n_waiting_taga_pre <= (others=> '1') when ((taga_in_match_cdb2_in = '1' or taga_in_match_cdb1_in = '1') and taga_in_nonnull = '1') else 
                           waiting_taga_in;
   n_waiting_tagb_pre <= (others=> '1') when ((tagb_in_match_cdb2_in = '1' or tagb_in_match_cdb1_in = '1') and tagb_in_nonnull = '1') else 
                           waiting_tagb_in;
   n_rega_value_pre <= cdb1_value_in when (taga_in_match_cdb1_in = '1'  and taga_in_nonnull = '1') else 
                        cdb2_value_in when (taga_in_match_cdb2_in = '1'  and taga_in_nonnull = '1' and cdb2_regwrite_in = '1') else
				            rega_value_in;
   n_regb_value_pre <= cdb1_value_in when (tagb_in_match_cdb1_in = '1'  and tagb_in_nonnull = '1') else 
                        cdb2_value_in when (tagb_in_match_cdb2_in = '1'  and tagb_in_nonnull = '1' and cdb2_regwrite_in = '1') else
				            regb_value_in;
   waiting_flag(1) <= '0' when (n_waiting_taga_pre(ROB_log2_Size) = '1') else
					         '1';
   waiting_flag(0) <= '0' when (n_waiting_tagb_pre(ROB_log2_Size) = '1') else
					         '1';
				 
   n_waiting_taga_pre_l <= (others=> '1') when ((taga_cur_match_cdb2_in = '1' or taga_cur_match_cdb1_in = '1') and taga_cur_nonnull = '1') else 
                           waiting_taga;
   n_waiting_tagb_pre_l <= (others=> '1') when ((tagb_cur_match_cdb2_in = '1' or tagb_cur_match_cdb1_in = '1') and tagb_cur_nonnull = '1') else 
                           waiting_tagb;
   n_rega_value_pre_l <= cdb1_value_in when (taga_cur_match_cdb1_in = '1'  and taga_cur_nonnull = '1' ) else 
                           cdb2_value_in when (taga_cur_match_cdb2_in = '1'  and taga_cur_nonnull = '1' and cdb2_regwrite_in = '1') else
				               rega_value_out_wire;
   n_regb_value_pre_l <= cdb1_value_in when (tagb_cur_match_cdb1_in = '1'  and tagb_cur_nonnull = '1') else 
                           cdb2_value_in when (tagb_cur_match_cdb2_in = '1'  and tagb_cur_nonnull = '1' and cdb2_regwrite_in = '1') else
				               regb_value_out_wire;
   waiting_flag_l(1) <= '0' when (n_waiting_taga_pre_l(ROB_log2_Size) = '1') else
					         '1';
   waiting_flag_l(0) <= '0' when (n_waiting_tagb_pre_l(ROB_log2_Size) = '1') else
					         '1';
   counter_plus_one <= 4*RS_DEPTH-1 when (counter_wire >= 4*RS_DEPTH-2) else
					         counter_wire + 2;
--   order_counter_plus_one <= 4*RS_DEPTH-1 when (order_counter_wire = 4*RS_DEPTH-1 or order_counter_wire = 4*RS_DEPTH-2) else
--					         order_counter_wire + 2;
	-- when write into entry, singal fill should be 1, stores the control and value bits.
	-- after that, should wait to ready and sent to EX stage.
   RES_entry_kom:process (fill_wire, waiting_flag_l, waiting_flag, n_rega_value_pre_l, n_regb_value_pre_l, n_waiting_taga_pre_l, n_waiting_tagb_pre_l, n_rega_value_pre, n_regb_value_pre, n_waiting_tagb_pre, n_waiting_taga_pre, dest_reg_out_wire, dest_reg_in, counter_plus_one, status_currently_empty, dest_tag_out_wire, Movn_out_wire, 
   imm_value_in, ALUOp_in, Movz_in, Link_in, ALUSrcImm_in, Regwrite_in, LLSC_in, Memread_in, Memwrite_in, Membyte_in, Memhalf_in, MemSignExtend_in, Left_in, Right_in, MemtoReg_in, Movn_in, dest_tag_in,
   RestartPC_in, IsBDS_in, Trap_in, TrapCond_in, EX_CanErr_in, M_CanErr_in, imm_value_out_wire, ALUOp_out_wire, Movz_out_wire, Link_out_wire, ALUSrcImm_out_wire, Regwrite_out_wire, LLSC_out_wire, Memread_out_wire, Memwrite_out_wire, Membyte_out_wire, Memhalf_out_wire, MemSignExtend_out_wire, Left_out_wire, Right_out_wire, MemtoReg_out_wire,
   RestartPC_out_wire, IsBDS_out_wire, Trap_out_wire, TrapCond_out_wire, EX_CanErr_out_wire, M_CanErr_out_wire, inst_inorder, inorder, inorder_out_wire)
   begin
      if (fill_wire = '1') then
         n_waiting_taga <= n_waiting_taga_pre;
         n_waiting_tagb <= n_waiting_tagb_pre;
         n_rega_value   <= n_rega_value_pre;
         n_regb_value   <= n_regb_value_pre;
         case waiting_flag is
            when "00" =>
               n_status  <= READY;
--			      n_counter <= 1;
            when "01" =>
               n_status  <= WAITING_B;
--			      n_counter <= 0;
            when "10" =>
               n_status  <= WAITING_A;
--			      n_counter <= 0;
            when "11" =>
               n_status  <= WAITING_BOTH;
--			      n_counter <= 0;
	    when others =>
         end case;
         n_dest_tag      <= dest_tag_in;
		   n_Imm_value     <= imm_value_in;
		   n_dest_reg      <= dest_reg_in;
         n_ALUOp         <= ALUOp_in;
--		   n_alu_shamt     <= alu_shamt_in;
		   n_Movn          <= Movn_in;
         n_Movz          <= Movz_in;
		   n_Link          <= Link_in;         				
         n_ALUSrcImm     <= ALUSrcImm_in;
         n_Regwrite      <= Regwrite_in;
         n_LLSC          <= LLSC_in;   	 	
         n_Memread       <= Memread_in;    	
         n_Memwrite      <= Memwrite_in;      	
         n_Membyte       <= Membyte_in;    	
         n_Memhalf       <= Memhalf_in;    	
         n_MemSignExtend <= MemSignExtend_in;        
         n_Left          <= Left_in; 	 	
         n_Right         <= Right_in;  		
         n_MemtoReg      <= MemtoReg_in;     	                    	 
         n_RestartPC		 <= RestartPC_in;	 
         n_IsBDS			 <= IsBDS_in;
         n_Trap			 <= Trap_in;	 
         n_TrapCond 		 <= TrapCond_in;	 
         n_EX_CanErr		 <= EX_CanErr_in;	 
         n_M_CanErr		 <= M_CanErr_in;
         n_inst_inorder  <= inst_inorder;
		 if (inorder = '1') then
			n_counter <= 1;
         else
			n_counter <= 0;
		 end if;	 
      else
         n_waiting_taga <= n_waiting_taga_pre_l;
         n_waiting_tagb <= n_waiting_tagb_pre_l;
         n_rega_value   <= n_rega_value_pre_l;
         n_regb_value   <= n_regb_value_pre_l;
         if (status_currently_empty = '1') then
            n_status    <= EMPTY;
            n_counter   <= 0;
         else
            case waiting_flag_l is
               when "00" =>
                  n_status  <= READY;
--				      n_counter <= counter_plus_one;
               when "01" =>
                  n_status  <= WAITING_B;
--				      n_counter <= 0;
               when "10" =>
                  n_status  <= WAITING_A;
--				      n_counter <= 0;
               when "11" =>
                  n_status  <= WAITING_BOTH;
--				      n_counter <= 0;
               when others =>
            end case;
         end if;
         n_dest_tag      <= dest_tag_out_wire;
		   n_dest_reg      <= dest_reg_out_wire;
		   n_Imm_value     <= Imm_value_out_wire;
         n_ALUOp         <= ALUOp_out_wire;
--         n_alu_shamt     <= alu_shamt_out_wire;
         n_Movn          <= Movn_out_wire;
         n_Movz          <= Movz_out_wire;
         n_Regwrite      <= Regwrite_out_wire;
         n_Link          <= Link_out_wire;         		 		
         n_ALUSrcImm     <= ALUSrcImm_out_wire;        	
         n_LLSC          <= LLSC_out_wire;   	 	
         n_Memread       <= Memread_out_wire;    	
         n_Memwrite      <= Memwrite_out_wire;      	
         n_Membyte       <= Membyte_out_wire;    	
         n_Memhalf       <= Memhalf_out_wire;    	
         n_MemSignExtend <= MemSignExtend_out_wire;        
         n_Left          <= Left_out_wire; 	 	
         n_Right         <= Right_out_wire;  		
         n_MemtoReg      <= MemtoReg_out_wire;     	                      		 
         n_RestartPC		 <= RestartPC_out_wire;	 
         n_IsBDS			 <= IsBDS_out_wire;
         n_Trap			 <= Trap_out_wire;	 
         n_TrapCond 		 <= TrapCond_out_wire;	 
         n_EX_CanErr		 <= EX_CanErr_out_wire;	 
         n_M_CanErr		 <= M_CanErr_out_wire;
         n_inst_inorder  <= inorder_out_wire;
         n_counter    <= counter_plus_one;		
      end if;
   end process;
   
   status_out 		   <= status_out_wire;
   dest_tag_out 	   <= dest_tag_out_wire;
   dest_reg_out 	   <= dest_reg_out_wire;
   rega_value_out 	<= rega_value_out_wire;
   regb_value_out 	<= regb_value_out_wire;
   Imm_value_out     <= Imm_value_out_wire;
   ALUOp_out 		   <= ALUOp_out_wire;
--   alu_shamt_out 	   <= alu_shamt_out_wire;
   Movn_out			   <= Movn_out_wire;
   Movz_out 		   <= Movz_out_wire;
   Regwrite_out  	   <= Regwrite_out_wire;
   Link_out          <= Link_out_wire;         		   		
	ALUSrcImm_out     <= ALUSrcImm_out_wire;        	
	LLSC_out          <= LLSC_out_wire;   	 	
	Memread_out       <= Memread_out_wire;    	
	Memwrite_out      <= Memwrite_out_wire;      	
	Membyte_out       <= Membyte_out_wire;    	
	Memhalf_out       <= Memhalf_out_wire;    	
	MemSignExtend_out <= MemSignExtend_out_wire;        
	Left_out          <= Left_out_wire; 	 	
	Right_out         <= Right_out_wire;  		
	MemtoReg_out      <= MemtoReg_out_wire;     	                  	 
	RestartPC_out 	   <=	RestartPC_out_wire;	 
	IsBDS_out			<=	IsBDS_out_wire;	
	Trap_out			   <=	Trap_out_wire;	 
	TrapCond_out 		<=	TrapCond_out_wire;	 
	EX_CanErr_out		<=	EX_CanErr_out_wire;	 
	M_CanErr_out		<=	M_CanErr_out_wire;	
	inorder_out			<=	inorder_out_wire;
   counter     <= std_ulogic_vector(to_unsigned(counter_wire, RS_log2_Size+2));
--   counter           <= std_ulogic_vector(to_unsigned(counter_wire, RS_log2_Size+1));
   RtRd_out          <= n_dest_reg;
end rtl;


