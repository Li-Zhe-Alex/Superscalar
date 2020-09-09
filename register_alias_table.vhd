-- ----------------------------------------------------------------------------
-- register_alias_table.vhd - register renaming
-- Original Zhe Li, Master thesis, IMS, Uni Hannover, 2020
-- ----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library core_superscalar_mips;
use core_superscalar_mips.superscalar_mips_package.all;

entity register_alias_table is
   generic (
	    ROB_DEPTH : natural := ROB_DEPTH_c;
      ROB_log2_Size : natural := ROB_log2_Size_c
    );
   port (
      clock           : in std_ulogic;
      reset           : in std_ulogic;
	   clk_en          : in std_ulogic;  -- clock enable
      inst1_valid     : in std_ulogic;  -- need write enable signal
      inst1_rega_in   : in std_ulogic_vector(04 downto 0);      
      inst1_regb_in   : in std_ulogic_vector(04 downto 0);      
      inst1_dest_in   : in std_ulogic_vector(04 downto 0);      
      inst1_tag_in    : in std_ulogic_vector(ROB_log2_Size downto 0);       --from ROB
	   inst2_valid     : in std_ulogic;  -- need write enable signal
      inst2_rega_in   : in std_ulogic_vector(04 downto 0);       
      inst2_regb_in   : in std_ulogic_vector(04 downto 0);       
      inst2_dest_in   : in std_ulogic_vector(04 downto 0);       
      inst2_tag_in    : in std_ulogic_vector(ROB_log2_Size downto 0);       --from ROB
	   CDB1_tag_in     : in std_ulogic_vector(ROB_log2_Size downto 0);
      CDB2_tag_in     : in std_ulogic_vector(ROB_log2_Size downto 0);
	   CDB2_Regwrite   : in std_ulogic;
      WB1_tag_in      : in std_ulogic_vector(ROB_log2_Size downto 0);       --from retire
      WB2_tag_in      : in std_ulogic_vector(ROB_log2_Size downto 0);       --from retire
      IS_Stall        : in std_ulogic;
	   flush_exc       : in std_ulogic;
      flush_tag_in    : in std_ulogic_vector(ROB_log2_Size downto 0);
      ROB_head_tag_in : in std_ulogic_vector(ROB_log2_Size downto 0);
      inst1_taga_out  : out std_ulogic_vector(ROB_log2_Size downto 0);      --To RS
      inst1_tagb_out  : out std_ulogic_vector(ROB_log2_Size downto 0);      --To RS
      inst2_taga_out  : out std_ulogic_vector(ROB_log2_Size downto 0);      --To RS
      inst2_tagb_out  : out std_ulogic_vector(ROB_log2_Size downto 0);       --To RS
      tag1a_finish_out : out std_ulogic_vector(ROB_log2_Size downto 0);      --To ROB
      tag1b_finish_out : out std_ulogic_vector(ROB_log2_Size downto 0);      --To ROB
      tag2a_finish_out : out std_ulogic_vector(ROB_log2_Size downto 0);      --To ROB
      tag2b_finish_out : out std_ulogic_vector(ROB_log2_Size downto 0)      --To ROB
   );
end register_alias_table;

ARCHITECTURE rtl OF register_alias_table is
   TYPE type_tag is ARRAY (31 downto 0) OF std_ulogic_vector(ROB_log2_Size downto 0);
   
   signal n_tag_table        : type_tag;
   signal tag_table          : type_tag;
   signal inst1_dest_nonzero : std_ulogic;
   signal inst2_dest_nonzero : std_ulogic;
   signal inst1_tag_nonnull  : std_ulogic;
   signal inst2_tag_nonnull  : std_ulogic;
   signal finish             : std_ulogic_vector(31 downto 0);
   signal n_finish           : std_ulogic_vector(31 downto 0);
   signal finish_tag1        : std_ulogic_vector(31 downto 0);
   signal finish_tag2        : std_ulogic_vector(31 downto 0);
   signal finish_tag2spe     : std_ulogic_vector(31 downto 0);
   signal WB_tag1       	  : std_ulogic_vector(31 downto 0);
   signal WB_tag2       	  : std_ulogic_vector(31 downto 0);
   signal flush_1,flush_2    : std_ulogic_vector(31 downto 0);
   signal flush_tag_num        : integer;
   signal ROB_head_num         : integer;
BEGIN

   inst1_dest_nonzero 	<= '1' when (inst1_dest_in /= "00000") else '0';
   inst2_dest_nonzero 	<= '1' when (inst2_dest_in /= "00000") else '0';
   inst1_tag_nonnull 	<= '1' when (inst1_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
   inst2_tag_nonnull 	<= '1' when (inst2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
   flush_tag_num <= to_integer(unsigned(flush_tag_in));
   ROB_head_num <= to_integer(unsigned(ROB_head_tag_in));
   -- read tag to RS --
   inst1_taga_out <= (others => '1') when (reset = '1' or inst1_valid = '0' or WB_tag1(to_integer(unsigned(inst1_rega_in))) = '1' or WB_tag2(to_integer(unsigned(inst1_rega_in))) = '1') else
                     tag_table(to_integer(unsigned(inst1_rega_in))) when (inst1_valid = '1' and finish(to_integer(unsigned(inst1_rega_in))) = '0') else
					      std_ulogic_vector(to_unsigned(2*ROB_DEPTH - 2, ROB_log2_Size+1)) when (inst1_valid = '1' and finish(to_integer(unsigned(inst1_rega_in))) = '1') else
					      (others => '1');
					 
   inst1_tagb_out <= (others => '1') when (reset = '1' or inst1_valid = '0' or WB_tag1(to_integer(unsigned(inst1_regb_in))) = '1' or WB_tag2(to_integer(unsigned(inst1_regb_in))) = '1') else
                     tag_table(to_integer(unsigned(inst1_regb_in))) when (inst1_valid = '1' and finish(to_integer(unsigned(inst1_regb_in))) = '0') else
					      std_ulogic_vector(to_unsigned(2*ROB_DEPTH - 2, ROB_log2_Size+1)) when (inst1_valid = '1' and finish(to_integer(unsigned(inst1_regb_in))) = '1') else
					      (others => '1');
   inst2_taga_out <= inst1_tag_in when ((inst2_rega_in = inst1_dest_in) and inst1_dest_nonzero = '1' and inst1_tag_nonnull = '1' and inst2_valid = '1') else
                     (others => '1') when (reset = '1' or inst2_valid = '0' or WB_tag1(to_integer(unsigned(inst2_rega_in))) = '1' or WB_tag2(to_integer(unsigned(inst2_rega_in))) = '1') else
                     tag_table(to_integer(unsigned(inst2_rega_in))) when (inst2_valid = '1' and finish(to_integer(unsigned(inst2_rega_in))) = '0') else
					      std_ulogic_vector(to_unsigned(2*ROB_DEPTH - 2, ROB_log2_Size+1)) when (inst2_valid = '1' and finish(to_integer(unsigned(inst2_rega_in))) = '1') else
					      (others => '1');
					 
   inst2_tagb_out <= inst1_tag_in when ((inst2_regb_in = inst1_dest_in) and inst1_dest_nonzero = '1' and inst1_tag_nonnull = '1' and inst2_valid = '1') else
                     (others => '1') when (reset = '1' or inst2_valid = '0' or WB_tag1(to_integer(unsigned(inst2_regb_in))) = '1' or WB_tag2(to_integer(unsigned(inst2_regb_in))) = '1') else
                     tag_table(to_integer(unsigned(inst2_regb_in))) when (inst2_valid = '1' and finish(to_integer(unsigned(inst2_regb_in))) = '0') else
					      std_ulogic_vector(to_unsigned(2*ROB_DEPTH - 2, ROB_log2_Size+1)) when (inst2_valid = '1' and finish(to_integer(unsigned(inst2_regb_in))) = '1') else
					      (others => '1');
   
   tag1a_finish_out <= tag_table(to_integer(unsigned(inst1_rega_in))) when (inst1_valid = '1' and finish(to_integer(unsigned(inst1_rega_in))) = '1') else 
					         (others => '1');
   tag1b_finish_out <= tag_table(to_integer(unsigned(inst1_regb_in))) when (inst1_valid = '1' and finish(to_integer(unsigned(inst1_regb_in))) = '1') else 
					         (others => '1');
   tag2a_finish_out <= tag_table(to_integer(unsigned(inst2_rega_in))) when (inst2_valid = '1' and finish(to_integer(unsigned(inst2_rega_in))) = '1') else 
					         (others => '1');
   tag2b_finish_out <= tag_table(to_integer(unsigned(inst2_regb_in))) when (inst2_valid = '1' and finish(to_integer(unsigned(inst2_regb_in))) = '1') else 
					         (others => '1');
	-- Write Tag In ----------------------------------------------------------------------
    tagtable : for j in 0 to 31 generate
      process (clock)
      begin
         if rising_edge(clock) then
			if (reset = '1') then
			   tag_table(j) <= (others => '1');
			   finish(j)    <= '0';
			else
				if (clk_en = '1') then 
					finish(j)    <= n_finish(j);
					if (IS_Stall = '0') then
					   tag_table(j) <= n_tag_table(j);
					end if;   
				end if;
			end if;
         end if;
      end process;
    end generate;
	
	
	ntagtable : for i in 0 tO 31 generate
	finish_tag1(i) <= '1' when (CDB1_tag_in = tag_table(i) and CDB1_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
	finish_tag2(i) <= '1' when (CDB2_tag_in = tag_table(i) and CDB2_Regwrite = '1' and CDB2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
	finish_tag2spe(i) <= '1' when (CDB2_tag_in = tag_table(i) and CDB2_Regwrite = '0' and CDB2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
	WB_tag1(i) <= '1' when (WB1_tag_in = tag_table(i) and WB1_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
	WB_tag2(i) <= '1' when (WB2_tag_in = tag_table(i) and WB2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
	flush_1(i) <= '1' when (flush_exc = '1' and ROB_head_num > flush_tag_num and flush_tag_num <= to_integer(unsigned(tag_table(i))) and to_integer(unsigned(tag_table(i))) < ROB_head_num) else '0';
	flush_2(i) <= '1' when (flush_exc = '1' and ROB_head_num < flush_tag_num and (to_integer(unsigned(tag_table(i))) < ROB_head_num or to_integer(unsigned(tag_table(i))) >= flush_tag_num)) else '0';
	process(reset, flush_1, flush_2, finish, tag_table, WB_tag1, WB_tag2, finish_tag1, finish_tag2, inst2_dest_in, inst1_tag_in, inst2_tag_in, inst1_dest_in, inst2_dest_nonzero, inst1_dest_nonzero, inst1_tag_nonnull, inst2_tag_nonnull, finish_tag2spe)
	begin
		if (reset = '1' or flush_1(i) = '1' or flush_2(i) = '1') then 
			n_tag_table(i) <= (others => '1');
			n_finish(i)    <= '0';
		else
			if ((inst2_dest_in = std_ulogic_vector(to_unsigned(i, 5))) and inst2_dest_nonzero = '1' and inst2_tag_nonnull = '1') then
				n_tag_table(i) <= inst2_tag_in;
				n_finish(i) <= '0';
			elsif ((inst1_dest_in = std_ulogic_vector(to_unsigned(i, 5))) and inst1_dest_nonzero = '1' and inst1_tag_nonnull = '1') then
				n_tag_table(i) <= inst1_tag_in;
				n_finish(i) <= '0';
			elsif (WB_tag1(i) = '1' or WB_tag2(i) = '1' or finish_tag2spe(i) = '1') then
				n_tag_table(i) <= (others => '1');
				n_finish(i) <= '0';
			elsif (finish_tag1(i) = '1' or finish_tag2(i) = '1') then
				n_tag_table(i) <= tag_table(i);
				n_finish(i) <= '1';
			else
				n_tag_table(i) <= tag_table(i);
				n_finish(i) <= finish(i);
			end if;
		end if;
	end process;
	end generate;  
   
end rtl;



