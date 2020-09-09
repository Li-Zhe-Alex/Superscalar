-- ----------------------------------------------------------------------------
-- reorder_buffer_entry.vhd - The entries of Reorder Buffer
-- Original Zhe Li, Master thesis, IMS, Uni Hannover, 2020
-- ----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

library core_superscalar_mips;
use core_superscalar_mips.superscalar_mips_package.all;

entity reorder_buffer_entry is
generic (
      ROB_log2_Size : natural := ROB_log2_Size_c
    );
   port (
		clock                   : in std_ulogic;
		reset                   : in std_ulogic;
		clk_en                  : in std_ulogic;
		write                   : in std_ulogic;
      clear                   : in std_ulogic;
		tag_in                  : in std_ulogic_vector(ROB_log2_Size downto 0);
		register_in             : in std_ulogic_vector(04 downto 0);
		cdb1_value_in           : in std_ulogic_vector(31 downto 0);
		cdb1_tag_in             : in std_ulogic_vector(ROB_log2_Size downto 0);
      cdb2_value_in           : in std_ulogic_vector(31 downto 0);
      cdb2_tag_in             : in std_ulogic_vector(ROB_log2_Size downto 0);
		Alu_regwrite_in         : in std_ulogic;
	   LdSt_regwrite_in        : in std_ulogic;
      EX_Stall                : in std_ulogic;

      value_out               : out std_ulogic_vector(31 downto 0);
      register_out            : out std_ulogic_vector(04 downto 0);
      state_out               : out std_ulogic_vector(01 downto 0);
	   regwrite_out   			: out std_ulogic
   );
end reorder_buffer_entry;

architecture rtl of reorder_buffer_entry is
   signal n_value                : std_ulogic_vector(31 DOWNTO 0);
   signal n_register             : std_ulogic_vector(04 DOWNTO 0);
   signal n_regwrite             : std_ulogic;

   subtype state_t is std_ulogic_vector(01 downto 0);
   constant ROB_EMPTY            : state_t := "00";
   constant ROB_INUSE            : state_t := "01";
   constant ROB_COMPLETE         : state_t := "10";
   constant ROB_UNUSED           : state_t := "11";
   signal n_state                : state_t;
   
   signal value_out_wire         : std_ulogic_vector(31 DOWNTO 0);
   signal register_out_wire      : std_ulogic_vector(04 DOWNTO 0);
   signal state_out_wire         : std_ulogic_vector(01 DOWNTO 0);
   signal regwrite_out_wire      : std_ulogic;
begin
   value_out    <= value_out_wire;
   register_out <= register_out_wire;
   state_out    <= state_out_wire;
   regwrite_out <= regwrite_out_wire;
  
   ROB_entry_seq: process (clock)
   begin
      if rising_edge(clock) then
         if (reset = '1') then
            state_out_wire <= ROB_EMPTY;
            value_out_wire <= (others => '0') ;
            register_out_wire <= "00000" ;
            regwrite_out_wire <= '0';
         else
            if (clk_en = '1') then
               if (EX_Stall = '0') then
                  if (clear = '0') then
                     state_out_wire <= n_state ;
                     value_out_wire <= n_value ;
                     register_out_wire <= n_register;
                     regwrite_out_wire <= n_regwrite;
                  else
                     state_out_wire <= ROB_EMPTY;
 --                    value_out_wire <= (others => '0') ;
 --                    register_out_wire <= "00000" ;
                     regwrite_out_wire <= '0';
                  end if;
               end if;
            end if;
         end if;
      end if;
   end process;
   -- write the DestReg into ROB entry, then wait the excute finish. when match with CDB, ready to retire
   ROB_entry_kom: process (write, register_in, tag_in, cdb1_tag_in, cdb1_value_in, cdb2_tag_in, cdb2_value_in, register_out_wire, state_out_wire, value_out_wire, alu_regwrite_in, Ldst_regwrite_in, regwrite_out_wire)
   begin
      if (write = '1') then
         n_state    <= ROB_INUSE;
         n_value    <= (others => '0');
         n_register <= register_in;
 		   n_regwrite <= '0';
      elsif (write = '0' and tag_in = cdb1_tag_in) then
         n_state    <= ROB_COMPLETE;
         n_value    <= cdb1_value_in;
         n_register <= register_out_wire;
		   n_regwrite <= alu_regwrite_in;
      elsif (write = '0' and tag_in = cdb2_tag_in) then
         n_state    <= ROB_COMPLETE;
         n_value    <= cdb2_value_in;
         n_register <= register_out_wire;
		   n_regwrite <= Ldst_regwrite_in;
      else
         n_state    <= state_out_wire;
         n_value    <= value_out_wire;
         n_register <= register_out_wire;
		   n_regwrite <= regwrite_out_wire;
      end if;
   end process;
end rtl;



