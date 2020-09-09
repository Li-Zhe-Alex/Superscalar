-- ----------------------------------------------------------------------------
-- reorder_buffer.vhd - In order to solve the name denpendancy, ROB stores the DestReg 
-- to be excuted, and assigns a tag, which is the serial number of the ROB entry. 
-- When the excution is completed, it is compared with the entry number in the ROB. 
-- if match, the result is stored and waiting for retire (write back). Since the excution
--  is out of order, in order to achieve precise exceptions, the ROB needs to be in the 
-- original order WB. Therefore, the introduction of ROB should be It is a ring buffer, 
-- with head and tail. When it is written, it is written into the entry where the 
-- tail is located. When retired, it writes back the destreg and value of the enrty 
-- where the head is located.
-- Original Zhe Li, Master thesis, IMS, Uni Hannover, 2020
-- ----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library core_superscalar_mips;
use core_superscalar_mips.superscalar_mips_package.all;

entity reorder_buffer is
    generic (
	  ROB_DEPTH : natural := ROB_DEPTH_c;
      ROB_log2_Size : natural := ROB_log2_Size_c
    );
   port (
      clock                   : in std_ulogic;
      reset                   : in std_ulogic;
	  clk_en                  : in std_ulogic;
    -- access --
    -- input      
      inst1_valid_in          : in std_ulogic;							
      inst1_dest_in           : in std_ulogic_vector(04 downto 0);        
	  inst2_valid_in          : in std_ulogic;							
      inst2_dest_in           : in std_ulogic_vector(04 downto 0);        
	-- RAT
	  tag1a_finish_in         : in std_ulogic_vector(ROB_log2_Size downto 0);
	  tag1b_finish_in         : in std_ulogic_vector(ROB_log2_Size downto 0);
	  tag2a_finish_in         : in std_ulogic_vector(ROB_log2_Size downto 0);
	  tag2b_finish_in         : in std_ulogic_vector(ROB_log2_Size downto 0);
    -- CDB 
      cdb1_tag_in             : in std_ulogic_vector(ROB_log2_Size downto 0);        --from CDB  
      cdb1_value_in           : in std_ulogic_vector(31 downto 0);        --from CDB  
      cdb2_tag_in             : in std_ulogic_vector(ROB_log2_Size downto 0);        --from CDB  
      cdb2_value_in           : in std_ulogic_vector(31 downto 0);        --from CDB 
	  Alu_regwrite_in         : in std_ulogic;
	  LdSt_regwrite_in        : in std_ulogic;
    -- EXCEPTION
	  IS_Stall				  : in std_ulogic;
	  EX_Stall				  : in std_ulogic;
	  WB_Stall				  : in std_ulogic;
	  flush_exc               : in std_ulogic;
	  flush_tag_in            : in std_ulogic_vector(ROB_log2_Size downto 0);         --from ex
    -- OUTPUT
	  inst1_tag_out           : out std_ulogic_vector(ROB_log2_Size downto 0);        --to RS/RAT
	  inst2_tag_out           : out std_ulogic_vector(ROB_log2_Size downto 0);        --to RS/RAT
	  value1a_finish_out      : out std_ulogic_vector(31 downto 0);       --to alu/ldst
	  value1b_finish_out      : out std_ulogic_vector(31 downto 0);		  --to alu/ldst
	  value2a_finish_out      : out std_ulogic_vector(31 downto 0);		  --to alu/ldst
	  value2b_finish_out      : out std_ulogic_vector(31 downto 0);		  --to alu/ldst
      inst1_dest_out          : out std_ulogic_vector(04 downto 0);        --to retire
      inst1_value_out         : out std_ulogic_vector(31 downto 0);        --to retire
      inst2_dest_out          : out std_ulogic_vector(04 downto 0);        --to retire
      inst2_value_out         : out std_ulogic_vector(31 downto 0);        --to retire
	  inst1_ROB_tag_out       : out std_ulogic_vector(ROB_log2_Size downto 0); 
	  inst2_ROB_tag_out       : out std_ulogic_vector(ROB_log2_Size downto 0); 
	  ROB_head_tag_out        : out std_ulogic_vector(ROB_log2_Size downto 0);        --TO RS
	  inst1_regwrite_out   	  : out std_ulogic;
	  inst2_regwrite_out   	  : out std_ulogic;
	  inst1_retire            : out std_ulogic;
	  inst2_retire            : out std_ulogic;
	-- hazards
	  tail_out 				  : out std_ulogic_vector(ROB_log2_Size-1 downto 0); 
	  ROB_DestRegs            : out ROB_array_register;
	  ROB_RegWrites           : out std_ulogic_vector(ROB_DEPTH-1 downto 0); 
	  ROB_states              : out ROB_array_state;
	  ROB_values              : out ROB_array_value;
      rob_full                : out std_ulogic
   );
end reorder_buffer;

architecture behav of reorder_buffer is
   component reorder_buffer_entry is
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
   end component;
     
	 
   type type_register is ARRAY (ROB_DEPTH - 1 downto 0) of std_ulogic_vector(04 downto 0);
   type type_value is ARRAY (ROB_DEPTH - 1 downto 0) of std_ulogic_vector(31 downto 0);
   type type_state is ARRAY (ROB_DEPTH - 1 downto 0) of std_ulogic_vector(01 downto 0);
   
   signal head_plus_one     : std_ulogic_vector(ROB_log2_Size downto 0);
   signal head_plus_two     : std_ulogic_vector(ROB_log2_Size downto 0);
   signal tail_plus_one     : std_ulogic_vector(ROB_log2_Size downto 0);
   signal tail_plus_two     : std_ulogic_vector(ROB_log2_Size downto 0);
   signal head              : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_head            : std_ulogic_vector(ROB_log2_Size downto 0);
   signal tail              : std_ulogic_vector(ROB_log2_Size downto 0);
   signal n_tail            : std_ulogic_vector(ROB_log2_Size downto 0);
   
   signal resets            : std_ulogic_vector(ROB_DEPTH - 1 downto 0);
   signal writes            : std_ulogic_vector(ROB_DEPTH - 1 downto 0);
   signal clears            : std_ulogic_vector(ROB_DEPTH - 1 downto 0);
   signal head_plus_two_c   : std_ulogic_vector(ROB_DEPTH - 1 downto 0);
   signal head_plus_one_c   : std_ulogic_vector(ROB_DEPTH - 1 downto 0);
   signal tail_plus_one_c   : std_ulogic_vector(ROB_DEPTH - 1 downto 0);
   signal tail_plus_two_c   : std_ulogic_vector(ROB_DEPTH - 1 downto 0);

   signal registers_in      : type_register;
   signal values_out        : type_value;
   signal registers_out     : type_register;
   signal states_out        : type_state;
   signal regwrites_out   	: std_ulogic_vector(ROB_DEPTH - 1 downto 0);

   signal inst1_tag_out_wire     : std_ulogic_vector(ROB_log2_Size downto 0);
   signal inst2_tag_out_wire     : std_ulogic_vector(ROB_log2_Size downto 0);
   signal inst1_valid 	 		 : std_ulogic;
   signal inst2_valid			 : std_ulogic;
   signal inst1_retire_wire      : std_ulogic;
   signal inst2_retire_wire      : std_ulogic;
   signal inst1_dispatch_wire    : std_ulogic;
   signal inst2_dispatch_wire    : std_ulogic;
   signal finish_tag1a_nonnull   : std_ulogic;
   signal finish_tag1b_nonnull   : std_ulogic;
   signal finish_tag2a_nonnull   : std_ulogic;
   signal finish_tag2b_nonnull   : std_ulogic;

   signal rob_full_wire          : std_ulogic;
   signal flush_tag_num          : integer range 0 to 2*ROB_DEPTH - 1;
   signal head_num               : integer range 0 to ROB_DEPTH - 1;
   signal tail_num               : integer range 0 to ROB_DEPTH - 1;
   signal fill_count             : integer range 0 to ROB_DEPTH;
begin
	rob_full <= rob_full_wire;
	ROB_head_tag_out <= head;
	inst1_retire <= inst1_retire_wire;
	inst2_retire <= inst2_retire_wire;
	inst1_tag_out <= inst1_tag_out_wire;
	inst2_tag_out <= inst2_tag_out_wire;
	tail_out      <= tail(ROB_log2_Size-1 downto 0);
	
	process (clock)
	begin
      	IF rising_edge(clock) then
			IF (reset = '1') then
				head <= std_ulogic_vector(to_unsigned(ROB_DEPTH - 1, ROB_log2_Size+1));
				tail <= std_ulogic_vector(to_unsigned(ROB_DEPTH - 1, ROB_log2_Size+1));
			else
				if (clk_en = '1') then
					if (IS_Stall = '0') then
						tail <= n_tail;
					end if;
					if (WB_Stall = '0') then
                  		if (EX_Stall = '1') then
							head <= head;
						else
							head <= n_head;
						end if;
					end if;
				end if;
			end IF;
		end IF;
	end process;
	-- output the tail number as Dest TAG to solve register renaming, send it to RS/RAT
	inst2_tag_out_wire <= tail_plus_two when (inst2_dispatch_wire = '1') else
						  tail_plus_one when (inst1_dispatch_wire = '1' and inst2_valid ='1') else
						  (others => '1');
	inst1_tag_out_wire <= tail_plus_one when (inst2_dispatch_wire = '1') else
						  tail_plus_one when (inst1_dispatch_wire = '1' and inst1_valid ='1') else
                          (others => '1');

	inst1_valid 	<= inst1_valid_in when (inst1_dest_in /= "00000") else '0';
    inst2_valid 	<= inst2_valid_in when (inst2_dest_in /= "00000") else '0';

	finish_tag1a_nonnull <= '1' when (tag1a_finish_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
    finish_tag1b_nonnull <= '1' when (tag1b_finish_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
	finish_tag2a_nonnull <= '1' when (tag2a_finish_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
    finish_tag2b_nonnull <= '1' when (tag2b_finish_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) else '0';
	value1a_finish_out <=  cdb1_value_in when ((cdb1_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag1a_nonnull = '1' and cdb1_tag_in = tag1a_finish_in) else
						   cdb2_value_in when ((cdb2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag1a_nonnull = '1' and cdb2_tag_in = tag1a_finish_in) else
						   values_out(to_integer(unsigned(tag1a_finish_in(ROB_log2_Size-1 downto 0)))) when (finish_tag1a_nonnull = '1' and states_out(to_integer(unsigned(tag1a_finish_in(ROB_log2_Size-1 downto 0)))) = "10") else
						  (others => '0');
	value1b_finish_out <=  cdb1_value_in when ((cdb1_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag1b_nonnull = '1' and cdb1_tag_in = tag1b_finish_in) else
						   cdb2_value_in when ((cdb2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag1b_nonnull = '1' and cdb2_tag_in = tag1b_finish_in) else
						   values_out(to_integer(unsigned(tag1b_finish_in(ROB_log2_Size-1 downto 0)))) when (finish_tag1b_nonnull = '1' and  states_out(to_integer(unsigned(tag1b_finish_in(ROB_log2_Size-1 downto 0)))) = "10") else
						  (others => '0');
	value2a_finish_out <=  cdb1_value_in when ((cdb1_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag2a_nonnull = '1' and cdb1_tag_in = tag2a_finish_in) else
						   cdb2_value_in when ((cdb2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag2a_nonnull = '1' and cdb2_tag_in = tag2a_finish_in) else
						   values_out(to_integer(unsigned(tag2a_finish_in(ROB_log2_Size-1 downto 0)))) when (finish_tag2a_nonnull = '1' and states_out(to_integer(unsigned(tag2a_finish_in(ROB_log2_Size-1 downto 0)))) = "10") else
						  (others => '0');
	value2b_finish_out <=  cdb1_value_in when ((cdb1_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag2b_nonnull = '1' and cdb1_tag_in = tag2b_finish_in) else
						   cdb2_value_in when ((cdb2_tag_in /= std_ulogic_vector(to_unsigned(2*ROB_DEPTH -1 , ROB_log2_Size+1))) and finish_tag2b_nonnull = '1' and cdb2_tag_in = tag2b_finish_in) else
						   values_out(to_integer(unsigned(tag2b_finish_in(ROB_log2_Size-1 downto 0)))) when (finish_tag2b_nonnull = '1' and states_out(to_integer(unsigned(tag2b_finish_in(ROB_log2_Size-1 downto 0)))) = "10") else
						  (others => '0');
	head_num <= to_integer(unsigned(head));
	tail_num <= to_integer(unsigned(tail));
	flush_tag_num <= to_integer(unsigned(flush_tag_in));
	fill_count <= tail_num - head_num when (head_num <= tail_num) else
				  tail_num - head_num + ROB_DEPTH;
	rob_full_wire <= '1' when ((fill_count >= ROB_DEPTH - 4 and inst2_dispatch_wire = '1') or (fill_count >= ROB_DEPTH - 3 and inst1_dispatch_wire = '1') or fill_count >= ROB_DEPTH - 2) else '0';
	
	inst1_dispatch_wire <= '1' when (flush_exc = '0' and (inst1_valid = '1' or (inst1_valid = '0' and inst2_valid = '1'))) else '0';
	inst2_dispatch_wire <= '1' when (flush_exc = '0' and (inst1_valid = '1' and inst2_valid = '1')) else '0';
	tail_plus_one <= std_ulogic_vector(to_unsigned(0, ROB_log2_Size+1)) when (tail = std_ulogic_vector(to_unsigned(ROB_DEPTH - 1, ROB_log2_Size+1))) else
					 std_ulogic_vector(to_unsigned(to_integer(unsigned(tail)) + 1 , ROB_log2_Size+1));
	tail_plus_two <= std_ulogic_vector(to_unsigned(1, ROB_log2_Size+1)) when (tail = std_ulogic_vector(to_unsigned(ROB_DEPTH - 1, ROB_log2_Size+1))) else
					 std_ulogic_vector(to_unsigned(0, ROB_log2_Size+1)) when (tail = std_ulogic_vector(to_unsigned(ROB_DEPTH - 2, ROB_log2_Size+1))) else
					 std_ulogic_vector(to_unsigned(to_integer(unsigned(tail)) + 2 , ROB_log2_Size+1));
					 
	inst1_retire_wire <= '1' when (states_out(to_integer(unsigned(head_plus_one))) = "10") else '0';
	inst2_retire_wire <= '1' when (inst1_retire_wire = '1' and (states_out(to_integer(unsigned(head_plus_two))) = "10") ) else '0';
	head_plus_one <= std_ulogic_vector(to_unsigned(0, ROB_log2_Size+1)) when (head = std_ulogic_vector(to_unsigned(ROB_DEPTH - 1, ROB_log2_Size+1))) else
                     std_ulogic_vector(to_unsigned(to_integer(unsigned(head)) + 1 , ROB_log2_Size+1));
	head_plus_two <= std_ulogic_vector(to_unsigned(1, ROB_log2_Size+1)) when (head = std_ulogic_vector(to_unsigned(ROB_DEPTH - 1, ROB_log2_Size+1))) else
                     std_ulogic_vector(to_unsigned(0, ROB_log2_Size+1)) when (head = std_ulogic_vector(to_unsigned(ROB_DEPTH - 2, ROB_log2_Size+1))) else
                     std_ulogic_vector(to_unsigned(to_integer(unsigned(head)) + 2 , ROB_log2_Size+1));
					 
	head_tail_comb: process(inst2_dispatch_wire, inst1_dispatch_wire, inst2_retire_wire, inst1_retire_wire, tail_plus_two, tail_plus_one, tail, head_plus_two, head_plus_one, head)
	begin
		if (inst2_dispatch_wire = '1') then
			n_tail <= tail_plus_two;
		elsif (inst1_dispatch_wire = '1') then
			n_tail <= tail_plus_one;
		else
			n_tail <= tail;
		end if;
		if (inst2_retire_wire = '1') then
			n_head <= head_plus_two;
		elsif (inst1_retire_wire = '1') then
			n_head <= head_plus_one;
		else
			n_head <= head;
		end if;
	end process;
	
	ROBinputs : For i in 0 TO ROB_DEPTH - 1 generate
		head_plus_one_c(i) <= '1' when (head_plus_one = std_ulogic_vector(to_unsigned(i, ROB_log2_Size+1)) and inst1_retire_wire = '1') else '0';
		head_plus_two_c(i) <= '1' when (head_plus_two =std_ulogic_vector(to_unsigned(i, ROB_log2_Size+1)) and inst2_retire_wire = '1') else '0';
		resets(i) <= '1' when ((flush_exc = '1' and head_num > flush_tag_num and flush_tag_num <= i and i < head_num) or (flush_exc = '1' and head_num < flush_tag_num and (i < head_num or i >= flush_tag_num))) else
								reset;
		clears(i) <= head_plus_two_c(i) or head_plus_one_c(i);

		tail_plus_one_c(i) <= '1' when (tail_plus_one =std_ulogic_vector(to_unsigned(i, ROB_log2_Size+1)) and inst1_dispatch_wire = '1') else '0';
		tail_plus_two_c(i) <= '1' when (tail_plus_two =std_ulogic_vector(to_signed(i, ROB_log2_Size+1)) and inst2_dispatch_wire = '1') else '0';
		writes(i) <= '0' when (flush_exc = '1') else
					 			tail_plus_one_c(i) or tail_plus_two_c(i) ;
		registers_in(i) <= inst1_dest_in when (tail_plus_one = std_ulogic_vector(to_unsigned(i, ROB_log2_Size+1)) and inst1_dispatch_wire = '1' and inst1_valid = '1') else
						   inst2_dest_in when (tail_plus_one = std_ulogic_vector(to_unsigned(i, ROB_log2_Size+1)) and inst1_dispatch_wire = '1' and inst2_valid = '1') else
						   inst2_dest_in when (tail_plus_two = std_ulogic_vector(to_unsigned(i, ROB_log2_Size+1)) and inst2_dispatch_wire = '1') else
                           "00000";
	end generate;
	
	
	ROBES : For i in 0 TO ROB_DEPTH - 1 generate

      entries : reorder_buffer_entry
         PorT MAP (
            clock                 => clock,
            reset                 => resets(i),
			clk_en                => clk_en,
			clear                 => clears(i),
            write                 => writes(i),
            
            tag_in                => std_ulogic_vector(to_unsigned(i, ROB_log2_Size+1)),
            register_in           => registers_in(i),
            
            cdb1_tag_in           => cdb1_tag_in,
            cdb1_value_in         => cdb1_value_in,
            cdb2_tag_in           => cdb2_tag_in,
            cdb2_value_in         => cdb2_value_in,
			Alu_regwrite_in       => Alu_regwrite_in,
			LdSt_regwrite_in      => LdSt_regwrite_in,
			EX_Stall              => EX_Stall,
            
            value_out             => values_out(i),
            register_out          => registers_out(i),
            state_out             => states_out(i),
			regwrite_out   		  => regwrites_out(i)
         );
   end generate;  
		
	process(inst1_retire_wire, inst2_retire_wire, head_plus_two, head_plus_one, registers_out, values_out, regwrites_out)
	begin
		if (inst1_retire_wire = '1' and inst2_retire_wire = '1') then
			inst1_dest_out <= registers_out(to_integer(unsigned(head_plus_one)));
			inst1_value_out <= values_out(to_integer(unsigned(head_plus_one)));
			inst1_regwrite_out <= regwrites_out(to_integer(unsigned(head_plus_one)));
			inst1_ROB_tag_out  <= head_plus_one;
			inst2_dest_out <= registers_out(to_integer(unsigned(head_plus_two)));
			inst2_value_out <= values_out(to_integer(unsigned(head_plus_two)));
			inst2_regwrite_out <= regwrites_out(to_integer(unsigned(head_plus_two)));
			inst2_ROB_tag_out <= head_plus_two;
		elsif (inst1_retire_wire = '1') then
			inst1_dest_out <= registers_out(to_integer(unsigned(head_plus_one)));
			inst1_value_out <= values_out(to_integer(unsigned(head_plus_one)));
			inst1_regwrite_out <= regwrites_out(to_integer(unsigned(head_plus_one)));
			inst1_ROB_tag_out  <= head_plus_one;
			inst2_dest_out <= (others => '0');
			inst2_value_out <= (others => '0');
			inst2_regwrite_out <= '0';
			inst2_ROB_tag_out <= (others => '1');
		else	
			inst1_dest_out <= (others => '0');
			inst1_value_out <= (others => '0');
			inst1_regwrite_out <= '0';
			inst1_ROB_tag_out <= (others => '1');
			inst2_dest_out <= (others => '0');
			inst2_value_out <= (others => '0');
			inst2_regwrite_out <= '0';
			inst2_ROB_tag_out <= (others => '1');
		end if;
	end process;
	Hazards : For i in 0 TO ROB_DEPTH - 1 generate
		ROB_DestRegs(i) <=  registers_out(i);
		ROB_RegWrites(i) <= regwrites_out(i);
		ROB_states(i)   <=  states_out(i);
		ROB_values(i)   <=  values_out(i); 
	end generate;
end behav;	