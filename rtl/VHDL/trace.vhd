--===========================================================================--
--
--  S Y N T H E Z I A B L E    Real Time Trace   C O R E
--
--  www.OpenCores.Org - May 2003
--  This core adheres to the GNU public license  
--
-- File name      : trace.vhd
--
-- entity name    : trace
--
-- Purpose        : Implements a trace buffer
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--
-- Author         : John E. Kent      
--
--===========================================================================----
--
-- Revision History:
--
-- Date:          Revision         Author
-- 19 June 2004     0.1              John Kent
--
--===========================================================================----
--
-- Register Memory Map
--
-- $00 - Address Comparitor High Byte
-- $01 - Address Comparitor Low byte
-- $02 - Data    Comparitor
-- $03 - Control Comparitor
-- $04 - Address Qualifier High Byte
-- $05 - Address Qualifier Low byte
-- $06 - Data    Qualifier
-- $07 - Control Qualifier
--
-- Address, Data and Control signals must match in the Comparitor registers 
-- Matches are qualified by setting a bit in the Qualifier registers
--
-- Control Comparitor / Qualify (write)
-- b0 - r/w        1=read   0=write
-- b1 - vma        1=valid  0=invalid
-- b5 - trace      1=enable 0=disable
-- b6 - pre/post   1=before 0=after
-- b7 - irq output 1=match  0=mismatch
--
-- Control Qualifier Read
-- b7 - match flag
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity trace is
	port (	
	 clk           : in  std_logic;
    rst           : in  std_logic;
    rs            : in  std_logic;	  -- register select
    bs            : in  std_logic;	  -- bank select
    rw            : in  std_logic;
	 vma           : in  std_logic;
    addr          : in  std_logic_vector(15 downto 0);
    data_in       : in  std_logic_vector(7 downto 0);
	 reg_data_out  : out std_logic_vector(7 downto 0);
	 buff_data_out : out std_logic_vector(7 downto 0);
    cpu_data_in   : in  std_logic_vector(7 downto 0);
	 irq           : out std_logic
  );
end;

architecture trace_arch of trace is

signal clk_n      : std_logic;
signal irq_out    : std_logic;
signal qual_write : std_logic;
signal qual_read  : std_logic;
--
-- bank select
--
signal bank_reg : std_logic_vector(7 downto 0);

--
-- Trigger registers
--
signal comp_addr_hi : std_logic_vector(7 downto 0);
signal comp_addr_lo : std_logic_vector(7 downto 0);
signal qual_addr_hi : std_logic_vector(7 downto 0);
signal qual_addr_lo : std_logic_vector(7 downto 0);
signal comp_data    : std_logic_vector(7 downto 0);
signal qual_data    : std_logic_vector(7 downto 0);
signal comp_ctrl    : std_logic_vector(7 downto 0);
signal qual_ctrl    : std_logic_vector(7 downto 0);
signal match_flag   : std_logic;

--
-- Trace counter
--
signal trace_clk        : std_logic;
signal trace_rst        : std_logic;
signal trace_stb        : std_logic;
signal trace_we         : std_logic;
signal trace_count      : std_logic_vector(9 downto 0);
signal trace_offset     : std_logic_vector(9 downto 0);

signal trace_data_out_0 : std_logic_vector(7 downto 0);
signal trace_data_out_1 : std_logic_vector(7 downto 0);
signal trace_data_out_2 : std_logic_vector(7 downto 0);
signal trace_data_out_3 : std_logic_vector(7 downto 0);
signal trace_data_out_4 : std_logic_vector(7 downto 0);
signal trace_data_out_5 : std_logic_vector(7 downto 0);

signal buff_stb         : std_logic_vector(5 downto 0);
signal buff_we          : std_logic;
signal buff_addr        : std_logic_vector(9 downto 0);

signal buff_data_out_0 : std_logic_vector(7 downto 0);
signal buff_data_out_1 : std_logic_vector(7 downto 0);
signal buff_data_out_2 : std_logic_vector(7 downto 0);
signal buff_data_out_3 : std_logic_vector(7 downto 0);
signal buff_data_out_4 : std_logic_vector(7 downto 0);
signal buff_data_out_5 : std_logic_vector(7 downto 0);

signal mux_stb         : std_logic_vector(5 downto 0);
signal mux_we          : std_logic;
signal mux_addr        : std_logic_vector(9 downto 0);

signal ctrl_in         : std_logic_vector(7 downto 0);
signal trigger         : std_logic;

component ram1k
    Port (
	    -- Port A is 8 bit
       WB_CLK_I  : in  std_logic;
       WB_RST_I  : in  std_logic;
       WB_ADR_I  : in  std_logic_vector (9 downto 0);
       WB_DAT_O  : out std_logic_vector (7 downto 0);
       WB_DAT_I  : in  std_logic_vector (7 downto 0);
	    WB_WE_I   : in  std_logic;
       WB_STB_I  : in  std_logic
    );
end component;


component BUFG 
  port (
     i: in std_logic;
	  o: out std_logic
  );
end component;

begin

--
-- Bank 1 = Trace buffer cpu control out
--
my_trace_buffer_1 : ram1k port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buff_data_out_1,
       WB_DAT_I  => ctrl_in(7 downto 0),
	    WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb(1)
    );

--
-- Trace buffer 2 is cpu address out high
--
my_trace_buffer_2 : ram1k port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buff_data_out_2,
       WB_DAT_I  => addr(15 downto 8),
	    WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb(2)
    );

--
-- Trace buffer 3 is cpu address out low
--
my_trace_buffer_3 : ram1k port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buff_data_out_3,
       WB_DAT_I  => addr(7 downto 0),
	    WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb(3)
    );

--
-- Trace buffer 4 is cpu data out
--
my_trace_buffer_4 : ram1k port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buff_data_out_4,
       WB_DAT_I  => data_in(7 downto 0),
	    WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb(4)
    );

--
-- Trace buffer 5 is cpu data in
--
my_trace_buffer_5 : ram1k port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buff_data_out_5,
       WB_DAT_I  => cpu_data_in(7 downto 0),
	    WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb(5)
    );


--clk_buffer : BUFG port map(
--      i => clk_n,
--	   o => trace_clk
--   );	 

--------------------------------
--
-- write page bank register
--
--------------------------------
bank_reg_write : process( clk, rst, rs, rw, data_in, bank_reg)
begin
	if clk'event and clk = '0' then
		if rst = '1' then
			bank_reg <= "00000000";
		else
			if rs='1' and rw='0' then
				bank_reg <= data_in;
			else
			   bank_reg <= bank_reg;
			end if;
		end if;
	end if;
end process;


--------------------------------
--
-- read page buffers
--
--------------------------------

buffer_read : process( 	bs, bank_reg,
								buff_data_out_0, buff_data_out_1, buff_data_out_2, buff_data_out_3,
								buff_data_out_4, buff_data_out_5 )
variable count : integer;
begin
	for count in 0 to 5 loop
		buff_stb(count) <= '0';
	end loop;

	case bank_reg(2 downto 0) is
	when "000" =>
		buff_stb(0)   <= bs;
		buff_data_out <= buff_data_out_0;
	when "001" =>
	   buff_stb(1)   <= bs;
		buff_data_out <= buff_data_out_1;
	when "010" =>
		buff_stb(2)   <= bs;
		buff_data_out <= buff_data_out_2;
	when "011" =>
		buff_stb(3)   <= bs;
		buff_data_out <= buff_data_out_3;
	when "100" =>
		buff_stb(4)   <= bs;
		buff_data_out <= buff_data_out_4;
	when "101" =>
		buff_stb(5)   <= bs;
		buff_data_out <= buff_data_out_5;
   when others =>
		buff_data_out <= "00000000";
	end case;	 	

	reg_data_out <= bank_reg;		

end process;

--------------------------------
--
-- write control registers
--
--------------------------------
trace_write : process( clk, rst, bs, rw, addr, data_in, qual_write,
                      comp_addr_hi, comp_addr_lo, comp_data, comp_ctrl,
                      qual_addr_hi, qual_addr_lo, qual_data, qual_ctrl )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
		  comp_addr_hi <= "00000000";
		  comp_addr_lo <= "00000000";
		  comp_data    <= "00000000";
		  comp_ctrl    <= "00000000";
		  qual_addr_hi <= "00000000";
		  qual_addr_lo <= "00000000";
		  qual_data    <= "00000000";
		  qual_ctrl    <= "00000000";
		  qual_write  <= '0';
    elsif buff_stb(0) = '1' and rw = '0' then
	   case addr(2 downto 0) is
		when "000" =>
		  comp_addr_hi <= data_in;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= comp_data;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= qual_data;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
		when "001" =>
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= data_in;
		  comp_data    <= comp_data;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= qual_data;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
		when "010" =>
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= data_in;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= qual_data;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
		when "011" =>
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= comp_data;
		  comp_ctrl    <= data_in;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= qual_data;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
		when "100" =>
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= comp_data;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= data_in;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= qual_data;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
		when "101" =>
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= comp_data;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= data_in;
		  qual_data    <= qual_data;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
		when "110" =>
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= comp_data;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= data_in;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
--  		when "111" =>
      when others =>
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= comp_data;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= qual_data;
		  qual_ctrl    <= data_in;
		  qual_write  <= '1';
		end case;
	 else
		  comp_addr_hi <= comp_addr_hi;
		  comp_addr_lo <= comp_addr_lo;
		  comp_data    <= comp_data;
		  comp_ctrl    <= comp_ctrl;
		  qual_addr_hi <= qual_addr_hi;
		  qual_addr_lo <= qual_addr_lo;
		  qual_data    <= qual_data;
		  qual_ctrl    <= qual_ctrl;
		  qual_write  <= '0';
	 end if;
  end if;
end process;

--
-- trap data output mux
--
trace_read : process( buff_stb, rw, addr, qual_read,
                     comp_addr_hi, comp_addr_lo, comp_data, comp_ctrl,
                     qual_addr_hi, qual_addr_lo, qual_data, qual_ctrl,
							match_flag )
begin
   case addr(2 downto 0) is
	when "000" =>
	   buff_data_out_0  <= comp_addr_hi;
		qual_read <= '0';
	when "001" =>
	   buff_data_out_0  <= comp_addr_lo;
		qual_read <= '0';
	when "010" =>
	   buff_data_out_0  <= comp_data;
		qual_read <= '0';
	when "011" =>
	   buff_data_out_0  <= comp_ctrl;
		qual_read <= '0';
	when "100" =>
	   buff_data_out_0  <= qual_addr_hi;
		qual_read <= '0';
	when "101" =>
	   buff_data_out_0  <= qual_addr_lo;
		qual_read <= '0';
	when "110" =>
	   buff_data_out_0  <= qual_data;
		qual_read <= '0';
--	when "111" =>
   when others =>
		qual_read <= buff_stb(0) and rw;
	   buff_data_out_0(6 downto 0) <= qual_ctrl(6 downto 0);
		buff_data_out_0(7) <= match_flag;
	end case;
end process;

mux_proc : process( bs, rw,
                    trace_count, buff_addr,
						  trace_we,   buff_we,
						  trace_stb,  buff_stb )
begin

	if bs='0' and rw='1' then
		mux_addr <= trace_count;
		mux_we   <= trace_we;
		mux_stb  <= trace_stb & trace_stb & trace_stb & trace_stb & trace_stb & trace_stb;
	else
		mux_addr <= buff_addr;
		mux_we   <= buff_we;
		mux_stb  <= buff_stb;
	end if;

end process;

--
-- Trigger hardware
--
trace_match : process( Clk, rst, rw, buff_stb, addr, vma, match_flag, data_in,
                      comp_addr_hi, comp_addr_lo, comp_data, comp_ctrl,
							 qual_addr_hi, qual_addr_lo, qual_data, qual_ctrl)
variable match         : std_logic;
variable match_addr_hi : std_logic;
variable match_addr_lo : std_logic;
variable match_data    : std_logic;
variable match_ctrl    : std_logic;

begin
  match_addr_hi := 
           ((comp_addr_hi(7) xor addr(15)  ) and qual_addr_hi(7) ) or
		     ((comp_addr_hi(6) xor addr(14)  ) and qual_addr_hi(6) ) or
		     ((comp_addr_hi(5) xor addr(13)  ) and qual_addr_hi(5) ) or
		     ((comp_addr_hi(4) xor addr(12)  ) and qual_addr_hi(4) ) or
		     ((comp_addr_hi(3) xor addr(11)  ) and qual_addr_hi(3) ) or
		     ((comp_addr_hi(2) xor addr(10)  ) and qual_addr_hi(2) ) or
		     ((comp_addr_hi(1) xor addr( 9)  ) and qual_addr_hi(1) ) or
		     ((comp_addr_hi(0) xor addr( 8)  ) and qual_addr_hi(0) );
  match_addr_lo :=
		     ((comp_addr_lo(7) xor addr( 7)  ) and qual_addr_lo(7) ) or
		     ((comp_addr_lo(6) xor addr( 6)  ) and qual_addr_lo(6) ) or
		     ((comp_addr_lo(5) xor addr( 5)  ) and qual_addr_lo(5) ) or
		     ((comp_addr_lo(4) xor addr( 4)  ) and qual_addr_lo(4) ) or
		     ((comp_addr_lo(3) xor addr( 3)  ) and qual_addr_lo(3) ) or
		     ((comp_addr_lo(2) xor addr( 2)  ) and qual_addr_lo(2) ) or
		     ((comp_addr_lo(1) xor addr( 1)  ) and qual_addr_lo(1) ) or
		     ((comp_addr_lo(0) xor addr( 0)  ) and qual_addr_lo(0) );
  match_data :=
           ((comp_data(7)    xor data_in(7)) and qual_data(7)    ) or
           ((comp_data(6)    xor data_in(6)) and qual_data(6)    ) or
           ((comp_data(5)    xor data_in(5)) and qual_data(5)    ) or
           ((comp_data(4)    xor data_in(4)) and qual_data(4)    ) or
           ((comp_data(3)    xor data_in(3)) and qual_data(3)    ) or
           ((comp_data(2)    xor data_in(2)) and qual_data(2)    ) or
           ((comp_data(1)    xor data_in(1)) and qual_data(1)    ) or
           ((comp_data(0)    xor data_in(0)) and qual_data(0)    );
  match_ctrl :=
           ((comp_ctrl(0)    xor rw        ) and qual_ctrl(0)    ) or
           ((comp_ctrl(1)    xor vma       ) and qual_ctrl(1)    );

   match := not ( match_addr_hi or match_addr_lo or match_data or match_ctrl);

    if clk'event and clk = '0' then
	   if rst = '1' then
		  match_flag <= '0';
      elsif buff_stb(0) = '1' and rw = '0' then
		  match_flag <= '0';
      else
		  if match = comp_ctrl(7) then
		    match_flag <= '1';
		  else
		    match_flag <= match_flag;
		  end if;

		end if;
    end if; 
	 trigger <= match_flag;
  end process;

trace_capture : process( clk, rst, qual_read, qual_write, 
								 trigger, trace_stb, qual_ctrl, irq_out, 
                         trace_count, trace_offset )
variable irq_tmp : std_logic;
begin
  if clk'event and clk = '1' then
    if rst = '1' then
		trace_count  <= "0000000000";
		trace_offset <= "0000000000";
		trace_stb    <= qual_ctrl(6);
	 else
	   --
		-- zero in bit 6 of the qalifier control register
		-- means start capture after trigger point.
		--
	 	if qual_ctrl(6) = '0' then

			if trace_stb = '0' then
				trace_stb    <= trigger;
				irq_tmp      := irq_out;
				trace_offset <= trace_count;
			else
				if trace_count = trace_offset then
					trace_stb <= '0';
					irq_tmp   := '1';
				else
					trace_stb  <= trace_stb;
					irq_tmp    := irq_out;
				end if;
				trace_offset <= trace_offset;
			end if;

			if qual_read = '1' then
				irq_out <= '0';
			else
				irq_out <= irq_tmp;
			end if;
	   --
		-- one in bit 6 of the qalifier control register
		-- means finish capture at trigger point.
		--
		else
	   	if trace_stb = '0' then
				trace_stb    <= qual_write;
				trace_offset <= trace_offset;
				irq_tmp      := irq_out;
			else
				if trigger = '1' then
				   trace_offset <= trace_count;
					trace_stb    <= '0';
					irq_tmp      := '1';
				else
					trace_offset <= trace_offset;
					trace_stb    <= trace_stb;
					irq_tmp      := irq_out;
				end if;
			end if;

			if qual_write = '1' then
				irq_out <= '0';
			else
				irq_out <= irq_tmp;
			end if;
		end if;
		trace_count <= trace_count + 1;
	 end if;
  end if;
end process;

--
-- Tie up a few signals
--
process( clk, rst, addr, vma, rw, irq_out, trace_offset, qual_ctrl, trace_stb )
begin
  trace_clk  <= clk;
  trace_rst  <= rst;
  trace_we   <= trace_stb;
  buff_addr  <= addr(9 downto 0) + trace_offset;
  buff_we    <= '0';
  irq        <= irq_out and qual_ctrl(7);
  ctrl_in(0) <= rw;
  ctrl_in(1) <= vma;
  ctrl_in(7 downto 2) <= "000000";
end process;

end trace_arch;
	
