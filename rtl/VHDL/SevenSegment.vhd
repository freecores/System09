--===========================================================================--
--
--  S Y N T H E Z I A B L E    Dynamic Address Translation Registers
--
--  www.OpenCores.Org - December 2002
--  This core adheres to the GNU public license  
--
-- File name      : SevenSegment.vhd
--
-- entity name    : SevenSegment
--
-- Purpose        : 4 x 8 bit lathes to display 7 segments
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
-- Date          Revision  Author 
-- 19 Oct 2004   0.1       John Kent
--
-- 21 Nov 2006   0.2       John Kent
-- Inverted segment registers 
-- so '0' in segment registers switches segment OFF
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity seven_segment is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(1 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0);
	 segments  : out std_logic_vector(7 downto 0);
	 digits	  : out std_logic_vector(3 downto 0)
	 );
end;

architecture rtl of seven_segment is
signal seg_reg0 : std_logic_vector(7 downto 0);
signal seg_reg1 : std_logic_vector(7 downto 0);
signal seg_reg2 : std_logic_vector(7 downto 0);
signal seg_reg3 : std_logic_vector(7 downto 0);

signal ClockDivider		: std_logic_vector(13 downto 0);
signal WhichDigit			: std_logic_vector(1 downto 0);

begin

---------------------------------
--
-- Write Segment registers
--
---------------------------------

seg_write : process( clk, rst, addr, cs, rw, data_in )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      seg_reg0 <= "00000000";
      seg_reg1 <= "00000000";
      seg_reg2 <= "00000000";
      seg_reg3 <= "00000000";
    else
	   if cs = '1' and rw = '0' then
        case addr is
	     when "00" =>
		    seg_reg0 <= data_in;
	     when "01" =>
		    seg_reg1 <= data_in;
	     when "10" =>
		    seg_reg2 <= data_in;
	     when "11" =>
		    seg_reg3 <= data_in;
        when others =>
		    null;
		  end case;
	   end if;
	 end if;
  end if;
end process;

---------------------------------
--
-- Read Segment registers
--
---------------------------------

seg_read : process(  addr,
                     seg_reg0, seg_reg1, seg_reg2, seg_reg3 )
begin
      case addr is
	     when "00" =>
		    data_out <= seg_reg0;
	     when "01" =>
		    data_out <= seg_reg1;
	     when "10" =>
		    data_out <= seg_reg2;
	     when "11" =>
		    data_out <= seg_reg3;
        when others =>
		    null;
		end case;
end process;

---------------------------------
--
-- Output Segment registers
--
---------------------------------

seg_out : process( rst, Clk)
begin
		if rst = '1' then
			ClockDivider <= (others => '0');
			WhichDigit   <= "00";
			Segments     <= "00000000";
			Digits	    <= "1111";
		elsif Clk'Event and Clk = '0' then
			if ClockDivider = "11000011010011" then
				ClockDivider <= (others => '0');
				case WhichDigit is	-- note that everything is pipelined
					when "00" => 
						Digits   <= "1110"; 
						Segments <= not( seg_reg0 );
					when "01" => 
						Digits <= "1101"; 
						Segments <= not( seg_reg1 );
					when "10" => 
						Digits <= "1011"; 
						Segments <= not( seg_reg2 );
					when "11" => 
						Digits <= "0111"; 
						Segments <= not( seg_reg3 );
					when others => 
					   null;
				end case;
				WhichDigit <= WhichDigit + 1;
			else
				ClockDivider <= ClockDivider + 1;
			end if;
		end if;
end process;

end rtl;
	
