-----------------------------------------------------------------
--
-- ACIA Clock Divider for System09
--
-----------------------------------------------------------------
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;

entity ACIA_Clock is
  generic (
     SYS_Clock_Frequency  : integer;
	  BAUD_Clock_Frequency : integer
  );   
  port(
    clk      : in  Std_Logic;  -- System Clock input
	 ACIA_Clk : out Std_Logic   -- ACIA Clock output
  );
end ACIA_Clock;

-------------------------------------------------------------------------------
-- Architecture for ACIA_Clock
-------------------------------------------------------------------------------
architecture rtl of ACIA_Clock is

constant full_cycle : integer :=  (SYS_Clock_Frequency / BAUD_Clock_Frequency) - 1;
constant half_cycle : integer :=  (full_cycle / 2) - 1;
--
-- Baud Rate Clock Divider
--
-- 25MHz / 27  = 926,000 KHz = 57,870Bd * 16
-- 50MHz / 54  = 926,000 KHz = 57,870Bd * 16
--
my_baud_clock: process( SysClk )
begin
    if(SysClk'event and SysClk = '0') then
		if( BaudCount = 53 )	then
			baudclk <= '0';
		   BaudCount <= "000000";
		else
		   if( BaudCount = 26 )	then
				baudclk <='1';
			else
				baudclk <=baudclk;
			end if;
		   BaudCount <= BaudCount + 1;
		end if;			 
    end if;
end process;
