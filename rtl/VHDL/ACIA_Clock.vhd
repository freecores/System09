-----------------------------------------------------------------
--
-- ACIA Clock Divider for System09
--
-----------------------------------------------------------------
--
library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_arith.all;
   use IEEE.std_logic_unsigned.all;

package bit_funcs is
   function log2(v: in natural) return natural;
end package bit_funcs;
      
library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_arith.all;
   use IEEE.std_logic_unsigned.all;

package body bit_funcs is
   function log2(v: in natural) return natural is
      variable n: natural;
      variable logn: natural;
   begin
      n := 1;
      for i in 0 to 128 loop
         logn := i;
         exit when (n>=v);
         n := n * 2;
      end loop;
      return logn;
   end function log2;

end package body bit_funcs;

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;
library work;
   use work.bit_funcs.all;

entity ACIA_Clock is
  generic (
     SYS_Clock_Frequency  : integer;
	  ACIA_Clock_Frequency : integer
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

constant FULL_CYCLE : integer :=  (SYS_Clock_Frequency / ACIA_Clock_Frequency);
constant HALF_CYCLE : integer :=  (FULL_CYCLE / 2);
signal   ACIA_Count  : Std_Logic_Vector(log2(FULL_CYCLE) downto 0) := (Others => '0');

begin
--
-- Baud Rate Clock Divider
--
-- 25MHz / 27  = 926,000 KHz = 57,870Bd * 16
-- 50MHz / 54  = 926,000 KHz = 57,870Bd * 16
--
--my_ACIA_clock: process( clk, ACIA_Count  )
my_ACIA_clock: process( clk  )
begin
    if(clk'event and clk = '0') then
		if( ACIA_Count = (FULL_CYCLE - 1) )	then
			ACIA_Clk   <= '0';
		   ACIA_Count <= (others => '0'); --"000000";
		else
		   if( ACIA_Count = (HALF_CYCLE - 1) )	then
				ACIA_Clk <='1';
			end if;
		   ACIA_Count <= ACIA_Count + 1; 
		end if;			 
    end if;
end process;

end rtl;
