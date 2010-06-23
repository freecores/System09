--===========================================================================----
--
--  S Y N T H E Z I A B L E    Clock_dll for System09 - SOC.
--
--===========================================================================----
--
-- This core adheres to the GNU public license
-- No responsibility is taken for this design.
-- Use at own risk.  
--
-- File name       : Clock_dll.vhd
--
-- Purpose         : Generates Clocks for System09
--                   For BurchED B3-Spartan2+ and B5-X300
--                   Assumes a 12.5 MHz system clock input
--                   Generates a x1 (12.5 MHz) CPU clock 
--                   Generates a x2 (25.0 MHz) VGA clock 
--                   Generates a x4 (50.0 MHz) MEM clock 
--
-- Dependencies    : ieee.Std_Logic_1164
--                   ieee.std_logic_unsigned
--                   ieee.std_logic_arith
--                   ieee.numeric_std
--
--
-- Revision History :
--
--   Rev         : 0.1
--   Date        : 7th September 2008
--   Description : Initial version.                 
-- 
--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;

entity clock_div is
  port(
    clk_in      : in  std_Logic;  -- System Clock input
	 sys_clk     : out std_logic;  -- System Clock Out    (1/1)
	 vga_clk     : out std_logic;  -- VGA Pixel Clock Out (1/2)
    cpu_clk     : out std_logic   -- CPU Clock Out       (1/4)
  );
end entity;

architecture RTL of clock_div is

signal div_clk     : std_logic;
signal div_count   : std_logic_vector(1 downto 0);

component IBUFG
  port (
		i: in  std_logic;
		o: out std_logic
  );
end component;

component BUFG 
  port (
		i: in  std_logic;
		o: out std_logic
  );
end component;


--
-- Start instantiation
--
begin

--
-- 50.0MHz  system clock
--
sys_clk_buffer : IBUFG 
  port map(
    i => clk_in,
	 o => div_clk
  );

--
-- 25 MHz VGA clock output
--
vga_clk_buffer : BUFG 
  port map(
    i => div_count(0),
	 o => vga_clk
  );

--
-- 12.5MHz CPU clock 
--
cpu_clk_buffer : BUFG 
  port map(
    i => div_count(1),
	 o => cpu_clk
  );

--
-- Clock divider
--
clock_div : process( div_clk )
begin
  if rising_edge( div_clk) then
    div_count <= div_count + "01";
  end if;
  sys_clk <= div_clk;
end process;

end architecture;
 