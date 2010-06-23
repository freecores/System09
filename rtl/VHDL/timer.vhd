--===========================================================================----
--
--  S Y N T H E Z I A B L E    timer - 9 bit timer
--
--  www.OpenCores.Org - September 2003
--  This core adheres to the GNU public license  
--
-- File name      : timer.vhd
--
-- Purpose        : 8 bit timer module for System 09
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--
-- Uses           : None
--
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--
--===========================================================================----
--
-- Revision History:
--===========================================================================--
--
-- Version 0.1 - 6 Sept 2002 - John Kent
-- converted to a single timer 
-- made syncronous with system clock
--
-- Version 1.0 - 6 Sept 2003 - John Kent
-- Realeased to open Cores
-- changed Clock Edge
--
-- Version 2.0 - 5th February 2008 - John Kent
-- removed Timer inputs and outputs
-- made into a simple 8 bit interrupt down counter
--
--===========================================================================
--
-- Register Addressing:
-- addr=0 rw=1 down count
-- addr=0 rw=0 preset count
-- addr=1 rw=1 status
-- addr=1 rw=0 control
--
-- Control register
-- b0 = counter enable
-- b7 = interrupt enable
--
-- Status register
-- b7 = interrupt flag
--
-- Operation:
-- Write count to counter register
-- Enable counter by setting bit 0 of the control register
-- enable interrupts by setting bit 7 of the control register
-- Counter will count down to zero
-- when it reaches zero the terminal flag is set
-- if the interrupt is enabled an interrupt is generated
-- The interrupt may be disabled by writing a 0 to bit 7 of the control register
-- or by loading a new down count into the counter register.
-- 
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity timer is
	port (	
	 clk        : in  std_logic;
    rst        : in  std_logic;
    cs         : in  std_logic;
    rw         : in  std_logic;
    addr       : in  std_logic;
    data_in    : in  std_logic_vector(7 downto 0);
	 data_out   : out std_logic_vector(7 downto 0);
	 irq        : out std_logic
  );
end;

architecture rtl of timer is
signal timer_ctrl  : std_logic_vector(7 downto 0);
signal timer_stat  : std_logic_vector(7 downto 0);
signal timer_count : std_logic_vector(7 downto 0);
signal timer_term  : std_logic; -- Timer terminal count
--
-- control/status register bits
--
constant T_enab   : integer := 0; -- 0=disable, 1=enabled
constant T_irq    : integer := 7; -- 0=disabled, 1-enabled

begin

--------------------------------
--
-- write control registers
-- doesn't do anything yet
--
--------------------------------
timer_write : process( clk, rst, cs, rw, addr, data_in,
                       timer_ctrl, timer_term, timer_count )
begin
  if rst = '1' then
	   timer_count <= "00000000";
		timer_ctrl  <= "00000000";
		timer_term  <= '0';
  elsif clk'event and clk = '0' then
    if cs = '1' and rw = '0' then
	   if addr='0' then
		  timer_count <= data_in;
		  timer_term  <= '0';
	   else
		  timer_ctrl <= data_in;
		end if;
	 else
	   if (timer_ctrl(T_enab) = '1') then
		  if (timer_count = "00000000" ) then
		    timer_term <= '1';
        else
          timer_count <= timer_count - 1;
		  end if;
		end if;
    end if;
  end if;
end process;

--
-- timer data output mux
--
timer_read : process( addr, timer_count, timer_stat )
begin
  if addr='0' then
    data_out <= timer_count;
  else
    data_out <= timer_stat;
  end if;
end process;

--
-- read timer strobe to reset interrupts
--
timer_interrupt : process( timer_term, timer_ctrl )
begin
	 irq <= timer_term and timer_ctrl( T_irq );
end process;

  --
  -- timer status register
  --
  timer_status : process( timer_ctrl, timer_term )
  begin
    timer_stat(6 downto 0) <= timer_ctrl(6 downto 0);
    timer_stat(T_irq) <= timer_term;
  end process;

end rtl;
	
