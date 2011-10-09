--===========================================================================--
--                                                                           --
--                        VHDL 6850 ACIA TestBench                           --
--                                                                           --
--===========================================================================--
--
--
-- File name      : ACIA_tb.vhd
--
-- Entity name    : ACIA6850_testbench
--
-- Purpose        : VHDL testbench for acia6850
--
-- Dependencies   : ieee.std_logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Author         : John E. Kent
--
-- Email          : dilbert57@opencores.org      
--
-- Web            : http://opencores.org/project,system09
--
--  Copyright (C) 2007 - 2011 John Kent
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
--===========================================================================--
--                                                                           --
--                                Revision History                           --
--                                                                           --
--===========================================================================--
--
-- Rev  Date        Author     Notes 
-- 0.1  2007-02-06  John Kent  Initial Version
-- 0.2  2011-10-09  John Kent  Renamed acia_6850 to acia6850
--
-------------------------------------------------------------------------------
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity ACIA6850_testbench is
end ACIA6850_testbench;

-------------------------------------------------------------------------------
-- Architecture for ACIA 6850 Unit
-------------------------------------------------------------------------------
architecture behavior of ACIA6850_testbench is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- CPU Interface signals
  signal SysClk       : Std_Logic;
  signal uart_reset   : Std_Logic;
  signal uart_cs      : Std_Logic;
  signal uart_rw      : Std_Logic;
  signal uart_addr    : Std_Logic;
  signal uart_data_in : Std_Logic_Vector(7 downto 0);
  signal uart_data_out: Std_Logic_Vector(7 downto 0);
  signal uart_irq     : Std_Logic;
  signal rxclk        : Std_Logic;
  signal txclk        : Std_Logic;
  signal rxbit        : Std_Logic;
  signal txbit        : Std_Logic;
  signal dcd_n        : Std_Logic;
  signal cts_n        : Std_Logic;
  signal rts_n        : Std_Logic;

-----------------------------------------------------------------
--
-- ACIA 6850 UART
--
-----------------------------------------------------------------
component ACIA6850
  port (
     --
	  -- CPU signals
	  --
     clk      : in  std_logic;  -- System Clock
     rst      : in  std_logic;  -- Reset input (active high)
     cs       : in  std_logic;  -- miniUART Chip Select
     rw       : in  std_logic;  -- Read / Not Write
     addr     : in  std_logic;  -- Register Select
     data_in  : in  std_logic_vector(7 downto 0); -- Data Bus In 
     data_out : out std_logic_vector(7 downto 0); -- Data Bus Out
     irq      : out std_logic;  -- Interrupt
     --
	  -- Uart Signals
	  --
     RxC      : in  std_logic;  -- Receive Baud Clock
     TxC      : in  std_logic;  -- Transmit Baud Clock
     RxD      : in  std_logic;  -- Receive Data
     TxD      : out std_logic;  -- Transmit Data
	  DCD_n    : in  std_logic;  -- Data Carrier Detect
     CTS_n    : in  std_logic;  -- Clear To Send
     RTS_n    : out std_logic );  -- Request To send
end component; --================== End of entity ==============================--

begin

  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_acia  : ACIA6850 port map (
    clk       => SysClk,
	 rst       => uart_reset,
    cs        => uart_cs,
	 rw        => uart_rw,
    addr      => uart_addr,
	 data_in   => uart_data_in,
	 data_out  => uart_data_out,
    irq       => uart_irq,
	 RxC       => rxclk,
	 TxC       => txclk,
	 RxD       => rxbit,
	 TxD       => txbit,
	 DCD_n     => dcd_n,
	 CTS_n     => cts_n,
	 RTS_n     => rts_n
	 );


  -- *** Test Bench - User Defined Section ***
   tb : PROCESS
	variable count : integer;
   BEGIN

   cts_n <= '0';
	dcd_n <= '0';

		for count in 0 to 4096 loop
		   if (count mod 16) = 0 then
		     rxclk <= '1';
			  txclk <= '1'; 
		   elsif (count mod 16) = 8 then
		     rxclk <= '0';
			  txclk <= '0'; 
         end if;

			case count is
			when 0 =>
				uart_reset <= '1';
 		      uart_cs <= '0';
				uart_rw <= '1';
				uart_addr <= '0';
				uart_data_in <= "00000000";
				rxbit <= '1';
			when 1 =>
				uart_reset <= '0';
			when 3 =>
 		      uart_cs <= '1';
				uart_rw <= '0'; -- write control
				uart_addr <= '0';
				uart_data_in <= "00010001";
			when 4 =>
 		      uart_cs <= '0';
				uart_rw <= '1';
				uart_addr <= '0';
				uart_data_in <= "00000000";
			when 5 =>
 		      uart_cs <= '1';
				uart_rw <= '0'; -- write data
				uart_addr <= '1';
				uart_data_in <= "01010101";
			when 6 =>
 		      uart_cs <= '0';
				uart_rw <= '1';
				uart_addr <= '1';
				uart_data_in <= "00000000";
			when 256 =>
            rxbit <= '0'; -- start
			when 512 =>
			   rxbit <= '1'; -- bit 0
			when 768 =>
            rxbit <= '0'; -- bit 1
			when 1024 =>
			   rxbit <= '1'; -- bit 2
			when 1280 =>
            rxbit <= '1'; -- bit3
			when 1536 =>
			   rxbit <= '0'; -- bit 4
			when 1792 =>
            rxbit <= '0'; -- bit 5
			when 2048 =>
			   rxbit <= '1'; -- bit 6
			when 2304 =>
            rxbit <= '0'; -- bit 7
			when 2560 =>
			   rxbit <= '1'; -- stop 1
			when 2816 =>
			   rxbit <= '1'; -- stop 2
			when 3100 =>
 		      uart_cs <= '1';
				uart_rw <= '1'; -- read control
				uart_addr <= '0';
			when 3101 =>
 		      uart_cs <= '0';
				uart_rw <= '1';
				uart_addr <= '0';
			when 3102 =>
 		      uart_cs <= '1';
				uart_rw <= '1'; -- read data
				uart_addr <= '1';
			when 3103 =>
 		      uart_cs <= '0';
				uart_rw <= '1';
				uart_addr <= '1';
			when others =>
			   null;
			end case;
			SysClk <= '1';
			wait for 40 ns;
			SysClk <= '0';
			wait for 40 ns;
		end loop;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***

end behavior; --===================== End of architecture =======================--

