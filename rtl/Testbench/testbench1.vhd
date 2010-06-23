--===========================================================================----
--
--  T E S T B E N C H    tesetbench1 - CPU09 Testbench.
--
--  www.OpenCores.Org - September 2003
--  This core adheres to the GNU public license  
--
-- File name      : Testbench1.vhd
--
-- Purpose        : cpu09 Microprocessor Test Bench 1
--                  Contains ROM to print out "Hello World"
--                  on a none existant Uart
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : cpu09    (cpu09.vhd)      CPU core
--                   
-- Author         : John E. Kent
--                  dilbert57@opencores.org      
--
--===========================================================================----
--
-- Revision History:
--===========================================================================--
--
-- Version 0.1 - 12st April 2003 - John Kent 
-- First version
--
-- Version 1.0- 6 Sep 2003 - John Kent
-- Initial release to Open Cores
--
-- Version 1.1 - 25th Jan 2004 - John Kent
-- removed "test_alu" and "test_cc"
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
-- library work;
--   use work.UART_Def.all;
--   use work.typedefines.all;
--   use work.memory.all;

entity my_testbench is
end my_testbench;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of my_testbench is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- CPU Interface signals
  signal SysClk      : Std_Logic;
  signal cpu_reset   : Std_Logic;
  signal cpu_rw      : Std_Logic;
  signal cpu_vma     : Std_Logic;
  signal cpu_addr    : Std_Logic_Vector(15 downto 0);
  signal cpu_data_in : Std_Logic_Vector(7 downto 0);
  signal cpu_data_out: Std_Logic_Vector(7 downto 0);
  signal cpu_irq     : Std_Logic;
  signal cpu_nmi     : std_logic;
  signal cpu_firq    : Std_Logic;


  constant width   : integer := 8;
  constant memsize : integer := 64;

  type rom_array is array(0 to memsize-1) of std_logic_vector(width-1 downto 0);

  constant rom_data : rom_array :=
  (
    "10001110", "11111000", "00101000", -- F800 - 8E F828  RESET LDX #MSG
	 "10000110", "00010001",             -- F803 - 86 11          LDA #$11
	 "10110111", "11100000", "00000100", -- F805 - B7 E004        STA UARTCR
    "10110110", "11100000", "00000100", -- F808 - B6 E004  POLL1 LDA UARTCR
	 "10000101", "00000010",             -- F80B - 85 02          BITA #TXBE
	 "00100110", "11111001",             -- F80D - 26 F9          BNE POLL1
	 "10100110", "10000000",             -- F80F - A6 80          LDA ,X+
	 "00100111", "00000110",             -- F811 - 27 06          BEQ POLL2
	 "00010010",                         -- F813 - 12             NOP
	 "10110111", "11100000", "00000101", -- F814 - B7 E005        STA UARTDR
    "00100110", "11101111",             -- F817 - 26 EF          BNE POLL1
	 "10110110", "11100000", "00000100", -- F819 - B6 E004  POLL2 LDA UARTCR
	 "10000101", "00000001",             -- F81C - 85 01          BITA #RXBF
	 "00100111", "11111001",             -- F81E - 27 F9          BEQ POLL2
	 "10110110", "11100000", "00000101", -- F820 - B6 E005        LDA UARTDR
	 "01111110", "11111000", "00000000", -- F823 - 7E F800        JMP RESET
	 "00000000", "00000000",             -- F826 - 00 00          fcb $00,$00
    "01001000", "01100101", "01101100", -- F828 - 48 65 6c MSG   FCC "Hel"
	 "01101100", "01101111", "00100000", -- F82B - 6c 6f 20       FCC "lo "
	 "01010111", "01101111", "01110010", -- F82E - 57 6f 72       FCC "Wor"
    "01101100", "01100100",             -- F831 - 6c 64          FCC "ld"
    "00001010", "00001101", "00000000", -- F833 - 0a 0d 00       FCB LF,CR,NULL
    "00000000", "00000000",             -- F836 - 00 00          fcb null,null           
	 "11111000", "00000000",             -- F838 - F8 00          fdb $F800 ; Timer irq
	 "11111000", "00000000",             -- F83A - F8 00          fdb $F800 ; Ext IRQ
	 "11111000", "00000000",             -- F83C - F8 00          fcb $F800 ; SWI
	 "11111000", "00000000"              -- F83E - F8 00          fdb $F800 ; Reset
	 );

component cpu09
  port (    
	 clk:	     in	std_logic;
    rst:	     in	std_logic;
    rw:	     out	std_logic;		-- Asynchronous memory interface
    vma:	     out	std_logic;
    address:  out	std_logic_vector(15 downto 0);
    data_in:  in	std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0);
	 halt:     in  std_logic;
	 hold:     in  std_logic;
	 irq:      in  std_logic;
	 nmi:      in  std_logic;
	 firq:     in  std_logic
  );
end component cpu09;


begin
cpu : cpu09  port map (    
	 clk	     => SysClk,
    rst	     => cpu_reset,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    address   => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 halt      => '0',
	 hold      => '0',
	 irq       => cpu_irq,
	 nmi       => cpu_nmi,
	 firq      => cpu_firq
  );

  -- *** Test Bench - User Defined Section ***
   tb : PROCESS
	variable count : integer;
   BEGIN

	cpu_reset <= '0';
	SysClk <= '0';
   cpu_irq <= '0';
   cpu_nmi <= '0';
	cpu_firq <= '0';

		for count in 0 to 512 loop
			SysClk <= '0';
			if count = 0 then
				cpu_reset <= '1';
			elsif count = 1 then
				cpu_reset <= '0';
			end if;
			wait for 100 ns;
			SysClk <= '1';
			wait for 100 ns;
		end loop;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***


  rom : PROCESS( cpu_addr )
  begin
    cpu_data_in <= rom_data(conv_integer(cpu_addr(5 downto 0))); 
  end process;

end behavior; --===================== End of architecture =======================--

