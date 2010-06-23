--===========================================================================--
--
-- MC6809 Microprocessor Test Bench 4
-- Test Software - SBUG ROM
--
--
-- John Kent 12st April 2003
--
-- Version 1.1 - 25th Jan 2004 - John Kent
-- removed "test_alu" and "test_cc"
--
--
-------------------------------------------------------------------------------
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use ieee.numeric_std.all;

entity my_testbench is
end my_testbench;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of my_testbench is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  signal cpu_irq    : std_Logic;
  signal cpu_firq   : std_logic;
  signal cpu_nmi    : std_logic;

  -- CPU Interface signals
  signal SysClk      : Std_Logic;
  signal cpu_reset   : Std_Logic;
  signal cpu_rw      : Std_Logic;
  signal cpu_vma     : Std_Logic;
  signal cpu_addr    : Std_Logic_Vector(15 downto 0);
  signal cpu_data_in : Std_Logic_Vector(7 downto 0);
  signal cpu_data_out: Std_Logic_Vector(7 downto 0);
  signal cpu_halt    : Std_logic;
  signal cpu_hold    : Std_logic;
  signal rom_data_out: Std_Logic_Vector(7 downto 0);
  signal ram_data_out: Std_Logic_Vector(7 downto 0);
  signal ram_cs      : Std_Logic;
 
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
end component;


component sbug_rom
    Port (
       MEMclk   : in  std_logic;
       MEMaddr  : in  std_logic_vector (10 downto 0);
       MEMrdata : out std_logic_vector (7 downto 0)
    );
end component;

component block_ram
    Port (
       MEMclk   : in  std_logic;
       MEMcs    : in  std_logic;
		 MEMrw    : in  std_logic;
       MEMaddr  : in  std_logic_vector (10 downto 0);
       MEMrdata : out std_logic_vector (7 downto 0);
       MEMwdata : in  std_logic_vector (7 downto 0)
    );
end component;

begin
my_cpu : cpu09  port map (    
	 clk	     => SysClk,
    rst	     => cpu_reset,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    address   => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 halt      => cpu_halt,
	 hold      => cpu_hold,
	 irq       => cpu_irq,
	 nmi       => cpu_nmi,
	 firq      => cpu_firq
  );


my_ram : block_ram port map (
       MEMclk   => SysClk,
		 MEMcs    => ram_cs,
		 MEMrw    => cpu_rw,
       MEMaddr  => cpu_addr(10 downto 0),
       MEMrdata => ram_data_out,
       MEMwdata => cpu_data_out
    );

my_rom : sbug_rom port map (
       MEMclk   => SysClk,
       MEMaddr  => cpu_addr(10 downto 0),
       MEMrdata => rom_data_out
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
   cpu_halt <= '0';
	cpu_hold <= '0';

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


  rom : PROCESS( cpu_addr, rom_data_out, ram_data_out )
  begin
    if( cpu_addr(15 downto 11) = "11111" ) then
      cpu_data_in <= rom_data_out;
		ram_cs <= '0';
 	 else
      cpu_data_in <= ram_data_out;
		ram_cs <= '1';
 	 end if;
  end process;

end behavior; --===================== End of architecture =======================--

