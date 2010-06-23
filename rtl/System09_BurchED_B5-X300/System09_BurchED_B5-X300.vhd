--===========================================================================----
--
--  S Y N T H E Z I A B L E    System09 - SOC.
--
--  www.OpenCores.Org - September 2003
--  This core adheres to the GNU public license  
--
-- File name      : System09.vhd
--
-- Purpose        : Top level file for 6809 compatible system on a chip
--                  Designed with Xilinx XC2S300e Spartan 2+ FPGA.
--                  Implemented With BurchED B5-X300 FPGA board,
--                  B5-SRAM module, B5-CF module and B5-FPGA-CPU-IO module
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : 
--                  cpu09      (cpu09.vhd)      CPU core
--                  mon_rom    (sys09bug_rom2k_b4.vhd) Monitor ROM
--                  dat_ram    (datram.vhd)     Dynamic Address Translation
--                  acia_6850  (ACIA_6850.vhd) ACIA / MiniUART
--                             (ACIA_RX.vhd)
--                             (ACIA_TX.vhd)
--                  ACIA_Clock (ACIA_Clock.vhd) ACIA Baud Clock Divider
--                  keyboard   (keyboard.vhd)   PS/2 Keyboard Interface
--                  vdu8       (vdu8.vhd)       80 x 25 Video Display
--                  timer      (timer.vhd)      Timer module
--                  trap	    (trap.vhd)       Bus Trap interrupt
--                  ioport     (ioport.vhd)     Parallel I/O port.
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--	Memory Map     :
-- $E000 - ACIA (SWTPc)
-- $E010 - Reserved for FD1771 FDC (SWTPc)
-- $E020 - Keyboard
-- $E030 - VDU
-- $E040 - Compact Flash
-- $E050 - Timer
-- $E060 - Bus trap
-- $E070 - Parallel I/O
-- $E080 - Reserved for 6821 PIA (?) (SWTPc)
-- $E090 - Reserved for 6840 PTM (?) (SWTPc)
--
--===========================================================================----
--
-- Revision History:
--===========================================================================--
-- Version 0.1 - 20 March 2003
-- Version 0.2 - 30 March 2003
-- Version 0.3 - 29 April 2003
-- Version 0.4 - 29 June 2003
--
-- Version 0.5 - 19 July 2003
-- prints out "Hello World"
--
-- Version 0.6 - 5 September 2003
-- Runs SBUG
--
-- Version 1.0- 6 Sep 2003 - John Kent
-- Inverted SysClk
-- Initial release to Open Cores
--
-- Version 1.1 - 17 Jan 2004 - John Kent
-- Updated miniUart.
--
-- Version 1.2 - 25 Jan 2004 - John Kent
-- removed signals "test_alu" and "test_cc" 
-- Trap hardware re-instated.
--
-- Version 1.3 - 11 Feb 2004 - John Kent
-- Designed forked off to produce System09_VDU
-- Added VDU component
--	VDU runs at 25MHz and divides the clock by 2 for the CPU
-- UART Runs at 57.6 Kbps
--
-- Version 1.4 - 21 Nov 2004 - John Kent
-- Changes to make compatible with Spartan3 starter kit version
-- Designed to run with a 50MHz clock input.
-- the VDU divides 50 MHz to generate a 
-- 25 MHz VDU Pixel Clock and a 12.5 MHz CPU clock
-- Changed Monitor ROM signals to make it look like
-- a standard 2K memory block
-- Re-assigned I/O port assignments so it is possible to run KBUG9
-- $E000 - ACIA
-- $E010 - Keyboard
-- $E020 - VDU
-- $E030 - Compact Flash
-- $E040 - Timer
-- $E050 - Bus trap
-- $E060 - Parallel I/O
--
-- Version 1.5 - 3rd February 2007 - John Kent
-- Changed VDU8 to use external clock divider
-- renamed miniUART to ACIA_6850
-- Memory decoding of ROM & IO now uses DAT
--
-- Version 1.6 - 7th Februaury 2007 - John Kent
-- Made ACIA Clock generator an external component
-- Added Generics to VDU and Keyboard
-- Changed decoding
--
-- Version 1.7 - 20th May 2007 - John Kent
-- Added 4 wait states to CF access
-- Removed DAT memory map control of ROM & IO
-- to allow for full use of RAM as a RAM disk.
-- Mapped in all 16 bits of the CF data bus.
-- 
--===========================================================================
--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;

entity System09 is
  port(
    SysClk      : in  Std_Logic;  -- System Clock input
	 Reset_n     : in  Std_logic;  -- Master Reset input (active low)
    LED         : out std_logic;  -- Diagnostic LED Flasher

    -- Memory Interface signals
    ram_csn     : out Std_Logic;
    ram_wrln    : out Std_Logic;
    ram_wrun    : out Std_Logic;
    ram_addr    : out Std_Logic_Vector(16 downto 0);
    ram_data    : inout Std_Logic_Vector(15 downto 0);

	 -- Stuff on the peripheral board

 	 -- PS/2 Keyboard
	 kb_clock    : inout Std_logic;
	 kb_data     : inout Std_Logic;

	 -- PS/2 Mouse interface
--	 mouse_clock : in  Std_Logic;
--	 mouse_data  : in  Std_Logic;

	 -- Uart Interface
    rxbit       : in  Std_Logic;
	 txbit       : out Std_Logic;
    rts_n       : out Std_Logic;
    cts_n       : in  Std_Logic;

	 -- CRTC output signals
	 v_drive     : out Std_Logic;
    h_drive     : out Std_Logic;
    blue_lo     : out std_logic;
    blue_hi     : out std_logic;
    green_lo    : out std_logic;
    green_hi    : out std_logic;
    red_lo      : out std_logic;
    red_hi      : out std_logic;
--	   buzzer      : out std_logic;

-- Compact Flash
    cf_rst_n     : out std_logic;
    cf_cs0_n     : out std_logic;
    cf_cs1_n     : out std_logic;
    cf_rd_n      : out std_logic;
    cf_wr_n      : out std_logic;
    cf_a         : out std_logic_vector(2 downto 0);
    cf_d         : inout std_logic_vector(15 downto 0);

-- Parallel I/O port
    porta        : inout std_logic_vector(7 downto 0);
    portb        : inout std_logic_vector(7 downto 0);

-- CPU bus
	 bus_clk      : out std_logic;
	 bus_reset    : out std_logic;
	 bus_rw       : out std_logic;
	 bus_cs       : out std_logic;
    bus_addr     : out std_logic_vector(15 downto 0);
	 bus_data     : inout std_logic_vector(7 downto 0);

-- timer
    timer_out    : out std_logic
	 );
end System09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of System09 is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_Clock_Frequency  : integer := 50000000;  -- FPGA System Clock
  constant PIX_Clock_Frequency  : integer := 25000000;  -- VGA Pixel Clock
  constant CPU_Clock_Frequency  : integer := 12500000;  -- CPU Clock
  constant BAUD_Rate            : integer := 57600;	  -- Baud Rate
  constant ACIA_Clock_Frequency : integer := BAUD_Rate * 16;

  type hold_state_type is ( hold_release_state, hold_request_state );

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- Monitor ROM
  signal rom_data_out  : Std_Logic_Vector(7 downto 0);
  signal rom_cs        : std_logic;

  -- UART Interface signals
  signal uart_data_out : Std_Logic_Vector(7 downto 0);  
  signal uart_cs       : Std_Logic;
  signal uart_irq      : Std_Logic;
  signal uart_clk       : Std_Logic;
  signal DCD_n         : Std_Logic;

  -- timer
  signal timer_data_out : std_logic_vector(7 downto 0);
  signal timer_cs    : std_logic;
  signal timer_irq   : std_logic;

  -- trap
  signal trap_cs         : std_logic;
  signal trap_data_out   : std_logic_vector(7 downto 0);
  signal trap_irq        : std_logic;

  -- Parallel I/O port
  signal ioport_data_out : std_logic_vector(7 downto 0);
  signal ioport_cs    : std_logic;

  -- compact flash port
  signal cf_data_out : std_logic_vector(7 downto 0);
  signal cf_cs       : std_logic;
  signal cf_rd       : std_logic;
  signal cf_wr       : std_logic;
  signal cf_hold     : std_logic;
  signal cf_release  : std_logic;
  signal cf_count    : std_logic_vector(3 downto 0);
  signal cf_hold_state : hold_state_type;

  -- keyboard port
  signal keyboard_data_out : std_logic_vector(7 downto 0);
  signal keyboard_cs       : std_logic;
  signal keyboard_irq      : std_logic;

  -- RAM
  signal ram_cs      : std_logic; -- memory chip select
  signal ram_wrl     : std_logic; -- memory write lower
  signal ram_wru     : std_logic; -- memory write upper
  signal ram_data_out    : std_logic_vector(7 downto 0);

  -- CPU Interface signals
  signal cpu_reset    : Std_Logic;
  signal cpu_clk      : Std_Logic;
  signal cpu_rw       : std_logic;
  signal cpu_vma      : std_logic;
  signal cpu_halt     : std_logic;
  signal cpu_hold     : std_logic;
  signal cpu_firq     : std_logic;
  signal cpu_irq      : std_logic;
  signal cpu_nmi      : std_logic;
  signal cpu_addr     : std_logic_vector(15 downto 0);
  signal cpu_data_in  : std_logic_vector(7 downto 0);
  signal cpu_data_out : std_logic_vector(7 downto 0);

  -- Dynamic address translation
  signal dat_cs       : std_logic;
  signal dat_addr     : std_logic_vector(7 downto 0);

  -- Video Display Unit
  signal pix_clk      : std_logic;
  signal vdu_cs       : std_logic;
  signal vdu_data_out : std_logic_vector(7 downto 0);
  signal vga_red      : std_logic;
  signal vga_green    : std_logic;
  signal vga_blue     : std_logic;

  -- Flashing Led test signals
  signal countL      : std_logic_vector(23 downto 0);
  signal clock_div   : std_logic_vector(1 downto 0);

-----------------------------------------------------------------
--
-- CPU09 CPU core
--
-----------------------------------------------------------------

component cpu09
  port (    
	 clk:	     in	std_logic;
    rst:      in	std_logic;
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


----------------------------------------
--
-- SBUG Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
    port (
       clk   : in  std_logic;
       rst   : in  std_logic;
       cs    : in  std_logic;
       rw    : in  std_logic;
       addr  : in  std_logic_vector (10 downto 0);
       wdata : in  std_logic_vector (7 downto 0);
       rdata : out std_logic_vector (7 downto 0)
    );
end component;


----------------------------------------
--
-- Dynamic Address Translation Registers
--
----------------------------------------
component dat_ram
  port (
    clk:      in  std_logic;
	 rst:      in  std_logic;
	 cs:       in  std_logic;
	 rw:       in  std_logic;
	 addr_lo:  in  std_logic_vector(3 downto 0);
	 addr_hi:  in  std_logic_vector(3 downto 0);
    data_in:  in  std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0)
	 );
end component;

-----------------------------------------------------------------
--
-- 6850 ACIA/UART
--
-----------------------------------------------------------------

component ACIA_6850
  port (
     clk      : in  Std_Logic;  -- System Clock
     rst      : in  Std_Logic;  -- Reset input (active high)
     cs       : in  Std_Logic;  -- miniUART Chip Select
     rw       : in  Std_Logic;  -- Read / Not Write
     irq      : out Std_Logic;  -- Interrupt
     Addr     : in  Std_Logic;  -- Register Select
     DataIn   : in  Std_Logic_Vector(7 downto 0); -- Data Bus In 
     DataOut  : out Std_Logic_Vector(7 downto 0); -- Data Bus Out
     RxC      : in  Std_Logic;  -- Receive Baud Clock
     TxC      : in  Std_Logic;  -- Transmit Baud Clock
     RxD      : in  Std_Logic;  -- Receive Data
     TxD      : out Std_Logic;  -- Transmit Data
	  DCD_n    : in  Std_Logic;  -- Data Carrier Detect
     CTS_n    : in  Std_Logic;  -- Clear To Send
     RTS_n    : out Std_Logic );  -- Request To send
end component;

-----------------------------------------------------------------
--
-- ACIA Clock divider
--
-----------------------------------------------------------------

component ACIA_Clock
  generic (
     SYS_Clock_Frequency  : integer :=  SYS_Clock_Frequency;
	  ACIA_Clock_Frequency : integer := ACIA_Clock_Frequency
  );   
  port (
     clk      : in  Std_Logic;  -- System Clock Input
	  ACIA_clk : out Std_logic   -- ACIA Clock output
  );
end component;

----------------------------------------
--
-- Timer module
--
----------------------------------------

component timer
  port (
     clk       : in std_logic;
     rst       : in std_logic;
     cs        : in std_logic;
     rw        : in std_logic;
     addr      : in std_logic;
     data_in   : in std_logic_vector(7 downto 0);
	  data_out  : out std_logic_vector(7 downto 0);
	  irq       : out std_logic;
     timer_in  : in std_logic;
	  timer_out : out std_logic
	  );
end component;

------------------------------------------------------------
--
-- Bus Trap logic
--
------------------------------------------------------------

component trap
	port (	
	 clk        : in  std_logic;
    rst        : in  std_logic;
    cs         : in  std_logic;
    rw         : in  std_logic;
    vma        : in  std_logic;
    addr       : in  std_logic_vector(15 downto 0);
    data_in    : in  std_logic_vector(7 downto 0);
	 data_out   : out std_logic_vector(7 downto 0);
	 irq        : out std_logic
  );
end component;

----------------------------------------
--
-- Dual 8 bit Parallel I/O module
--
----------------------------------------
component ioport
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(1 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0);
	 porta_io  : inout std_logic_vector(7 downto 0);
	 portb_io  : inout std_logic_vector(7 downto 0)
	 );
end component;

----------------------------------------
--
-- PS/2 Keyboard
--
----------------------------------------

component keyboard
  generic(
  KBD_Clock_Frequency : integer := CPU_Clock_Frequency
  );
  port(
  clk             : in    std_logic;
  rst             : in    std_logic;
  cs              : in    std_logic;
  rw              : in    std_logic;
  addr            : in    std_logic;
  data_in         : in    std_logic_vector(7 downto 0);
  data_out        : out   std_logic_vector(7 downto 0);
  irq             : out   std_logic;
  kbd_clk         : inout std_logic;
  kbd_data        : inout std_logic
  );
end component;

----------------------------------------
--
-- Video Display Unit.
--
----------------------------------------
component vdu8
      generic(
        VDU_CLOCK_FREQUENCY    : integer := CPU_Clock_Frequency; -- HZ
        VGA_CLOCK_FREQUENCY    : integer := PIX_Clock_Frequency; -- HZ
	     VGA_HOR_CHARS          : integer := 80; -- CHARACTERS
	     VGA_VER_CHARS          : integer := 25; -- CHARACTERS
	     VGA_PIXELS_PER_CHAR    : integer := 8;  -- PIXELS
	     VGA_LINES_PER_CHAR     : integer := 16; -- LINES
	     VGA_HOR_BACK_PORCH     : integer := 40; -- PIXELS
	     VGA_HOR_SYNC           : integer := 96; -- PIXELS
	     VGA_HOR_FRONT_PORCH    : integer := 24; -- PIXELS
	     VGA_VER_BACK_PORCH     : integer := 13; -- LINES
	     VGA_VER_SYNC           : integer := 1;  -- LINES
	     VGA_VER_FRONT_PORCH    : integer := 36  -- LINES
      );
      port(
		-- control register interface
      vdu_clk      : in  std_logic;	 -- CPU Clock - 12.5MHz
      vdu_rst      : in  std_logic;
		vdu_cs       : in  std_logic;
		vdu_rw       : in  std_logic;
		vdu_addr     : in  std_logic_vector(2 downto 0);
      vdu_data_in  : in  std_logic_vector(7 downto 0);
      vdu_data_out : out std_logic_vector(7 downto 0);

      -- vga port connections
		vga_clk      : in  std_logic;	-- VGA Pixel Clock - 25 MHz
      vga_red_o    : out std_logic;
      vga_green_o  : out std_logic;
      vga_blue_o   : out std_logic;
      vga_hsync_o  : out std_logic;
      vga_vsync_o  : out std_logic
   );
end component;


component BUFG 
  port (
		i: in  std_logic;
		o: out std_logic
  );
end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

----------------------------------------
--
-- CPU09 CPU Core
--
----------------------------------------
my_cpu : cpu09  port map (    
	 clk	     => cpu_clk,
    rst       => cpu_reset,
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

----------------------------------------
--
-- SBUG / KBUG / SYS09BUG Monitor ROM
--
----------------------------------------
my_rom : mon_rom port map (
       clk   => cpu_clk,
		 rst   => cpu_reset,
		 cs    => rom_cs,
		 rw    => '1',
       addr  => cpu_addr(10 downto 0),
		 wdata => cpu_data_out,
       rdata => rom_data_out
    );

----------------------------------------
--
-- Dynamic Address Translation Registers
--
----------------------------------------
my_dat : dat_ram port map (
    clk        => cpu_clk,
	 rst        => cpu_reset,
	 cs         => dat_cs,
	 rw         => cpu_rw,
	 addr_hi    => cpu_addr(15 downto 12),
	 addr_lo    => cpu_addr(3 downto 0),
    data_in    => cpu_data_out,
	 data_out   => dat_addr(7 downto 0)
	 );

----------------------------------------
--
-- ACIA/UART Serial interface
--
----------------------------------------
my_ACIA  : ACIA_6850 port map (
	 clk	     => cpu_clk,
	 rst       => cpu_reset,
    cs        => uart_cs,
	 rw        => cpu_rw,
    irq       => uart_irq,
    Addr      => cpu_addr(0),
	 Datain    => cpu_data_out,
	 DataOut   => uart_data_out,
	 RxC       => uart_clk,
	 TxC       => uart_clk,
	 RxD       => rxbit,
	 TxD       => txbit,
	 DCD_n     => dcd_n,
	 CTS_n     => cts_n,
	 RTS_n     => rts_n
	 );

----------------------------------------
--
-- ACIA Clock
--
----------------------------------------
my_ACIA_Clock : ACIA_Clock
  generic map(
    SYS_Clock_Frequency  => SYS_Clock_Frequency,
	 ACIA_Clock_Frequency => ACIA_Clock_Frequency
  ) 
  port map(
    clk        => SysClk,
    acia_clk   => uart_clk
  ); 

----------------------------------------
--
-- PS/2 Keyboard Interface
--
----------------------------------------
my_keyboard : keyboard
   generic map (
	KBD_Clock_Frequency => CPU_Clock_frequency
	) 
   port map(
	clk          => cpu_clk,
	rst          => cpu_reset,
	cs           => keyboard_cs,
	rw           => cpu_rw,
	addr         => cpu_addr(0),
	data_in      => cpu_data_out(7 downto 0),
	data_out     => keyboard_data_out(7 downto 0),
	irq          => keyboard_irq,
	kbd_clk      => kb_clock,
	kbd_data     => kb_data
	);

----------------------------------------
--
-- Video Display Unit instantiation
--
----------------------------------------
my_vdu : vdu8 
  generic map(
      VDU_CLOCK_FREQUENCY    => CPU_Clock_Frequency, -- HZ
      VGA_CLOCK_FREQUENCY    => PIX_Clock_Frequency, -- HZ
	   VGA_HOR_CHARS          => 80, -- CHARACTERS
	   VGA_VER_CHARS          => 25, -- CHARACTERS
	   VGA_PIXELS_PER_CHAR    => 8,  -- PIXELS
	   VGA_LINES_PER_CHAR     => 16, -- LINES
	   VGA_HOR_BACK_PORCH     => 40, -- PIXELS
	   VGA_HOR_SYNC           => 96, -- PIXELS
	   VGA_HOR_FRONT_PORCH    => 24, -- PIXELS
	   VGA_VER_BACK_PORCH     => 13, -- LINES
	   VGA_VER_SYNC           => 1,  -- LINES
	   VGA_VER_FRONT_PORCH    => 36  -- LINES
  )
  port map(

		-- Control Registers
		vdu_clk       => cpu_clk,					 -- 12.5 MHz System Clock in
      vdu_rst       => cpu_reset,
		vdu_cs        => vdu_cs,
		vdu_rw        => cpu_rw,
		vdu_addr      => cpu_addr(2 downto 0),
		vdu_data_in   => cpu_data_out,
		vdu_data_out  => vdu_data_out,

      -- vga port connections
      vga_clk       => pix_clk,					 -- 25 MHz VDU pixel clock
      vga_red_o     => vga_red,
      vga_green_o   => vga_green,
      vga_blue_o    => vga_blue,
      vga_hsync_o   => h_drive,
      vga_vsync_o   => v_drive
   );

----------------------------------------
--
-- Timer Module
--
----------------------------------------
my_timer  : timer port map (
    clk       => cpu_clk,
	 rst       => cpu_reset,
    cs        => timer_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(0),
	 data_in   => cpu_data_out,
	 data_out  => timer_data_out,
    irq       => timer_irq,
	 timer_in  => CountL(5),
	 timer_out => timer_out
    );

----------------------------------------
--
-- Bus Trap Interrupt logic
--
----------------------------------------
my_trap : trap port map (	
	 clk        => cpu_clk,
    rst        => cpu_reset,
    cs         => trap_cs,
    rw         => cpu_rw,
	 vma        => cpu_vma,
    addr       => cpu_addr,
    data_in    => cpu_data_out,
	 data_out   => trap_data_out,
	 irq        => trap_irq
    );

----------------------------------------
--
-- Parallel I/O Port
--
----------------------------------------
my_ioport  : ioport port map (
	 clk       => cpu_clk,
    rst       => cpu_reset,
    cs        => ioport_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(1 downto 0),
    data_in   => cpu_data_out,
	 data_out  => ioport_data_out,
	 porta_io  => porta,
	 portb_io  => portb
	 );

--
-- 12.5 MHz CPU clock
--
cpu_clk_buffer : BUFG port map(
    i => clock_div(1),
	 o => cpu_clk
    );
	 	 
--
-- 25 MHz VGA Pixel clock
--
vga_clk_buffer : BUFG port map(
    i => clock_div(0),
	 o => pix_clk
    );	 
	 
----------------------------------------------------------------------
--
-- Process to decode memory map
--
----------------------------------------------------------------------

mem_decode: process( cpu_clk, Reset_n, dat_addr,
                     cpu_addr, cpu_rw, cpu_vma,
					      rom_data_out, 
							ram_data_out,
					      cf_data_out,
						   timer_data_out, 
							trap_data_out, 
							ioport_data_out,
						   uart_data_out,
							keyboard_data_out,
							vdu_data_out,
							bus_data )
variable decode_addr : std_logic_vector(4 downto 0);
begin
    decode_addr := dat_addr(3 downto 0) & cpu_addr(11);
--    decode_addr := cpu_addr(15 downto 11);

    if cpu_addr( 15 downto 8 ) = "11111111" then
	 		cpu_data_in <= rom_data_out;
			rom_cs      <= cpu_vma;              -- read ROM
			dat_cs      <= cpu_vma;              -- write DAT
			ram_cs      <= '0';
			uart_cs     <= '0';
			cf_cs       <= '0';
			timer_cs    <= '0';
			trap_cs     <= '0';
			ioport_cs   <= '0';
			keyboard_cs <= '0';
			vdu_cs      <= '0';
			bus_cs      <= '0';
	 else
      case decode_addr is
	   --
		-- SBUG/KBUG/SYS09BUG Monitor ROM $F800 - $FFFF
		--
		when "11111" => -- $F800 - $FFFF
		   cpu_data_in <= rom_data_out;
			rom_cs      <= cpu_vma;              -- read ROM
			dat_cs      <= '0';
			ram_cs      <= '0';
			uart_cs     <= '0';
			cf_cs       <= '0';
			timer_cs    <= '0';
			trap_cs     <= '0';
			ioport_cs   <= '0';
			keyboard_cs <= '0';
			vdu_cs      <= '0';
			bus_cs      <= '0';

      --
		-- IO Devices $E000 - $E7FF
		--
		when "11100" => -- $E000 - $E7FF
			rom_cs    <= '0';
		   dat_cs    <= '0';
			ram_cs    <= '0';
		   case cpu_addr(7 downto 4) is
			--
			-- UART / ACIA $E000
			--
			when "0000" => -- $E000
		     cpu_data_in <= uart_data_out;
			  uart_cs     <= cpu_vma;
			  cf_cs       <= '0';
			  timer_cs    <= '0';
			  trap_cs     <= '0';
			  ioport_cs   <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';
			  bus_cs      <= '0';

			--
			-- WD1771 FDC sites at $E010-$E01F
			--

         --
         -- Keyboard port $E020 - $E02F
			--
			when "0010" => -- $E020
           cpu_data_in <= keyboard_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
           timer_cs    <= '0';
			  trap_cs     <= '0';
			  ioport_cs   <= '0';
			  keyboard_cs <= cpu_vma;
			  vdu_cs      <= '0';
			  bus_cs      <= '0';

         --
         -- VDU port $E030 - $E03F
			--
			when "0011" => -- $E030
           cpu_data_in <= vdu_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
           timer_cs    <= '0';
			  trap_cs     <= '0';
			  ioport_cs   <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= cpu_vma;
			  bus_cs      <= '0';

         --
			-- Compact Flash $E040 - $E04F
			--
			when "0100" => -- $E040
           cpu_data_in <= cf_data_out;
			  uart_cs     <= '0';
           cf_cs       <= cpu_vma;
			  timer_cs    <= '0';
			  trap_cs     <= '0';
			  ioport_cs   <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';
			  bus_cs      <= '0';

         --
         -- Timer $E050 - $E05F
			--
			when "0101" => -- $E050
           cpu_data_in <= timer_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
           timer_cs    <= cpu_vma;
			  trap_cs     <= '0';
			  ioport_cs   <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';
			  bus_cs      <= '0';

         --
         -- Bus Trap Logic $E060 - $E06F
			--
			when "0110" => -- $E060
           cpu_data_in <= trap_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
           timer_cs    <= '0';
			  trap_cs     <= cpu_vma;
			  ioport_cs   <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';
			  bus_cs      <= '0';

         --
         -- I/O port $E070 - $E07F
			--
			when "0111" => -- $E070
           cpu_data_in <= ioport_data_out;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
           timer_cs    <= '0';
			  trap_cs     <= '0';
			  ioport_cs   <= cpu_vma;
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';
			  bus_cs      <= '0';

			when others => -- $E080 to $E7FF
           cpu_data_in <= bus_data;
			  uart_cs     <= '0';
			  cf_cs       <= '0';
			  timer_cs    <= '0';
			  trap_cs     <= '0';
			  ioport_cs   <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';
			  bus_cs      <= cpu_vma;
		   end case;
		--
		-- Everything else is RAM
		--
		when others =>
		  cpu_data_in <= ram_data_out;
		  rom_cs      <= '0';
		  dat_cs      <= '0';
		  ram_cs      <= cpu_vma;
		  uart_cs     <= '0';
		  cf_cs       <= '0';
		  timer_cs    <= '0';
		  trap_cs     <= '0';
		  ioport_cs   <= '0';
		  keyboard_cs <= '0';
		  vdu_cs      <= '0';
		  bus_cs      <= '0';
		end case;
	end if;
end process;


--
-- B5-SRAM Control
-- Processes to read and write memory based on bus signals
--
ram_process: process( cpu_clk, Reset_n,
                      cpu_addr, cpu_rw, cpu_vma, cpu_data_out,
					       dat_addr,
                      ram_cs, ram_wrl, ram_wru, ram_data_out )
begin
    ram_csn <= not( ram_cs and Reset_n );
	 ram_wrl  <= (not cpu_addr(0)) and (not cpu_rw) and cpu_clk;
	 ram_wrln <= not (ram_wrl);
    ram_wru  <= cpu_addr(0) and (not cpu_rw) and cpu_clk;
	 ram_wrun <= not (ram_wru);
	 ram_addr(16 downto 11) <= dat_addr(5 downto 0);
	 ram_addr(10 downto 0) <= cpu_addr(11 downto 1);

    if ram_wrl = '1' then
		ram_data(7 downto 0) <= cpu_data_out;
	 else
      ram_data(7 downto 0)  <= "ZZZZZZZZ";
	 end if;

	 if ram_wru = '1' then
		ram_data(15 downto 8) <= cpu_data_out;
	 else
      ram_data(15 downto 8)  <= "ZZZZZZZZ";
    end if;

	 if cpu_addr(0) = '1' then
      ram_data_out <= ram_data(15 downto 8);
	 else
      ram_data_out <= ram_data(7 downto 0);
    end if;
end process;

--
-- Compact Flash Control
--
compact_flash: process( Reset_n,
                 cpu_addr, cpu_rw, cpu_vma, cpu_data_out,
					  cf_cs, cf_rd, cf_wr, cf_d )
begin
	 cf_rst_n  <= Reset_n;
	 cf_cs0_n  <= not( cf_cs ) or cpu_addr(3);
	 cf_cs1_n  <= not( cf_cs and cpu_addr(3));
	 cf_wr     <= cf_cs and (not cpu_rw);
	 cf_rd     <= cf_cs and cpu_rw;
	 cf_wr_n   <= not cf_wr;
	 cf_rd_n   <= not cf_rd;
	 cf_a      <= cpu_addr(2 downto 0);
	 if cf_wr = '1' then
	   cf_d(7 downto 0)  <= cpu_data_out;
         cf_d(15 downto 8) <= (others => '0');
	 else
	   cf_d(7 downto 0)  <= (others => 'Z');
         cf_d(15 downto 8) <= (others => 'Z');
	 end if;
	 cf_data_out <= cf_d(7 downto 0);
end process;

--
-- Hold CF access	for a few cycles
--
cf_hold_proc: process( cpu_clk, Reset_n )
begin
    if Reset_n = '0' then
		 cf_release    <= '0';
		 cf_count      <= "0000";
	    cf_hold_state <= hold_release_state;
	 elsif cpu_clk'event and cpu_clk='0' then
	    case cf_hold_state is
		 when hold_release_state =>
          cf_release <= '0';
		    if cf_cs = '1' then
			    cf_count      <= "0011";
				 cf_hold_state <= hold_request_state;
			 end if;

		 when hold_request_state =>
		    cf_count <= cf_count - "0001";
			 if cf_count = "0000" then
             cf_release    <= '1';
				 cf_hold_state <= hold_release_state;
			 end if;
       when others =>
		    null;
       end case;
	 end if;
end process;

--
-- Interrupts and other bus control signals
--
interrupts : process( Reset_n, 
							 cf_cs, cf_hold, cf_release,
                      uart_irq, trap_irq, timer_irq, keyboard_irq
							 )
begin
    cf_hold   <= cf_cs and (not cf_release);
 	 cpu_reset <= not Reset_n; -- CPU reset is active high
    cpu_irq   <= uart_irq or keyboard_irq;
	 cpu_nmi   <= trap_irq;
	 cpu_firq  <= timer_irq;
	 cpu_halt  <= '0';
	 cpu_hold  <= cf_hold;
end process;

--
-- CPU bus signals
--
my_bus : process( cpu_clk, cpu_reset, cpu_rw, cpu_addr, cpu_data_out )
begin
	bus_clk   <= cpu_clk;
   bus_reset <= cpu_reset;
	bus_rw    <= cpu_rw;
   bus_addr  <= cpu_addr;
	if( cpu_rw = '1' ) then
	   bus_data <= "ZZZZZZZZ";
   else
	   bus_data <= cpu_data_out;
   end if;
end process;

  --
  -- flash led to indicate code is working
  --
my_LED_Flasher: process (cpu_clk, CountL )
begin
    if(cpu_clk'event and cpu_clk = '0') then
      countL <= countL + 1;			 
    end if;
	 LED <= countL(23);
	 dcd_n <= '0';
end process;

--
-- Clock divider
--
my_clock_divider: process( SysClk )
begin
	if SysClk'event and SysClk='0' then
		clock_div <= clock_div + "01";
	end if;
end process;
--
-- Assign VDU VGA colour output
-- only 8 colours are handled.
--
my_vga_out: process( vga_red, vga_green, vga_blue )
begin
	   red_lo   <= vga_red;
      red_hi   <= vga_red;
      green_lo <= vga_green;
      green_hi <= vga_green;
      blue_lo  <= vga_blue;
      blue_hi  <= vga_blue;
end process;

end rtl; --===================== End of architecture =======================--

