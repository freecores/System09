-- $Id: System09_Digilent_3S500E.vhd,v 1.4 2008-08-20 06:00:55 davidgb Exp $
--===========================================================================----
--
--  S Y N T H E Z I A B L E    System09 - SOC.
--
--  This core adheres to the GNU public license  
--
-- File name      : System09.vhd
--
-- Purpose        : Top level file for 6809 compatible system on a chip
--                  Designed with Xilinx XC3S500E Spartan 3E FPGA.
--                  Implemented With Digilent Xilinx Starter FPGA board,
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : mon_rom  (kbug_rom2k.vhd)       Monitor ROM
--                  cpu09    (cpu09.vhd)      CPU core
--                  miniuart (minitUART3.vhd) ACIA / MiniUART
--                           (rxunit3.vhd)
--                           (tx_unit3.vhd)
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
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
-- Inverted CLK_50MHZ
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
-- Version 2.0 - 2 September 2004 - John Kent
-- ported to Digilent Xilinx Spartan3 starter board
--	removed Compaact Flash and Trap Logic.
-- Replaced SBUG with KBug9s
--
-- Version 3.0 - 22 April 2006 - John Kent
-- Port to Digilent Spartan 3E Starter board
-- Removed keyboard, vdu, timer, and trap logic
-- added PIA with counters attached.
-- Uses 32Kbytes of internal Block RAM
--
-- Version 4.0 - 8th April 2007 - John kent
-- Added VDU and PS/2 keyboard
-- Updated miniUART to ACIA6850
-- Reduce monitor ROM to 2KB
-- Re-assigned I/O port assignments so it is possible to run KBUG9
-- $E000 - ACIA
-- $E020 - Keyboard
-- $E030 - VDU
-- $E040 - Compact Flash (not implemented)
-- $E050 - Timer
-- $E060 - Bus trap
-- $E070 - Parallel I/O
--
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity my_system09 is
  port(
    CLK_50MHZ     : in  Std_Logic;  -- System Clock input
    BTN_SOUTH     : in  Std_Logic;

 	 -- PS/2 Keyboard
	 PS2_CLK      : inout Std_logic;
	 PS2_DATA     : inout Std_Logic;

	 -- CRTC output signals
	 VGA_VSYNC     : out Std_Logic;
    VGA_HSYNC     : out Std_Logic;
    VGA_BLUE      : out std_logic;
    VGA_GREEN     : out std_logic;
    VGA_RED       : out std_logic;

	 -- Uart Interface
	 RS232_DCE_RXD : in  std_logic;
    RS232_DCE_TXD : out std_logic;
	 
	 -- LEDS & Switches
	 LED           : out std_logic_vector(7 downto 0)
	 );
end my_system09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture my_computer of my_system09 is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_Clock_Frequency  : integer := 50000000;  -- FPGA System Clock
  constant PIX_Clock_Frequency  : integer := 25000000;  -- VGA Pixel Clock
  constant CPU_Clock_Frequency  : integer := 25000000;  -- CPU Clock
  constant BAUD_Rate            : integer := 57600;	  -- Baud Rate
  constant ACIA_Clock_Frequency : integer := BAUD_Rate * 16;

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- BOOT ROM
  signal rom_cs         : Std_logic;
  signal rom_data_out   : Std_Logic_Vector(7 downto 0);

  -- UART Interface signals
  signal uart_data_out  : Std_Logic_Vector(7 downto 0);  
  signal uart_cs        : Std_Logic;
  signal uart_irq       : Std_Logic;
  signal uart_clk       : Std_Logic;
  signal rxbit          : Std_Logic;
  signal txbit          : Std_Logic;
  signal DCD_n          : Std_Logic;
  signal RTS_n          : Std_Logic;
  signal CTS_n          : Std_Logic;

  -- timer
  signal timer_data_out : std_logic_vector(7 downto 0);
  signal timer_cs       : std_logic;
  signal timer_irq      : std_logic;

  -- trap
  signal trap_cs        : std_logic;
  signal trap_data_out  : std_logic_vector(7 downto 0);
  signal trap_irq       : std_logic;

  -- PIA Interface signals
  signal pia_data_out   : Std_Logic_Vector(7 downto 0);  
  signal pia_cs         : Std_Logic;
  signal pia_irq_a      : Std_Logic;
  signal pia_irq_b      : Std_Logic;

  -- keyboard port
  signal keyboard_data_out : std_logic_vector(7 downto 0);
  signal keyboard_cs       : std_logic;
  signal keyboard_irq      : std_logic;

  -- Video Display Unit
  signal pix_clk      : std_logic;
  signal vdu_cs       : std_logic;
  signal vdu_data_out : std_logic_vector(7 downto 0);

  -- RAM
  signal ram_cs       : std_logic; -- memory chip select
  signal ram_data_out : std_logic_vector(7 downto 0);

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

  -- CLK_50MHZ clock divide by 2
  signal clock_div    : std_logic_vector(1 downto 0);
  signal SysClk       : std_logic;
  signal Reset_n      : std_logic;
  signal CountL       : std_logic_vector(23 downto 0);
  
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
-- Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
    Port (
       clk   : in  std_logic;
		 rst   : in  std_logic;
		 cs    : in  std_logic;
		 rw    : in  std_logic;
       addr  : in  std_logic_vector (10 downto 0);
       rdata : out std_logic_vector (7 downto 0);
       wdata : in  std_logic_vector (7 downto 0)
    );
end component;

----------------------------------------
--
-- Block RAM Monitor
--
----------------------------------------
component ram_32k
    Port (
       clk   : in  std_logic;
		 rst   : in  std_logic;
		 cs    : in  std_logic;
		 rw    : in  std_logic;
       addr  : in  std_logic_vector (14 downto 0);
       rdata : out std_logic_vector (7 downto 0);
       wdata : in  std_logic_vector (7 downto 0)
    );
end component;

-----------------------------------------------------------------
--
-- 6822 compatible PIA with counters
--
-----------------------------------------------------------------

component pia_timer
	port (	
	 clk       : in    std_logic;
    rst       : in    std_logic;
    cs        : in    std_logic;
    rw        : in    std_logic;
    addr      : in    std_logic_vector(1 downto 0);
    data_in   : in    std_logic_vector(7 downto 0);
	 data_out  : out   std_logic_vector(7 downto 0);
	 irqa      : out   std_logic;
	 irqb      : out   std_logic
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
	  irq       : out std_logic 
	  -- ;
     -- timer_in  : in std_logic;
	  -- timer_out : out std_logic
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
     i: in std_logic;
	  o: out std_logic
  );
end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

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

my_rom : mon_rom port map (
       clk   => cpu_clk,
		 rst   => cpu_reset,
		 cs    => rom_cs,
		 rw    => '1',
       addr  => cpu_addr(10 downto 0),
       rdata => rom_data_out,
       wdata => cpu_data_out
    );

my_ram : ram_32k port map (
       clk   => cpu_clk,
		 rst   => cpu_reset,
		 cs    => ram_cs,
		 rw    => cpu_rw,
       addr  => cpu_addr(14 downto 0),
       rdata => ram_data_out,
       wdata => cpu_data_out
    );

my_pia  : pia_timer port map (
	 clk	     => cpu_clk,
	 rst       => cpu_reset,
    cs        => pia_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(1 downto 0),
	 data_in   => cpu_data_out,
	 data_out  => pia_data_out,
    irqa      => pia_irq_a,
    irqb      => pia_irq_b
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
	kbd_clk      => PS2_CLK,
	kbd_data     => PS2_DATA
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
		vdu_clk       => cpu_clk,					 -- 25 MHz System Clock in
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
      vga_hsync_o   => vga_hsync,
      vga_vsync_o   => vga_vsync
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
    irq       => timer_irq
	 -- ,
	 -- timer_in  => CountL(5)
--	 timer_out => timer_out
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

--
-- 25 MHz CPU clock
--
cpu_clk_buffer : BUFG port map(
    i => clock_div(0),
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

mem_decode: process( cpu_clk, Reset_n,
                     cpu_addr, cpu_rw, cpu_vma,
					      rom_data_out, 
							ram_data_out,
						   timer_data_out, 
							trap_data_out, 
							pia_data_out,
						   uart_data_out,
							keyboard_data_out,
							vdu_data_out )
variable decode_addr : std_logic_vector(3 downto 0);
begin
--    decode_addr := dat_addr(3 downto 0) & cpu_addr(11);
    decode_addr := cpu_addr(15 downto 12);

      case decode_addr is
	   --
		-- SBUG/KBUG/SYS09BUG Monitor ROM $F800 - $FFFF
		--
		when "1111" => -- $F000 - $FFFF
		   cpu_data_in <= rom_data_out;
			rom_cs      <= cpu_vma;              -- read ROM
			ram_cs      <= '0';
			uart_cs     <= '0';
			timer_cs    <= '0';
			trap_cs     <= '0';
			pia_cs      <= '0';
			keyboard_cs <= '0';
			vdu_cs      <= '0';

      --
		-- IO Devices $E000 - $EFFF
		--
		when "1110" => -- $E000 - $E7FF
			rom_cs    <= '0';
			ram_cs    <= '0';
		   case cpu_addr(7 downto 4) is
			--
			-- UART / ACIA $E000
			--
			when "0000" => -- $E000
		     cpu_data_in <= uart_data_out;
			  uart_cs     <= cpu_vma;
			  timer_cs    <= '0';
			  trap_cs     <= '0';
			  pia_cs      <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';

			--
			-- WD1771 FDC sites at $E010-$E01F
			--
			when "0001" => -- $E010
           cpu_data_in <= (others => '0');
			  uart_cs     <= '0';
			  timer_cs    <= '0';
			  trap_cs     <= '0';
			  pia_cs      <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';

         --
         -- Keyboard port $E020 - $E02F
			--
			when "0010" => -- $E020
           cpu_data_in <= keyboard_data_out;
			  uart_cs     <= '0';
           timer_cs    <= '0';
			  trap_cs     <= '0';
			  pia_cs      <= '0';
			  keyboard_cs <= cpu_vma;
			  vdu_cs      <= '0';

         --
         -- VDU port $E030 - $E03F
			--
			when "0011" => -- $E030
           cpu_data_in <= vdu_data_out;
			  uart_cs     <= '0';
           timer_cs    <= '0';
			  trap_cs     <= '0';
			  pia_cs      <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= cpu_vma;

         --
			-- Compact Flash $E040 - $E04F
			--
			when "0100" => -- $E040
           cpu_data_in <= (others => '0');
			  uart_cs     <= '0';
			  timer_cs    <= '0';
			  trap_cs     <= '0';
			  pia_cs      <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';

         --
         -- Timer $E050 - $E05F
			--
			when "0101" => -- $E050
           cpu_data_in <= timer_data_out;
			  uart_cs     <= '0';
           timer_cs    <= cpu_vma;
			  trap_cs     <= '0';
			  pia_cs      <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';

         --
         -- Bus Trap Logic $E060 - $E06F
			--
			when "0110" => -- $E060
           cpu_data_in <= trap_data_out;
			  uart_cs     <= '0';
           timer_cs    <= '0';
			  trap_cs     <= cpu_vma;
			  pia_cs      <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';

         --
         -- I/O port $E070 - $E07F
			--
			when "0111" => -- $E070
           cpu_data_in <= pia_data_out;
			  uart_cs     <= '0';
           timer_cs    <= '0';
			  trap_cs     <= '0';
			  pia_cs      <= cpu_vma;
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';

			when others => -- $E080 to $E7FF
           cpu_data_in <= (others => '0');
			  uart_cs     <= '0';
			  timer_cs    <= '0';
			  trap_cs     <= '0';
			  pia_cs      <= '0';
			  keyboard_cs <= '0';
			  vdu_cs      <= '0';
		   end case;

		--
		-- $8000 to $DFFF = null
		--
      when "1101" | "1100" | "1011" | "1010" |
		     "1001" | "1000" =>
		  cpu_data_in <= (others => '0');
		  rom_cs      <= '0';
		  ram_cs      <= '0';
		  uart_cs     <= '0';
		  timer_cs    <= '0';
		  trap_cs     <= '0';
		  pia_cs      <= '0';
		  keyboard_cs <= '0';
		  vdu_cs      <= '0';
		--
		-- Everything else is RAM
		--
		when others =>
		  cpu_data_in <= ram_data_out;
		  rom_cs      <= '0';
		  ram_cs      <= cpu_vma;
		  uart_cs     <= '0';
		  timer_cs    <= '0';
		  trap_cs     <= '0';
		  pia_cs      <= '0';
		  keyboard_cs <= '0';
		  vdu_cs      <= '0';
		end case;
end process;

--
-- Interrupts and other bus control signals
--
interrupts : process( Reset_n, 
                      pia_irq_a, pia_irq_b, uart_irq, trap_irq, timer_irq, keyboard_irq
							 )
begin
 	 cpu_reset <= not Reset_n; -- CPU reset is active high
    cpu_irq   <= uart_irq or keyboard_irq;
	 cpu_nmi   <= pia_irq_a or trap_irq;
	 cpu_firq  <= pia_irq_b or timer_irq;
	 cpu_halt  <= '0';
    cpu_hold  <= '0';
end process;

--
--
my_led_flasher: process( SysClk, Reset_n, CountL )
begin
    if Reset_n = '0' then
		   CountL <= "000000000000000000000000";
    elsif(SysClk'event and SysClk = '0') then
		   CountL <= CountL + 1;
    end if;
	 LED(7 downto 0) <= CountL(23 downto 16);
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

DCD_n <= '0';
CTS_n <= '0';
Reset_n <= not BTN_SOUTH; -- CPU reset is active high
SysClk <= CLK_50MHZ;
rxbit <= RS232_DCE_RXD;
RS232_DCE_TXD <= txbit;

end my_computer; --===================== End of architecture =======================--

