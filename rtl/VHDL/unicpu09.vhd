--===========================================================================----
--
--  S Y N T H E Z I A B L E    unicpu09.vhd - Single 6809 processor core
--
--===========================================================================----
--
--  This core adheres to the GNU public license  
--
-- File name      : System09.vhd
--
-- Purpose        : Top level file for Hex Core 6809 compatible system on a chip
--                  Designed with Xilinx XC3S1000 Spartan 3 FPGA.
--                  Implemented With Digilent Xilinx Starter FPGA board,
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : 
--                  cpu09  (cpu09.vhd)           6809 CPU core
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--
--===========================================================================----
--
-- Revision History:
--
--===========================================================================--
-- Version 0.1 - 20 March 2003
--
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;

entity unicpu09 is

  port(
	 clk      :	in  std_logic;
    rst      : in  std_logic;
	 --
	 -- cpu side signals
	 --
    rw       : out std_logic;
    vma      : out std_logic;
    addr     : out std_logic_vector(19 downto 0);
    id       : in  std_logic_vector( 7 downto 0);
	 --
	 -- memory side signals
	 --
    mem_rw   : in  std_logic;
    mem_vma  :	in  std_logic;
    mem_addr : in  std_logic_vector(19 downto 0);
    mem_dati : in	 std_logic_vector(7 downto 0);
	 mem_dato : out std_logic_vector(7 downto 0);
	 --
	 -- controls
	 --
	 halt     : in  std_logic;
	 hold     : in  std_logic;
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic
    );
end entity;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture RTL of unicpu09 is

  -- CPU Interface signals
  signal cpu_rw       : std_logic;
  signal cpu_vma      : std_logic;
  signal cpu_addr     : std_logic_vector(15 downto 0);
  signal cpu_dati     : std_logic_vector(7 downto 0);
  signal cpu_dato     : std_logic_vector(7 downto 0);

  -- BOOT ROM
  signal rom_cs        : Std_logic;
  signal rom_dato      : Std_Logic_Vector(7 downto 0);

  -- cache host signals
  signal cache_cpu_addr : std_logic_vector(31 downto 0);
  signal cache_cpu_dati : std_logic_vector(15 downto 0);
  signal cache_cpu_dato : std_logic_vector(15 downto 0);
  signal cache_cpu_vma  : std_logic;
  signal cache_cpu_en   : std_logic;

  -- cache memory signals
  signal cache_mem_addr : std_logic_vector(31 downto 0);
  signal cache_mem_dati : std_logic_vector(15 downto 0);
  signal cache_mem_dato : std_logic_vector(15 downto 0);
  signal cache_mem_vma  : std_logic;

  -- Dynamic Address Translation
  signal dat_cs       : std_logic;
  signal dat_addr     : std_logic_vector(15 downto 0);

  -- 32 bit harware multiplier
  signal mul_cs       : std_logic;
  signal mul_dato     : std_logic_vector(7 downto 0);

  -- external access
  signal ext_cs       : std_logic;
  signal ext_dato     : std_logic_vector(7 downto 0);

component cpu09
  port (    
	 clk      :	in  std_logic;
    rst      : in  std_logic;
    rw       :	out std_logic;
    vma      :	out std_logic;
    address  : out std_logic_vector(15 downto 0);
    data_in  : in	 std_logic_vector(7 downto 0);
	 data_out : out std_logic_vector(7 downto 0);
	 halt     : in  std_logic;
	 hold     : in  std_logic;
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic
  );
end component;

----------------------------------------
--
-- Dynamic Address Translation Registers
--
----------------------------------------
component dat_ram
  port (
    clk      : in  std_logic;
	 rst      : in  std_logic;
	 cs       : in  std_logic;
	 rw       : in  std_logic;
	 addr_lo  : in  std_logic_vector(3 downto 0);
	 addr_hi  : in  std_logic_vector(3 downto 0);
    data_in  : in  std_logic_vector(7 downto 0);
	 data_out : out std_logic_vector(7 downto 0)
  );
end component;


----------------------------------------
--
-- 4KByte Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
  Port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    cs       : in  std_logic;
    rw       : in  std_logic;
    addr     : in  std_logic_vector (11 downto 0);
    rdata    : out std_logic_vector (7 downto 0);
    wdata    : in  std_logic_vector (7 downto 0)
    );
end component;

----------------------------------------
--
-- Dual Port cache memory
--
----------------------------------------
component dpr_2k
  port (
    clk_a   : in  std_logic;
    rst_a   : in  std_logic;
    cs_a    : in  std_logic;
    rw_a    : in  std_logic;
    addr_a  : in  std_logic_vector (9 downto 0);
    dati_a  : in  std_logic_vector (15 downto 0);
    dato_a  : out std_logic_vector (15 downto 0);
    clk_b   : in  std_logic;
    rst_b   : in  std_logic;
    cs_b    : in  std_logic;
    rw_b    : in  std_logic;
    addr_b  : in  std_logic_vector (9 downto 0);
    dati_b  : in  std_logic_vector (15 downto 0);
    dato_b  : out std_logic_vector (15 downto 0)
  );
end component;

----------------------------------------
--
-- 32 bit hardware multiplier
--
----------------------------------------
component mul32
  port (
    clk      : in  std_logic;
	 rst      : in  std_logic;
	 cs       : in  std_logic;
	 rw       : in  std_logic;
	 addr     : in  std_logic_vector(3 downto 0);
    dati     : in  std_logic_vector(7 downto 0);
	 dato     : out std_logic_vector(7 downto 0)
  );
end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_cpu : cpu09  port map (    
	 clk	     => clk,
    rst       => rst,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    address   => cpu_addr(15 downto 0),
    data_in   => cpu_dati,
	 data_out  => cpu_dato,
	 halt      => halt,
	 hold      => hold,
	 irq       => irq,
	 nmi       => nmi,
	 firq      => firq
    );

my_dat : dat_ram port map (
    clk       => clk,
	 rst       => rst,
	 cs        => dat_cs,
	 rw        => cpu_rw,
	 addr_hi   => cpu_addr(15 downto 12),
	 addr_lo   => cpu_addr(3 downto 0),
    data_in   => cpu_dato,
	 data_out  => dat_addr(7 downto 0)
	 );

my_rom : mon_rom port map (
    clk       => clk,
    rst       => rst,
	 cs        => rom_cs,
	 rw        => '1',
    addr      => cpu_addr(11 downto 0),
    rdata     => rom_dato,
    wdata     => cpu_dato
    );
--
-- high address cache
--
my_dpr_0 : dpr_2k port map (
    clk_a     => clk,
    rst_a     => rst,
    cs_a      => cpu_vma,
    rw_a      => '0',
    addr_a    => cpu_addr(9 downto 0),
    dati_a    => dat_addr(15 downto 0),
    dato_a    => cache_cpu_addr(31 downto 16),
    clk_b     => clk,
    rst_b     => rst,
    cs_b      => mem_vma,
    rw_b      => '1',
    addr_b    => mem_addr(9 downto 0),
    dati_b    => (others=>'0'),
    dato_b    => cache_mem_addr(31 downto 16)
  );
--
-- low address cache
--
my_dpr_1 : dpr_2k port map (
    clk_a     => clk,
    rst_a     => rst,
    cs_a      => cpu_vma,
    rw_a      => '0',
    addr_a    => cpu_addr(9 downto 0),
    dati_a    => cpu_addr(15 downto 0),
    dato_a    => cache_cpu_addr(15 downto 0),
    clk_b     => clk,
    rst_b     => rst,
    cs_b      => mem_vma,
    rw_b      => '1',
    addr_b    => mem_addr(9 downto 0),
    dati_b    => (others=>'0'),
    dato_b    => cache_mem_addr(15 downto 0)
  );

--
-- data cache
--
my_dpr_2 : dpr_2k port map (
    clk_a     => clk,
    rst_a     => rst,
    cs_a      => cache_cpu_vma,
    rw_a      => cpu_rw,
    addr_a    => cpu_addr(9 downto 0),
    dati_a    => cache_cpu_dati(15 downto 0),
    dato_a    => cache_cpu_dato(15 downto 0),
    clk_b     => clk,
    rst_b     => rst,
    cs_b      => cache_mem_vma,
    rw_b      => mem_rw,
    addr_b    => mem_addr(9 downto 0),
    dati_b    => cache_mem_dati(15 downto 0),
    dato_b    => cache_mem_dato(15 downto 0)
  );

my_mul32 : mul32 port map (
    clk       => clk,
	 rst       => rst,
	 cs        => mul_cs,
	 rw        => cpu_rw,
	 addr      => cpu_addr(3 downto 0),
    dati      => cpu_dato,
	 dato      => mul_dato
	 );

----------------------------------------------------------------------
--
-- Process to decode internal registers
--
----------------------------------------------------------------------

int_decode: process( cpu_addr, cpu_rw, cpu_vma,
                     cache_cpu_dato,
					      dat_cs, dat_addr,
							cache_cpu_dato,
							mul_dato,
							rom_dato
							)
begin
  cpu_dati     <= cache_cpu_dato( 7 downto 0);
  cache_cpu_en <= '1';
  ext_cs       <= cpu_vma; -- Assume external access
  dat_cs       <= '0';     -- Dynamic Address Translation
  rom_cs       <= '0';
  mul_cs       <= '0';     -- Hardware Multiplier

  if cpu_addr( 15 downto 8 ) = "11111111" then
    --
    -- DAT write registers at $FFF0 to $FFFF
	 --
    cpu_dati     <= rom_dato;
    rom_cs       <= cpu_vma;
    dat_cs       <= cpu_vma;
	 ext_cs       <= '0';
    cache_cpu_en <= '0';
  --
  -- ROM  $F000 - $FFFF
  --
  elsif dat_addr(3 downto 0) = "1111" then -- $XE000 - $XEFFF
    cpu_dati     <= rom_dato;
    rom_cs       <= cpu_vma;
    cache_cpu_en <= '0';
  --
  -- IO Devices $E000 - $EFFF
  --
  elsif dat_addr(3 downto 0) = "1110" then -- $XE000 - $XEFFF
    --
	 -- disable cache for I/O
	 --
    cache_cpu_en <= '0';

	 case cpu_addr(11 downto 8) is
    --
    -- CPU specific registers from $E200 to $E2FF
    --
    when "0010" =>
	   ext_cs   <= '0';              -- assume this segment is internal
      cpu_dati <= (others=>'0');		-- default to null data
      --
      -- Unique number to identify CPU
      --
      case cpu_addr(7 downto 4) is
      when "0000" =>
        cpu_dati <= cpu_id;  -- CPU ID register
      --
      -- hardware 32 bit multiplier
      --
      when "0001" =>
        cpu_dati <= mul_dato;  -- Hardware Multiplier register
        mul_cs   <= cpu_vma;

      when others =>
		  null;
      end case;

    --
    -- Everything else is external
    --
    when others =>
	   null;
 
    end case;
  end if;
end process;

--
-- cpu side cache controller
--
my_cpu_cache : process( cpu_vma, cpu_rw, cpu_dato, cpu_addr, dat_addr, 
                        cache_cpu_addr, cache_cpu_en, ext_cs )
begin
  dat_addr(15 downto 8) <= (others=>'0');
  addr(19 downto 12) <= dat_addr xor id;
  addr(11 downto  0) <= cpu_addr(11 downto 0);
  rw   <= cpu_rw;
  vma  <= '0';
  --
  -- external access if cache miss or write through or if i/o space
  --
  if (cache_cpu_addr(23 downto 16) /= dat_addr( 7 downto 0) ) or
	  (cache_cpu_addr(11 downto  0) /= cpu_addr(11 downto 0) ) or
     (cpu_rw = '0') or (cache_cpu_en = '0') then
    vma <= ext_cs;
  end if;
  cache_cpu_dati( 7 downto 0) <= cpu_dato;    
  cache_cpu_dati(15 downto 8) <= (others=>'0');    
end process;

--
-- memory side cache controller
--
my_mem_cache : process( mem_vma, mem_addr, mem_dati, mem_rw, 
                        cache_mem_addr, cache_mem_dato )
begin
  --
  -- write through from another CPU will update cache entry
  -- if there is a cache hit
  --
  cache_mem_vma <= '0';
  if (cache_mem_addr(23 downto 16) = mem_addr(19 downto 12)) and
     (cache_mem_addr(11 downto  0) = mem_addr(11 downto  0)) then
    cache_mem_vma <= mem_vma;
  end if;
  mem_dato <= cache_mem_dato( 7 downto 0);
  cache_mem_dati( 7 downto 0) <= mem_dati;    
  cache_mem_dati(15 downto 8) <= (others=>'0');      
end process;

end architecture;

  -----------------------------------------------------------------------------
