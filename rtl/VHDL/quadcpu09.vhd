--===========================================================================--
--                                                                           --
--       Synthesizable Quad Core 6809 instruction compatible CPU Module      --
--                                                                           --
--===========================================================================--
--
--  File name      : quadcpu09.vhd
--
--  Entity name    : quadcpu09
--
--  Purpose        : Top level file for quad Core 6809 compatible system on a chip
--                   Designed for Xilinx XC3S1000 Spartan 3 FPGA.
--                   Intended for Digilent Xilinx Spartan 3 Starter FPGA board,
--                   or XESS XSA-3S1000 FPGA board.
--
--  Dependencies   : ieee.Std_Logic_1164
--                   ieee.std_logic_unsigned
--                   ieee.std_logic_arith
--                   ieee.numeric_std
--                   unisim.vcomponents
--                   work.bit_funcs
--
--  Status:        : *** Still under development ***
--
--  Uses           : unicpu09  (unicpu09.vhd)   CPU09 core module
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  quadcpu09.vhd is a top level file for a quad CPU09 core written in VHDL.
-- 
--  Copyright (C) 2003 - 2010 John Kent
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
--                              Revision  History                            --
--                                                                           --
--===========================================================================--
--
-- Version  Author        Date               Description
-- 0.1      John Kent     20th March 2003    Initial version
-- 0.2      John Kent     30th May 2010      Updated Header and GPL
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.std_logic_arith.all;
   use ieee.std_logic_unsigned.all;
   use ieee.numeric_std.all;

library unisim;
	use unisim.vcomponents.all;

library work;
   use work.bit_funcs.all;

entity quadcpu09 is
  port (    
	 clk      :	in  std_logic;
    rst      : in  std_logic;
    vma      :	out std_logic;
    addr     : out std_logic_vector(19 downto 0);
    rw       :	out std_logic;
    data_in  : in	 std_logic_vector(7 downto 0);
	 data_out : out std_logic_vector(7 downto 0);
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic;
	 halt     : in  std_logic;
	 hold     : in  std_logic
  );
end entity;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture RTL of quadcpu09 is

  constant CPU_MAX    : integer :=  4;
  constant ADDR_WIDTH : integer := 20;
  constant DATA_WIDTH : integer :=  8;

  type addr_type  is std_logic_vector(ADDR_WIDTH-1 downto 0);
  type data_type  is std_logic_vector(DATA_WIDTH-1 downto 0);
  type cpu_type   is std_logic_vector(   CPU_MAX-1 downto 0);
  type addr_array is array(0 to (CPU_MAX-1)) of addr_type;
  type data_array is array(0 to (CPU_MAX-1)) of data_type;

  -- CPU Interface signals
  signal cpu_rw       : cpu_type;
  signal cpu_vma      : cpu_type;
  signal cpu_addr     : addr_array;
  signal cpu_id       : data_array;
  signal cpu_halt     : cpu_type;
  signal cpu_hold     :	cpu_type;
  signal cpu_irq      :	cpu_type;
  signal cpu_nmi      :	cpu_type;
  signal cpu_firq     :	cpu_type;
  signal mem_rw       :	std_logic;
  signal mem_vma      :	std_logic;
  signal mem_addr     :	addr_type;
  signal mem_dati     : data_type;
  signal mem_dato     : data_array;

  --
  -- priority encoder
  --
  signal pri_rot      : std_logic;                                   -- rotate the priority 
  signal pri_cnt      : std_logic_vector( log2(CPU_MAX)-1 downto 0);	-- priority rotation counter
  signal pri_mux      : std_logic_vector( (CPU_MAX-1) downto 0);     -- rotated bus request
  signal pri_enc      : std_logic_vector( log2(CPU_MAX)-1 downto 0);	-- encoded rotated bus request
  signal pri_req      : std_logic;												-- encoded bus request valid


component my_unicpu09
  port(
	 clk      :	in  std_logic;
    rst      : in  std_logic;
	 --
	 -- cpu side signals
	 --
    vma      : out std_logic;
    addr     : out addr_type;
    rw       : out std_logic;
    cpu_id   : in  data_type;
	 --
	 -- memory side signals
	 --
    mem_rw   : in  std_logic;
    mem_vma  :	in  std_logic;
    mem_addr : in  addr_type;
    mem_dati : in	 data_type;
	 mem_dato : out data_type;
	 --
	 -- controls
	 --
	 halt     : in  std_logic;
	 hold     : in  std_logic;
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic
    );
end component;


begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_unicpu09_0 : unicpu09  
port map (    
	 clk	     => clk,
    rst       => rst,
    vma       => cpu_vma(0),
    addr      => cpu_addr(0),
    rw	     => cpu_rw(0),
    id        => "00000000",
 	 --
	 -- memory side signals
	 --
    mem_vma   => mem_vma,
    mem_addr  => mem_addr,
    mem_rw    => mem_rw,
    mem_dati  => mem_dati,
	 mem_dato  => mem_dato(0),
    --
	 -- cpu controls
	 --
	 irq       => cpu_irq(0),
	 nmi       => cpu_nmi(0),
	 firq      => cpu_firq(0),
	 halt      => cpu_halt(0),
	 hold      => cpu_hold(0)
    );

my_unicpu09_1 : unicpu09  
port map (    
	 clk	     => clk,
    rst       => rst,
    vma       => cpu_vma(1),
    addr      => cpu_addr(1),
    rw	     => cpu_rw(1),
    id        => "00010000",
 	 --
	 -- memory side signals
	 --
    mem_vma   => mem_vma,
    mem_addr  => mem_addr,
    mem_rw    => mem_rw,
    mem_dati  => mem_dati,
	 mem_dato  => mem_dato(1),
    --
	 -- cpu controls
	 --
	 irq       => cpu_irq(1),
	 nmi       => cpu_nmi(1),
	 firq      => cpu_firq(1),
	 halt      => cpu_halt(1),
	 hold      => cpu_hold(1)
    );

my_unicpu09_2 : unicpu09  
port map (    
	 clk	     => clk,
    rst       => rst,
    vma       => cpu_vma(2),
    addr      => cpu_addr(2),
    rw	     => cpu_rw(2),
    id        => "00100000"
 	 --
	 -- memory side signals
	 --
    mem_vma   => mem_vma,
    mem_addr  => mem_addr,
    mem_rw    => mem_rw,
    mem_dati  => mem_dati,
	 mem_dato  => mem_dato(2),
    --
	 -- cpu controls
	 --
	 irq       => cpu_irq(2),
	 nmi       => cpu_nmi(2),
	 firq      => cpu_firq(2),
	 halt      => cpu_halt(2),
	 hold      => cpu_hold(2)
    );

my_unicpu09_3 : unicpu09  
port map (    
	 clk	     => clk,
    rst       => rst,
    vma       => cpu_vma(3),
    addr      => cpu_addr(3),
    rw	     => cpu_rw(3),
    id        => "00110000",
 	 --
	 -- memory side signals
	 --
    mem_vma   => mem_vma,
    mem_addr  => mem_addr,
    mem_rw    => mem_rw,
    mem_dati  => mem_dati,
	 mem_dato  => mem_dato(3),
    --
	 -- cpu controls
	 --
	 irq       => cpu_irq(3),
	 nmi       => cpu_nmi(3),
	 firq      => cpu_firq(3),
	 halt      => cpu_halt(3),
	 hold      => cpu_hold(3)
    );

--
-- Rotating priority
--
my_pri_rotate : process( rst, clk )
variable cpu_count : integer := 0;
begin
  if rst = '1' then
    pri_cnt <= (others=>0);
  if falling_edge(clk) then
    if pri_rot = '1' then
      pri_cnt <= pri_cnt + 1;
    end if;
  end if;
end process;

--
-- Rotate VMA request
--
my_pri_mux : process( pri_cnt )
begin
  case pri_cnt is
  when "00" =>
    pri_mux <= cpu_vma(3 downto 0);
  when "01" =>
    pri_mux <= cpu_vma(2 downto 0) & cpu_vma(3);
  when "10" =>
    pri_mux <= cpu_vma(1 downto 0) & cpu_vma(3 downto 2);
  when "11" =>
    pri_mux <= cpu_vma(0)          & cpu_vma(3 downto 1);
  when other =>
    null;
  end case;   
end process;

--
-- Priority Encode Rotated VMA Request
--
my_pri_encoder : process( pri_mux )
variable enc_bits : integer := 0;
variable cpu_bits : integer := 0;
begin
    for cpu_bits in 0 to (CPU_MAX-1) loop
      pri_req := pri_req or pri_mux(cpu_bits);
    end loop;

    for enc_bits in 0 to log2(CPU_MAX)-1 loop
	   pri_enc(enc_bits) <= '0';
      for cpu_bits in 0 to (CPU_MAX-1) loop
		  if (cpu_bits and pow2(enc_bits)) /= 0 then 		
	       pri_enc(enc_bits) <= pri_enc(enc_bits) or pri_mux(cpu_bits);
        end if;
      end loop;
	 end loop;
end process;

--
-- Grant highest priority requesting processor access to the bus
--
my_bus_grant : process( rst, clk )
begin

end process;

--
-- Hold processor until bus cycle acknowledged
--
my_hold_machine : process( rst, clk )
variable cpu_bits : integer := 0;
begin
  for cpu_bits in 0 to (CPU_MAX-1) loop
    if rst = '1' then
	   cpu_hold( cpu_bits ) <= '0';
    elsif rising_edge( clk ) then
      cpu_hold( cpu_bits ) <= cpu_vma( cpu_bits ) and (not cpu_ack( cpu_bits ));
    end if;
  end loop;
end process;

end architecture;
