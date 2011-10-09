--===========================================================================--
--                                                                           --
--      eioport.vhd - Synthesizable Enhanced Bidirectionsal I/O Port         --
--                                                                           --
--===========================================================================--
--
--  File name      : eioport.vhd
--
--  Purpose        : Implements an enhanced bidirectional I/O port which is 
--                   capable of generating an interrupt output from each of 
--                   the input port lines. It is intended for use with system09 
--                   and other systemXX microcomputer systems on a chip. 
-- 
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_unsigned
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Description    : The enhanced I/O port is mapped as 4 contiguous registers,
--                   a data register, data direction register, interrupt enable 
--                   register and interrupt input level register. All registers 
--                   are readable and writable.
--
--                   The data bus width is specified with a generic and defaults
--                   to 8 bits. 
--
--                   The Data Register holds the output value when written to
--                   When read, reads the input port levels if the correponding 
--                   data direction bit is set to a zero or reads the output register
--                   value if the corresponding data direction bit is set to a one.
--
--                   The Data Direction Register determines if individual bits
--                   on the IO port are inputs or outputs. If a data direction bit
--                   is set to zero the corresponding io port bit is set to an input.
--                   If the data direction bit is set to a one the corresponding 
--                   io port bit is set to an output.
--
--                   Each port bit can generate an interrupt if programmed as an input
--                   and if the corresponding interrupt enable bit is set. 
--                   An Interrupt Enable Register is used to enable an interrupt from each
--                   of the inputs on the io port if the interrupt enable bit is set to 
--                   a one or disables an interrupt from that bit if set to a zero.
--
--                   An Interrupt Level Register determines if a high or a low level input 
--                   on the io port generates an active high on the interrupt output. 
--                   If the interrupt level register bit is set to a zero then a high level
--                   input on the corresporting io port bit will generate a high interrupt
--                   output. If the interrupt level bit is set to a one then a low level on 
--                   the corrrsponding input bit will generate a high interrupt output level
--                   on IRQ.
--
--  address  function
--  =======  ========
--  base+0   port data register
--           bits 0 - 7 = I/O
--  base+1   port direction register 
--           0 => port bit = input
--           1 => port bit = output
--  base+2   port nterrupt enable register
--           0 => port bit = interrupt disabled
--           1 => port bit = interrupt enabled
--  base+3   port interrupt level register
--           0 => port bit = logic high interrupt
--           1 => port bit = logic low interrupt
--
--  Copyright (C) 2002 - 2011 John Kent
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
-- 0.1      John E. Kent  11 October 2002    Used a loop counter for 
--                                           data direction & read port signals
-- 0.2      John E. Kent  5 September 2003   Reduced to 2 x 8 bit ports
-- 1.0      John E. Kent  6 September 2003   Changed Clock Edge
-- 1.1      John E. Kent  25 Februrary 2007  Modified sensitivity lists
-- 1.2      John E. Kent  30 May 2010        Updated Header, added unisim library
-- 2.0      John E. Kent  30 April 2011      modified for XuLA System09 I/O
-- 3.0      John E. Kent   1 May 2011        single enhanced io port with additional
--                                           interrupt enable & level registers
--===========================================================================
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity eioport is
  generic (
    DATA_WIDTH : integer := 8
  );
	 
  port (	
    clk       : in    std_logic;
    rst       : in    std_logic;
    cs        : in    std_logic;
    rw        : in    std_logic;
    addr      : in    std_logic_vector(1 downto 0);
    data_in   : in    std_logic_vector(DATA_WIDTH-1 downto 0);
    data_out  : out   std_logic_vector(DATA_WIDTH-1 downto 0);
    port_io   : inout std_logic_vector(DATA_WIDTH-1 downto 0);
    irq       : out   std_logic
  );
end;

architecture rtl of eioport is

signal port_out : std_logic_vector(DATA_WIDTH-1 downto 0);  -- output register
signal port_ddr : std_logic_vector(DATA_WIDTH-1 downto 0);  -- data direction register
signal port_ier : std_logic_vector(DATA_WIDTH-1 downto 0);  -- interrupt enable register
signal port_ilr : std_logic_vector(DATA_WIDTH-1 downto 0);  -- interrupt level register

begin

--------------------------------
--
-- read port registers
--
--------------------------------

eioport_read : process( addr, port_out, port_io, port_ddr, port_ier, port_ilr)
variable count : integer;
begin
  case addr is
  when "00" =>
    for count in 0 to (DATA_WIDTH-1) loop
      if port_ddr(count) = '1' then
        data_out(count) <= port_out(count);
      else
        data_out(count) <= port_io(count);
      end if;
    end loop;
  when "01" =>
    data_out <= port_ddr;
  when "10" =>
    data_out <= port_ier;
  when "11" =>
    data_out <= port_ilr;
  when others =>
    null;
  end case;
 
end process;

---------------------------------
--
-- Write port registers
--
---------------------------------

eioport_write : process( clk, rst, cs, rw, addr, data_in )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      port_out <= (others=>'0');
      port_ddr <= (others=>'0');
      port_ier <= (others=>'0');
      port_ilr <= (others=>'0');
    else
      if cs = '1' and rw = '0' then
        case addr is
        when "00" =>
           port_out <= data_in;
        when "01" =>
           port_ddr <= data_in;
        when "10" =>
           port_ier <= data_in;
        when "11" =>
           port_ilr <= data_in;
        when others =>
           null;
        end case;
      end if;
    end if;
  end if;
end process;

---------------------------------
--
-- direction control
--
---------------------------------
eioport_direction : process ( port_ddr, port_out )
variable count : integer;
begin
  for count in 0 to (DATA_WIDTH-1) loop
    if port_ddr(count) = '1' then
      port_io(count) <= port_out(count);
    else
      port_io(count) <= 'Z';
    end if;
  end loop;
end process;


---------------------------------
--
-- interrupt control
--
---------------------------------
eioport_interrupt : process ( port_io, port_ilr, port_ddr, port_ier )
variable count : integer;
variable irq_temp : std_logic;
begin
  --
  -- Interrupt level sets the polarity of the interrupt
  -- Data direction register must be set for input
  -- Interrupt enable bit must be set to generate an interrupt
  --
  irq_temp := '0';
  for count in 0 to (DATA_WIDTH-1) loop
    irq_temp := (((port_io(count) xor port_ilr(count)) and not(port_ddr(count)))
                   and port_ier(count)) or irq_temp;
  end loop;
  irq <= irq_temp;
end process;
---------------------------------

end rtl;
	
