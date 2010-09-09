--===========================================================================--
--                                                                           --
--               Synthesizable 6844 Compatible DMA Controller                --
--                                                                           --
--===========================================================================--
--
--  File name      : dma6844.vhd
--
--  Entity name    : dma6844
--
--  Purpose        : Implements a 6844 compatible Direct Memory Access Controller
--                   It is intended for use with 68xx compatible FPGA SoCs.
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_unsigned
--                   ieee.std_logic_arith
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Registers      :
--
--  4 Channel version
--
--  IO +  0 = DMA_AH0 = Address Register 0 High
--  IO +  1 = DMA_AL0 = Address Register 0 Low
--  IO +  2 = DMA_CH0 = Count   Register 0 High
--  IO +  3 = DMA_CL0 = Count   Register 0 Low
--
--  IO +  4 = DMA_AH1 = Address Register 1 High
--  IO +  5 = DMA_AL1 = Address Register 1 Low
--  IO +  6 = DMA_CH1 = Count   Register 1 High
--  IO +  7 = DMA_CL1 = Count   Register 1 Low
-- 
--  IO +  8 = DMA_AH2 = Address Register 2 High
--  IO +  9 = DMA_AL2 = Address Register 2 Low
--  IO + 10 = DMA_CH2 = Count   Register 2 High
--  IO + 11 = DMA_CL2 = Count   Register 2 Low
-- 
--  IO + 12 = DMA_AH3 = Address Register 3 High
--  IO + 13 = DMA_AL3 = Address Register 3 Low
--  IO + 14 = DMA_CH3 = Count   Register 3 High
--  IO + 15 = DMA_CL3 = Count   Register 3 Low
--  
--  IO + 16 = DMA_CC0 = Channel Control Register 0
--  IO + 17 = DMA_CC1 = Channel Control Register 1
--  IO + 18 = DMA_CC2 = Channel Control Register 2
--  IO + 19 = DMA_CC3 = Channel Control Register 3
--    Bit[7] = DMA_DEF = DMA END FLAG (DEND)
--    Bit[6] = DMA_BSY = DMA BUSY FLAG (READ ONLY)
--    Bit[3] = DMA_AUD = DMA ADDRESS NOT UP/DOWN
--    Bit[2] = DMA_MCA = DMA MODE CONTROL 0=>DRQ2   1=>DRQ1 
--    Bit[1] = DMA_MCB = DMA MODE CONTROL 0=>SINGLE 1=>BLOCK 
--       0 0 = DMA_MD2 = DMA MODE 2 - SINGLE TRANSFER - DRQ2
--       0 1 = DMA_MD3 = DMA MODE 3 - BLOCK  TRANSFER - DRQ2
--       1 0 = DMA_MD1 = DMA MODE 1 - SINGLE TRANSFER - DRQ1
--       1 1 = DMA_MDU = DMA MODE 4 - BLOCK  TRANSFER - DRQ1 - ACTUALLY UDEFINED 
--    Bit[0] = DMA_RW  = DMA READ/NOT WRITE
--
--  IO + 20  = DMA_PRI = DMA Priority Control Register
--    Bit[7] = DMA_ROT = DMA Rotate Control 0=>FIXED 1=>ROTATE
--    Bit[3] = DMA_RE3 = DMA REQUEST ENABLE #3
--    Bit[2] = DMA_RE2 = DMA REQUEST ENABLE #2
--    Bit[1] = DMA_RE1 = DMA REQUEST ENABLE #1
--    Bit[0] = DMA_RE0 = DMA REQUEST ENABLE #0
-- 
--  IO + 21  = DMA_INT = DMA Interrupt Control Register
--    Bit[7] = DMA_IEF = DMA END IRQ FLAG
--    Bit[3] = DMA_IE3 = DMA END IRQ ENABLE #3
--    Bit[2] = DMA_IE2 = DMA END IRQ ENABLE #2
--    Bit[1] = DMA_IE1 = DMA END IRQ ENABLE #1
--    Bit[0] = DMA_IE0 = DMA END IRQ ENABLE #0
--  
--  IO + 22  = DMA_CHN = DMA Data Chain register
--    Bit[3] = DMA_C24 = TWO/FOUR CHANNEL SELECT
--    Bit[2] = DMA_DCB = DATA CHAIN CHANNEL SELECT B
--    Bit[1] = DMA_DCA = DATA CHAIN CHANNEL SELECT A
--    Bit[0] = DMA_DCE = DATA CHAIN ENABLE 
--
--  Copyright (C) 2010 John Kent
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
-- Revision Author        Date               Description
-- 0.1      John E. Kent  18th April 2010    Initial release
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.std_logic_arith.all;

library unisim;
  use unisim.vcomponents.all;

entity dma6844 is
   generic (
      ADDR_WIDTH = 16;
      DATA_WIDTH = 8;
      CHAN_COUNT = 4
      )
	port (
      --
      -- CPU Slave Interface
      --	
		clk      : in  std_logic;
		rst      : in  std_logic;
		rw       : out std_logic;
		cs       : out std_logic;
		addr     : in  std_logic_vector(LOG2(CHAN_COUNT*4*ADDR_WIDTH/DATA_WIDTH)-1 downto 0);
	   data_in  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
	   data_out : out std_logic_vector(DATA_WIDTH-1 downto 0);
      irq      : out std_logic;
      --
      -- Bus Master Interface
      --
      breq     : out std_logic;
      bgnt     : in  std_logic;
      brw      : out std_logic;
      bvma     : out std_logic;
      baddr    : out std_logic_vector(ADDR_WIDTH-1 downto 0);
      --
      -- Device Interface
      --
      txreq    : in  std_logic_vector(CHAN_COUNT-1 downto 0);
      txstb    : out std_logic_vector(CHAN_COUNT-1 downto 0);
      txack    : out std_logic_vector(CHAN_COUNT-1 downto 0);
      txend    : out std_logic_vector(CHAN_COUNT-1 downto 0)
		);
end dma6844;

architecture rtl of dma6844 is

constant REG_COUNT : integer = (CHAN_COUNT * 2 * ADDR_WIDTH / DATA_WIDTH) + DMA_CHAN + 3;

subtype addr_subtype is std_logic_vector(ADDR_WIDTH-1 downto 0);
subtype data_subtype is std_logic_vector(DATA_WIDTH-1 downto 0);

type addr_type is array(0 to CHAN_COUNT-1) of addr_subtype;
type data_type is array(0 to CHAN_COUNT-1) of data_subtype;
type reg_type  is array(0 to REG_COUNT-1)  of data_subtype;

signal dma_addr    : addr_type;
signal dma_count   : addr_type;
signal dma_in_reg  : reg_type;
signal dma_out_reg : reg_type;

signal dma_reg_wr : std_logic := '0';
signal dma_reg_rd : std_logic := '0';

--
-- Registers
--
signal dma_adh_reg : data_type;
signal dma_adl_reg : data_type;
signal dma_cth_reg : data_type;
signal dma_ctl_reg : data_type;
signal dma_chc_reg : data_type;
signal dma_pri_reg : std_logic_vector(DATA_WIDTH-1 downto 0);
signal dma_irq_reg : std_logic_vector(DATA_WIDTH-1 downto 0);
signal dma_chn_reg : std_logic_vector(DATA_WIDTH-1 downto 0);

begin
--
-- Write to DMA input register
--
dma_reg_write : process( clk, rst )
variable reg_addr   : integer := 0;
begin
  if( falling_edge(clk) )
    if( rst = '1' ) then
      for i in 0 to CHAN_COUNT-1 loop
        dma_addr(i)   <= (others=>'0');
        dma_count(i)  <= (others=>'0');
      end loop;
      for i in 0 to REG_COUNT-1 loop
        dma_in_reg(i) <= (others=>'0');
      end loop;
    else
      if( cs='1' and rw='0' ) then
        reg_addr := conv_integer(addr(ADDR_WIDTH-1 downto 0));
        if( reg_addr < REG_COUNT ) then
          dma_in_reg(reg_addr) <= data_in;
        end if;
      end if;
    end if;
  end if;
end process;

--
-- Assign input register to specific register names
--
dma_reg_assign : process( dma_in_reg )
begin
  for i in 0 to CHAN_COUNT-1 loop
    dma_adh_reg(i) <= dma_in_reg((i*4)+0);
    dma_adl_reg(i) <= dma_in_reg((i*4)+1);
    dma_cth_reg(i) <= dma_in_reg((i*4)+2);
    dma_ctl_reg(i) <= dma_in_reg((i*4)+3);
    dma_chc_reg(i) <= dma_in_reg((CHAN_COUNT*4)+i);
  end loop;
  dma_pri_reg <= dma_in_reg((CHAN_COUNT*5)+0);
  dma_irq_reg <= dma_in_reg((CHAN_COUNT*5)+1);
  dma_chn_reg <= dma_in_reg((CHAN_COUNT*5)+2);
end process;

--
-- Process Transfer Request Inputs
--
dma_tx_req : process( clk, rst, txreq )
begin
  if( rising_edge( clk ) ) then
    if( rst='1' ) then
    else
      for i in 0 to CHAN_COUNT-1 loop
      end loop;
    end if;
  end if; 
end process;
    
end architecture rtl;

