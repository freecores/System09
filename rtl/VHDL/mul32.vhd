--===========================================================================--
--
--  S Y N T H E Z I A B L E    Dynamic Address Translation Registers
--
--===========================================================================--
--
--  This core adheres to the GNU public license  
--
-- File name      : mul32.vhd
--
-- entity name    : mul32
--
-- Purpose        : Implements a 32 bit x 32 bit hardware multiplier
--                  with 64 bit reset
--                  R/W Registers 0 to 3 are the left input MSB first
--                  R/W Registers 4 to 7 are the right input MSB first
--                  RO Registers 8 to 15 are the 64 bit result 
--                  
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--
-- Author         : John E. Kent      
--
--===========================================================================----
--
-- Revision History:
--
-- Date          Revision  Author 
-- 7th Sep 2008   0.1       John Kent
--
--
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
library unisim;
  use unisim.vcomponents.all;

entity mul32 is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(3 downto 0);
    dati      : in  std_logic_vector(7 downto 0);
	 dato      : out std_logic_vector(7 downto 0));
end entity;

architecture rtl of mul32 is
--
-- registers
--
signal mul_reg0 : std_logic_vector(7 downto 0);
signal mul_reg1 : std_logic_vector(7 downto 0);
signal mul_reg2 : std_logic_vector(7 downto 0);
signal mul_reg3 : std_logic_vector(7 downto 0);
signal mul_reg4 : std_logic_vector(7 downto 0);
signal mul_reg5 : std_logic_vector(7 downto 0);
signal mul_reg6 : std_logic_vector(7 downto 0);
signal mul_reg7 : std_logic_vector(7 downto 0);
signal mul_reg8 : std_logic_vector(7 downto 0);
signal mul_reg9 : std_logic_vector(7 downto 0);
signal mul_reg10 : std_logic_vector(7 downto 0);
signal mul_reg11 : std_logic_vector(7 downto 0);
signal mul_reg12 : std_logic_vector(7 downto 0);
signal mul_reg13 : std_logic_vector(7 downto 0);
signal mul_reg14 : std_logic_vector(7 downto 0);
signal mul_reg15 : std_logic_vector(7 downto 0);

begin

---------------------------------
--
-- Write Multiplier Registers
--
---------------------------------

mul_write : process( clk )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      mul_reg0  <= "00000000";
      mul_reg1  <= "00000000";
      mul_reg2  <= "00000000";
      mul_reg3  <= "00000000";
      mul_reg4  <= "00000000";
      mul_reg5  <= "00000000";
      mul_reg6  <= "00000000";
      mul_reg7  <= "00000000";
    else
	   if cs = '1' and rw = '0' then
        case addr is
	     when "0000" =>
		    mul_reg0 <= dati;
	     when "0001" =>
		    mul_reg1 <= dati;
	     when "0010" =>
		    mul_reg2 <= dati;
	     when "0011" =>
		    mul_reg3 <= dati;
	     when "0100" =>
		    mul_reg4 <= dati;
	     when "0101" =>
		    mul_reg5 <= dati;
	     when "0110" =>
		    mul_reg6 <= dati;
	     when "0111" =>
		    mul_reg7 <= dati;
        when others =>
		    null;
		  end case;
	   end if;
	 end if;
  end if;
end process;

---------------------------------
--
-- Read Multiplier Registers
--
---------------------------------

mul_read : process(  addr,
                     mul_reg0, mul_reg1, mul_reg2, mul_reg3,
                     mul_reg4, mul_reg5, mul_reg6, mul_reg7,
                     mul_reg8, mul_reg9, mul_reg10, mul_reg11,
                     mul_reg12, mul_reg13, mul_reg14, mul_reg15 )
begin
      case addr is
	     when "0000" =>
		    dato <= mul_reg0;
	     when "0001" =>
		    dato <= mul_reg1;
	     when "0010" =>
		    dato <= mul_reg2;
	     when "0011" =>
		    dato <= mul_reg3;
	     when "0100" =>
		    dato <= mul_reg4;
	     when "0101" =>
		    dato <= mul_reg5;
	     when "0110" =>
		    dato <= mul_reg6;
	     when "0111" =>
		    dato <= mul_reg7;
	     when "1000" =>
		    dato <= mul_reg8;
	     when "1001" =>
		    dato <= mul_reg9;
	     when "1010" =>
		    dato <= mul_reg10;
	     when "1011" =>
		    dato <= mul_reg11;
	     when "1100" =>
		    dato <= mul_reg12;
	     when "1101" =>
		    dato <= mul_reg13;
	     when "1110" =>
		    dato <= mul_reg14;
	     when "1111" =>
		    dato <= mul_reg15;
        when others =>
		    null;
		end case;
		
end process;

---------------------------------
--
-- Perform 32 x 32 multiply
--
---------------------------------

my_mul32 : process(
                     mul_reg0, mul_reg1, mul_reg2, mul_reg3,
                     mul_reg4, mul_reg5, mul_reg6, mul_reg7
							 )
variable mul_left_hi  : std_logic_vector(17 downto 0);
variable mul_left_lo  : std_logic_vector(17 downto 0);
variable mul_right_hi : std_logic_vector(17 downto 0);
variable mul_right_lo : std_logic_vector(17 downto 0);
variable mul_out_0    : std_logic_vector(35 downto 0);
variable mul_out_1    : std_logic_vector(35 downto 0);
variable mul_out_2    : std_logic_vector(35 downto 0);
variable mul_out_3    : std_logic_vector(35 downto 0);
variable mul_out      : std_logic_vector(63 downto 0);
begin
  mul_left_hi  := "00" & mul_reg0 & mul_reg1;
  mul_left_lo  := "00" & mul_reg2 & mul_reg3; 
  mul_right_hi := "00" & mul_reg4 & mul_reg5;
  mul_right_lo := "00" &mul_reg6 & mul_reg7;
  mul_out_0    := mul_left_hi * mul_right_hi;
  mul_out_1    := mul_left_hi * mul_right_lo;
  mul_out_2    := mul_left_lo * mul_right_hi;
  mul_out_3    := mul_left_lo * mul_right_lo;
  mul_out      := (mul_out_0( 31 downto 0) & "0000000000000000" & "0000000000000000") +
                  ("0000000000000000" & mul_out_1( 31 downto 0) & "0000000000000000") +
                  ("0000000000000000" & mul_out_2( 31 downto 0) & "0000000000000000") +
                  ("0000000000000000" & "0000000000000000" & mul_out_3( 31 downto 0));
  mul_reg8  <= mul_out(63 downto 56);
  mul_reg9  <= mul_out(55 downto 48);
  mul_reg10 <= mul_out(47 downto 40);
  mul_reg11 <= mul_out(39 downto 32);
  mul_reg12 <= mul_out(31 downto 24);
  mul_reg13 <= mul_out(23 downto 16);
  mul_reg14 <= mul_out(15 downto  8);
  mul_reg15 <= mul_out( 7 downto  0);
end process;

end rtl;
	
