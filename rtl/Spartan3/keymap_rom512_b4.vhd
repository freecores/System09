---------------------------------------------------------
--
-- PS2 Keycode look up table
-- converts 7 bit key code to ASCII
-- Address bit 7 = CAPS Lock
-- Address bit 8 = Shift
--
-- J.E.Kent
-- 18th Oct 2004
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
library unisim;
	use unisim.vcomponents.all;

entity keymap_rom is
    Port (
       clk   : in  std_logic;
       rst   : in  std_logic;
       cs    : in  std_logic;
       rw    : in  std_logic;
       addr  : in  std_logic_vector (8 downto 0);
       rdata : out std_logic_vector (7 downto 0);
       wdata : in  std_logic_vector (7 downto 0)
    );
end keymap_rom;

architecture rtl of keymap_rom is


signal we : std_logic;

begin

  ROM : RAMB4_S8
    generic map (
    INIT_00 => x"00327761737a0000003171000000000000600900000000000000000000000000",
    INIT_01 => x"003837756a6d00000036796768626e0000357274667620000033346564786300",
    INIT_02 => x"00005c005d0d000000003d5b00270000002d703b6c2f2e000039306f696b2c00",
    INIT_03 => x"0000000000000000001b000000007f0000000000000000000008000000000000",
    INIT_04 => x"00325741535a00000031510000000000007e0900000000000000000000000000",
    INIT_05 => x"003837554a4d00000036594748424e0000355254465620000033344544584300",
    INIT_06 => x"00005c005d0d000000003d5b00270000002d503b4c2f2e000039304f494b2c00",
    INIT_07 => x"0000000000000000001b000000007f0000000000000000000008000000000000",
    INIT_08 => x"00405741535a00000021510000000000007e0900000000000000000000000000",
    INIT_09 => x"002a26554a4d0000005e594748424e0000255254465620000023244544584300",
    INIT_0A => x"00007c007d0d000000002b7b00220000005f503a4c3f3e000028294f494b3c00",
    INIT_0B => x"0000000000000000001b000000007f0000000000000000000008000000000000",
    INIT_0C => x"00407761737a0000002171000000000000600900000000000000000000000000",
    INIT_0D => x"002a26756a6d0000005e796768626e0000257274667620000023246564786300",
    INIT_0E => x"00007c007d0d000000002b7b00220000005f703a6c3f3e000028296f696b3c00",
    INIT_0F => x"0000000000000000001b000000007f0000000000000000000008000000000000"
    )

    port map (
		clk => clk,
		en => cs,
		we => we,
		rst => rst,
		addr => addr,
		di => wdata,
		do => rdata
	);


my_keymap_rom : process ( rw )
begin
	 we    <= not rw;
end process;

end architecture rtl;

