LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY mon_rom IS
        PORT (
          cs       : IN STD_LOGIC;
          addr     : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
          data_out : OUT STD_LOGIC_VECTOR (14 DOWNTO 0));
END mon_rom;

ARCHITECTURE behavior OF mon_rom IS

COMPONENT asyn_rom_256x15
-- pragma translate_off
     GENERIC (LPM_FILE : string);

-- pragma translate_on
     PORT (
        Address : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        MemEnab : IN STD_LOGIC;
        Q       : OUT STD_LOGIC_VECTOR(14 DOWNTO 0)
     );
END COMPONENT;

BEGIN

   u1: asyn_rom_256x15
-- pragma translate_off
        GENERIC MAP (LPM_FILE => "u1.hex")
-- pragma translate_on
   PORT MAP (Address => addr, MemEnab => memenab, Q =>q);
END behavior;
