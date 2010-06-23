library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

--  Uncomment the following lines to use the declarations that are
--  provided for instantiating Xilinx primitive components.
--library UNISIM;
--use UNISIM.VComponents.all;

entity SevenSegmentDisplay is
    Port ( Clk : in std_logic;
           Reset : in std_logic;
           Value0 : in std_logic_vector(3 downto 0);
           Value1 : in std_logic_vector(3 downto 0);
           Value2 : in std_logic_vector(3 downto 0);
           Value3 : in std_logic_vector(3 downto 0);
			  DPs    : in std_logic_vector(3 downto 0);
			  Blanks	: in std_logic_vector(3 downto 0);
           DigitSelect : out std_logic_vector(3 downto 0);
           Segments : out std_logic_vector(7 downto 0));
end SevenSegmentDisplay;

architecture Behavioral of SevenSegmentDisplay is

	signal ClockDivider		: std_logic_vector(13 downto 0);
	signal WhichDigit			: std_logic_vector(1 downto 0);
	signal Result				: std_logic_vector(7 downto 0);
	signal DigitValue			: std_logic_vector(3 downto 0);
	signal DP					: std_logic;
	signal Blank				: std_logic;

	component DecoderDriver
	port(
		DigitValue	: in std_logic_vector(3 downto 0);
		DP				: in std_logic;
		Blank			: in std_logic;
		Segments		: out std_logic_vector(7 downto 0));
	end component;

begin

	process(Reset,Clk) is
	begin
		if Reset = '1' then
			ClockDivider <= (others => '0');
			WhichDigit   <= "00";
			DigitSelect	<= "1111";
		elsif Clk = '1' and Clk'Event then
			if ClockDivider = "11000011010011" then
				ClockDivider <= (others => '0');
				Segments <= Result;
				case WhichDigit is	-- note that everything is pipelined
					when "00" => 
						DigitSelect <= "1110"; 
						DigitValue <= Value1; 
						DP <= DPs(1); 
						Blank <= Blanks(1);
					when "01" => 
						DigitSelect <= "1101"; 
						DigitValue <= Value2; 
						DP <= DPs(2); 
						Blank <= Blanks(2);
					when "10" => 
						DigitSelect <= "1011"; 
						DigitValue <= Value3; 
						DP <= DPs(3); 
						Blank <= Blanks(3);
					when "11" => 
						DigitSelect <= "0111"; 
						DigitValue <= Value0; 
						DP <= DPs(0); 
						Blank <= Blanks(0);
					when others => null;
				end case;
				WhichDigit <= WhichDigit + 1;
			else
				ClockDivider <= ClockDivider + 1;
			end if;
		end if;
	end process;

	Inst_DecoderDriver: DecoderDriver PORT MAP (
		DigitValue => DigitValue,
		DP => DP,
		Blank => Blank,
		Segments => Result
	);

end Behavioral;
