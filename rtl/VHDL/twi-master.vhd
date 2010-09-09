--===========================================================================--
--
-- SYNTHEZIABLE VHDL TWO WIRE INTERFACE
--
--===========================================================================
--
-- This core adheres to the GNU public license  
--
-- Design units   : TWI Master core
--
-- File name      : twi.vhd
--
-- Purpose        : Implements an I2C master Interface 
--                  
-- Dependencies   : ieee.std_logic_1164
--                  ieee.numeric_std
--                  unisim.vcomponents
--
-- Revision list  :
--
-- Version Author        Date         Changes
--
-- 0.1     John Kent     2010-05-04   New model
--
--         dilbert57@opencores.org
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library unisim;
      use unisim.vcomponents.all;

-----------------------------------------------------------------------
-- Entity for TWI                                                    --
-----------------------------------------------------------------------

entity twi is
  generic (
    CLK_FREQ : integer := 25_000_000;
  );

  port (
    --
    -- CPU signals
    --
    clk      : in  std_logic;                     -- System Clock
    rst      : in  std_logic;                     -- Reset input (active high)
    cs       : in  std_logic;                     -- Chip Select
    rw       : in  std_logic;                     -- Read / Not Write
    irq      : out std_logic;                     -- Interrupt
    addr     : in  std_logic;                     -- Register Select
    data_in  : in  std_logic_vector(7 downto 0);  -- Data Bus In 
    data_out : out std_logic_vector(7 downto 0);  -- Data Bus Out

    -- I2C Signals
    --
    scl     : inout std_logic;                    -- serial clock
    sda     : inout std_logic                     -- serial data
    );
end twi;  

-------------------------------------------------------------------------------
-- Architecture for Two Wire Interface registers
-------------------------------------------------------------------------------

architecture rtl of twi is

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  ----------------------------------------------------------------------
  --  Status Register: twi_status_reg 
  ----------------------------------------------------------------------
  --
  -- IO address + 0 Read
  --
  --+-------+-------+-------+-------+-------+-------+-------+-------+
  --| RXIRQ | TXIRQ | ACKE  |       |       |       | TXRDY | RXRDY |
  --+-------+-------+-------+-------+-------+-------+-------+-------+
  -- RXIRQ - Bit[7] - Receive Interrupt Request
  -- TXIRQ - Bit[6] - Transmit Interrupt Request
  -- ACKE  - Bit[5] - Acknowledge Error
  -- TXRDY - Bit[1] - Transmit Ready (byte transmitted)
  -- RXRDY - Bit[0] - Receive Ready (byte received)
  -- 
  signal twi_status_reg : std_logic_vector(7 downto 0) := (others => '0');

  ----------------------------------------------------------------------
  --  Control Register: twi_control_reg
  ----------------------------------------------------------------------
  --
  -- IO address + 0 Write
  --
  --+--------+-------+--------+--------+--------+--------+--------+--------+
  --|  RXIE  | TXIE  | TWPS(1)| TWPS(0)| TWBR(3)| TWBR(2)| TWBR(1)| TWBR(0)|
  --+--------+-------+--------+--------+--------+--------+--------+--------+
  -- RXIE - Bit[7]
  -- 0       - Rx Interrupt disabled
  -- 1       - Rx Interrupt enabled
  -- TXIE - Bit[6]
  -- 0       - Tx Interrupt disabled
  -- 1       - Tx Interrupt enabled
  --
  -- SCL frequency = CPU Clock Frequency / ( 16 + 2(TWBR) . 4^TWPS)
  --
  -- TWPS - Bits[5..4]
  -- 0 0     - Prescale by  1
  -- 0 1     - Prescale by  4
  -- 1 0     - Prescale by 16
  -- 1 1     - Prescale by 64
  --
  -- TWBR - Bits[3..0]
  -- 0 0 0 0   - Baud Clk divide by  1
  -- 0 0 0 1   - Baud Clk divide by  2
  -- 0 0 1 0   - Baud Clk divide by  3
  -- 0 0 1 1   - Baud Clk divide by  4
  -- 0 1 0 0   - Baud Clk divide by  5
  -- 0 1 0 1   - Baud Clk divide by  6
  -- 0 1 1 0   - Baud Clk divide by  7
  -- 0 1 1 1   - Baud Clk divide by  8
  -- 1 0 0 0   - Baud Clk divide by  9
  -- 1 0 0 1   - Baud Clk divide by 10
  -- 1 0 1 0   - Baud Clk divide by 11
  -- 1 0 1 1   - Baud Clk divide by 12
  -- 1 1 0 0   - Baud Clk divide by 13
  -- 1 1 0 1   - Baud Clk divide by 14
  -- 1 1 1 0   - Baud Clk divide by 15
  -- 1 1 1 1   - Baud Clk divide by 16
  

  signal twi_control_reg : std_logic_vector(7 downto 0) := (others => '0'); -- control register

  ----------------------------------------------------------------------
  -- Receive Register
  ----------------------------------------------------------------------
  --
  -- IO address + 1     Read
  --
  signal twi_rx_reg : std_logic_vector(7 downto 0) := (others => '0');

  ----------------------------------------------------------------------
  -- Transmit Register
  ----------------------------------------------------------------------
  --
  -- IO address + 1     Write
  --
  signal twi_tx_reg : std_logic_vector(7 downto 0) := (others => '0');
  
  signal Reset    : std_logic;          -- Reset (Software & Hardware)
  signal RxRst    : std_logic;          -- Receive Reset (Software & Hardware)
  signal TxRst    : std_logic;          -- Transmit Reset (Software & Hardware)
  signal TxDbit   : std_logic;          -- Transmit data bit
  signal RxDR     : std_logic := '0';   -- Receive Data ready
  signal TxIdle   : std_logic;          -- Transmitter idle
  signal TxBE     : std_logic := '0';   -- Transmit buffer empty
  signal TxAck    : std_logic;          -- Byte transmitted to transmitter
  --
  signal FErr     : std_logic := '0';   -- Frame error
  signal OErr     : std_logic := '0';   -- Output error
  signal PErr     : std_logic := '0';   -- Parity Error
  --
  signal TxIEnb   : std_logic := '0';   -- Transmit interrupt enable
  signal RxIEnb   : std_logic := '0';   -- Receive interrupt enable
  --
  signal ReadRR   : std_logic := '0';   -- Read receive buffer
  signal WriteTR  : std_logic := '0';   -- Write transmit buffer
  signal ReadSR   : std_logic := '0';   -- Read Status register
  --
  signal DCDState : DCD_State_Type;     -- DCD Reset state sequencer
  signal DCDDel   : std_logic := '0';   -- Delayed DCD_n
  signal DCDEdge  : std_logic := '0';   -- Rising DCD_N Edge Pulse
  signal DCDInt   : std_logic := '0';   -- DCD Interrupt

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

  RxDev : entity ACIA_RX port map (
    Clk    => clk,
    RxRst  => RxRst,
    RxRd   => ReadRR,
    WdFmt  => CtrlReg(4 downto 2),
    BdFmt  => CtrlReg(1 downto 0),
    RxClk  => RxC,
    RxDat  => RxD,
    RxFErr => FErr,
    RxOErr => OErr,
    RxPErr => PErr,
    RxRdy  => RxDR,
    RxDout => RecvReg
   );

  TxDev : entity ACIA_TX port map (
    Clk   => clk,
    Reset => TxRst,
    Wr    => WriteTR,
    Din   => TxReg,
    WdFmt => CtrlReg(4 downto 2),
    BdFmt => CtrlReg(1 downto 0),
    TxClk => TxC,
    Dat   => TxDbit,
    Empty => TxIdle
   );

---------------------------------------------------------------
-- ACIA Reset may be hardware or software
---------------------------------------------------------------
  ACIA_Reset : process(clk, rst)
  begin
    -- Asynchronous External reset
    if rst = '1' then
      Reset <= '1';
    elsif falling_edge(clk) then
      -- Synchronous Software reset
      Reset <= CtrlReg(1) and CtrlReg(0);
    end if;              

  end process;

  -- Transmitter reset
  TxRst <= Reset;
  -- Receiver reset
  RxRst <= Reset or DCD_n;

  -----------------------------------------------------------------------------
  -- ACIA Status Register
  -----------------------------------------------------------------------------

  ACIA_Status : process(Reset, clk)
  begin
    if Reset = '1' then
      StatReg <= (others => '0');
    elsif falling_edge(clk) then
      StatReg(0) <= RxDR;                  -- Receive Data Ready
      StatReg(1) <= TxBE and (not CTS_n);  -- Transmit Buffer Empty
      StatReg(2) <= DCDInt;                -- Data Carrier Detect
      StatReg(3) <= CTS_n;                 -- Clear To Send
      StatReg(4) <= FErr;                  -- Framing error
      StatReg(5) <= OErr;                  -- Overrun error
      StatReg(6) <= PErr;                  -- Parity error
      StatReg(7) <= (RxIEnb and RxDR) or
                    (RxIEnb and DCDInt) or
                    (TxIEnb and TxBE);
    end if;
  end process;


-----------------------------------------------------------------------------
-- ACIA Transmit Control
-----------------------------------------------------------------------------

  ACIA_Control : process(CtrlReg, TxDbit)
  begin
    case CtrlReg(6 downto 5) is
      when "00" =>                      -- Disable TX Interrupts, Assert RTS
        TxD    <= TxDbit;
        TxIEnb <= '0';
      when "01" =>                      -- Enable TX interrupts, Assert RTS
        TxD    <= TxDbit;
        TxIEnb <= '1';
      when "10" =>                      -- Disable Tx Interrupts, Clear RTS
        TxD    <= TxDbit;
        TxIEnb <= '0';
      when "11" =>                      -- Disable Tx interrupts, Assert RTS, send break
        TxD    <= '0';
        TxIEnb <= '0';
      when others =>
        null;
    end case;

    RxIEnb <= CtrlReg(7);
  end process;

  tx_process : process(clk, reset)
  begin
    if reset = '1' then
      WriteTR <= '0';
      TxAck   <= '0';
    elsif falling_edge(clk) then
      WriteTR <= '0';
      TxAck   <= '0';
      if TxBE = '0' and TxIdle = '1' then
        WriteTR <= '1';
        TxAck   <= '1';
      end if;
    end if;
  end process;
        
-----------------------------------------------------------------------------
-- Generate Read / Write strobes.
-----------------------------------------------------------------------------

  ACIA_Read_Write : process(clk, Reset)
  begin
    if reset = '1' then
      CtrlReg <= (others => '0');
      TxReg   <= (others => '0');
      ReadRR  <= '0';
      ReadSR  <= '0';
      TxBE    <= '1';
    elsif falling_edge(clk) then
      ReadRR <= '0';
      ReadSR <= '0';
      if TxAck = '1' then
        TxBE <= '1';
      end if;
      if cs = '1' then
        if Addr = '0' then              -- Control / Status register
          if rw = '0' then              -- write control register
            CtrlReg <= DataIn;
          else                          -- read status register
            ReadSR <= '1';
          end if;
        else                            -- Data Register
          if rw = '0' then              -- write transmiter register
            TxReg <= DataIn;
            TxBE  <= '0';
          else                          -- read receiver register
            ReadRR <= '1';
          end if;
        end if;
      end if;
    end if;
  end process;

---------------------------------------------------------------
-- Set Data Output Multiplexer
--------------------------------------------------------------

  ACIA_Data_Mux : process(Addr, RecvReg, StatReg)
  begin
    if Addr = '1' then
      DataOut <= RecvReg;               -- read receiver register
    else
      DataOut <= StatReg;               -- read status register
    end if;
  end process;

  irq <= StatReg(7);

---------------------------------------------------------------
-- Data Carrier Detect Edge rising edge detect
---------------------------------------------------------------
  ACIA_DCD_edge : process(reset, clk)
  begin
    if reset = '1' then
      DCDEdge <= '0';
      DCDDel  <= '0';
    elsif falling_edge(clk) then
      DCDDel  <= DCD_n;
      DCDEdge <= DCD_n and (not DCDDel);
    end if;
  end process;


---------------------------------------------------------------
-- Data Carrier Detect Interrupt
---------------------------------------------------------------
-- If Data Carrier is lost, an interrupt is generated
-- To clear the interrupt, first read the status register
--      then read the data receive register

  ACIA_DCD_Int : process(reset, clk)
  begin
    if reset = '1' then
      DCDInt   <= '0';
      DCDState <= DCD_State_Idle;
    elsif falling_edge(clk) then
      case DCDState is
        when DCD_State_Idle =>
          -- DCD Edge activates interrupt
          if DCDEdge = '1' then
            DCDInt   <= '1';
            DCDState <= DCD_State_Int;
          end if;
        when DCD_State_Int =>
          -- To reset DCD interrupt, 
          -- First read status
          if ReadSR = '1' then
            DCDState <= DCD_State_Reset;
          end if;
        when DCD_State_Reset =>
          -- Then read receive register
          if ReadRR = '1' then
            DCDInt   <= '0';
            DCDState <= DCD_State_Idle;
          end if;
        when others =>
          null;
      end case;
    end if;
  end process;

  rts_n <= RxDR;

end rtl;

