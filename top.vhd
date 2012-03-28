----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    10:02:19 03/24/2012 
-- Design Name: 
-- Module Name:    top - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library UNISIM;
use UNISIM.VComponents.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity top is
    Port ( sys_clk_in : in  STD_LOGIC;
           rst_in : in  STD_LOGIC;
           sc_clkout : out STD_LOGIC;
			  sc_dout : out STD_LOGIC;
			  sc_urstb : out STD_LOGIC;
			  sc_srstb : out STD_LOGIC;
           ftdi_d : inout  STD_LOGIC_VECTOR (7 downto 0);
           ftdi_rxf : in  STD_LOGIC;
           ftdi_txe : in  STD_LOGIC;
           ftdi_rd : out  STD_LOGIC;
           ftdi_wr : out  STD_LOGIC);
end top;

architecture Behavioral of top is

COMPONENT usb
	PORT(
		clk_in : IN std_logic;
		rst_in : IN std_logic;
		sc_load : OUT std_logic;
		sc_seq : OUT std_logic_vector(16 downto 0);
		ftdi_rxf : IN std_logic;
		ftdi_txe : IN std_logic;         
		ftdi_rd : OUT std_logic;
		ftdi_wr : OUT std_logic;
		ftdi_dir : OUT std_logic;
		ftdi_din : IN std_logic_vector(7 downto 0);
		ftdi_dout : OUT std_logic_vector(7 downto 0)
		);
	END COMPONENT;

COMPONENT scan_config
	PORT(
		clk_in : in  STD_LOGIC;
		rst : in  STD_LOGIC;
		sequence_in : in STD_LOGIC_VECTOR(16 downto 0);
      shift_out : out  STD_LOGIC;
		clk_out : out STD_LOGIC;
		urstb : out STD_LOGIC;
		srstb : out STD_LOGIC
		);
	END COMPONENT;
	
signal ftdi_dir : std_logic;
signal ftdi_dout : std_logic_vector(7 downto 0);
signal ftdi_din : std_logic_vector(7 downto 0);

signal sc_load : std_logic;
signal sc_seq : std_logic_vector(16 downto 0);

signal usb_clk : std_logic;
signal usb_clk_divider : std_logic_vector(3 downto 0);
	
begin

Inst_usb: usb 
	PORT MAP(
		clk_in        => usb_clk,
		rst_in        => rst_in, 
		sc_load		  => sc_load,
		sc_seq		  => sc_seq,
		ftdi_rxf      => ftdi_rxf,
		ftdi_txe      => ftdi_txe,
		ftdi_rd       => ftdi_rd,
		ftdi_wr       => ftdi_wr,
		ftdi_dir      => ftdi_dir,
		ftdi_din      => ftdi_din,
		ftdi_dout     => ftdi_dout
	);
	
Inst_scan_config: scan_config
	PORT MAP(
		clk_in 			=> sys_clk_in,
		rst 				=> sc_load,
		sequence_in		=> sc_seq,
      shift_out 		=> sc_dout,
		clk_out 			=> sc_clkout,
		urstb 			=>	sc_urstb,
		srstb 			=> sc_srstb
	);
	

		--Divide clock down to ~4 MHz
	CLK_DIVIDE : process (sys_clk_in)
	begin
		if(rising_edge(sys_clk_in)) then
			if (rst_in = '0') then
				usb_clk <= usb_clk_divider(3);
				usb_clk_divider <= usb_clk_divider + '1';
			else
				usb_clk <= '0';
				usb_clk_divider <= "0000";
			end if;
		end if;
	end process;

	usb_iobuf:
   for i in 0 to 7 generate -- generate the 8 required USB IOBUFs
	   begin
	     inst_IOBUF : IOBUF
        generic map (
          DRIVE => 12,
          IBUF_DELAY_VALUE => "0", -- Specify the amount of added input delay for buffer, "0"-"16" (Spartan-3E only)
          IFD_DELAY_VALUE => "AUTO", -- Specify the amount of added delay for input register, "AUTO", "0"-"8" (Spartan-3E only)
          IOSTANDARD => "LVCMOS33",
          SLEW => "FAST")
        port map (
          O => ftdi_din(i),   -- Buffer output
          IO => ftdi_d(i),    -- Buffer inout port (connect directly to top-level port)
          I => ftdi_dout(i),  -- Buffer input
          T => ftdi_dir       -- 3-state enable input 
       );
   end generate;
	
end Behavioral;

