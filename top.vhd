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
--use IEEE.STD_LOGIC_ARITH.ALL;
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
    Port ( 	sys_clk_in : in  STD_LOGIC;
				rst_in : in  STD_LOGIC;
				sc_clkout : out STD_LOGIC;
				sc_dout : out STD_LOGIC;
				ftdi_d : inout  STD_LOGIC_VECTOR (7 downto 0);
				ftdi_rxf : in  STD_LOGIC;
				ftdi_txe : in  STD_LOGIC;
				ftdi_rd : out  STD_LOGIC;
				ftdi_wr : out  STD_LOGIC;
				adc_dout : in STD_LOGIC;
				adc_eoc : in STD_LOGIC;
				adc_cs : out STD_LOGIC;
				adc_cnvst : out STD_LOGIC;
				adc_sclk : out STD_LOGIC;
				adc_din : out STD_LOGIC;
				ls_oe : out STD_LOGIC;
				ls_dir : out STD_LOGIC;
				srstb : out STD_LOGIC;
				urstb : out STD_LOGIC;
				c1rstb : out STD_LOGIC;
				c2rstb : out STD_LOGIC
				);
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
		ftdi_dout : OUT std_logic_vector(7 downto 0);
		adc_go	: OUT std_logic;
		adc_running	  : IN std_logic;
		adc_done		  : IN std_logic;
		adc_samp1	  : IN std_logic_vector(11 downto 0);
		adc_samp2	  : IN std_logic_vector(11 downto 0);
		adc_samp3	  : IN std_logic_vector(11 downto 0);
		adc_samp4	  : IN std_logic_vector(11 downto 0);
		adc_samp5	  : IN std_logic_vector(11 downto 0);
		adc_samp6	  : IN std_logic_vector(11 downto 0);
		adc_samp7	  : IN std_logic_vector(11 downto 0);
		c1rstb		  : OUT std_logic;
		c2rstb		  : OUT std_logic
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
		srstb : out STD_LOGIC;
		c1rstb : out STD_LOGIC;
		c2rstb : out STD_LOGIC
		);
	END COMPONENT;
	
COMPONENT adc
	PORT(
		clk_in : IN std_logic;
		rst_in : IN std_logic;
		dout : IN std_logic;
		eoc_n : IN std_logic;
		go : IN std_logic;          
		running : OUT std_logic;
		done : OUT std_logic;
		cs_n : OUT std_logic;
		cnvst_n : OUT std_logic;
		sclk : OUT std_logic;
		din : OUT std_logic;
		samp1 : OUT std_logic_vector(11 downto 0);
		samp2 : OUT std_logic_vector(11 downto 0);
		samp3 : OUT std_logic_vector(11 downto 0);
		samp4 : OUT std_logic_vector(11 downto 0);
		samp5 : OUT std_logic_vector(11 downto 0);
		samp6 : OUT std_logic_vector(11 downto 0);
		samp7 : OUT std_logic_vector(11 downto 0)
		);
	END COMPONENT;
	
signal ftdi_dir : std_logic;
signal ftdi_dout : std_logic_vector(7 downto 0);
signal ftdi_din : std_logic_vector(7 downto 0);

signal sc_load : std_logic;
signal sc_seq : std_logic_vector(16 downto 0);

signal usb_clk : std_logic;
signal usb_clk_divider : std_logic_vector(3 downto 0);

signal adc_samp1,adc_samp2,adc_samp3,adc_samp4,adc_samp5,adc_samp6,adc_samp7 : std_logic_vector(11 downto 0);
signal adc_go,adc_running,adc_done : std_logic;

signal sc_c1rstb, sc_c2rstb : std_logic;	
signal usb_c1rstb, usb_c2rstb : std_logic;

begin

--Constant outputs for the level shifter
ls_oe <= '0';
ls_dir <= '1';


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
		ftdi_dout     => ftdi_dout,
		adc_go		  => adc_go,
		adc_running	  => adc_running,
		adc_done		  => adc_done,
		adc_samp1	  => adc_samp1,
		adc_samp2	  => adc_samp2,
		adc_samp3	  => adc_samp3,
		adc_samp4	  => adc_samp4,
		adc_samp5	  => adc_samp5,
		adc_samp6	  => adc_samp6,
		adc_samp7	  => adc_samp7,
		c1rstb		  => usb_c1rstb,
		c2rstb		  => usb_c2rstb
	);
	
Inst_scan_config: scan_config
	PORT MAP(
		clk_in 			=> sys_clk_in,
		rst 				=> sc_load,
		sequence_in		=> sc_seq,
      shift_out 		=> sc_dout,
		clk_out 			=> sc_clkout,
		urstb 			=>	urstb,
		srstb 			=> srstb,
		c1rstb			=> sc_c1rstb,
		c2rstb			=> sc_c2rstb
	);

Inst_adc: adc PORT MAP(
		clk_in 		=> usb_clk,
		rst_in 		=> rst_in,
		dout 			=> adc_dout,
		eoc_n 		=> adc_eoc,
		go 			=> adc_go,
		running 		=> adc_running,
		done 			=> adc_done,
		cs_n 			=> adc_cs,
		cnvst_n 		=> adc_cnvst,
		sclk 			=> adc_sclk,
		din 			=> adc_din,
		samp1 		=> adc_samp1,
		samp2 		=> adc_samp2,
		samp3 		=> adc_samp3,
		samp4 		=> adc_samp4,
		samp5 		=> adc_samp5,
		samp6 		=> adc_samp6,
		samp7 		=> adc_samp7
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

	CAP_RESETS : process(sys_clk_in)
	begin
		if(rising_edge(sys_clk_in)) then
			if(usb_c1rstb = '0' or sc_c1rstb = '0') then
				c1rstb <= '0';
			else
				c1rstb <= '1';
			end if;

			if(usb_c2rstb = '0' or sc_c2rstb = '0') then
				c2rstb <= '0';
			else
				c2rstb <= '1';
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

