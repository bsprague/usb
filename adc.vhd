----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:14:18 04/01/2012 
-- Design Name: 
-- Module Name:    adc - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

-- Recent changes: Switched conversion reg to use external reference (should change back?)
-- Switched register setup to every start cmd instead of just on startup

entity adc is
    Port ( 	clk_in : in STD_LOGIC;
				rst_in : in STD_LOGIC;
				dout : in  STD_LOGIC;
				eoc_n : in  STD_LOGIC;
				go : in	STD_LOGIC;
				running : out STD_LOGIC;
				done : out STD_LOGIC;
				cs_n : out  STD_LOGIC;
				cnvst_n : out  STD_LOGIC;
				sclk : out  STD_LOGIC;
				din : out  STD_LOGIC;
				samp1 : out STD_LOGIC_VECTOR(11 downto 0);
				samp2 : out STD_LOGIC_VECTOR(11 downto 0);
				samp3 : out STD_LOGIC_VECTOR(11 downto 0);
				samp4 : out STD_LOGIC_VECTOR(11 downto 0);
				samp5 : out STD_LOGIC_VECTOR(11 downto 0);
				samp6 : out STD_LOGIC_VECTOR(11 downto 0);
				samp7 : out STD_LOGIC_VECTOR(11 downto 0)
				);
end adc;

architecture Behavioral of adc is

	type state_type is (init,setupreg,regpause,cnvreg,idle,scanrequest,scanwait,bytesample,bytewait);
	
	signal state, next_state : state_type;

	signal sclk_i,cs_n_i,cnvst_n_i,din_i,running_i,done_i : std_logic;	--Internal signals for outputs
	signal bit_count : integer range 0 to 15;
	signal channel_count : std_logic_vector(2 downto 0);
	
	-- ADC register configurations
	constant setupregister : std_logic_vector(7 downto 0) := "01001000";
	constant cnvregister : std_logic_vector(7 downto 0) := "10110000";
	
begin
	
	SYNC_PROC: process (clk_in)
   begin
      if (rising_edge(clk_in)) then
         if (rst_in = '1') then
            state 		<= init;
         else
            state 		<= next_state;
         end if;        
      end if;
   end process;
	
	sclk <= sclk_i;
	cs_n <= cs_n_i;
	cnvst_n <= cnvst_n_i;
	din <= din_i;
	running <= running_i;
	done <= done_i;
	
	OUTPUT_DECODE: process (state,clk_in,bit_count)
	begin
		case(state) is
			when init =>
				cs_n_i 		<= '1';
				cnvst_n_i 	<= '1';
				sclk_i 		<= '0';
				din_i 		<= '0';
				running_i	<= '0';
				done_i		<= '0';
			when setupreg =>
				cs_n_i 		<= '0';
				cnvst_n_i 	<= '1';
				sclk_i 		<= not clk_in;
				din_i 		<= setupregister(7 - bit_count); -- MSB first
				running_i	<= '0';
				done_i		<= '0';	
			when regpause =>
				cs_n_i 		<= '1';
				cnvst_n_i 	<= '1';
				sclk_i 		<= '0';
				din_i 		<= '0';
				running_i	<= '0';
				done_i		<= '0';
			when cnvreg =>
				cs_n_i 		<= '0';
				cnvst_n_i 	<= '1';
				sclk_i 		<= not clk_in;
				din_i 		<= cnvregister(7 - bit_count);	-- MSB first;
				running_i	<= '0';
				done_i		<= '0';
			when idle =>
				cs_n_i 		<= '1';
				cnvst_n_i 	<= '1';
				sclk_i 		<= '0';
				din_i 		<= '0';
				running_i	<= '0';
				done_i		<= '1'; -- Ready and waiting for 'go'
			when scanrequest =>
				cs_n_i 		<= '1';
				cnvst_n_i 	<= '0';
				sclk_i 		<= '0';
				din_i 		<= '0';
				running_i	<= '1';	-- Sampling, usb_sm should deassert 'go'
				done_i		<= '0';
			when scanwait =>
				cs_n_i 		<= '1';
				cnvst_n_i 	<= '1';
				sclk_i 		<= '0';
				din_i 		<= '0';
				running_i	<= '1';
				done_i		<= '0';
			when bytesample =>
				cs_n_i		<= '0';
				cnvst_n_i 	<= '1';
				sclk_i 		<= not clk_in;
				din_i 		<= '0';
				running_i	<= '1';
				done_i		<= '0';
			when bytewait =>
				cs_n_i		<= '0'; 	-- Wait in between bytes (CS low?)
				cnvst_n_i 	<= '1';
				sclk_i 		<= '0';
				din_i 		<= '0';
				running_i	<= '1';
				done_i		<= '0';
			when others =>
				cs_n_i		<= '1';
				cnvst_n_i 	<= '1';
				sclk_i 		<= '0';
				din_i 		<= '0';
				running_i	<= '0';
				done_i		<= '0';
		end case;
	end process;

	NEXTSTATE_DECODE: process (state,bit_count,channel_count,eoc_n,go)
	begin
		case(state) is
			when init => 
				next_state <= setupreg;
			when setupreg =>
				if (bit_count = 7) then
					next_state <= regpause;
				else 
					next_state <= setupreg;
				end if;
			when regpause =>
				next_state <= cnvreg;
			when cnvreg =>
				if (bit_count = 7) then
					next_state <= idle;
				else 
					next_state <= cnvreg;
				end if;
			when idle =>
				if (go = '1') then
					next_state <= scanrequest;
				else
					next_state <= idle;
				end if;
			when scanrequest =>
				next_state <= scanwait;
			when scanwait =>
				if (eoc_n = '0') then
					next_state <= bytewait;
				else
					next_state <= scanwait;
				end if;
			when bytesample =>
				if (bit_count = 7) then 
					next_state <= bytewait;
				elsif (bit_count = 15) then
					if (channel_count = "111") then
						next_state <= idle;
					else
						next_state <= bytewait;
					end if;
				else
					next_state <= bytesample;
				end if;
			when bytewait =>
				next_state <= bytesample;
			when others =>
				next_state <= idle;
		end case;
	end process;
	
	SYNC_COUNTERS: process(clk_in) is
	begin
		if(rising_edge(clk_in)) then
			if(rst_in = '1') then
				bit_count <= 0;
				channel_count <= "000";
			else
				case (state) is
					when init =>
						bit_count <= 0;
						channel_count <= "000";
					when setupreg =>
						bit_count <= bit_count + 1;
					when regpause =>
						bit_count <= 0;
					when cnvreg =>
						bit_count <= bit_count + 1;
					when idle =>
						bit_count <= 0;
						channel_count <= "000";
					when bytesample =>
						if bit_count = 15 then
							bit_count <= 0;
						else
							bit_count <= bit_count + 1;
						end if;
						channel_count <= channel_count;
					when bytewait =>
						if (bit_count = 8) then
							channel_count <= channel_count;
						else
							channel_count <= channel_count + '1';
						end if;
					when others =>
						bit_count <= bit_count;
						channel_count <= channel_count;
				end case;
			end if;
		end if;
	end process;
	
	SYNC_DATA: process(sclk_i) is
	begin
		if(rising_edge(sclk_i)) then
			if(state = bytesample) then
				if(bit_count > 3) then -- First 4 bits are empty
					case(channel_count) is				-- Record data sample (MSB -> LSB)
						when "001" =>
							samp1(15 - bit_count) <= dout; 	-- First bit is 4, 14 - 4 = 11 (MSB)
						when "010" =>
							samp2(15 - bit_count) <= dout; 
						when "011" =>
							samp3(15 - bit_count) <= dout; 
						when "100" =>
							samp4(15 - bit_count) <= dout; 
						when "101" =>
							samp5(15 - bit_count) <= dout; 
						when "110" =>
							samp6(15 - bit_count) <= dout; 
						when "111" =>
							samp7(15 - bit_count) <= dout; 
						when others =>
					end case;
				end if;
			elsif(state = setupreg) then
				samp1 <= (others => '0');
				samp2 <= (others => '0');
				samp3 <= (others => '0');
				samp4 <= (others => '0');
				samp5 <= (others => '0');
				samp6 <= (others => '0');
				samp7 <= (others => '0');
			end if;
		end if;
	end process;
	
	
end Behavioral;

