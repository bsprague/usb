----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    16:06:58 02/27/2012 
-- Design Name: 
-- Module Name:    scan_config - Behavioral 
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

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity scan_config is
	generic(
			--Change width to change output clock frequency
			--Input clock = 66 MHz?
		WIDTH : integer range 1 to 64 := 18
		);

	Port (
		clk_in : in  STD_LOGIC;
		rst : in  STD_LOGIC;
		sequence_in : in STD_LOGIC_VECTOR(16 downto 0);
      shift_out : out  STD_LOGIC;
		clk_out : out STD_LOGIC;
		urstb : out STD_LOGIC := '1';
		srstb : out STD_LOGIC := '1');
end scan_config;
	
architecture Behavioral of scan_config is

	signal sequence : std_logic_vector(16 downto 0);
	signal counter : std_logic_vector(WIDTH - 1 downto 0);
	signal shift_clk : std_logic;
	signal shift_cnt : std_logic_vector(4 downto 0);
	
begin

	process(clk_in,rst)
	begin
		if rst = '1' then
			counter <= (others => '0');
		elsif clk_in = '1' and clk_in'event then
			counter <= counter + 1;
		end if;
	end process;
	
	process(counter(WIDTH - 1), rst, shift_cnt,sequence_in)
	begin
		if rst = '1' then
				sequence <= sequence_in; --Sequence to be loaded into scan chain
				shift_cnt <= "00000";
				shift_clk <= '0';
				urstb <= '0';
				srstb <= '0';
				
		elsif rising_edge(counter(WIDTH - 1)) then
		
				srstb <= '1';
			
				-- set up our shift_clk
				if shift_cnt >= "10001" then
					shift_clk <= '0';
				else
					shift_clk <= not shift_clk;
				end if;
				
				-- when the clk is low, set up our new data
				if shift_clk = '1' then
					sequence(16 downto 1) <= sequence(15 downto 0);
					sequence(0) <= '0';
					
					if shift_cnt < "10011" then
						shift_cnt <= shift_cnt + 1;
					else
						urstb <= '1';
					end if;
					
				end if;
		end if;
	end process;
	
	shift_out <= sequence(16);
	clk_out <= shift_clk;

end Behavioral;