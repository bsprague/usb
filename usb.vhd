----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    07:33:52 03/24/2012 
-- Design Name: 
-- Module Name:    usb - Behavioral 
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

entity usb is
    Port ( clk_in : in  STD_LOGIC;
           ftdi_rxf : in  STD_LOGIC;
           ftdi_txe : in  STD_LOGIC;
           ftdi_din : in  STD_LOGIC_VECTOR (7 downto 0);
			  ftdi_dout : out  STD_LOGIC_VECTOR (7 downto 0);
			  ftdi_dir : out STD_LOGIC;
           ftdi_rd : out  STD_LOGIC;
           ftdi_wr : out  STD_LOGIC;
			  sc_load : out STD_LOGIC;
			  sc_seq : out STD_LOGIC_VECTOR(16 downto 0);
           rst_in : in  STD_LOGIC);
end usb;

architecture Behavioral of usb is

	signal WR_i,RD_i : std_logic;  
	signal send_i, send : std_logic;
	signal command : std_logic_vector(7 downto 0);
	signal sc_chunk : std_logic_vector(1 downto 0);
	signal sc_load_i, sc_capture, sc_capture_i : std_logic;
	signal sc_seq_i : std_logic_vector(16 downto 0);
	signal capture_i, capture : std_logic;
	
	signal samplenum : std_logic_vector(15 downto 0);
	
	type state_type is (idle,avail,rbyte,cbyte1,cbyte2,style,pbyte,sbyte,sampledone,samplecheck,
								sc_check,sc_wait,sc_send);
	
	signal state, next_state : state_type;

begin

	SYNC_PROC: process (clk_in)
   begin
      if (rising_edge(clk_in)) then
         if (rst_in = '1') then
            state 		<= idle;
				send			<= '0';
				capture		<= '0';
				ftdi_rd		<= '1';
				ftdi_wr		<= '1';
				sc_load		<= '0';
				sc_capture	<= '0';
				sc_seq		<= (others => '0');
         else
            state 		<= next_state;
				send			<= send_i;
				capture		<= capture_i;
				ftdi_rd		<= RD_i;
				ftdi_wr		<= WR_i;
				sc_load		<= sc_load_i;
				sc_capture	<= sc_capture_i;
				sc_seq		<= sc_seq_i;
         end if;        
      end if;
   end process;
	
	OUTPUT_DECODE: process (state)
	begin
		case (state) is
			when idle => 	-- IDLE state: Waiting for ftdi_rxf to go low
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when avail => 	-- AVAIL state: ftdi_rxf low, byte(s) available
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when rbyte => 	-- RBYTE state: Done waiting, issue read
				RD_i 				<= '0';	-- assert read to get byte
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when cbyte1 =>	-- CBYTE1 state: Capture byte
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '1';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when cbyte2 =>	-- CBYTE2 state: Capture byte
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '1';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when style => 	-- Style state: Decode command
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when pbyte => 	-- PBYTE state: Present byte, wait for ftdi_txe
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '1';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when sbyte =>	-- SBYTE state: Send byte
				RD_i 				<= '1';	
            WR_i			 	<= '0';	-- assert write to send byte
				send_i			<= '1';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';	
			when sampledone => 	-- SAMPLEDONE state: Done with sample
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				capture_i		<= '0';
			when samplecheck => 	-- SAMPLECHECK state: Check for more samples
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when sc_check => 	-- SC_CHECK state: Check if done with sc payload
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '1';
			when sc_wait => 	-- SC_WAIT state: Wait for next byte in sc payload
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
			when sc_send => 	-- SC_SEND state: Send scan chain sequence
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '1';
				sc_capture_i	<= '0';
			when others =>
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
		end case;
	end process;
	
	NEXT_STATE_DECODE: process (state,ftdi_rxf,ftdi_txe,command,samplenum,sc_chunk)
   begin
      --declare default state for next_state to avoid latches
      next_state <= state;  --default is to stay in current state
		case (state) is
			---------------------------------------------------------------------
			-- IDLE: Waiting for ftdi_rxf to go low
			when idle =>
            if ftdi_rxf = '0' then -- data ready to be read from USB
				   next_state <= avail;
				else
				   next_state <= idle;
				end if;
			---------------------------------------------------------------------
			-- AVAIL: ftdi_rxf goes low indicating byte(s) available
			when avail =>
				 next_state <= rbyte;
			---------------------------------------------------------------------
			-- RBYTE: issue read
			when rbyte =>
				 next_state <= cbyte1;
			---------------------------------------------------------------------
			-- CBYTE1: capture byte
			when cbyte1 =>
				 next_state <= cbyte2;
			---------------------------------------------------------------------
			-- CBYTE2: capture byte - dummy state (command wasn't captured in time)
			when cbyte2 =>
				 if(sc_chunk = "00") then	--Not loading scan chain payload
					next_state <= style;		--Interpret command
				 else
					next_state <= sc_check;	--Capture scan chain payload
				 end if;
			---------------------------------------------------------------------
			-- STYLE: Determine command, wait for local done
			when style =>
				case (command(7 downto 4)) is
					when X"1" => 
						next_state <= pbyte; -- new cmd, send byte
					when X"4" => 
						next_state <= sc_check; -- new cmd, send byte
					when others => 
						next_state <= idle;
				end case;
			---------------------------------------------------------------------
			-- PBYTE: Present Byte, wait for ftdi_txe assertion
			when pbyte =>
            if (ftdi_txe='0') then -- data ready to be writen to USB
				   next_state <= sbyte;
				else
				   next_state <= pbyte;
				end if;
			---------------------------------------------------------------------
			-- SBYTE: Send Byte, wait timer done
			when sbyte =>
				next_state <= sampledone;
			---------------------------------------------------------------------
			-- SAMPLEDONE: Sample done
			when sampledone =>
            next_state <= samplecheck;
			---------------------------------------------------------------------
			-- SAMPLECHECK: Check if done with samples
			when samplecheck =>
            if samplenum = X"0000" then
					next_state <= idle; -- done
				else
				   next_state <= pbyte;
				end if;
			---------------------------------------------------------------------
			-- SC_CHECK: Check if done with sc payload
			when sc_check =>
				if sc_chunk = "10" then
					next_state <= sc_send;
				else
				   next_state <= sc_wait;
				end if;
			---------------------------------------------------------------------
			-- SC_WAIT: Wait for next byte in sc payload
			when sc_wait =>
				if ftdi_rxf = '0' then -- data ready to be read from USB
				   next_state <= avail;
				else
				   next_state <= sc_wait;
				end if;
			---------------------------------------------------------------------
			-- SC_SEND: Send scan chain sequence
			when sc_send =>
				next_state <= idle;
			when others =>
				next_state <= idle;
		end case;
	end process;
	
	SYNC_CAPTURE: process (clk_in)
   begin
      if (rising_edge(clk_in)) then
         if (rst_in = '1') then
				command  <= X"00";
			else
				if(capture = '1') then
					command <= ftdi_din;
				else
					command <= command;
				end if;
			end if;
		end if;
	end process;
	
	SYNC_DATAOUT: process (clk_in)
   begin
      if (rising_edge(clk_in)) then
         if (rst_in = '1') then
				ftdi_dout  <= X"00";
			else
				ftdi_dout <= X"FF";--samplenum(7 downto 0);
			end if;
		end if;
	end process;
	
	SYNC_DIR : process (clk_in, rst_in) 
	begin
		if rst_in='1' then
			ftdi_dir <= '1'; --tristates enabled for input
		elsif (rising_edge(clk_in)) then
			ftdi_dir <= NOT(send);
		end if;
	end process;
	
	SYNC_SC: process (clk_in)
	begin
		if (rising_edge(clk_in)) then
         if (rst_in = '1') then
				sc_chunk <= "00";
				sc_seq_i <= (others => '0');
			else
				if(sc_load_i = '1') then
					sc_seq_i <= sc_seq_i;
					sc_chunk <= "00";
				elsif(capture = '1') then
					case(sc_chunk) is
						when "00" =>
							sc_seq_i(16) <= ftdi_din(0);
						when "01" =>
							sc_seq_i(15 downto 8) <= ftdi_din;
						when "10" =>
							sc_seq_i(7 downto 0) <= ftdi_din;
						when others =>
							sc_seq_i <= sc_seq_i;
					end case;
					--sc_chunk <= sc_chunk + 1;
				elsif(sc_capture = '1') then
					sc_chunk <= sc_chunk + 1;
				else
					sc_seq_i <= sc_seq_i;
					sc_chunk <= sc_chunk;
				end if;
			end if;
		end if;
	end process;
	
	SAMPLECOUNTER : process (clk_in)
	begin
		if (rising_edge(clk_in)) then
			case (state) is
				when style =>
					case (command(3 downto 0)) is
						when X"0" =>
							samplenum <= X"0001";
						when X"1" =>
							samplenum <= X"0002";
						when X"2" =>
							samplenum <= X"0004";
						when X"3" =>
							samplenum <= X"0008";
						when X"4" =>
							samplenum <= X"0010";
						when X"5" =>
							samplenum <= X"0020";
						when X"6" =>
							samplenum <= X"0040";
						when X"7" =>
							samplenum <= X"0080";
						when X"8" =>
							samplenum <= X"0100";
						when X"9" =>
							samplenum <= X"0200";
						when X"A" =>
							samplenum <= X"0400";
						when X"B" =>
							samplenum <= X"0800";
						when X"C" =>
							samplenum <= X"1000";
						when X"D" =>
							samplenum <= X"2000";
						when X"E" =>
							samplenum <= X"4000";
						when X"F" =>
							samplenum <= X"8000";
						when others =>
							samplenum <= X"0001";
					end case;
				when sampledone =>
					samplenum <= samplenum - X"0001";
				when others =>
					samplenum <= samplenum;
			end case;
		end if;
	end process;
					

end Behavioral;

