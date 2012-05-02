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
	generic(
			--Width of capacitor reset pulse counter
		CR_WIDTH : integer range 1 to 64 := 18
		);
		
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
         rst_in : in  STD_LOGIC;
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
			c1rstb		 	: OUT std_logic;
			c2rstb		 	: OUT std_logic
			);
end usb;

architecture Behavioral of usb is

	signal WR_i,RD_i : std_logic;  
	signal send_i, send : std_logic;
	signal command : std_logic_vector(7 downto 0);
	signal sc_chunk : std_logic_vector(1 downto 0);
	signal sc_load_i, sc_capture, sc_capture_i : std_logic;
	signal sc_seq_i : std_logic_vector(16 downto 0);
	signal capture_i, capture : std_logic;
	signal byte_count : std_logic_vector(3 downto 0);
	signal sample_en : std_logic;
	signal sample_timer : std_logic_vector(22 downto 0);
	signal adc_go_i : std_logic;
	signal sample_period : integer range 0 to 22;
	signal c1rstb_i,c2rstb_i : std_logic;
	signal sample_counter : std_logic_vector(11 downto 0); -- Testing
	signal cap_count : std_logic_vector(CR_WIDTH - 1 downto 0);
	
	type state_type is (init,idle,avail,rbyte,cbyte1,cbyte2,style,pbyte,sbyte,caprsts,sample_start,sample_ack,sample_sampling,sample_cbyte,sample_wait,
								sc_check,sc_wait,sc_send);
	
	signal state, next_state : state_type;

begin
	SYNC_PROC: process (clk_in)
   begin
      if (rising_edge(clk_in)) then
         if (rst_in = '1') then
            state 		<= init;
				send			<= '0';
				capture		<= '0';
				ftdi_rd		<= '1';
				ftdi_wr		<= '1';
				sc_load		<= '0';
				sc_capture	<= '0';
				sc_seq		<= (others => '0');
				adc_go		<= '0';
				c1rstb		<= '1';
				c2rstb		<= '1';
         else
            state 		<= next_state;
				send			<= send_i;
				capture		<= capture_i;
				ftdi_rd		<= RD_i;
				ftdi_wr		<= WR_i;
				sc_load		<= sc_load_i;
				sc_capture	<= sc_capture_i;
				sc_seq		<= sc_seq_i;
				adc_go		<= adc_go_i;
				c1rstb		<= c1rstb_i;
				c2rstb		<= c2rstb_i;
         end if;        
      end if;
   end process;
	
	OUTPUT_DECODE: process (state)
	begin
		case (state) is
			when init => 	-- INIT state
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '0';
				c2rstb_i			<= '0';
			when idle => 	-- IDLE state: Waiting for ftdi_rxf to go low
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when avail => 	-- AVAIL state: ftdi_rxf low, byte(s) available
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when rbyte => 	-- RBYTE state: Done waiting, issue read
				RD_i 				<= '0';	-- assert read to get byte
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when cbyte1 =>	-- CBYTE1 state: Capture byte
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '1';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when cbyte2 =>	-- CBYTE2 state: Capture byte
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '1';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when style => 	-- Style state: Decode command
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when caprsts =>	-- Pulse Capacitor Resets after start cmd
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0'; 
				c1rstb_i			<= '0';
				c2rstb_i			<= '0';
			when sample_start => 	-- SAMPLE state: Sample data
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0'; 
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sample_ack => 	-- SAMPLE state: Wait for ADC ack
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '1'; 
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sample_sampling => 	-- SAMPLE state: ADC is sampling data
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sample_cbyte => 	-- SAMPLE_BYTE state: Load output value from sample data
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sample_wait => 	-- SAMPLE_WAIT state: Wait for sample data or accept cmd
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when pbyte => 	-- PBYTE state: Present byte, wait for ftdi_txe
				RD_i 				<= '1';
            WR_i	 			<= '1';
				send_i			<= '1';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sbyte =>	-- SBYTE state: Send byte
				RD_i 				<= '1';	
            WR_i			 	<= '0';	-- assert write to send byte
				send_i			<= '1';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sc_check => 	-- SC_CHECK state: Check if done with sc payload
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '1';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sc_wait => 	-- SC_WAIT state: Wait for next byte in sc payload
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when sc_send => 	-- SC_SEND state: Send scan chain sequence
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '1';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
			when others =>
				RD_i				<= '1';
				WR_i				<= '1';
				send_i			<= '0';
				capture_i		<= '0';
				sc_load_i		<= '0';
				sc_capture_i	<= '0';
				adc_go_i			<= '0';
				c1rstb_i			<= '1';
				c2rstb_i			<= '1';
		end case;
	end process;
	
	NEXT_STATE_DECODE: process (state,ftdi_rxf,ftdi_txe,command,sc_chunk,sample_en,byte_count,adc_running,adc_done,cap_count)
   begin
      --declare default state for next_state to avoid latches
      next_state <= state;  --default is to stay in current state
		case (state) is
			---------------------------------------------------------------------
			-- INIT
			when init =>
            next_state <= idle;
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
						next_state <= caprsts; -- start cmd, sample data
					when X"2" =>
						next_state <= idle;	-- stop cmd
					when X"4" => 
						next_state <= sc_check; -- scan chain cmd, load config
					when others => 
						next_state <= idle;
				end case;	
			---------------------------------------------------------------------
			-- CAPRSTS: Pulse Capacitor Resets
			when caprsts =>
				if(cap_count(CR_WIDTH-1) = '1') then
					next_state <= sample_start;
				else
					next_state <= caprsts;
				end if;
			---------------------------------------------------------------------
			-- SAMPLE_START: Sample data
			when sample_start =>
				next_state <= sample_ack;
			---------------------------------------------------------------------
			-- SAMPLE_ACK: Wait for ADC ack
			when sample_ack =>
				if(adc_running = '1') then	-- Wait for ADC to acknowledge 'go' signal
					next_state <= sample_sampling;	
				else
					next_state <= sample_ack;
				end if;
			---------------------------------------------------------------------
			-- SAMPLE_SAMPLING: ADC is sampling data
			when sample_sampling => 
				if(adc_done = '1') then
					next_state <= sample_cbyte;
				else
					next_state <= sample_sampling;
				end if;
			---------------------------------------------------------------------
			-- SAMPLE_CBYTE: Capture sample data
			when sample_cbyte =>
				next_state <= pbyte;
			---------------------------------------------------------------------
			-- SAMPLE_WAIT: Wait to sample data or accept new cmd
			when sample_wait =>
				if ftdi_rxf = '0' then -- data ready to be read from USB
				   next_state <= avail;
				elsif sample_en = '1' then
					next_state <= sample_start;
				else
					next_state <= sample_wait;
				end if;
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
				if(byte_count = X"B") then
					next_state <= sample_wait;
				else
					next_state <= sample_cbyte;
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
				next_state <= init;
		end case;
	end process;
	
	SYNC_SAMPLES: process (clk_in)
	begin
      if (rising_edge(clk_in)) then
         if (rst_in = '1') then
				byte_count <= X"0";
				ftdi_dout <= X"00";
				sample_counter <= X"000";
			else
				if(state = init) then
					byte_count <= X"0";
					ftdi_dout <= X"00";
					sample_counter <= X"000";
				elsif(state = sample_cbyte) then
					case (byte_count) is
						when X"0" =>
							ftdi_dout <= sample_counter(11 downto 4);
						when X"1" =>
							ftdi_dout(7 downto 4) <= sample_counter(3 downto 0);
							ftdi_dout(3 downto 0) <= adc_samp1(11 downto 8);
						when X"2" =>
							ftdi_dout <= adc_samp1(7 downto 0);
						when X"3" =>
							ftdi_dout <= adc_samp2(11 downto 4);
						when X"4" =>
							ftdi_dout(7 downto 4) <= adc_samp2(3 downto 0);
							ftdi_dout(3 downto 0) <= adc_samp3(11 downto 8);
						when X"5" =>
							ftdi_dout <= adc_samp3(7 downto 0);
						when X"6" =>
							ftdi_dout <= adc_samp4(11 downto 4);
						when X"7" =>
							ftdi_dout(7 downto 4) <= adc_samp4(3 downto 0);
							ftdi_dout(3 downto 0) <= adc_samp5(11 downto 8);
						when X"8" =>
							ftdi_dout <= adc_samp5(7 downto 0);
						when X"9" =>
							ftdi_dout <= adc_samp6(11 downto 4);
						when X"A" =>
							ftdi_dout(7 downto 4) <= adc_samp6(3 downto 0);
							ftdi_dout(3 downto 0) <= adc_samp7(11 downto 8);
						when X"B" =>
							ftdi_dout <= adc_samp7(7 downto 0);
						when others =>
					end case;
				elsif(state = sbyte) then
					byte_count <= byte_count + 1;
				elsif(state = sample_start) then
					byte_count <= X"0";
					sample_counter <= sample_counter + '1';
				end if;
			end if;
		end if;
	end process;
	
	SYNC_START : process (clk_in)
	begin
		if (rising_edge(clk_in)) then
         if (rst_in = '1') then
				sample_period <= 0;
			else
				if(state = style) then
					case(command(3 downto 0)) is
						when X"0" =>				-- USB clock is divided down by 2^sample_period for ADC sampling
							sample_period <= 22;	-- Results in sample freq ~1 Hz
						when X"1" =>
							sample_period <= 21; -- ~2 Hz
						when X"2" =>
							sample_period <= 20; -- ~4 Hz
						when X"3" =>
							sample_period <= 19; -- ~8 Hz
						when X"4" =>
							sample_period <= 18; -- ~16 Hz
						when X"5" =>
							sample_period <= 17; -- ~32 Hz
						when X"6" =>
							sample_period <= 16; -- ~64 Hz
						when X"7" =>
							sample_period <= 15; -- ~128 Hz
						when X"8" =>
							sample_period <= 14; -- ~256 Hz 
						when X"9" =>
							sample_period <= 13; -- ~512 Hz
						when X"A" =>
							sample_period <= 12; -- ~1 kHz
						when X"B" =>
							sample_period <= 11; -- ~2 kHz
						when X"C" =>
							sample_period <= 10; -- ~4 kHz
						when X"D" =>
							sample_period <= 9;  -- ~8 kHz
						when X"E" =>
							sample_period <= 8;  -- ~16 kHz
						when X"F" =>
							sample_period <= 7;  -- ~32 kHz -- Can't actually go this fast? Sampling takes about 40 us?
						when others =>
							sample_period <= 22; 
					end case;
				end if;
			end if;
		end if;
	end process;
	
	SAMPLE_TIMING: process (clk_in)
   begin
      if (rising_edge(clk_in)) then
         if (rst_in = '1') then
				sample_timer  <= (others => '0');
				sample_en <= '0';
			else
				if(state = sample_wait) then
					sample_timer <= sample_timer + 1;
					if (sample_timer(sample_period) = '1') then
						sample_en <= '1';
					else
						sample_en <= '0';
					end if;
				else
					sample_timer <= (others => '0');
					sample_en <= '0';
				end if;
			end if;
		end if;
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

	SYNC_CR: process (clk_in)
	begin
		if (rising_edge(clk_in)) then
         if (rst_in = '1') then
				cap_count <= (others => '0');
			else
				if(state = caprsts) then
					cap_count <= cap_count + '1';
				else
					cap_count <= (others => '0');
				end if;
			end if;
		end if;
	end process;

end Behavioral;

