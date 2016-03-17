; Keyer 3.0.18
; Goody K3NG
; Contributions and updates from W0ANM
; 2004.04.01

; This is a PIC based keyer.  Currently supported devices are the 16F628, 16F628A, and 16F648A.  This will compile for the
; 16F84 and 16F84A, however I don't test these anymore.  The 16F628 doesn't cost much more, switch over to it!  :-)
;
; http://www.qrpis.org
; goody @ qrpis . org


;	TODO - code practice, random callsigns
;	TODO - configurable memory repeat delay and store in eeprom
;	TODO - RX mute line (use TX wake line)
;	TODO - LCD Support
;	TODO - DDS Support
;	TODO - Configurable beacon sleep time
;	TODO - include_beacon_fox_message




; ************************************** user configurable stuff **********************************************

; processor definition - uncomment one and be sure to set your compiler for the right proc
;#define processor_16f84
;#define processor_16f84a
;#define processor_16f628
;#define processor_16f628a
#define processor_16f648a

; ***************************
; *     code inclusion      *
; ***************************

; comment out these to reduce the code size or change features
; note that some modules of code depend on others; read the notes
; bytes last updated 2004-09-18

; The Main Stuff

#define include_wavesidetone_code			; cost: ? bytes - additional support of wavesidetone
;#define include_function_button_code		; cost: 160 bytes (includes some dependent code); support for buttons other than mode
;#define include_toggle_sidetone_on_off_code	; cost: 18 bytes - code to turn off sidetone during TX
;#define include_mode_button_hold_code		; cost: 43 bytes - code for mode button hold shortcuts (programming memories, quick speed adj)
#define include_funky_beeps_code			; cost: 17 bytes - code for high and low beeps using wave sidetone; not applicable if using externally keyed sidetone oscillator (requires wavesidetone_code)
;#define include_m_and_h_cmd_code			; cost: 86 bytes - command modes M and H: program and playback memories in command mode
;#define include_eis_mh_cmd_code				; instead of 0,1,2 for memory select, it's e,i,s

; Frequency Counter Code
;#define include_freq_counter_code			; cost: 502 bytes - base frequency counter code
;#define include_freq_counter_button_code	; cost: 24 bytes - support for frequency counter button (requires freq_counter_code, function_button_code)
;#define	include_freq_counter_calib_code	; cost: 56 bytes - code for frequency calibration mode, command L (reqs freq_counter_code, function_button_code)

; Optional Features
;#define include_iambic_mode_code			; cost: 16 bytes - support for iambic a mode
;#define include_bug_code					; cost: 33 bytes - bug paddle mode
;#define include_call_cq_code				; cost: 55 bytes - call cq mode
;#define include_beacon_code				; cost: 60 bytes - beacon mode support
;#define include_nnnnn_code					; cost: 23 bytes - this code allows you to go into command mode after five consecutive Ns are sent
;#define include_txwake_line_code			; cost: ? bytes - code for TX_WAKE line
;#define include_tune_mode_code				; cost: 70 bytes - code for tune mode
;#define include_weighting_code				; cost: 77 bytes - command mode W: dah weighting
;#define include_paddle_reverse_code		; cost: 39 bytes - command mode R: reverse paddles
;#define include_needless_feature_code		; cost: 31 bytes - unimportant stuff
;#define include_say_hi_code					; cost: ? bytes - say hi at startup
;#define include_calibration_code			; cost: 34 bytes - this adds another command mode for calibrating the speed counters
;#define include_cw_rx_practice_code			; cost: 113 bytes - command mode P: random five letter group code practice
;#define include_cw_tx_practice_code			; cost: 25 bytes - command mode Q: send code without transmitting

; Experimental / Under Construction
;#define include_9850_dds_code				; experimental / under construction
;#define include_serial_cw_keyboard			; experimental
;#define include_beacon_serial_ctrl_code	; cost: ? bytes - control the beacon using a serial port
;#define include_fox_code					; under construction

; Debug code - turn all of these off for production chips
;#define include_debug_code					; misc debug code - comment out for production chips
;#define include_serial_rx_debug_code
;#define exclude_delays_debug


; *********************************************
; * dependencies - do not change this section *
; *********************************************

	; include_serial_port_rx_code required by include_beacon_serial_ctrl_code, include_serial_cw_keyboard
	ifdef include_beacon_serial_ctrl_code
#define include_serial_port_rx_code
	endif
	ifdef include_serial_cw_keyboard
	ifndef include_serial_port_rx_code
#define include_serial_port_rx_code
	endif
	endif

	; include_cw_table_long required by include_serial_port_rx_code, include_cw_rx_practice_code
	ifdef include_serial_port_rx_code
#define include_cw_table_long
	endif
	ifdef include_cw_rx_practice_code
	ifndef include_cw_table_long
#define include_cw_table_long
	endif
	endif

	; include_rotateBCD012_left required by include_serial_port_rx_code, include_freq_counter_code
	ifdef include_serial_port_rx_code
#define include_rotateBCD012_left
	endif
	ifdef include_freq_counter_code
	ifndef include_rotateBCD012_left
#define include_rotateBCD012_left
	endif
	endif

; ***************************
; *        options          *
; ***************************


#define _4_mhz_clock

; aliases for pins
#define mode_switch	PORTB,0x00			; momentary switch
;					PORTB,0x01			; Serial Port RX
#define dit			PORTB,0x02			; dit of paddle
#define dah			PORTB,0x03			; dah of paddle
	ifndef include_wavesidetone_code
#define sidetone	PORTB,0x04			; keys external sidetone oscillator
	else
#define wavesidetone PORTB,0x04			; square wave sidetone output
	endif
#define key			PORTB,0x05			; transmitter cw key
	ifdef include_txwake_line_code
#define txwake		PORTB,0x06			; goes high to turn on TX if used as FM CW unit, or T/R relay
	endif

	ifdef include_function_button_code
#define switch0		PORTA,0x00
#define switch1		PORTA,0x01
#define switch2		PORTA,0x02
	endif



#define initial_speed_wpm d'20'				; this is the default wpm speed when the cw_unit it first powered up
#define maxcwspeed  d'40'					; this is the max speed
#define initial_memory_repeat_delay d'35'	; this time in between repeating a memory in units of 100mS


; *************** don't adjust anything below this line unless you know what the hell you are doing *************

; eeprom addresses - this defines eeprom memory locations
#define eeprom_addr_speed_wpm 0x00			; CW speed in binary format
#define eeprom_wavesidetone_setting 0x01	; sidetone frequency setting
#define eeprom_settings1_location 0x02		; various user settings, register eeprom_settings1
#define eeprom_freq_calib_high 0x03			; frequency counter calibration, high byte
#define eeprom_freq_calib_low 0x04			; frequency counter calibration, low byte
#define eeprom_freq_offset_high 0x05		; frequency counter offset, lower nibble is BCD2
#define eeprom_freq_offset_low 0x06			; frequency counter offset, upper nibble is BCD1, lower nibble is BCD0
#define eeprom_memory_location_0 0x07		; cw memory 0 ( callsign )
#define eeprom_memory_loc_0_cw_units d'28'

	; first byte in memory location is a count of cw units stored in that memory
	; the remaining bytes are the stored cw units
	; a cw unit is two bits and is read MSB to LSB

	ifdef processor_16f84
#define eeprom_memory_location_1 0x0F 	; cw memory 1
#define eeprom_memory_location_2 0x28	; cw memory 2
#define eeprom_memory_loc_1_cw_units d'96'				; four cw units can be stored per byte
#define eeprom_memory_loc_2_cw_units d'92'
#define processor_low_end
	endif ;processor_16f84

	ifdef processor_16f84a
#define eeprom_memory_location_1 0x0F 	; cw memory 1
#define eeprom_memory_location_2 0x28	; cw memory 2
#define eeprom_memory_loc_1_cw_units d'96'				; four cw units can be stored per byte
#define eeprom_memory_loc_2_cw_units d'92'
#define processor_low_end
	endif ;processor_16f84a
	

	ifdef processor_16f628
#define eeprom_memory_location_1 0x0F	; cw memory 1
#define eeprom_memory_location_2 0x48	; cw memory 2
#define eeprom_memory_loc_1_cw_units d'224'
#define eeprom_memory_loc_2_cw_units d'220'
	endif ;processor_16f628

	ifdef processor_16f628a
#define eeprom_memory_location_1 0x0F	; cw memory 1
#define eeprom_memory_location_2 0x48	; cw memory 2  ( = eeprom_memory_location_1 + (eeprom_memory_loc_1_cw_units / 4) + 1 )
#define eeprom_memory_loc_1_cw_units d'224'		
#define eeprom_memory_loc_2_cw_units d'220'
	endif ;processor_16f628a

	ifdef processor_16f648a
#define eeprom_memory_location_1 0x0F	; cw memory 1
#define eeprom_memory_location_2 0x48	; cw memory 2
#define eeprom_memory_loc_1_cw_units d'224'
#define eeprom_memory_loc_2_cw_units d'220'
	endif ;processor_16f648a

; timing calibrations - these must be adjusted for clock speed
	
	ifdef _4_mhz_clock
#define _100ms_counter1_calibration 0xff
#define _100ms_counter2_calibration 0x78
#define _100ms_counter3_calibration 0x01
#define _cw_unit_wpm_factor d'20000' 			; this adjustment control the time length of one CW unit ( a dit )
#define _beep_length d'20'
#define	_high_beep_tone d'20'
#define	_low_beep_tone d'55'
#define _initial_wavesidetone_setting d'34'  	; this is the default wavesidetone frequency (600Hz)
#define _frequency_counter_calib	d'48900'	; higher = shorter sampling duration (1/4 second)
												; delay = 15 * 1uS * (65535 - _frequency_counter_calib) 
#define _txwake_delay_time d'30000'
#define _txwake_delay_time_straight_key d'400'
											
	endif ;_4_mhz_clock

	; processor declarations

	ifdef processor_16f84
	processor 16f84
	include <p16f84.inc>
	__config _HS_OSC & _WDT_OFF
	#define cblock_start 0x0c
	errorlevel -312			; turn off nagging about device not needing page selection
	endif ;processor_16f84
	
	ifdef processor_16f84a
	processor 16f84a
	include <p16f84a.inc>
	__config _HS_OSC & _WDT_OFF
	#define cblock_start 0x0c
	errorlevel -312			; turn off nagging about device not needing page selection
	endif ;processor_16f84a

	ifdef processor_16f628
	processor 16f628
	include <p16f628.inc>
	__config _HS_OSC & _WDT_OFF & _LVP_OFF & _BODEN_OFF
	#define cblock_start 0x20
	errorlevel -312			; turn off nagging about device not needing page selection
	endif ;processor_16f628
	
	ifdef processor_16f628a
	processor 16f628a
	include <p16f628a.inc>
	__config _HS_OSC & _WDT_OFF & _LVP_OFF & _BODEN_OFF
	#define cblock_start 0x20
	errorlevel -312			; turn off nagging about device not needing page selection
	endif ;processor_16f628a

	ifdef processor_16f648a
	processor 16f648a
	include <p16f648a.inc>
	__config _HS_OSC & _WDT_OFF & _LVP_OFF & _BODEN_OFF
	#define cblock_start 0x20
	#define multi_memory_page_support
	endif ;processor_16f648a
	
	; define registers

	cblock cblock_start
		speed_wpm:1					;global
		bit_stuff1:1				;global
		bit_stuff2:1				;global
		bit_stuff3:1				;global
		cw_unit_counter:2			;loop_cw_unit
		counter1:1					;pause_100_ms, loop_cw_unit
		counter2:1					;pause_100_ms, loop_cw_unit
		counter3:1					;pause_100_ms, send_cw 
		cw_char_to_send:2			;send_cw(parm)
		temp:1						;send_cw
		dividend:2					;divideby16bit
		divisor:2					;divideby16bit
		quotient:2					;divideby16bit
		remainder:2					;divideby16bit
		count:1						;divideby16bit
		binary2bcd_binary_in:2		;binary2bcd
		BCD0:1						;binary2bcd
		BCD1:1						;binary2bcd
		BCD2:1						;binary2bcd
		bcd_low_nibble:1			;speed_mode
		bcd_high_nibble:1			;speed_mode
		write_verify_data:1			;write_eeprom
		write_verify_addr:1			;write_eeprom
		command_mode_loop_count1:1	; check_mode_button (enhanced)
		command_mode_loop_count2:1	; check_mode_button (enhanced)
		command_mode_loop_count3:1	; check_mode_button (enhanced)
		command_mode_buffer1:1		; check_mode_button (enhanced)
		command_mode_buffer2:1		; check_mode_button (enhanced)
		recording_loop_counter1:1	; program_memory
		recording_loop_counter2:1	; program_memory
		cw_unit_count:1				; program_memory
		temp_memory:1				; program_memory
		temp_memory_eeprom_pointer:1 ;program_memory
		temp_memory_mask:1			; program_memory
		temp_w:1					; program_memory.store_temp_memory_to_eeprom
		send_cw_temp:1				; send_cw
		memory_number:1				; program_memory, play_memory
		eeprom_memory_locations:3	; program_memory, play_memory
		eeprom_memory_loc_limits:3	; program_memory
		start_of_memory_location:1	; program_memory, play_memory
		porta_temp:1				; check_function_buttons
		eeprom_settings1:1
		wavesidetone_counter
		wavesidetone_counter_setting
	endc


	ifdef include_txwake_line_code	
		cblock
			txwakecounter:2
		endc
	endif ;include_txwake_line_code	

	
	ifdef include_nnnnn_code
		cblock
			ditdah_history1:1			; global - include_nnnnn_code		
			ditdah_history2:1			; global - include_nnnnn_code		
			ditdah_history_timer1:1		; global - include_nnnnn_code		
		endc
	endif
	

	ifdef include_weighting_code
		cblock
			cw_unit_counter_dah:2
			cw_unit_counter_dit:2
			speed_wpm_dah
		endc
	endif
	
	ifdef include_cw_rx_practice_code
		cblock
			randomnum:2
		endc
	endif
	
	ifdef include_freq_counter_code
		cblock
			freqcount:3
			BCD3:1
			frequency_counter_calib:2
			freq_offset_binary:2
			L_temp:1
			H_temp:1
		endc

bcd2binary_binary_out equ binary2bcd_binary_in

	ifdef include_debug_code
bcd2binary_binary_out_2 equ binary2bcd_binary_in+1	; for debugger work
freq_offset_binary_2 equ freq_offset_binary+1		; for debugger work
	endif ;include_debug_code

	endif ;include_freq_counter_code
	
#define dit_buffer					bit_stuff1,0x00			; used to store a dit in iambic operation
#define	dah_buffer					bit_stuff1,0x01			; used to store a dah
#define key_tx_active				bit_stuff1,0x02			; 0 = do not turn on key_tx line when sending cw
#define sending_dit					bit_stuff1,0x03			; 1 = send dit in send_dit_or_dah
	ifdef include_tune_mode_code
#define tune_latch_mode				bit_stuff1,0x04			; 1 = tx is latched on
	endif
#define temp_memory_dirty 			bit_stuff1,0x05			; used by memory record functions
#define paddle_was_hit				bit_stuff1,0x06
	ifdef include_function_button_code
#define memory_playback_manual_exit bit_stuff1,0x07
	endif

#define send_cw_preemptible			bit_stuff2,0x00			; send_cw routine can be interrupted by user when this flag is set
	ifndef include_wavesidetone_code
#define wavesidetone				bit_stuff2,0x01
	else
#define sidetone					bit_stuff2,0x01
	endif
#define expert_commands_on			bit_stuff2,0x02			; 1 = expert commands have been activated in command mode
#define bit_temp					bit_stuff2,0x03
	ifdef include_freq_counter_code
#define command_mode_v_cmd			bit_stuff2,0x04
#define command_mode_k_cmd			bit_stuff2,0x05
	endif
	ifdef include_call_cq_code
#define dit_hit						bit_stuff2,0x06
	endif ;include_call_cq_code
	ifdef include_m_and_h_cmd_code
#define command_mode_m_cmd			bit_stuff2,0x07

#define command_mode_h_cmd			bit_stuff3,0x00
	endif ;include_m_and_h_cmd_code

#define squeeze_detected			bit_stuff3,0x01


;eeprom_settings1 is used for settings that need to be stored in nonvolatile memory
;they are written to eeprom using call write_eeprom_settings1
#define sidetone_off_during_tx		eeprom_settings1,0x00		; 1 = no sidetone during TX (for internal use in rigs that alread have sidetone)
	ifdef include_iambic_mode_code
#define iambic_b_mode				eeprom_settings1,0x01		; 1 = iambic b mode activated
	endif
	ifdef include_paddle_reverse_code
#define paddle_reverse				eeprom_settings1,0x02		; 1 = paddle reverse mode activated
	endif
	ifdef include_freq_counter_code
#define eight_digit_mode			eeprom_settings1,0x03		; 1 = 8 digit frequency counter mode
#define offset0						eeprom_settings1,0x04		; offset0 and 1 stores frequency counter offset mode (table below)	
#define offset1						eeprom_settings1,0x05
	endif
	ifdef include_bug_code
#define bug_mode_on					eeprom_settings1,0x06		; 1 = bug mode activated
	endif



	; frequency counter offset table
	; offset0		offset1
	;	0				0		no offset
	;	0				1		reading = offset - measurement
	;	1				0		reading = measurement - offset
	;	1				1		reading = measurement + offset



	org 0x00
	pagesel main_program_start
	errorlevel -306
	goto main_program_start
	errorlevel +306
		
; Tables -------------------------------------------------------------


cw_table
	
	; this is a lookup table for sending numbers in CW using send_cw routine
	
	; 00 = termination
	; 01 = dit
	; 11 = dah
	
	; stream goes MSB -> LSB  (MSB first)
	; two bytes long
	

	addwf PCL, F
	retlw b'11111111'	;0		0
	retlw b'11000000'
	retlw b'01111111'	;1		1
	retlw b'11000000'
	retlw b'01011111'	;2		2
	retlw b'11000000'
	retlw b'01010111'	;3		3
	retlw b'11000000'
	retlw b'01010101'	;4		4
	retlw b'11000000'
	retlw b'01010101'	;5		5
	retlw b'01000000'
	retlw b'11010101'	;6		6
	retlw b'01000000'
	retlw b'11110101'	;7		7
	retlw b'01000000'
	retlw b'11111101'	;8		8
	retlw b'01000000'
	retlw b'11111111'	;9		9
	retlw b'01000000'
	ifdef include_cw_table_long
	retlw b'01110000'	;A		10
	retlw b'00000000'
	retlw b'11010101'	;B		11
	retlw b'00000000'
	retlw b'11011101'	;C		12
	retlw b'00000000'
	retlw b'11010100'	;D		13
	retlw b'00000000'
	retlw b'01000000'	;E		14
	retlw b'00000000'
	retlw b'01011101'	;F		15
	retlw b'00000000'
	retlw b'11110100'	;G		16
	retlw b'00000000'
	retlw b'01010101'	;H		17
	retlw b'00000000'
	retlw b'01010000'	;I		18
	retlw b'00000000'
	retlw b'01111111'	;J		19
	retlw b'00000000'
	retlw b'11011100'	;K		20
	retlw b'00000000'
	retlw b'01110101'	;L		21
	retlw b'00000000'
	retlw b'11110000'	;M		22
	retlw b'00000000'
	retlw b'11010000'	;N		23
	retlw b'00000000'
	retlw b'01111101'	;P		24
	retlw b'00000000'
	retlw b'11110111'	;Q		25
	retlw b'00000000'
	retlw b'01110100'	;R		26
	retlw b'00000000'
	retlw b'01010100'	;S		27
	retlw b'00000000'
	retlw b'11000000'	;T		28
	retlw b'00000000'
	retlw b'01011100'	;U		29
	retlw b'00000000'
	retlw b'01010111'	;V		30
	retlw b'00000000'
	retlw b'01111100'	;W		31
	retlw b'00000000'
	retlw b'11010111'	;X		32
	retlw b'00000000'
	retlw b'11011111'	;Y		33
	retlw b'00000000'
	retlw b'11110101'	;Z		34
	retlw b'00000000'

	endif ;include_cw_table_long

; Subroutines -------------------------------------------------------------

check_paddles

	banksel PORTA
	pagesel $

	ifdef include_paddle_reverse_code
	btfsc paddle_reverse
	goto check_paddles_reverse
	endif ;include_paddle_reverse_code

	btfss dit		; if dit is not keyed, skip next
	bsf dit_buffer	; set dit_buffer
	btfss dah		; if dah is not keyed, skip next
	bsf dah_buffer	; set dah_buffer
    goto squeeze_check

	ifdef include_paddle_reverse_code
check_paddles_reverse	
	btfss dit		; if dit is not keyed, skip next
	bsf dah_buffer	; set dah_buffer
	btfss dah		; if dah is not keyed, skip next
	bsf dit_buffer	; set dit_buffer
	endif ;include_paddle_reverse_code

squeeze_check
	ifdef include_iambic_mode_code
	btfsc iambic_b_mode				; don't do squeeze check if we're in iambic b mode
	return
	endif ;include_iambic_mode_code
	btfsc dit		; if both dit and dah are squeezed, set the squeeze_detected flag
	return
	btfsc dah
    return
    bsf squeeze_detected

    return

; ------------------


send_w_BCD_in_cw

	; this sends the lower nibble of w (BCD binary coded decimal) in CW

	movwf temp_memory
	addwf temp_memory, F 		; double it so we're in the right spot in cw_table
	movfw temp_memory			; put it in W for lookup table		
	call cw_table
	movwf cw_char_to_send+0
	movfw temp_memory
	addlw 0x01					; add 1 to get next byte in table
	call cw_table
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	return
	
; ------------------

	ifdef include_funky_beeps_code

beep
	movwf temp_w						; store w temporarily
	movfw wavesidetone_counter_setting	; store real wavesidetone setting in temp_memory
	movwf temp_memory
	movfw temp_w						; get temp_w and put in for wavesidetone setting
	movwf wavesidetone_counter_setting

	movlw _beep_length
	movwf counter1
	movwf counter2
	
	bsf sidetone
	
beep_loop
	call produce_wavesidetone
	decfsz counter1, F
	goto beep_loop
	decfsz counter2, F
	goto beep_loop
	
	bcf sidetone
	
	movfw temp_memory					; restore real wavesidetone setting
	movwf wavesidetone_counter_setting	
	return
	

; ------------------

high_beep
	ifndef exclude_delays_debug
	movlw _high_beep_tone
	call beep
	endif ;exclude_delays_debug
	return
	
; ------------------

low_beep
	ifndef exclude_delays_debug
	movlw _low_beep_tone
	call beep
	endif ;exclude_delays_debug
	return

; ------------------

beep_boop
	ifndef exclude_delays_debug
	call high_beep
	call low_beep
	endif ;exclude_delays_debug
	return

; ------------------

boop_beep
	call low_beep
	call high_beep
	return

	endif ;include_funky_beeps_code

; ------------------

divide16bit

	; dividend / divisor = quotient
	; this code from Mark Sullivan www.markworld.com (thanks for saving me hours of work ! :-)
	
	banksel PORTA
	pagesel $

	movlw d'16'
	movwf count
	clrf quotient+0	;quotient
	clrf quotient+1
	clrf remainder+0	;remainder
	clrf remainder+1
udiv1
	bcf	STATUS,C
	rlf	dividend+1,F
	rlf	dividend+0,F
	rlf	remainder+1,F
	rlf	remainder+0,F
	movf divisor+0,W
	subwf remainder+0,W
	btfss STATUS,Z
	goto udiv2
	movf divisor+1,W
	subwf remainder+1,W
	btfss STATUS,C
	goto udiv4
	movwf remainder+1
	movf divisor+0,W
	subwf remainder+0,F
	goto udiv3
udiv2
	btfss STATUS,C
	goto udiv4
	movf divisor+1,W
	subwf remainder+1,F
	btfss STATUS,C
	decf remainder+0,F
	movf divisor+0,W
	subwf remainder+0,F
udiv3
	bsf	STATUS,C
udiv4
	rlf	quotient+1,F
	rlf	quotient+0,F
	decfsz count,F
	goto udiv1
	retlw 0

; ------------------

	ifdef include_freq_counter_code

bcd2binary

	; BCD to 16 bit binary converter
	; adapted from Microchip AN526
	; http://www.microchip.com
	; thanks, Microchip !

	;input parameters:
	;bcd2binary+0 : MSB
	;bcd2binary+1 : LSB

	;output:
	;BCD0 = most significant digits
	;BCD1
	;BCD2 = least significant digits

	;each BCD register contains and upper and lower nibble BCD digit

	banksel PORTA
	pagesel $

	clrf bcd2binary_binary_out+0
	movf BCD0,W
	andlw 0x0F
	movwf bcd2binary_binary_out+1
	call mpy10a          ; result = 10a+b
	swapf BCD1,W
	call mpy10b          ; result = 10[10a+b]
	movf BCD1,W
	call mpy10b          ; result = 10[10[10a+b]+c]
	swapf BCD2,W
	call mpy10b          ; result = 10[10[10[10a+b]+c]+d]
	movf BCD2,W
	andlw 0x0F
	addwf bcd2binary_binary_out+1, F
	btfsc STATUS,C
	incf bcd2binary_binary_out+0, F       ; result = 10[10[10[10a+b]+c]+d]+e
	return               ; BCD to binary conversion done

	; subroutines

mpy10b 
	andlw 0x0F
	addwf bcd2binary_binary_out+1, F
	btfsc STATUS,C
	incf bcd2binary_binary_out+0, F
mpy10a  
	bcf STATUS,C        ; multiply by 2
	rlf bcd2binary_binary_out+1,W
	movwf L_temp
	rlf     bcd2binary_binary_out+0,W        ; (H_temp,L_temp) = 2*N
	movwf   H_temp
	bcf     STATUS,C        ; multiply by 2
	rlf     bcd2binary_binary_out+1, F
	rlf     bcd2binary_binary_out+0, F
	bcf     STATUS,C        ; multiply by 2
	rlf     bcd2binary_binary_out+1, F
	rlf     bcd2binary_binary_out+0, F
	bcf     STATUS,C        ; multiply by 2
	rlf     bcd2binary_binary_out+1, F
	rlf     bcd2binary_binary_out+0, F       ; (bcd2binary_binary_out+0,bcd2binary_binary_out+1) = 8*N
	movf    L_temp,W
	addwf   bcd2binary_binary_out+1, F
	btfsc   STATUS,C
	incf    bcd2binary_binary_out+0, F
	movf    H_temp,W
	addwf   bcd2binary_binary_out+0, F
	return              ; (bcd2binary_binary_out+0,bcd2binary_binary_out+1) = 10*N


	endif ;include_freq_counter_code

; ------------------

binary2bcd

	; 16 bit binary to bcd converter
	; adapted from Microchip AN526
	; http://www.microchip.com
	; thanks, Microchip !

	banksel PORTA
	pagesel $

	bcf     STATUS, C                ; clear the carry bit
	movlw   .16
	movwf   count
	clrf    BCD0		; most significant digit
	clrf    BCD1
	clrf    BCD2		; least significant digit
loop16
	rlf     binary2bcd_binary_in+1, F	; LSB
	rlf     binary2bcd_binary_in+0, F	; MSB
	rlf     BCD2, F
	rlf     BCD1, F
	rlf     BCD0, F
	decfsz  count, F
	goto    adjDEC
	retlw   0
adjDEC
	movlw   BCD2
	movwf   FSR
	call    adjBCD
	movlw   BCD1
	movwf   FSR
	call    adjBCD
	movlw   BCD0
	movwf   FSR
	call    adjBCD
	goto    loop16
adjBCD
	movlw   3
	addwf   INDF, W
	movwf   temp
	btfsc   temp,3          ; test if result > 7
	movwf   INDF
	movlw   30
	addwf   INDF, W
	movwf   temp
	btfsc   temp,7          ; test if result > 7
	movwf   INDF               ; save as MSD
	retlw   0
	
; ------------------

write_eeprom

	pagesel $

	; save EEDATA and EEADR for write verify
	errorlevel -302
	banksel EEDATA
	movfw EEDATA
	banksel PORTA
	movwf write_verify_data
	banksel EEADR
	movfw EEADR
	banksel PORTA
	movwf write_verify_addr
	errorlevel +302

start_write
	errorlevel -302
	banksel INTCON
	bcf	INTCON, GIE		; disable interrupts
	banksel EECON1
	bsf	EECON1, WREN	; enable write
	movlw 0x55			; required write sequence
	movwf EECON2		; required write sequence
	movlw 0xaa			; required write sequence
	movwf EECON2		; required write sequence
	bsf EECON1,WR 		; write
wait_to_complete_write
	btfsc EECON1, WR	; watch for WR flag to clear
	goto wait_to_complete_write
	
	; check if write failed, loop a couple times to try again
	banksel write_verify_addr
	movfw write_verify_addr	; select the address
	banksel EEADR
	movwf EEADR
	banksel EECON1
	bsf EECON1, RD		; read it
	banksel EEDATA
	movfw EEDATA		; get the data
	banksel PORTA
	subwf write_verify_data, W	; compare it
	btfss STATUS, Z		; skip next if its equal
	goto write_again	; otherwise, write it again

write_eeprom_blowout
	banksel INTCON
	bsf INTCON, GIE		; enable interrupts
	banksel EECON1
	bcf EECON1, WREN	; clear write enable flag
	bcf EECON1, EEIF	; clear EE Interrupt flag for the hell of it
	banksel PORTA
	return
	
write_again
	banksel write_verify_data
	movfw write_verify_data		; reload EEDATA and EEADR with the original values
	banksel EEDATA
	movwf EEDATA
	banksel write_verify_addr
	movfw write_verify_addr
	banksel EEADR
	movwf EEADR	
	banksel PORTA
	goto start_write

	errorlevel -302
	
; ------------------	
calculate_cw_unit_values

	banksel PORTA
	pagesel $

	movlw LOW _cw_unit_wpm_factor
	movwf dividend+1
	movlw HIGH _cw_unit_wpm_factor
	movwf dividend+0
	movfw speed_wpm
	movwf divisor+1		; lsb - divisor
	movlw 0x00
	movwf divisor+0		; msb - divisor
	
	call divide16bit
	movf	quotient+1, W	;load LSB of result
	movwf	cw_unit_counter+1
	movf	quotient+0, W	;load MSB of result
	movwf	cw_unit_counter+0

	ifdef include_weighting_code
	movlw LOW _cw_unit_wpm_factor
	movwf dividend+1
	movlw HIGH _cw_unit_wpm_factor
	movwf dividend+0
	movfw speed_wpm_dah
	movwf divisor+1		; lsb - divisor
	movlw 0x00
	movwf divisor+0		; msb - divisor
	
	call divide16bit
	movf	quotient+1, W	;load LSB of result
	movwf	cw_unit_counter_dah+1
	movf	quotient+0, W	;load MSB of result
	movwf	cw_unit_counter_dah+0
	endif ;include_weighting_code

	return

; ------------------
	ifdef include_mode_button_hold_code

	banksel PORTA
	pagesel $

wait_for_mode_switch_release
	btfss mode_switch
	goto wait_for_mode_switch_release
	call pause_100_ms
	call pause_100_ms
	bcf key_tx_active
	return
	
	endif

; ------------------

pause_100_ms


	banksel PORTA ; for good measure

	movlw _100ms_counter1_calibration
	movwf counter1
	movlw _100ms_counter2_calibration
	movwf counter2
	movlw _100ms_counter3_calibration
	movwf counter3

looplooploop
	ifndef exclude_delays_debug
	decfsz counter1, F
	goto looplooploop
	decfsz counter2, F
	goto looplooploop
	decfsz counter3, F
	goto looplooploop
	endif ;exclude_delays_debug
	return
	
; ------------------

pause_w_100ms

	; pause W * 100 milliseconds

	banksel PORTA	; for good measure

	movwf count
	ifndef exclude_delays_debug
loop_decrement
	call pause_100_ms
	decfsz count, F
	goto loop_decrement
	endif ;exclude_delays_debug
	return

; ------------------

produce_wavesidetone
	btfss sidetone						; is sidetone on right now ?
	goto wavesidetone_off				; no, go turn the wavesidetone line off
	incf wavesidetone_counter, F
	movfw wavesidetone_counter_setting
	subwf wavesidetone_counter, W
	btfss STATUS, Z
	goto wavesidetone_not_equal
	btfss wavesidetone					; see what state we are in and toggle it
	goto on_and_reset_wavesidetone_ctr	; wavesidetone is off right now, jump up and set it
	nop
	bcf wavesidetone					; wavesidetone is on right now, turn it off
	goto reset_wavesidetone_counter		; jump ahead and reset counter
on_and_reset_wavesidetone_ctr
	bsf wavesidetone					; turn on wavesidetone line
reset_wavesidetone_counter
	clrf wavesidetone_counter
	goto end_of_wavesidetone			;-------------------------------------------
wavesidetone_off
	bcf wavesidetone
	nop									; time padding with nops
	nop									; this evens things out so each run through this
	nop									;    soubroutine should be the same amount of time
	nop									;    regardless of the state or branches
	nop
wavesidetone_not_equal
	nop
	nop
	nop
	nop
	nop
	nop
	nop
end_of_wavesidetone

	return


; ------------------

loop_cw_unit

	; this subroutine loops for one CW unit (i.e. a dit)
	; if the sidetone line has been set high it will produce a square wave sidetone signal on the wavesidetone line
	; during the looping, the dit and dah paddles are checked and dit_buffer and dah_buffer set if either are pressed
	; this enables dit and dah insertion for iambic operation

	; initialize counters with calculated values
	movfw cw_unit_counter+0
	movwf counter1	;MSB
	movfw cw_unit_counter+1
	movwf counter2	;LSB
	
	ifdef include_paddle_reverse_code
	btfsc paddle_reverse				; are we in reverse paddle mode?
	goto loop_cw_unit_reverse_paddle	; yes, go to subroutine for reverse paddle mode
	endif

loop_de_loop_init
	movlw d'3'
	movwf counter3

loop_de_loop

	btfss sending_dit				; skip next line if we are sending dit
	goto check_for_dit				; if we are sending dit, check dah paddle
	btfss dah						; check the dah paddle, if = 1, skip over next line
	bsf dah_buffer					; dah is keyed, set buffer
	goto skip_over_check_for_dit	; skip over check_for_dit
	
check_for_dit						; we're sending a dah, check the dit paddle
	btfss dit						; if dit is not keyed, skip over next line
	bsf dit_buffer					; set the dit buffer
	nop
skip_over_check_for_dit

	call produce_wavesidetone

	decfsz counter3,F
	goto loop_de_loop
	decfsz counter2, F
	goto loop_de_loop_init
	decfsz counter1, F
	goto loop_de_loop_init
	
		
	return


	ifdef include_paddle_reverse_code

loop_cw_unit_reverse_paddle
	
loop_de_loop_init_rev
	movlw d'3'
	movwf counter3
loop_de_loop_rev
	btfss sending_dit		; skip next line if we are sending dit
	goto check_for_dit_rev		; if we are sending dit, check dah paddle
	btfss dit				; check the dit paddle, if = 1, skip over next line
	bsf dah_buffer			; dah is keyed, set buffer
	goto skip_over_check_for_dit_rev			; skip over check_for_dit
	
check_for_dit_rev				; we're sending a dah, check the dit paddle
	btfss dah				; if dah is not keyed, skip over next line
	bsf dit_buffer			; set the dit buffer
	nop
skip_over_check_for_dit_rev

	call produce_wavesidetone

	decfsz counter3,F
	goto loop_de_loop_rev
	decfsz counter2, F
	goto loop_de_loop_init_rev
	decfsz counter1, F
	goto loop_de_loop_init_rev
		
	return

	endif ;include_reverse_paddle_code

; ------------------

loop_6_cw_units
	call loop_cw_unit
	call loop_cw_unit
	call loop_cw_unit
	call loop_cw_unit	
	call loop_cw_unit
	call loop_cw_unit
	return
	
; ------------------


send_dit_or_dah

	; send a dit or dah
	; parms:
	;
	;	sending_dit
	;		1 = send dit
	;		0 = send dah
	;	key_tx_active
	;		1 = key tx line
	;		0 = do not key tx line
	;
	; notes:
	;
	;	- sidetone line is always keyed (unless sidetone_off_during_tx is true and key_tx_active is true)

	ifdef include_toggle_sidetone_on_off_code
	btfsc sidetone_off_during_tx
	btfss key_tx_active
	endif
	bsf sidetone		; turn on sidetone
	btfsc key_tx_active	; if key_tx is clear, jump - don't key tx line
	bsf key				; key tx line
	btfss sending_dit	; are we sending a dit ?
	goto send_a_dah		; if not, jump to send_a_dah
	call loop_cw_unit	; loop for one unit, a dit
	goto clear_lines	; get out and clear all the lines
	
send_a_dah

	ifdef include_weighting_code
	movfw cw_unit_counter+1			; store the regular counter values
	movwf cw_unit_counter_dit+1	
	movfw cw_unit_counter+0
	movwf cw_unit_counter_dit+0
	movfw cw_unit_counter_dah+1		; switch in the dah values for weighting
	movwf cw_unit_counter+1
	movfw cw_unit_counter_dah+0
	movwf cw_unit_counter+0
	endif ;include_weighting_code
	
	call loop_cw_unit	; key for three lengths (dah)
	call loop_cw_unit
	call loop_cw_unit	
	
	ifdef include_weighting_code
	movfw cw_unit_counter_dit+1		; put regular counter values back
	movwf cw_unit_counter+1
	movfw cw_unit_counter_dit+0
	movwf cw_unit_counter+0		
	endif ;include_weighting_code
	
clear_lines
	bcf sidetone	; deactivate sidetone
	bcf key			; deactivate key line
	call loop_cw_unit	; loop while unkeyed for inter-cw_unit spacing

	ifdef include_nnnnn_code			; this code blows, so I keep it turned off.
										; only use if you're too cheap to buy a mode switch
	; keep a history of dits and dahs
	;   history is stored in a word consisting of ditdah_history1 and 2
	;   each bit is one cw_unit : 0 = dit, 1 = dah
	;                                     m             l     m             l
	;                                     s             s     s             s
	;                                     b             b     b             b     
	;   cw_units are pushed like this --> ditdah_history1 --> ditdah_history2
	btfss key_tx_active		; are we actually keying the tx ?
	return					; if not, blow out and do not add to history
	bsf STATUS, C			; set the carry bit, 1 = dah
	btfsc sending_dit		; did we just send a dit ?
	bcf STATUS, C			; if so, clear the carry bit, 0 = dit
	rrf ditdah_history1, F	; place bit into history register
	rrf ditdah_history2, F	; carry over into the second byte
	movlw 0xff
	movwf ditdah_history_timer1
	endif ;include_nnnnn_code
	
	return


; ------------------

	ifdef include_freq_counter_code

store_freq_offset_in_eeprom

	errorlevel -302
	movlw eeprom_freq_offset_high
	banksel EEADR
	movwf EEADR
	banksel freq_offset_binary
	movfw freq_offset_binary+0
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom

	movlw eeprom_freq_offset_low
	banksel EEADR
	movwf EEADR
	banksel freq_offset_binary
	movfw freq_offset_binary+1
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom

	return

	errorlevel +302

	endif ;include_freq_counter_code

; ------------------

initialize_eeprom_first_time

	errorlevel -302
	movlw eeprom_addr_speed_wpm		; address 0x00 last wpm speed
	banksel EEADR
	movwf EEADR
	movlw initial_speed_wpm
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom
	
	
	; write wavesidetone freq to eeprom
	movlw eeprom_wavesidetone_setting
	banksel EEADR
	movwf EEADR
	movlw _initial_wavesidetone_setting
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom

	;clean out memory locations
	movlw eeprom_memory_location_0
	banksel EEADR
	movwf EEADR
	movlw 0xff
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom
	movlw eeprom_memory_location_1
	banksel EEADR
	movwf EEADR
	banksel PORTA
	call write_eeprom
	movlw eeprom_memory_location_2
	banksel EEADR
	movwf EEADR
	banksel PORTA
	call write_eeprom
	errorlevel +302

	ifdef include_freq_counter_code
	
	errorlevel -302
	movlw eeprom_freq_calib_high
	banksel EEADR
	movwf EEADR
	movlw high _frequency_counter_calib
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom

	movlw eeprom_freq_calib_low
	banksel EEADR
	movwf EEADR
	movlw low _frequency_counter_calib
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom

	call store_freq_offset_in_eeprom

	errorlevel +302

	endif ;include_freq_counter_code


	; clear eeprom_settings1 location
	clrf eeprom_settings1
	call write_eeprom_settings1
	
	ifndef exclude_delays_debug
	ifdef include_funky_beeps_code
	call beep_boop
	call beep_boop
	call beep_boop
	endif ;include_funky_beeps_code
	endif ;exclude_delays_debug

	banksel PORTA

	return

; ------------------

write_eeprom_settings1

	errorlevel -302
	movlw eeprom_settings1_location
	banksel EEADR
	movwf EEADR
	banksel eeprom_settings1
	movfw eeprom_settings1
	banksel EEDATA
	movwf EEDATA
	banksel PORTA
	call write_eeprom
	errorlevel +302

	banksel PORTA

	return

; ------------------
	
initialize

	ifndef exclude_delays_debug
	movlw d'2'			; pause to make sure everything is settled down on power up
	call pause_w_100ms
	endif ;exclude_delays_debug

	;bcf OPTION_REG,NOT_RBPU

	errorlevel -302
	ifndef processor_low_end
	banksel CMCON
	movlw 0x07			; setup Port A for I/O, not comparator mode
	movwf CMCON
	endif ;processor_low_end
	errorlevel +302

	; initialize IO Ports
	errorlevel -302
	banksel TRISA
	movlw B'00010111'
	movwf TRISA
	movlw B'00001101'	; 1 = input, 0 = output
	movwf TRISB
	errorlevel -302
	banksel PORTA
	bcf dit_buffer
	bcf dah_buffer
	bcf key
	bcf sidetone
	ifdef include_txwake_line_code
	bcf txwake
	endif
	bsf key_tx_active
	
	ifdef include_nnnnn_code
	clrf ditdah_history1
	clrf ditdah_history2
	endif

	; see if eeprom has been written to before
	movlw 0x00
	errorlevel -302
	banksel EEADR
	movwf EEADR
	banksel EECON1
	bsf EECON1, RD		; read address 0x00
	banksel EEDATA
	btfsc EEDATA,0x07					; is bit 7 set high ?
	call initialize_eeprom_first_time	; if so, we got a new chip here
	banksel PORTA
	errorlevel +302	

	; read the last wpm stored in eeprom
	movlw eeprom_addr_speed_wpm
	call read_eeprom
	movwf speed_wpm
	ifdef include_weighting_code
	movwf speed_wpm_dah
	endif
	
	call calculate_cw_unit_values

	; setup eeprom memory locations
	movlw eeprom_memory_location_0
	movwf eeprom_memory_locations+0
	movlw eeprom_memory_location_1
	movwf eeprom_memory_locations+1
	movlw eeprom_memory_location_2
	movwf eeprom_memory_locations+2

	movlw eeprom_memory_loc_0_cw_units
	movwf eeprom_memory_loc_limits+0
	movlw eeprom_memory_loc_1_cw_units
	movwf eeprom_memory_loc_limits+1
	movlw eeprom_memory_loc_2_cw_units
	movwf eeprom_memory_loc_limits+2

	;read eeprom_settings1 from eeprom
	movlw eeprom_settings1_location
	call read_eeprom
	movwf eeprom_settings1

	movlw eeprom_wavesidetone_setting
	call read_eeprom
	movwf wavesidetone_counter_setting
	clrf wavesidetone_counter

	ifdef include_interrupt_code
	; set up interrupt stuff
	errorlevel -302
	banksel OPTION_REG
	bcf OPTION_REG, T0CS	; TIMER0 clock source set to internal clock (clock / 4)
	bcf OPTION_REG, PSA		; assign prescaler to TIMER0 clock
	bsf OPTION_REG, PS0		; set TIMER0 prescaler to 111 ( / 256 )
	bsf OPTION_REG, PS1
	bsf OPTION_REG, PS2
	banksel TMR0
	movlw d'150'
	movwf TMR0
	banksel INTCON
	bsf INTCON, T0IE		; enable TIMER0 overflow interrupt
	bsf INTCON, GIE			; enable interrupts
	errorlevel -302
	banksel PORTA
	endif

	ifdef include_cw_rx_practice_code
	movlw 0x30				; seed random number generator
	movwf randomnum+0
	movlw 0x45
	movwf randomnum+1
	endif


	ifdef include_freq_counter_code
	movlw eeprom_freq_calib_high
	call read_eeprom
	movwf frequency_counter_calib+0
	movlw eeprom_freq_calib_low
	call read_eeprom
	movwf frequency_counter_calib+1

	; read freq_offset from EEPROM
	movlw eeprom_freq_offset_high
	call read_eeprom
	movwf freq_offset_binary+0
	movlw eeprom_freq_offset_low
	call read_eeprom
	movwf freq_offset_binary+1

	endif ;include_freq_counter_code



	ifdef include_serial_port_rx_code
	errorlevel -302
	banksel TRISB
	bsf TRISB,0x01		; make RB1 an input 
	banksel TXSTA
;    bsf TXSTA,BRGH		; high speed bit generator
    bcf TXSTA,BRGH		; low speed bit generator
	movlw d'25'			; 9600 baud with 4Mhz clock / 2400 with low speed bit generator
	banksel SPBRG
	movwf SPBRG
	banksel TXSTA
	bcf TXSTA,SYNC		; asyncronous mode
	;bcf STATUS,RP0		; select bank 0
	banksel RCSTA
	bsf RCSTA,SPEN
	bsf RCSTA,CREN		; enable serial receive
read_rx_fifo_loop
	call pause_100_ms
	banksel PIR1
	btfss PIR1,RCIF		; do we have something in the buffer?
	goto read_rx_fifo_loop_exit				; no, blow out of here
	banksel RCREG
	movfw RCREG			; pull byte out of the serial rx FIFO
	goto read_rx_fifo_loop
read_rx_fifo_loop_exit
	errorlevel +302
	endif ;include_serial_port_code
	
	banksel PORTA
	
	bcf send_cw_preemptible
	bcf squeeze_detected

	retlw	0

; ------------------


; ------------------

	ifdef include_serial_cw_keyboard

check_serial_cw_keyboard
	btfsc PIR1,OERR		; did we overrun the buffer?
	goto handle_overrun
	btfsc PIR1,FERR		; did we have a frame error?
	goto handle_frame_error
	btfss PIR1,RCIF		; do we have something in the buffer?
	return				; no, blow out of here
	movfw RCREG			; pull byte out of the serial rx FIFO

	ifdef include_serial_rx_debug_code
	; debug code - send received byte in decimal CW
	movwf binary2bcd_binary_in+1
	clrf binary2bcd_binary_in+0
	call binary2bcd
	call rotate_BCD012_left
	call rotate_BCD012_left
	call send_BCD2_in_cw
	call rotate_BCD012_left
	call rotate_BCD012_left
	call send_BCD2_in_cw
	call rotate_BCD012_left
	call rotate_BCD012_left
	call send_BCD2_in_cw
	return
	else

	sublw d'49'					; convert ascii code to cw table lookup 0-9
	movwf temp_memory			; double it
	addwf temp_memory, F
	movfw temp_memory
	call cw_table				; get the first byte of code
	movwf cw_char_to_send+0
	movfw temp_memory
	addlw 0x01					; increment it to get the next byte
	call cw_table
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	return
	
	endif ;include_serial_rx_debug_code

handle_overrun
	movfw RCREG
	ifdef include_debug_code
	call low_beep
	endif
	return

handle_frame_error
	bcf RCSTA,CREN
	bsf RCSTA,CREN
	ifdef include_debug_code
	call low_beep
	endif
	return


	endif ;include_serial_port_rx_code

; ------------------
	
check_straight_key_mode

	; this checks if the dah paddle is keyed or mode button is pressed, if so, we loop in straight key mode forever
	btfss mode_switch 
	goto go_into_straight_key_mode
	btfsc dah  	; if dah = 0 go to straight_key_loop
	retlw 0		;   otherwise, return
go_into_straight_key_mode
	bsf sending_dit
	movlw d'60'
	movwf speed_wpm
	call calculate_cw_unit_values
	
	ifdef include_txwake_line_code	
	bcf txwake
	endif ;include_txwake_line_code

straight_key_loop		
	call loop_cw_unit
	btfss mode_switch
	goto key_on
	btfsc dit   		; if dit = 0 jump to key_on
	goto key_off		; else go to key_off
key_on
	ifdef include_txwake_line_code
	;call reset_txwake_timer
	movlw HIGH _txwake_delay_time_straight_key
	movwf txwakecounter+0
	movlw LOW _txwake_delay_time_straight_key
	movwf txwakecounter+1
	bsf txwake
	endif
	bsf key				; key the TX
	ifdef include_toggle_sidetone_on_off_code
	btfss sidetone_off_during_tx
	endif ;include_toggle_sidetone_on_off_code
	bsf sidetone		; key the sidetone
	goto straight_key_loop
key_off
	ifdef include_txwake_line_code
	bcf dit_buffer
	bcf dah_buffer
	pagesel check_txwake_clear_time
	errorlevel -306
	call check_txwake_clear_time
	errorlevel +306
	endif
	bcf key			; unkey the TX
	bcf sidetone	; unkey the sidetone
	goto straight_key_loop			
	
	retlw 0

; ------------------

send_buffer

	btfss dit_buffer	; if dit_buffer is set, skip next line
	goto check_dah_buffer
	ifdef include_txwake_line_code
	bsf txwake
	endif
	bcf dit_buffer
	bsf sending_dit
	call send_dit_or_dah

	ifdef include_iambic_mode_code
    btfsc iambic_b_mode
    goto check_dah_buffer
	btfss squeeze_detected
    goto check_dah_buffer
	btfss dit
	goto check_dah_buffer
	btfss dah
	goto check_dah_buffer
	bcf squeeze_detected
	bcf dit_buffer
	bcf dah_buffer
	endif ;include_iambic_mode_code

	
check_dah_buffer
	btfss dah_buffer	; if dah_buffer is set, skip next line
	return
	ifdef include_txwake_line_code
	bsf txwake
	endif
	bcf dah_buffer
	bcf sending_dit

	ifdef include_bug_code		; HACK ALERT: special hack to accomodate bug mode
	btfss bug_mode_on
	goto no_bug_mode
	movfw speed_wpm				; temporarily store the current wpm
	movwf temp_memory
	movlw d'60'					; jack up the wpm to 60 so loop is responsive
	movwf speed_wpm
	call calculate_cw_unit_values
	bsf sending_dit
bug_loop
	ifdef include_toggle_sidetone_on_off_code
	btfss sidetone_off_during_tx
	endif ;include_toggle_sidetone_on_off_code
	bsf sidetone
	bsf key
	call loop_cw_unit
	btfss dah
	goto bug_loop				; loop as long as dah is on
	bcf sidetone
	bcf key	
	movfw temp_memory			; restore original wpm
	movwf speed_wpm
	call calculate_cw_unit_values
	bcf dah_buffer
	return
no_bug_mode
	endif ;include_bug_code
	
	call send_dit_or_dah

	ifdef include_iambic_mode_code
    btfsc iambic_b_mode
    return
	btfss squeeze_detected
    return
	btfss dit
	return
	btfss dah
	return
	bcf squeeze_detected
	bcf dit_buffer
	bcf dah_buffer
	endif ;include_iambic_mode_code


	return
; ------------------

decrement_wpm
	
	movfw speed_wpm
	sublw d'4'			; 5 wpm lower limit
	btfsc STATUS, C		;if 4 - speed_wpm > 0, return and do not decrement
	return
	decf speed_wpm, F
	ifdef include_weighting_code
	decf speed_wpm_dah, F
	endif
	call calculate_cw_unit_values
	return

; ------------------

increment_wpm

	movfw speed_wpm
	sublw maxcwspeed	; max wpm upper limit
	btfss STATUS, C		;if maxcwspeed - speed_wpm < 0, return and do not increment
	return
	incf speed_wpm, F
	ifdef include_weighting_code
	incf speed_wpm_dah, F
	endif
	call calculate_cw_unit_values
	return

; ------------------

send_cw

	; sends two bytes of CW
	; parms:
	;	cw_char_to_send:2

	; 00 = terminator = end of character or string, return
	; 01 = dit
	; 11 = dah
	; 10 = intercharacter spacing - pause for two more cw_units lengths and go on
	
	; cw_char_to_send+0 is sent first, cw_char_to_send+1 is sent second; MSB is the first sent

	movlw cw_char_to_send-1	; setup pointer, set to first byte - 1
	movwf FSR
	movlw 0x02				; loop through twice
	movwf count

loop_initialize
	incf FSR, F
	movlw b'11000000'		; initialize send_cw_temp
	movwf send_cw_temp

cw_byte_decode

	btfss send_cw_preemptible
	goto no_send_cw_preemptible
	call check_for_button_hit		; see if a button was hit for us to exit out
	sublw 0x01
	btfsc STATUS,Z
	goto send_cw_button_hit_loop
no_send_cw_preemptible
	movfw send_cw_temp
	movwf temp				;make a copy of send_cw_temp
	movfw INDF
	andwf send_cw_temp, W
	btfsc STATUS, Z			; if 0, we have a terminator, exit
	goto loop_2_cw_units_and_exit
	subwf send_cw_temp, W
	btfsc STATUS, Z ; if 0, we have a dah
	goto send_cw_dah_byte
	; check for 10
	movlw b'10101010'	; create a bit mask 
	andwf temp, F		; and it with the temp register so we get 10 in the right position
	movfw INDF		; get a fresh byte of cw again
	subwf temp, W		; subtract
	btfsc STATUS, Z		; if we have zero, this is 10 - an intercharacter space
	goto loop_two_cw_units ; zero set = 10 
	bsf sending_dit		; zero not set = 01 - send a dit
	call send_dit_or_dah
	goto cw_byte_decode_next
send_cw_dah_byte
	bcf sending_dit
	call send_dit_or_dah	
cw_byte_decode_next		; get things ready to read the next two bits to the right
	bcf STATUS, C		; rotate bits to the right twice
	rrf send_cw_temp, F		; to select the next two bits
	rrf send_cw_temp, F
	btfss STATUS, C		; if a bit gets into carry flag, we're at zero and we're done
	goto cw_byte_decode	; otherwise, go back for more with this byte
	decf count, F		; we're done with this byte, decrement the count
	btfss STATUS, Z		; did count hit zero ?
	goto loop_initialize
	
loop_2_cw_units_and_exit
	; loop two cw_units lengths so we have proper inter character spacing
	call loop_cw_unit
	call loop_cw_unit
	
	return
	
loop_two_cw_units
	call loop_cw_unit
	call loop_cw_unit
	goto cw_byte_decode_next


send_cw_button_hit_loop
	call loop_check_for_button_hit
	ifdef include_function_button_code
	bsf memory_playback_manual_exit
	endif
	return

;-------------

send_BCD2_in_cw

	swapf BCD2,F			; send upper nibble
	movfw BCD2
	andlw b'00001111'
	call send_w_BCD_in_cw
	swapf BCD2,F			; send lower nibble
	movfw BCD2
	andlw b'00001111'
	call send_w_BCD_in_cw
	
	return
	
;-------------

announce_speed_wpm

	; convert speed_wpm to BCD
	movlw 0x00
	movwf binary2bcd_binary_in+0
	movfw speed_wpm
	movwf binary2bcd_binary_in+1
	call binary2bcd

	call send_BCD2_in_cw

	return

;-------------

speed_mode

	; this mode allows you to change the sending speed
	; dit paddle speeds up
	; dah paddle slows down
	; mode button or paddle squeeze exits
	

	bsf sending_dit

	movlw 0x04
	call pause_w_100ms	
	
dit_loop
	call send_dit_or_dah	; sound a dit
	
	btfss dit				; dit is pressed, dah is not
	btfss dah
	goto skip_increment_wpm
	call increment_wpm
	goto dit_loop
skip_increment_wpm
	btfss dah				; dah is pressed, dit is not
	btfss dit
	goto skip_decrement_wpm
	call decrement_wpm
	goto dit_loop
skip_decrement_wpm
	
	btfss mode_switch		; if switch is pressed, get out of loop
	goto loop_while_pressed2
	btfss dit				; if both paddles are pressed, this also exits
	btfsc dah
	goto dit_loop
	
loop_while_pressed2			; wait for mode_switch, dit and dah to be released
	call pause_100_ms
	btfss mode_switch
	goto loop_while_pressed2
	btfss dit
	goto loop_while_pressed2
	btfss dah
	goto loop_while_pressed2

	movlw 0x05
	call pause_w_100ms

	call write_speed_wpm_to_eeprom

	movlw 0x05
	call pause_w_100ms	

	bcf dit_buffer
	bcf dah_buffer
	
	return

; ------------------


write_speed_wpm_to_eeprom

	movlw eeprom_addr_speed_wpm
	errorlevel -302
	banksel EEADR
	movwf EEADR
	banksel speed_wpm
	movfw speed_wpm
	banksel EEDATA
	movwf EEDATA
	errorlevel +302
	banksel PORTA
	
	call write_eeprom

	return

; ------------------

	
	ifdef include_tune_mode_code

tune_mode

	; this mode is used for tuning up a rig
	; left paddle intermittently turns on TX
	; right paddle latches TX
	; mode switch or paddle squeeze exits

	bcf tune_latch_mode
	movlw 0x02
	call pause_w_100ms
	bsf sending_dit
	call send_dit_or_dah
	movfw speed_wpm		; temporarily store the current wpm
	movwf temp_memory
	movlw d'60'			; jack up the wpm to 60 so loop is responsive
	movwf speed_wpm
	call calculate_cw_unit_values

tune_mode_loop
	
	call loop_cw_unit
	btfss dit				; is dit pressed ?
	goto tune_dit_pressed	; if so, go to tune_dit_pressed
	btfss dah				; is dah pressed ?
	goto tune_dah_pressed	; if so, go to tune_dah_pressed
	btfsc tune_latch_mode	; are we in latch mode ?
	goto tune_mode_check_for_exit	; if so jump over
	bcf key					; we're not in latch mode and dit is not pressed
	bcf sidetone			;  so clear key and sidetone
	ifdef include_txwake_line_code
	bcf txwake
	endif
	
tune_mode_check_for_exit
	btfss mode_switch		; if switch is pressed, get out of loop
	goto tune_mode_loop_while_pressed
	btfss dit				; if both paddles are pressed, this also exits
	btfsc dah
	goto tune_mode_loop
	
tune_mode_loop_while_pressed			; wait for mode_switch, dit and dah to be released


	bcf key						
	bcf sidetone
	ifdef include_txwake_line_code
	bcf txwake
	endif

	call pause_100_ms
	btfss mode_switch
	goto tune_mode_loop_while_pressed
	btfss dit
	goto tune_mode_loop_while_pressed
	btfss dah
	goto tune_mode_loop_while_pressed



	movfw temp_memory			; restore original wpm
	movwf speed_wpm
	call calculate_cw_unit_values

	movlw 0x02					; exiting dit
	call pause_w_100ms
	bsf sending_dit
	call send_dit_or_dah
		
	bcf dit_buffer				; clean out buffers in case they got set in loop_cw_unit
	bcf dah_buffer
	
	return
	
tune_dit_pressed
	bcf tune_latch_mode		; get out of latch mode
	ifdef include_txwake_line_code
	bsf txwake
	endif
	bsf key					; key tx
	ifdef include_toggle_sidetone_on_off_code
	btfss sidetone_off_during_tx
	endif ;include_toggle_sidetone_on_off_code
	bsf sidetone			; key sidetone
	call loop_cw_unit		;
	goto tune_mode_check_for_exit
	
tune_dah_pressed
	bsf tune_latch_mode		; go into latch mode
	btfss key
	goto tune_key
	bcf key
	bcf sidetone
	call pause_100_ms
	ifdef include_txwake_line_code
	bcf txwake
	endif
	goto tune_dah_pressed_loop
tune_key
	ifdef include_txwake_line_code
	bsf txwake
	endif
	bsf key
	ifdef include_toggle_sidetone_on_off_code
	btfss sidetone_off_during_tx
	endif ;include_toggle_sidetone_on_off_code
	bsf sidetone
	call loop_cw_unit		;

tune_dah_pressed_loop		; wait for dah to be released
	call loop_cw_unit
	btfss dit						; if dit is pressed,
	goto tune_mode_check_for_exit	;  blow out of the loop because both dit and dah are squeezed
	btfss dah				; if dah is not pressed, skip over
	goto tune_dah_pressed_loop

	goto tune_mode_check_for_exit

	endif ;include_tune_mode_code

; ------------------	

	ifdef include_calibration_code

calibration_check_mode

	; this mode allows you to check the cw unit timing calibration
	; hold down the dit paddle for a minute or half minute
	; after releasing the paddle, the number of dits will be announced in CW
	; use the formula below to determine the proper number of dits for the wpm setting
	; speed in wpm = dits per min / 25
	; 20 WPM equals 500 dits per minute or 250 dits per 20 seconds

	clrf binary2bcd_binary_in+0
	clrf binary2bcd_binary_in+1
	bsf sending_dit

calibration_check_mode_wait_loop
	btfsc dit							; loop while waiting for dit to be pressed
	goto calibration_check_mode_wait_loop
calibration_dit_loop
	call send_dit_or_dah				; send a dit
	incf binary2bcd_binary_in+1, F		; increment the dit counter
	btfsc STATUS, Z						; did the lower byte roll over ?
	incf binary2bcd_binary_in+0, F		; if so, increment the upper byte
	btfss dit							; if dit is still pressed
	goto calibration_dit_loop			;   continue to loop
	
	movlw 0x05
	call pause_w_100ms
	
	call binary2bcd

	; break out lower nibble of middle BCD digit
	movfw BCD1
	andlw b'00001111'
	movwf bcd_low_nibble
	addwf bcd_low_nibble, F 	; double it so we're in the right spot in cw_table

	movfw bcd_low_nibble		; send the low nibble BCD digit in CW
	call cw_table
	movwf cw_char_to_send+0
	movfw bcd_low_nibble
	addlw 0x01
	call cw_table
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306

	; send least signficant BCD digit
	call send_BCD2_in_cw

	return	
	

	endif
; ------------------



program_memory

	; parms
	;	memory_number (0 = callsign memory)
	;
	; calls
	;	send_dit_or_dah
	;	check_paddles
	;	pause_w_100ms
	;	divide16bit

	; send a dit or boop_beep to signify start	
	movlw 0x02
	call pause_w_100ms
	ifdef include_funky_beeps_code
	call boop_beep
	else
	bsf sending_dit
	call send_dit_or_dah
	endif

	; initialize counters
	clrf cw_unit_count

	bcf temp_memory_dirty
		
	movlw b'11000000'
	movwf temp_memory_mask
	clrf temp_memory

	; get parameters for this memory
	movlw eeprom_memory_locations			; get the *address* of memory location array
	movwf FSR
	movfw memory_number						; add for the appropriate memory location
	addwf FSR, F		
	movfw INDF
	movwf temp_memory_eeprom_pointer		; initialize the live eeprom pointer
	movwf start_of_memory_location			; initialize the static starting location
	incf temp_memory_eeprom_pointer, F		; actual recording starts at +1
	

initial_recording_loop	
	; check if mode button was hit
	pagesel we_are_done_with_programming
	errorlevel -306
	btfss mode_switch
	goto we_are_done_with_programming
	errorlevel +306
	pagesel check_paddles
	errorlevel -306
	call check_paddles
	errorlevel +306
	btfsc dit_buffer
	goto add_dit_or_dah_to_memory
	btfsc dah_buffer
	goto add_dit_or_dah_to_memory
	goto initial_recording_loop	
	
init_recording_loop_counters
	movlw 0x00
	movwf recording_loop_counter1
	movwf recording_loop_counter2

recording_loop
	ifdef include_function_button_code
	btfss switch0
	goto we_are_done_with_programming
	btfss switch1
	goto we_are_done_with_programming
	btfss switch2
	goto we_are_done_with_programming
	endif
	pagesel we_are_done_with_programming
	errorlevel -306
	btfss mode_switch
	goto we_are_done_with_programming
	errorlevel +306
	pagesel check_paddles
	errorlevel -306
	call check_paddles
	errorlevel +306
	btfsc dit_buffer
	goto store_cw_unit_in_temp_memory
	btfsc dah_buffer
	goto store_cw_unit_in_temp_memory
	; nothing is active right now, increment the inactive counters
	incfsz recording_loop_counter1, F
	goto recording_loop
	movfw recording_loop_counter2
	sublw 0xFF							
	btfss STATUS, Z						
	incf recording_loop_counter2, F
	goto recording_loop
	
we_are_done_with_programming
	; store the last byte if temp_memory is dirty
	btfsc temp_memory_dirty
	call store_temp_memory_to_eeprom
	movfw cw_unit_count				;see if cw_unit_count is zero
	btfsc STATUS,Z
	decf cw_unit_count,F			;if so, store FF in cw_unit_count to signify blank memory
	; store the cw_unit_count in the first byte
	movfw start_of_memory_location
	errorlevel -302
	banksel EEADR
	movwf EEADR
	errorlevel +302
	banksel PORTA
	movfw cw_unit_count
	errorlevel -302
	banksel EEDATA
	movwf EEDATA
	errorlevel +302
	banksel PORTA
	call write_eeprom
	movlw 0x03
	call pause_w_100ms
	ifdef include_funky_beeps_code
	call beep_boop
	else
	bsf sending_dit
	call send_dit_or_dah
	endif
	return ; we are totally done

    ;------------- sub-subroutines


store_cw_unit_in_temp_memory

	; store the silent timee before the cw_unit
	; first, calculate the ratio between the silent and the cw unit time
	movfw cw_unit_counter+1
	movwf divisor+1
	movfw cw_unit_counter+0
	movwf divisor+0
	movfw recording_loop_counter1	; recording_loop_counter / cw_unit_counter = space ratio
	movwf dividend+1				; this is used below to determine if we have an intercharacter space
	movfw recording_loop_counter2	; or an interword space
	movwf dividend+0
	call divide16bit
	movfw quotient+0				; check if quotient MSB is zero
	btfsc STATUS, Z		
	goto measure_up_silent_time		; if so, skip over this stuff
	movlw 0xff						; if not, set LSB to FF
	movwf quotient

measure_up_silent_time
	movfw quotient+1	; see how many cw_unit lengths we had silent before this cw_unit
	sublw d'1'							; is this an intercharacter space ? was 10
	btfsc STATUS, C						
	goto add_dit_or_dah_to_memory		; no, it's negative, don't add a space cw_unit
	movfw quotient+1					; is this an interword space ?
	sublw d'14'							; was 70
	btfsc STATUS, C
	goto skip_over_1					; result is negative, we have less than 6 cw_units
	movlw b'00000000'					; 00 = 6 cw_unit space
	goto skip_over_2
skip_over_1
	movlw b'10101010'					; 10 = 2 cw_unit space
skip_over_2
	call store_w_in_temp_memory

add_dit_or_dah_to_memory
	btfss dit_buffer
	goto skip_over_dit
	bsf sending_dit
	call send_dit_or_dah
	movlw b'01010101'	; 01 = dit
	bcf dit_buffer		; clean out dit buffer
	goto skip_over_dah
skip_over_dit
	bcf sending_dit
	call send_dit_or_dah
	movlw b'11111111'	; 11 = dah
	bcf dah_buffer		; clean out dah buffer
skip_over_dah
	call store_w_in_temp_memory
	
	; enforce cw unit limit count so we don't run over into another memory
	; get cw_unit count limit
	movlw eeprom_memory_loc_limits+0
	movwf FSR
	movfw memory_number						; add for the appropriate memory location
	addwf FSR, F		
	movfw INDF
	subwf cw_unit_count, W					; does cw_unit_count = cw_unit count limit ?
	btfss STATUS, Z
	goto init_recording_loop_counters 		; no, go back for more
	bsf sidetone							; yes, lock up the sidetone
wait_for_user_to_realize	
	ifdef include_funky_beeps_code
	call low_beep
	endif
	call pause_100_ms
	btfss dit
	goto wait_for_user_to_realize
	btfss dah
	goto wait_for_user_to_realize
	bcf sidetone
	bcf dit_buffer
	bcf dah_buffer
	goto we_are_done_with_programming
		
	;cleanup

store_w_in_temp_memory
	andwf temp_memory_mask, W			; apply the mask to W
	addwf temp_memory, F				; add the cw_unit to temp_memory
	;increment cw_unit count	
	incf cw_unit_count, F	

;setup_for_next_store_w
	bsf temp_memory_dirty
	rrf temp_memory_mask, F				; move the bit mask over
	rrf temp_memory_mask, F
	btfsc STATUS, C						; if we carried, we need to start a new byte
	call store_temp_memory_to_eeprom
	return

store_temp_memory_to_eeprom
	movwf temp_w	; preserve w
	; write the temp memory byte to eeprom
	movfw temp_memory_eeprom_pointer
	errorlevel -302
	banksel EEADR
	movwf EEADR
	errorlevel +302
	banksel PORTA
	movfw temp_memory
	errorlevel -302
	banksel EEDATA
	movwf EEDATA
	errorlevel +302
	banksel PORTA
	errorlevel -306
	pagesel check_paddles
	call check_paddles
	errorlevel +306
	call write_eeprom
	errorlevel -306
	pagesel check_paddles
	call check_paddles
	errorlevel +306
	;get things ready for new byte
	movlw b'11000000'
	movwf temp_memory_mask
	clrf temp_memory
	; point to the next eeprom byte to be written
	incf temp_memory_eeprom_pointer, F
	; TODO - check if we're at the limit for this memory
	movfw temp_w	; restore w
	bcf temp_memory_dirty
	return

;	endif
	
; ------------------

read_eeprom

	; read eeprom address in w
	; return data in w
	errorlevel -302
	banksel EEADR
	movwf EEADR
	banksel EECON1
	bsf EECON1, RD		
	banksel EEDATA
	movfw EEDATA
	errorlevel +302
	banksel PORTA
	return

; ------------------

check_for_button_hit
	btfss dit
	retlw 1
	btfss dah
	retlw 1
	btfss mode_switch
	retlw 1
	ifdef include_function_button_code
	btfss switch0
	retlw 1
	btfss switch1
	retlw 1
	btfss switch2
	retlw 1
	endif
	retlw 0

; ------------------

loop_check_for_button_hit
	call check_for_button_hit
	sublw 0x01
	btfsc STATUS,Z
	goto loop_check_for_button_hit
	call pause_100_ms
	return

; ------------------

play_memory

	ifdef include_function_button_code
	bcf memory_playback_manual_exit			; clear flag to tell calling subroutines if user manaully exited
	endif
	
	; get parameters for this memory
	movlw eeprom_memory_locations			; get the *address* of memory location array
	movwf FSR
	movfw memory_number						; add for the appropriate memory location
	addwf FSR, F		
	movfw INDF
	movwf temp_memory_eeprom_pointer		; initialize the live eeprom pointer
	; read the cw_unit count for this memory	
	call read_eeprom
	movwf cw_unit_count

	; check for blank memory	
	movlw 0xFF
	subwf cw_unit_count, W	; is the cw_unit count 0xFF ?
	btfss STATUS, Z
	goto byte_loop			; no, skip over this
	ifdef include_funky_beeps_code
	call low_beep
	else
	bsf sending_dit			; yes, send three dits and blow out
	bcf key_tx_active
	call send_dit_or_dah
	call send_dit_or_dah
	call send_dit_or_dah
	bsf key_tx_active
	endif ;include_funky_beeps_code
	return

byte_loop
	; get a byte from eeprom
	incf temp_memory_eeprom_pointer, F	; move to the next byte
	movfw temp_memory_eeprom_pointer
	call read_eeprom
	movwf temp_memory
	movlw 0x04				; four cw_units in a byte
	movwf temp_memory_mask	; use temp_memory_mask as a cw_unit within a byte counter
cw_unit_loop
	; TODO - exit if a switch or paddle is hit


	rlf temp_memory, F
	btfsc STATUS, C
	goto first_bit_1
;first_bit_0
	rlf temp_memory, F
	btfsc STATUS, C
	goto cw_unit_01
;cw_unit_00	- 6 cw_unit space
	call loop_cw_unit
	call loop_cw_unit
	call loop_cw_unit
	call loop_cw_unit	
	call loop_cw_unit
	call loop_cw_unit
	goto next_cw_unit_setup
first_bit_1
	rlf temp_memory, F
	btfsc STATUS,C
	goto cw_unit_11
;cw_unit_10 - 2 cw_unit space
	call loop_cw_unit
	call loop_cw_unit
	goto next_cw_unit_setup	
cw_unit_11 ; dah
	bcf sending_dit
	call send_dit_or_dah
	goto next_cw_unit_setup
cw_unit_01
	bsf sending_dit
	call send_dit_or_dah
next_cw_unit_setup
	decf cw_unit_count, F
	btfsc STATUS, Z				; did we do the last cw_unit ?
	return						; yes, blow out
	call check_for_button_hit
	sublw 0x01
	btfsc STATUS,Z
	goto button_hit_loop
	decf temp_memory_mask, F	; no
	btfsc STATUS, Z				; did we do the last cw_unit in this byte ?
	goto byte_loop				; yes, grab another byte
	goto cw_unit_loop			; no, grab another cw_unit in this byte
	return
	
button_hit_loop
	call loop_check_for_button_hit
	ifdef include_function_button_code
	bsf memory_playback_manual_exit
	endif
	return
	
; ------------------

program_memory_0

	clrf memory_number
	call program_memory

	return



; ------------------



program_memory_1

	movlw 0x01
	movwf memory_number
	call program_memory

	return


; ------------------


program_memory_2

	movlw 0x02
	movwf memory_number
	call program_memory

	return

;	endif

; ------------------


playback_memory_0
	clrf memory_number
	call play_memory
	return

; ------------------

playback_memory_1
	movlw 0x01
	movwf memory_number
	call play_memory
	return
	
; ------------------

playback_memory_2
	movlw 0x02
	movwf memory_number
	call play_memory
	return

; ---------------------------------------------


	ifdef multi_memory_page_support
	org 0800h
	endif


;----------------------------------------------

check_mode_button

	banksel PORTA
	pagesel $

	ifdef include_nnnnn_code
	; see if ditdah_history time has expired
	; TODO - make this driven by speed_wpm.  This may get falsely triggered at higher speeds
	decf ditdah_history_timer1, F
	btfsc STATUS, Z
	clrf ditdah_history1
	; check ditdah_history for NNNNN
	movfw ditdah_history1
	sublw b'10101010'		; check for dit, dah, dit, dah, dit, dah, dit, dah
	btfss STATUS, Z
	goto check_the_mode_switch ; we didn't find it, check the mode button
	movfw ditdah_history2	; we did find it, check the next byte
	andlw b'11000000'		; apply a bit mask so we get just two bits
	sublw b'10000000'		; check for dit, dah
	btfsc STATUS, Z
	goto loop_while_mode_pressed	; if we found it, go into command mode
	endif	; include_nnnnn_code


check_the_mode_switch
	btfsc	mode_switch		; if mode switch is pressed (0), keep going
	return					; otherwise there's nothing to do, blow out of here
	
loop_while_mode_pressed

	ifdef include_cw_rx_practice_code
	call get_random_number16	; while we're not doing anything, randomize
	endif

	ifdef include_mode_button_hold_code		; ---- special shortcuts by holding the mode button ----
	ifdef include_function_button_code
	btfss switch0
	goto call_program_memory_0_hold
	btfss switch1
	goto call_program_memory_1_hold
	btfss switch2	
	goto call_program_memory_2_hold
	endif ;include_function_button_code
	btfss dit
	goto speed_mode_increment
	btfss dah
	goto speed_mode_decrement
	goto no_other_buttons_pressed

call_program_memory_0_hold
	errorlevel -306
	pagesel wait_for_mode_switch_release
	call wait_for_mode_switch_release
	pagesel program_memory_0
	call program_memory_0
	errorlevel +306
	goto blow_out_of_here_no_bk

call_program_memory_1_hold
	errorlevel -306
	pagesel wait_for_mode_switch_release
	call wait_for_mode_switch_release
	pagesel program_memory_1
	call program_memory_1
	errorlevel +306
	goto blow_out_of_here_no_bk

call_program_memory_2_hold
	errorlevel -306
	pagesel wait_for_mode_switch_release
	call wait_for_mode_switch_release
	pagesel program_memory_2
	call program_memory_2
	errorlevel +306
	goto blow_out_of_here_no_bk

quick_speed_mode_check
	btfss dit
	goto speed_mode_increment
	btfss dah
	goto speed_mode_decrement
	btfss mode_switch
	goto quick_speed_mode_check			; mode button is still depressed, keep looping
	errorlevel -306
	pagesel write_speed_wpm_to_eeprom
	call write_speed_wpm_to_eeprom		; mode button not pressed, save the speed setting and blow out
	errorlevel +306
	goto blow_out_of_here_no_bk

speed_mode_increment
	bcf key_tx_active
	errorlevel -306
	pagesel increment_wpm
	call increment_wpm
	errorlevel +306
	bsf sending_dit
	errorlevel -306
	pagesel send_dit_or_dah
	call send_dit_or_dah
	errorlevel +306
	goto quick_speed_mode_check

speed_mode_decrement
	bcf key_tx_active
	errorlevel -306
	pagesel decrement_wpm
	call decrement_wpm
	errorlevel +306
	bsf sending_dit
	errorlevel -306
	pagesel send_dit_or_dah
	call send_dit_or_dah
	errorlevel +306
	goto quick_speed_mode_check



no_other_buttons_pressed					; ---- end of mode button hold shortcuts ----
	endif ;include_mode_button_hold_code
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms			; button debounce time
	errorlevel +306
	pagesel $
	btfss	mode_switch			; wait until switch is released
	goto loop_while_mode_pressed
	banksel PORTA
	bcf key_tx_active			; disable tx keying line


	bcf expert_commands_on		; initialize some flags

	ifdef include_freq_counter_code
	bcf command_mode_v_cmd
	bcf command_mode_k_cmd
	endif ;include_freq_counter_code

	ifdef include_m_and_h_cmd_code
	bcf command_mode_m_cmd
	bcf command_mode_h_cmd
	endif ;include_m_and_h_cmd_code

	ifdef include_funky_beeps_code
	errorlevel -306
	pagesel boop_beep
	call boop_beep
	errorlevel +306
	else
	bsf sending_dit				; send dit to announce command mode
	errorlevel -306
	pagesel send_dit_or_dah
	call send_dit_or_dah
	errorlevel +306
	endif

initialize_command_mode
	pagesel $
	banksel PORTA
	bcf paddle_was_hit
	clrf command_mode_buffer1
	clrf command_mode_buffer2

	; command_mode_buffer1 and 2 hold the dits and dahs that are sent in command mode
	; each element (dit or dah) takes up two bits
	; dit(01)/dah(11) -> command_mode_buffer1 -> command_mode_buffer2
	; so B equals 01010111 00000000
	;    , equals 11110101 11110000

	; now, the real stuff begins

init_command_mode_loop_vars

	; TODO - calculate directly using wpm

	movfw cw_unit_counter+1				; initialize command mode loop variables
	movwf command_mode_loop_count1
	movfw cw_unit_counter+0
	movwf command_mode_loop_count2
	movlw 0x01
	movwf command_mode_loop_count3

user_input_loop_mode		; ---- collect the user's command ---
	pagesel $
	banksel PORTA
	btfss mode_switch		; if mode switch is pressed, get out of command mode
	goto loop_while_mode_pressed_exit
	errorlevel -306
	pagesel check_paddles
	call check_paddles	
	errorlevel +306
	pagesel $
	banksel PORTA
	btfsc dit_buffer		; was dit pressed ?
	goto dit_pressed
	btfsc dah_buffer		; was dah pressed ?
	goto dah_pressed
	btfss paddle_was_hit	; has a paddle been hit already ?
	goto user_input_loop_mode	; if not, loop back without decrementing counters
	decf command_mode_loop_count1, F
	btfss STATUS, Z
	goto user_input_loop_mode
	decf command_mode_loop_count2, F
	btfss STATUS, Z
	goto user_input_loop_mode
	decf command_mode_loop_count3, F
	btfss STATUS, Z
	goto user_input_loop_mode
figure_out_the_command	
	; figure out what command was sent
	movlw 0x04
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms	
	errorlevel +306
	banksel PORTA
	pagesel $
	ifdef include_freq_counter_code
	btfsc command_mode_v_cmd		; V commands (freq counter offset mode) are two characters
	goto  command_mode_v_commands	; go to a special handler for these
	btfsc command_mode_k_cmd		; K command is several chanracters
	goto  command_mode_k_commands	; go to a special handler for these
	endif ;include_freq_counter_code

	ifdef include_m_and_h_cmd_code
	btfsc command_mode_m_cmd		; M commands (freq counter offset mode) are two characters
	goto  command_mode_m_commands	; go to a special handler for these
	btfsc command_mode_h_cmd		; H command is several chanracters
	goto  command_mode_h_commands	; go to a special handler for these
	endif ;include_m_and_h_cmd_code

	movfw command_mode_buffer2
	btfss STATUS, Z					; is second buffer byte 0 ?
	goto more_than_four_elements	; no, go direct to the more than four element commands
									; yes, decode only the first byte


	; ---- one character, four element regular commands ----

	banksel PORTA
	pagesel $

	ifdef include_iambic_mode_code								
	movlw b'11010000'			; a = iambic a mode
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto set_iambic_a_mode
	movlw b'01010111'			; b = iambic b mode
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto set_iambic_b_mode
	endif								



	movlw b'01000000'			; e = activate expert commands
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto activate_expert_commands
	
	ifdef include_needless_feature_code
	movlw b'01111100'			
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto easter_egg
	endif

	ifdef include_paddle_reverse_code
	movlw b'01110100'			; r = toggle paddle reverse
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_paddle_reverse_toggle
	endif 
								
	movlw b'01010100'			; s = speed mode
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_speed_mode
	
	ifdef include_tune_mode_code
	movlw b'11000000'			; t = tuning mode
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_tune_mode
	endif ;include_tune_mode_code
	
	ifdef include_bug_code
	movlw b'11010100'			; u = toggle bug mode
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_toggle_bug_mode
	endif

	ifdef include_m_and_h_cmd_code
	movlw b'11110000'			; m = playback memory
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto command_mode_m_activate
	endif ;include_m_and_h_cmd_code

	ifdef include_cw_rx_practice_code
	movlw b'01111101'			; p = rx code practice
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_code_rx_practice_mode
	endif
	
	ifdef include_cw_tx_practice_code
	movlw b'11011111'			; q = send code practice
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_code_tx_practice_mode
	endif

	ifdef include_m_and_h_cmd_code
	movlw b'01010101'			; h = program memory
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto command_mode_h_activate
	endif ;include_m_and_h_cmd_code

	ifdef include_weighting_code
	movlw b'11110100'			; w = weighting adjust
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_weighting_adjust
	endif

	ifdef include_freq_counter_code
	movlw b'11110111'			; y = frequency counter
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_count_frequency
	endif

	ifdef include_beacon_code
	movlw b'01011111'			; z = play beacon once
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_play_beacon
	endif
	

	movlw b'11010111'			; x = exit
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto blow_out_of_here

	btfss expert_commands_on
	goto wtf

	;-------------------- below here is four element or less expert commands ----------------------------------

	ifdef include_calibration_code
	
	movlw b'01110111'			; c = calibration check
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_calibration_check_mode
	
	endif

	ifdef include_toggle_sidetone_on_off_code
	movlw b'01011100'			; d = toggle sidetone on/off
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_sidetone_on_off
	endif

	ifdef include_wavesidetone_code
	movlw b'01110101'			; f = sidetone frequency adjust
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_sidetone_freq_adj
	endif

	ifdef include_freq_counter_calib_code
	movlw b'01011101'			; l = frequency counter calibration
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_freq_counter_calib_mode
	endif ;include_freq_counter_calib_code

	ifdef include_freq_counter_code
	movlw b'11010101'			; v = V command (two character command)
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto command_mode_v_activate
	endif ;include_freq_counter_code

	ifdef include_freq_counter_code
	movlw b'11011100'			; k = K command (two character command)
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto command_mode_k_activate
	endif ;include_freq_counter_code


	goto wtf

	;-------------------- end of four element or less expert commands ----------------------------------

more_than_four_elements		;------------------ more than four element commands --------------------------

	banksel PORTA
	pagesel $

	movlw b'01010000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_question_command
	movlw b'01011111'			; ? = query speed
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_announce_speed_wpm

skip_over_question_command

	btfss expert_commands_on
	goto wtf

more_than_four_elements_expert	;---------------------------- more than four element expert commands -------------

	ifdef include_freq_counter_code
	movlw b'11110000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_comma_command
	movlw b'11110101'			; comma = toggle 8 digit/3 digit mode
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_toggle_digit_mode
skip_over_comma_command
	endif
	

	;--------------------- end of all commands ----------------------------

wtf								; I don't know what the hell you sent
	movlw b'01011111' 			; send ?
	movwf cw_char_to_send+0
	movlw b'01010000'			
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	banksel PORTA
	pagesel $
	ifdef include_freq_counter_code
	bcf command_mode_v_cmd		; clear out any in progress multicharacter commands
	bcf command_mode_k_cmd
	endif ;include_freq_counter_code
	goto initialize_command_mode

	; ---- exit routines after commands ----

blow_out_of_here
	ifdef include_funky_beeps_code
	errorlevel -306
	pagesel beep_boop
	call beep_boop
	errorlevel +306
	banksel PORTA
	pagesel $
	else
	movlw b'11010101' 			; send BK in CW
	movwf cw_char_to_send+0
	;movlw b'11011100'			
	movlw b'10110111'			
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	banksel PORTA
	pagesel $
	endif ;include_funky_beeps_code
blow_out_of_here_no_bk
	;set things back to normal
	bcf dit_buffer
	bcf dah_buffer
	bsf key_tx_active
	ifdef include_nnnnn_code
	clrf ditdah_history1
	clrf ditdah_history2
	endif
	return
	
loop_while_mode_pressed_exit
	btfss	mode_switch		; wait until switch is released
	goto loop_while_mode_pressed_exit
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	banksel PORTA
	;bsf key_tx_active
	pagesel blow_out_of_here
	goto blow_out_of_here
	
	; ---- handle dits and dahs from user ----

dit_pressed
	; sound a dit
	bsf sending_dit
	errorlevel -306
	pagesel send_dit_or_dah
	call send_dit_or_dah
	errorlevel +306
	banksel PORTA
	pagesel $
	bsf paddle_was_hit
	bcf dit_buffer
	pagesel add_to_buffer
	goto add_to_buffer

dah_pressed
	; sound a dah
	bcf sending_dit
	errorlevel -306
	pagesel send_dit_or_dah
	call send_dit_or_dah
	errorlevel +306
	banksel PORTA
	pagesel $
	bsf paddle_was_hit
	bcf dah_buffer
add_to_buffer
	bsf STATUS, C
	rrf command_mode_buffer1, F
	rrf command_mode_buffer2, F
	bcf STATUS, C	; if it's a dit, put a 0 in next
	btfss sending_dit
	bsf STATUS, C	; otherwise, put a 1 in next
	rrf command_mode_buffer1, F
	rrf command_mode_buffer2, F
	pagesel init_command_mode_loop_vars
	goto init_command_mode_loop_vars

	;--------------------- double character commands ---------------------------


	ifdef include_m_and_h_cmd_code
command_mode_m_commands
    ifdef include_eis_mh_cmd_code ;-----------------------New Code from W0ANM----------------------
	movlw b'01000000'			; me      EIS code uses e,i and s instead of 0,1, and 2
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_playback_memory_0
	movlw b'01010000'			; mi
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_playback_memory_1
	movlw b'01010100'			; ms
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_playback_memory_2
	goto wtf
	else ;include_eis_mh_cmd_code ;-----------------------Original Code--------------------------------
	movlw b'11000000'				
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_m0_command
	movlw b'11111111'			; m0
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_playback_memory_0
skip_over_m0_command
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_m1_command
	movlw b'11111111'			; m1
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_playback_memory_1
skip_over_m1_command
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto wtf
	movlw b'11111101'			; m2
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_playback_memory_2
	goto wtf
	endif ;include_eis_mh_cmd_code
	endif ;include_m_and_h_cmd_code

	ifdef include_m_and_h_cmd_code
command_mode_h_commands
    ifdef include_eis_mh_cmd_code ;-----------------------New Code from W0ANM ----------------------
	movlw b'01000000'			; he           EIS code uses e,i and s instead of 0,1, and 2
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_program_memory_0
	movlw b'01010000'			; hi
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_program_memory_1
	movlw b'01010100'			; hs
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_program_memory_2
	goto wtf
	else ;include_eis_mh_code     ;------------------------Original Code----------------------------------
	movlw b'11000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_h0_command
	movlw b'11111111'			; h0
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_program_memory_0
skip_over_h0_command
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_h1_command
	movlw b'11111111'			; h1
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_program_memory_1
skip_over_h1_command
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto wtf
	movlw b'11111101'			; h2
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_program_memory_2
	goto wtf
	endif ;include_eis_mh_cmd_code
	endif ;include_m_and_h_cmd_code


	ifdef include_freq_counter_code
command_mode_v_commands				; V commands : frequency counter offset mode
	movlw b'11000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_v0_command
	movlw b'11111111'			; v0 = no freq counter offset
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_v0_mode
skip_over_v0_command
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_v1_command
	movlw b'11111111'			; v1
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_v1_mode
skip_over_v1_command
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto skip_over_v2_command
	movlw b'11111101'			; v2
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_v2_mode
skip_over_v2_command
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfss STATUS,Z
	goto wtf
	movlw b'11110101'			; v3
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto call_v3_mode
	goto wtf
	endif ;include_freq_counter_code


	ifdef include_freq_counter_code
command_mode_k_commands				; K command : set frequency counter offset

	; the K command is K + digits, terminated with an X (Example: K123X


	movfw command_mode_buffer2	; check for the X which means time to blow out
	btfss STATUS,Z
	goto not_x_character
	movlw b'11010111'			
	subwf command_mode_buffer1, W
	btfsc STATUS, Z
	goto exit_out_of_k_command

not_x_character
	movlw b'11000000'
	subwf command_mode_buffer2, W
	btfsc STATUS,Z
	goto k_6_7_8_9_0
	movlw b'01000000'
	subwf command_mode_buffer2, W
	btfsc STATUS,Z
	goto k_1_2_3_4_5
	goto wtf

k_1_2_3_4_5
	movlw b'11111111'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_1
	movlw d'1'
	goto add_to_freq_offset
skip_k_1
	movlw b'11111101'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_2
	movlw d'2'
	goto add_to_freq_offset
skip_k_2
	movlw b'11110101'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_3
	movlw d'3'
	goto add_to_freq_offset
skip_k_3
	movlw b'11010101'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_4
	movlw d'4'
	goto add_to_freq_offset
skip_k_4
	movlw b'01010101'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto wtf
	movlw d'5'
	goto add_to_freq_offset

k_6_7_8_9_0
	movlw b'01010101'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_6
	movlw d'6'
	goto add_to_freq_offset
skip_k_6
	movlw b'01010111'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_7
	movlw d'7'
	goto add_to_freq_offset
skip_k_7
	movlw b'01011111'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_8
	movlw d'8'
	goto add_to_freq_offset
skip_k_8
	movlw b'01111111'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto skip_k_9
	movlw d'9'
	goto add_to_freq_offset
skip_k_9
	movlw b'11111111'			
	subwf command_mode_buffer1, W
	btfss STATUS, Z
	goto wtf
	movlw d'0'
	goto add_to_freq_offset

add_to_freq_offset

	movwf temp_w					; save w which is the number that was sent

	movlw d'4'						; rotate the BCD digits to the left four bits to make room for new digit
	movwf counter1
	bcf STATUS,C					
add_to_freq_offset_loop
	rlf BCD2,F
	rlf BCD1,F
	rlf BCD0,F
	decf counter1,F
	btfss STATUS,Z
	goto add_to_freq_offset_loop

	movfw temp_w					; add the new char to lower nibble of BCD2
	addwf BCD2,F

	pagesel initialize_command_mode
	goto initialize_command_mode


exit_out_of_k_command
	errorlevel -306
	pagesel bcd2binary
	call bcd2binary				;convert what was sent (now in BCD) to two byte binary
	errorlevel +306
	movfw bcd2binary_binary_out+0
	movwf freq_offset_binary+0

	movfw bcd2binary_binary_out+1
	movwf freq_offset_binary+1

	errorlevel -306
	pagesel store_freq_offset_in_eeprom
	call store_freq_offset_in_eeprom ;save to EEPROM
	errorlevel +306
	bcf command_mode_k_cmd		; clear out K command mode flag
	goto acknowledgement_dit

	endif ;include_freq_counter_code

	;------------------ end of double character commands ---------------------------

	; ---- command mode activates ----

	; these commands are multiple character commands
	; bit flags are set to indicate we're in the middle of a multiple character command

	ifdef include_freq_counter_code
command_mode_v_activate					; V command : frequency counter offset mode
	bsf command_mode_v_cmd

	goto initialize_command_mode
	endif ;include_freq_counter_code

	ifdef include_freq_counter_code
command_mode_k_activate					; K command : set frequency counter offset
	bsf command_mode_k_cmd
	clrf BCD0
	clrf BCD1
	clrf BCD2
	goto initialize_command_mode
	endif ;include_freq_counter_code


	ifdef include_m_and_h_cmd_code
command_mode_m_activate					; M command : play memory #
	bsf command_mode_m_cmd
	goto initialize_command_mode
	endif ;include_m_and_h_cmd_code

	ifdef include_m_and_h_cmd_code
command_mode_h_activate					; H command : program memory #
	bsf command_mode_h_cmd
	goto initialize_command_mode
	endif ;include_m_and_h_cmd_code



activate_expert_commands				; E command : activates expert commands
	bsf expert_commands_on
	ifdef include_funky_beeps_code
	errorlevel -306
	pagesel high_beep
	call high_beep
	pagesel high_beep
	call high_beep
	pagesel high_beep
	call high_beep
	errorlevel +306
	banksel PORTA
	pagesel $
	else
	bsf sending_dit
	call send_dit_or_dah
	call send_dit_or_dah
	call send_dit_or_dah
	endif ;include_funky_beeps_code
	pagesel initialize_command_mode
	goto initialize_command_mode

	; --- calls to command subroutines ----

	ifdef include_bug_code
call_toggle_bug_mode
	call toggle_bug_mode
	call write_eeprom_settings1
	goto acknowledgement_dit
	endif

	ifdef include_freq_counter_code
call_count_frequency
	errorlevel -306
	pagesel count_frequency
	call count_frequency
	errorlevel +306
	goto initialize_command_mode

call_toggle_digit_mode
	call toggle_digit_mode
	errorlevel -306
	pagesel write_eeprom_settings1
	call write_eeprom_settings1
	errorlevel +306
	goto initialize_command_mode

call_v0_mode
	call v0_mode
	bcf command_mode_v_cmd
	goto acknowledgement_dit

call_v1_mode
	call v1_mode
	bcf command_mode_v_cmd
	goto acknowledgement_dit

call_v2_mode
	call v2_mode
	bcf command_mode_v_cmd
	goto acknowledgement_dit

call_v3_mode
	call v3_mode
	bcf command_mode_v_cmd
	goto acknowledgement_dit
	endif ;include_freq_counter_code

call_speed_mode
	errorlevel -306
	pagesel speed_mode
	call speed_mode
	errorlevel +306
	errorlevel -306
	pagesel announce_speed_wpm
	call announce_speed_wpm
	errorlevel +306
	goto initialize_command_mode

call_announce_speed_wpm
	errorlevel -306
	pagesel announce_speed_wpm
	call announce_speed_wpm
	errorlevel +306
	goto initialize_command_mode

	ifdef include_tune_mode_code

call_tune_mode
	call tune_mode
	goto initialize_command_mode

	endif ;include_tune_mode_code
	
	ifdef include_calibration_code
call_calibration_check_mode
	call calibration_check_mode
	goto initialize_command_mode
	endif ; include_calibration_code
	
	ifdef include_m_and_h_cmd_code
call_program_memory_0
	errorlevel -306
	pagesel program_memory_0
	call program_memory_0
	errorlevel +306
	bcf command_mode_h_cmd
	goto initialize_command_mode

call_program_memory_1
	errorlevel -306
	pagesel program_memory_1
	call program_memory_1
	errorlevel +306
	bcf command_mode_h_cmd
	goto initialize_command_mode

call_program_memory_2
	errorlevel -306
	pagesel program_memory_2
	call program_memory_2
	errorlevel +306
	bcf command_mode_h_cmd
	goto initialize_command_mode
	
call_playback_memory_0	
	errorlevel -306
	pagesel playback_memory_0
	call playback_memory_0
	errorlevel +306
	bcf command_mode_m_cmd
	goto initialize_command_mode

call_playback_memory_1
	errorlevel -306
	pagesel playback_memory_1
	call playback_memory_1
	errorlevel +306
	bcf command_mode_m_cmd
	goto initialize_command_mode

call_playback_memory_2	
	errorlevel -306
	pagesel playback_memory_2
	call playback_memory_2
	errorlevel +306
	bcf command_mode_m_cmd
	goto initialize_command_mode
	endif ;include_m_and_h_cmd_code

	ifdef include_beacon_code
call_play_beacon
	errorlevel -306
	pagesel play_beacon
	call play_beacon
	errorlevel +306
	goto initialize_command_mode
	endif ;include_beacon_code

	ifdef include_iambic_mode_code
set_iambic_a_mode
	bcf iambic_b_mode
	errorlevel -306
	pagesel write_eeprom_settings1
	call write_eeprom_settings1
	errorlevel +306
	goto acknowledgement_dit

set_iambic_b_mode
	bsf iambic_b_mode
	errorlevel -306
	pagesel write_eeprom_settings1
	call write_eeprom_settings1
	errorlevel +306
	goto acknowledgement_dit
	endif ;include_iambic_mode_code
	
	ifdef include_wavesidetone_code
call_sidetone_freq_adj
	errorlevel -306
	pagesel sidetone_freq_adj
	call sidetone_freq_adj
	errorlevel +306
	goto initialize_command_mode
	endif ;include_wavesidetone_code
	
	ifdef include_toggle_sidetone_on_off_code
call_sidetone_on_off	
	call sidetone_on_off
	errorlevel -306
	pagesel write_eeprom_settings1
	call write_eeprom_settings1
	errorlevel +306
	goto acknowledgement_dit
	endif ;include_toggle_sidetone_on_off_code

	ifdef include_weighting_code
call_weighting_adjust
	call weighting_adjust
	goto initialize_command_mode
	endif ;include_weighting_code

	ifdef include_paddle_reverse_code
call_paddle_reverse_toggle
	call paddle_reverse_toggle
	goto acknowledgement_dit
	endif ;include_paddle_reverse_code

	ifdef include_needless_feature_code
easter_egg
	movlw b'11011100' 			; K
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	movlw b'01010111'			; 3
	movwf cw_char_to_send+0
	movlw b'11000000'
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	movlw b'11010000' 			; N
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	movlw b'11110100' 			; G
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	goto initialize_command_mode
	endif ;include_needless_feature_code

	ifdef include_cw_rx_practice_code
call_code_rx_practice_mode
	call code_rx_practice_mode
	goto initialize_command_mode
	endif ;include_cw_rx_practice_code

	ifdef include_cw_tx_practice_code
call_code_tx_practice_mode
	call code_tx_practice_mode
	goto initialize_command_mode
	endif ;include_cw_tx_practice_code

	ifdef include_freq_counter_calib_code
call_freq_counter_calib_mode
	ifdef include_function_button_code
	call freq_counter_calib_mode
	endif ;include_function_button_code
	goto initialize_command_mode
	endif ;include_freq_counter_calib_code

	; ---- end of calls to command subroutines ----

acknowledgement_dit
	movlw 0x03
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	banksel PORTA
	bsf sending_dit
	errorlevel -306
	pagesel send_dit_or_dah
	call send_dit_or_dah
	errorlevel +306
	banksel PORTA
	pagesel initialize_command_mode
	goto initialize_command_mode


; ------------------ end of check_mode_button ---------------------------------------------------

	ifdef include_freq_counter_code
v0_mode
	bcf offset0
	bcf offset1
	return
v1_mode
	bcf offset0
	bsf offset1
	return
v2_mode
	bsf offset0
	bcf offset1
	return
v3_mode
	bsf offset0
	bsf offset1
	return
	endif ;include_freq_counter_code




; ------------------

	ifdef include_freq_counter_calib_code
	ifdef include_function_button_code
freq_counter_calib_mode
	bcf bit_temp					; store eight_digit_mode so it can be restored later
	btfsc eight_digit_mode			
	bsf bit_temp 	
	bsf eight_digit_mode
freq_counter_calib_mode_loop
	btfsc switch0		; is freq button pressed ?
	goto freq_calib_check_paddles
	errorlevel -306
	pagesel count_frequency
	btfss switch1
	call count_frequency
	errorlevel +306
freq_calib_check_paddles
	btfss dit	; check dit and dah paddles
	goto decrement_freq_calib
	btfss dah
	goto increment_freq_calib
	btfsc mode_switch
	goto freq_counter_calib_mode_loop

	
	; write settings to eeprom
	movlw eeprom_freq_calib_high
	errorlevel -302
	banksel EEADR
	movwf EEADR
	errorlevel +302
	banksel PORTA
	movfw frequency_counter_calib+0
	errorlevel -302
	banksel EEDATA
	movwf EEDATA
	errorlevel +302
	banksel PORTA
	errorlevel -306
	pagesel write_eeprom
	call write_eeprom
	errorlevel +306

	movlw eeprom_freq_calib_low
	errorlevel -302
	banksel EEADR
	movwf EEADR
	errorlevel +302
	banksel PORTA
	movfw frequency_counter_calib+1
	errorlevel -302
	banksel EEDATA
	movwf EEDATA
	errorlevel +302
	banksel PORTA
	errorlevel -306
	pagesel write_eeprom
	call write_eeprom
	errorlevel +306

	; restore original eight_digit_mode value
	btfsc bit_temp
	return
	bcf eight_digit_mode
	return

decrement_freq_calib
	ifdef include_funky_beeps_code
	errorlevel -306
	pagesel high_beep
	call high_beep
	errorlevel +306
	else
	bsf sending_dit
	call send_dit_or_dah
	endif
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	movlw 0xff
	decf frequency_counter_calib+1, F
	subwf frequency_counter_calib+1, W	; did we roll over to FF ?
	btfsc STATUS,Z
	decf frequency_counter_calib+0, F
	goto freq_counter_calib_mode

increment_freq_calib
	ifdef include_funky_beeps_code
	errorlevel -306
	pagesel low_beep
	call low_beep
	errorlevel +306
	else
	bsf sending_dit
	call send_dit_or_dah
	endif
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	incfsz frequency_counter_calib+1, F
	goto freq_counter_calib_mode
	incf frequency_counter_calib+0, F
	goto freq_counter_calib_mode

	endif ;include_function_button_code
	endif ;include_freq_counter_calib_code

; ------------------

	ifdef include_freq_counter_code

toggle_digit_mode
	btfsc eight_digit_mode
	goto go_to_three_digit_mode
	bsf eight_digit_mode
	ifdef include_funky_beeps_code
	errorlevel -306
	pagesel high_beep
	call high_beep
	errorlevel +306
	else
	bsf sending_dit
	call send_dit_or_dah
	call send_dit_or_dah
	endif ;include_funky_beeps_code
	return

go_to_three_digit_mode
	bcf eight_digit_mode
	ifdef include_funky_beeps_code
	errorlevel -306
	pagesel low_beep
	call low_beep
	errorlevel +306
	else
	bsf sending_dit
	call send_dit_or_dah
	call send_dit_or_dah
	endif ;include_funky_beeps_code
	return

	endif ;include_freq_counter_code

; ------------------

	ifdef include_bug_code

toggle_bug_mode
	btfsc bug_mode_on
	goto clear_bug_mode
	bsf bug_mode_on
	return
	
clear_bug_mode
	bcf bug_mode_on
	return

	endif ;include_bug_code

; ------------------

	ifdef include_cw_tx_practice_code

code_tx_practice_mode

	bsf sending_dit
	call send_dit_or_dah

code_tx_practice_mode_loop
	errorlevel -306
	pagesel check_paddles
	call check_paddles					; check if the paddles are active and fill buffers as needed
	errorlevel +306
	call send_buffer					; send any dits and dahs if the buffers have stuff
	
	btfsc mode_switch
	goto code_tx_practice_mode_loop

code_tx_mode_sw_loop
	btfss mode_switch
	goto code_tx_mode_sw_loop

	bcf dit_buffer
	bcf dah_buffer
	call pause_100_ms

	bsf sending_dit
	call send_dit_or_dah

	return

	endif ;include_cw_tx_practice_code

; ------------------

	ifdef include_cw_rx_practice_code
	
get_random_number16
	rlf randomnum+0,W			
	xorwf randomnum+0,W
	movwf temp_w
	rlf temp_w, F	
	movfw temp_w
	swapf randomnum+0, F
	swapf randomnum+1,W
	bcf STATUS,C
	movwf temp_w
	rlf temp_w, F
	movfw temp_w
	xorwf randomnum+0,W
	swapf randomnum+0, F
	andlw 0x01
	rlf randomnum+1, F
	xorwf randomnum+1, F
	rlf randomnum+0, F
	return
	
; ------------------

code_rx_practice_mode

	clrf BCD0		; used to keep track of number of characters sent in a group
	
code_rx_practice_loop

	;check paddles and mode button
	call check_for_button_hit
	sublw 0x01
	btfsc STATUS,Z
	goto code_rx_practice_exit

	call get_random_number16	; get random number
	movfw randomnum
	movwf temp_memory
	movlw b'00111111'			; get it under 65
	andwf temp_memory, F		; with a bit mask
	movlw d'34'					; upper limit of the number we want
	subwf temp_memory, W		; test it
	btfsc STATUS,C				; is result negative ?
	goto code_rx_practice_loop		; number wasn't within limit, get another

	movfw temp_memory			; double the validated random number
	addwf temp_memory, F		;  so we're in the right spot in the lookup table

	;send code
	movfw temp_memory
	call cw_table				; get the first byte of code
	movwf cw_char_to_send+0
	movfw temp_memory
	addlw 0x01					; increment it to get the next byte
	call cw_table
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306

	; pause after five character group
	incf BCD0,F
	movfw BCD0
	sublw 0x05
	btfss STATUS,Z
	goto code_rx_practice_loop
	call loop_6_cw_units
	clrf BCD0
	goto code_rx_practice_loop

code_rx_practice_exit
	call loop_check_for_button_hit		; loop until all buttons are off
	return
	
	endif ;include_cw_rx_practice_code
; ------------------

	ifdef include_paddle_reverse_code

paddle_reverse_toggle
	
	btfss paddle_reverse
	goto set_paddle_reverse
	bcf paddle_reverse
	return
set_paddle_reverse
	bsf paddle_reverse
	return

	endif ;include_paddle_reverse_code
	
; ------------------

	ifdef include_weighting_code
	
weighting_adjust
	bsf sending_dit
	call send_dit_or_dah
	bcf sending_dit
	call send_dit_or_dah
	btfss dit				; dit is pressed, dah is not
	btfss dah
	goto skip_decrease_weight
	call decrease_weight
	goto weighting_adjust
skip_decrease_weight
	btfss dah				; dah is pressed, dit is not
	btfss dit
	goto skip_increase_weight
	call increase_weight
	goto weighting_adjust
skip_increase_weight	
	btfss mode_switch		; if switch is pressed, get out of loop
	goto loop_while_pressed3
	btfss dit				; if both paddles are pressed, this also exits
	btfsc dah
	goto weighting_adjust
	
loop_while_pressed3			; wait for mode_switch, dit and dah to be released
	call pause_100_ms
	btfss mode_switch
	goto loop_while_pressed3
	btfss dit
	goto loop_while_pressed3
	btfss dah
	goto loop_while_pressed3
	
	; clear buffers
	bcf dit_buffer
	bcf dah_buffer
	return	
	
increase_weight
	movfw speed_wpm_dah
	sublw d'4'			; 5 wpm lower limit
	btfsc STATUS, C		;if 4 - speed_wpm > 0, return and do not decrement
	return
	decf speed_wpm_dah, F
	call calculate_cw_unit_values
	return

decrease_weight
	movfw speed_wpm_dah
	sublw d'60'			; 60 wpm upper limit
	btfss STATUS, C		;if 60 - speed_wpm < 0, return and do not increment
	return
	incf speed_wpm_dah, F
	call calculate_cw_unit_values
	return
	
	endif	;include_weighting_code

; ------------------


	ifdef include_toggle_sidetone_on_off_code
sidetone_on_off
	errorlevel -306
	pagesel clear_sidetone_off_during_tx
	btfsc sidetone_off_during_tx
	goto clear_sidetone_off_during_tx
	errorlevel +306
	bsf sidetone_off_during_tx
	return
clear_sidetone_off_during_tx
	bcf sidetone_off_during_tx	
	return
	endif ;include_toggle_sidetone_on_off_code
	
; ------------------
	
	ifdef include_wavesidetone_code

freq_up
	
	movfw wavesidetone_counter_setting
	sublw d'19'			; lower limit
	btfss STATUS, Z		
	decf wavesidetone_counter_setting, F
	return

	endif

; ------------------

	ifdef include_wavesidetone_code

freq_down

	movfw wavesidetone_counter_setting
	sublw d'77'			;  upper limit
	btfss STATUS, Z		
	incf wavesidetone_counter_setting, F
	return
	
	endif
	
; ------------------	

	ifdef include_wavesidetone_code
	
sidetone_freq_adj

	bcf sending_dit
	bsf sidetone
	
sidetone_loop
	errorlevel -306
	pagesel loop_cw_unit
	call loop_cw_unit
	errorlevel +306
	btfss dit				; dit is pressed, dah is not
	btfss dah
	goto skip_freq_up
	call freq_up
	goto sidetone_loop
skip_freq_up
	btfss dah				; dah is pressed, dit is not
	btfss dit
	goto skip_freq_down
	call freq_down
	goto sidetone_loop
skip_freq_down
	errorlevel -306
	pagesel loop_while_pressed_freq_adj
	btfss mode_switch		; if switch is pressed, get out of loop
	goto loop_while_pressed_freq_adj
	errorlevel +306
	btfss dit				; if both paddles are pressed, this also exits
	btfsc dah
	errorlevel -306
	pagesel sidetone_loop
	goto sidetone_loop
	errorlevel +306
loop_while_pressed_freq_adj			; wait for mode_switch, dit and dah to be released
	bcf sidetone
	bcf wavesidetone
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	btfss mode_switch
	goto loop_while_pressed_freq_adj
	btfss dit
	goto loop_while_pressed_freq_adj
	btfss dah
	goto loop_while_pressed_freq_adj

	; write wavesidetone freq to eeprom
	movlw eeprom_wavesidetone_setting
	errorlevel -302
	banksel EEADR
	movwf EEADR
	errorlevel +302
	banksel PORTA
	movfw wavesidetone_counter_setting
	errorlevel -302
	banksel EEDATA
	movwf EEDATA
	errorlevel +302
	banksel PORTA
	errorlevel -306
	pagesel write_eeprom
	call write_eeprom
	errorlevel +306

	bcf dit_buffer
	bcf dah_buffer

	return

	endif


	
; ------------------

	ifdef include_beacon_code
	
check_beacon_mode

	; see if we should go into infinite loop beacon mode

	btfsc dit				; if dit is keyed, go into beacon loop
	return
beacon_mode_loop
	ifdef include_txwake_line_code
	bsf txwake
	endif
	movlw d'3'				; wait a bit to allow tx to come up
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	call play_beacon		; run the beacon
	movlw d'4'				; txwake hang time
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	ifdef include_txwake_line_code
	bcf txwake
	endif
	ifdef include_beacon_serial_ctrl_code
	call check_serial_beacon_control
	endif ;include_beacon_serial_ctrl_code
	movlw d'10'				; beacon sleep time
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	goto beacon_mode_loop
	
	endif ;include_beacon_code


; ------------------

	ifdef include_beacon_serial_ctrl_code

check_serial_beacon_control
	btfsc PIR1,OERR		; did we overrun the buffer?
	goto handle_ser_beacon_overrun
	btfsc PIR1,FERR		; did we have a frame error?
	goto handle_ser_beacon_frame_error
	btfss PIR1,RCIF		; do we have something in the buffer?
	return				; no, blow out of here
	movfw RCREG			; pull byte out of the serial rx FIFO
	sublw d'48'			; was a zero sent?
	btfsc STATUS,Z
	goto beacon_sleep_loop	; yes, sleep in a loop
	return					; no, get out of here

handle_ser_beacon_overrun
	movfw RCREG
	return

handle_ser_beacon_frame_error
	bcf RCSTA,CREN
	bsf RCSTA,CREN
	return

beacon_sleep_loop
	btfsc PIR1,OERR		; did we overrun the buffer?
	call handle_ser_beacon_overrun
	btfsc PIR1,FERR		; did we have a frame error?
	call handle_ser_beacon_frame_error
	btfss PIR1,RCIF		; do we have something in the buffer?
	goto beacon_sleep_loop
	movfw RCREG			; pull byte out of the serial rx FIFO
	sublw d'49'			; was a one sent?
	btfsc STATUS,Z
	return					; yes, get out of here
	goto beacon_sleep_loop	; no, continue in loop

	endif ;include_beacon_serial_ctrl_code

; ------------------

	ifdef include_fox_code
	
check_fox_mode

	; see if we should go into infinite loop fox beacon mode

	btfsc dit				; if dit is keyed, check mode button
	return
	btfsc mode_switch		; if mode_switch is low, go into fox beacon mode
	return	

fox_mode_loop
	ifdef include_txwake_line_code
	bsf txwake
	endif
	movlw d'3'				; wait a bit to allow tx to come up
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	call play_beacon		; run the beacon
	movlw d'4'				; txwake hang time
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	ifdef include_txwake_line_code
	bcf txwake
	endif
	movlw d'10'				; beacon sleep time
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	goto fox_mode_loop
	
	endif ;include_fox_code

; ------------------

	ifdef include_beacon_code

play_beacon

	pagesel send_vvv_de_id
	errorlevel -306
	call send_vvv_de_id
	errorlevel +306
	ifdef include_beacon_serial_ctrl_code
	call check_serial_beacon_control
	endif ;include_beacon_serial_ctrl_code
	errorlevel -306
	pagesel playback_memory_1
	call playback_memory_1
	errorlevel +306
	ifdef include_beacon_serial_ctrl_code
	call check_serial_beacon_control
	endif ;include_beacon_serial_ctrl_code
	errorlevel -306
	pagesel loop_6_cw_units
	call loop_6_cw_units
	errorlevel +306
	pagesel send_vvv_de_id
	errorlevel -306
	call send_vvv_de_id
	errorlevel +306
	ifdef include_beacon_serial_ctrl_code
	call check_serial_beacon_control
	endif ;include_beacon_serial_ctrl_code
	errorlevel -306
	pagesel playback_memory_2
	call playback_memory_2
	errorlevel +306
	ifdef include_beacon_serial_ctrl_code
	call check_serial_beacon_control
	endif ;include_beacon_serial_ctrl_code
	errorlevel -306
	pagesel loop_6_cw_units
	call loop_6_cw_units
	errorlevel +306
	pagesel send_vvv_de_id
	errorlevel -306
	call send_vvv_de_id
	errorlevel +306
	ifdef include_beacon_serial_ctrl_code
	pagesel check_serial_beacon_control
	errorlevel -306
	call check_serial_beacon_control
	errorlevel +306
	endif ;include_beacon_serial_ctrl_code
	return
	
send_vvv_de_id
	movlw b'00000000'
	movwf cw_char_to_send+1
	movlw b'01010111' 			; V
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306	
	movlw b'01010111' 			; V
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306	
	movlw b'01010111' 			; V
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306	
	movlw b'11010100' 			; D
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306	
	movlw b'01000000' 			; E
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	pagesel playback_memory_0
	call playback_memory_0		; send callsign
	pagesel loop_cw_unit
	call loop_cw_unit
	pagesel loop_cw_unit
	call loop_cw_unit
	pagesel loop_cw_unit
	call loop_cw_unit
	errorlevel +306
	movlw b'11010111'			; /
	movwf cw_char_to_send+0
	movlw b'01000000'
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306	
	movlw b'11010101' 			; B
	movwf cw_char_to_send+0
	movlw b'00000000'
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw	
	pagesel loop_6_cw_units
	call loop_6_cw_units
	errorlevel +306
	return
	
	
	endif ;include_beacon_code
	
; ------------------
	
	ifdef include_function_button_code
	
	
playback_memory_loop_delay
	movlw initial_memory_repeat_delay
	movwf temp_memory
	bcf memory_playback_manual_exit

playback_memory_loop
	errorlevel -306
	pagesel check_for_button_hit
	call check_for_button_hit		; check for user exit
	errorlevel +306
	sublw 0x01
	btfsc STATUS,Z
	goto playback_memory_loop_user_exit	
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	decfsz temp_memory,F
	goto playback_memory_loop
	return

playback_memory_loop_user_exit	
	bsf memory_playback_manual_exit
	return

	endif ;include_function_button_code

; ------------------

	ifdef include_function_button_code

check_function_buttons

	; this checks if any memory buttons are pressed
	; single hit = play memory


	ifdef include_call_cq_code
	bcf dit_hit
	endif ;include_call_cq_code
	
	;check if any button lines are low
	btfss switch0
	goto a_function_button_is_pressed	
	btfss switch1
	goto a_function_button_is_pressed	
	btfss switch2
	goto a_function_button_is_pressed	
	return								; no buttons pressed, blow out of here

a_function_button_is_pressed

	movlw b'00000111'
	movwf porta_temp

	movlw d'30'
	movwf count

function_button_read_loop			; this loop reads the switch lines over and over again to prevent bounce and misreads
	movfw PORTA						; get port a register
	andwf porta_temp, F				; and it with a temporary register
	decfsz count,F
	goto function_button_read_loop
	

	ifdef include_freq_counter_button_code
	ifdef include_freq_counter_code
	btfsc porta_temp,0							; freq counter button: switch0 = 0, switch1 = 0, switch2 = 1
	goto no_freq_counter_button_press
	btfsc porta_temp,1
	goto no_freq_counter_button_press
	btfss porta_temp,2
	goto no_freq_counter_button_press	
	bcf key_tx_active
	call count_frequency
	bsf key_tx_active
	goto get_out
no_freq_counter_button_press
	
	endif ;include_freq_counter_code
	endif ;include_freq_counter_button_code
	
	btfss switch0
	goto handle_switch0
	btfss switch1
	goto handle_switch1
	btfss switch2
	goto handle_switch2
	return

handle_switch0
	ifdef include_call_cq_code
	btfss dit					; was dit hit ?
	bsf dit_hit				; if so, set bit_temp
	endif ;include_call_cq_code
	incf count,F				; button hold check code
	btfsc switch0
	goto switch0_off_now
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	goto handle_switch0
switch0_off_now
	ifdef include_call_cq_code
	btfsc dit_hit				; check if dit was hit
	goto call_call_cq
	endif ;include_call_cq_code
	movlw 0x05
	subwf count,F
	btfsc STATUS,C 			; 5 or more 100 mS equals a button hold
	goto playback_memory_0_loop
	errorlevel -306
	pagesel playback_memory_0
	call playback_memory_0
	errorlevel +306
	goto get_out

playback_memory_0_loop
	errorlevel -306
	pagesel playback_memory_0
	call playback_memory_0					; play the memory once
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	goto get_out
	call playback_memory_loop_delay			; do a delay of silence
	btfsc memory_playback_manual_exit		; check if the user exited
	goto get_out
	goto playback_memory_0_loop

handle_switch1
	incf count,F				; button hold check code
	btfsc switch1
	goto switch1_off_now
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	goto handle_switch1
switch1_off_now
	movlw 0x05
	subwf count,F
	btfsc STATUS,C 			; 5 or more 100 mS equals a button hold
	goto playback_memory_1_loop
	errorlevel -306
	pagesel playback_memory_1
	call playback_memory_1
	errorlevel +306
	goto get_out

playback_memory_1_loop
	errorlevel -306
	pagesel playback_memory_1
	call playback_memory_1					; play the memory once
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	goto get_out
	call playback_memory_loop_delay			; do a delay of silence
	btfsc memory_playback_manual_exit		; check if the user exited
	goto get_out
	goto playback_memory_1_loop

handle_switch2
	incf count,F				; button hold check code
	btfsc switch2
	goto switch2_off_now
	errorlevel -306
	pagesel pause_100_ms
	call pause_100_ms
	errorlevel +306
	goto handle_switch2
switch2_off_now
	movlw 0x05
	subwf count,F
	btfsc STATUS,C 			; 5 or more 100 mS equals a button hold
	goto playback_memory_2_loop
	errorlevel -306
	pagesel playback_memory_2
	call playback_memory_2					; play the memory once
	errorlevel +306
	goto get_out

playback_memory_2_loop
	errorlevel -306
	pagesel playback_memory_2
	call playback_memory_2					; play the memory once
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	goto get_out
	call playback_memory_loop_delay			; do a delay of silence
	btfsc memory_playback_manual_exit		; check if the user exited
	goto get_out
	goto playback_memory_2_loop
	
get_out
	bcf dit_buffer
	bcf dah_buffer	
	return

	ifdef include_call_cq_code
call_call_cq
	bsf send_cw_preemptible
	call call_cq
	bcf send_cw_preemptible
	goto get_out
	endif ;include_call_cq_mode

	endif ;include_function_button_code


; ------------------

	ifdef include_call_cq_code

call_cq

	bcf memory_playback_manual_exit
	call send_cq
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	call send_cq
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	call send_cq
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	movlw b'11010100'			; D
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	movlw b'01000000'			; E
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	errorlevel -306
	pagesel loop_6_cw_units
	call loop_6_cw_units
	pagesel playback_memory_0
	call playback_memory_0
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	errorlevel -306
	pagesel loop_6_cw_units
	call loop_6_cw_units
	pagesel playback_memory_0
	call playback_memory_0
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	errorlevel -306
	pagesel loop_6_cw_units
	call loop_6_cw_units
	pagesel playback_memory_0
	call playback_memory_0
	errorlevel +306
	btfsc memory_playback_manual_exit		; check if the user exited
	return
	errorlevel -306
	pagesel loop_6_cw_units
	call loop_6_cw_units
	errorlevel +306
	movlw b'01110111'			; AR
	movwf cw_char_to_send+0
	movlw b'01000000'
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	return

send_cq
	movlw b'11011101'			; C
	movwf cw_char_to_send+0
	movlw b'00000000'
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	movlw b'11110111'			; Q
	movwf cw_char_to_send+0
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	return

	endif ;include_call_cq_mode

; ------------------

	ifdef include_freq_counter_code
	ifdef include_freq_counter_button_code
	ifdef include_function_button_code

check_freq_counter_mode   

	; check if frequency button is pressed
	; if so, loop forever in frequency counter readout mode
	; (this subroutine is called at power up only)

	btfsc switch0
	return
	btfsc switch1
	return
	bcf key_tx_active
freq_counter_loop
	call count_frequency
	movlw d'5'
	errorlevel -306
	pagesel pause_w_100ms
	call pause_w_100ms
	errorlevel +306
	goto freq_counter_loop

	endif ;include_function_button_code
	endif ;include_freq_counter_button_code                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	endif ;include_freq_counter_code

; ------------------
	ifdef include_txwake_line_code	

check_txwake_clear_time
	
	;if there's stuff in a buffer, reset the timer
	btfsc dit_buffer
	goto reset_txwake_timer
	btfsc dah_buffer
	goto reset_txwake_timer
	decfsz txwakecounter+1, F
	return
	decfsz txwakecounter+0, F
	return
	bcf txwake
	return

reset_txwake_timer
	movlw HIGH _txwake_delay_time
	movwf txwakecounter+0
	movlw LOW _txwake_delay_time
	movwf txwakecounter+1
	return

	endif


; ------------------

check_startup_modes

	; see if any of the special startup modes have been invoked by the user at power up

	btfss dit
	btfsc dah
	goto check_other_modes
	goto reset_to_defaults				; if both dit and dah lines are low, reset to defaults
check_other_modes
	ifdef include_fox_code
	pagesel check_fox_mode
	errorlevel -306
	call check_fox_mode
	errorlevel +306
	endif
	errorlevel -306
	pagesel check_straight_key_mode
	call check_straight_key_mode		; check if the user wants to go into brain dead straight key mode
	errorlevel +306
	ifdef include_beacon_code
	errorlevel -306
	pagesel check_beacon_mode
	call check_beacon_mode				; check if the user wants to go into beacon mode
	errorlevel +306
	endif ;include_beacon_code
	ifdef include_freq_counter_button_code
	ifdef include_freq_counter_code
	ifdef include_function_button_code
    call check_freq_counter_mode      	; check if the user is holding the freq button and go into freq count loop mode
	endif ;include_function_button_code                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
	endif ;include_freq_counter_code
	endif ;include_freq_counter_button_code

	return								; no startup mode is needed, blow out of here

; ------------------	

reset_to_defaults
	btfss dit							; loop until one paddle is released
	goto reset_to_defaults
	btfss dah
	goto reset_to_defaults
	errorlevel -306
	pagesel initialize_eeprom_first_time
	call initialize_eeprom_first_time
	pagesel initialize
	call initialize
	errorlevel +306
	return	
	
; ------------------

	ifdef include_freq_counter_code

	; 1.  Read the frequency on pin RA4 for 250 mS.  The signal goes through the prescaler /32
	; 2.  Multiply measurement by 128  [ /32 (prescaler) + /4 (250 mS sample time) ]
	; 3.  Convert the three byte binary reading to eight BCD digits
	; 4.  Calculate the reading by applying the offset if applicable
	; 5.  Readout the frequency in CW either in eight or three digit mode, with leading zero suppression

count_frequency

	errorlevel -302
	banksel OPTION_REG
	movlw b'00110100'	
	; bit 5		: 1 = RA4 input
	; bit 4		: 0 = low to high transistion
	; bit 3		: 0 = prescaler assigned to TIMER0
	; bit 2-0	: set prescaler to 100 (/32)
	movwf OPTION_REG
	errorlevel +302
	banksel PORTA
	movfw frequency_counter_calib+1
	movwf counter1
	movfw frequency_counter_calib+0
	movwf counter2	
	clrf freqcount+0
	clrf freqcount+1
	clrf freqcount+2
	bcf INTCON,T0IF	
	clrf TMR0			; clear TIMER0 and prescaler
loop_250_ms
	movfw TMR0
	btfss INTCON,T0IF	; did TIMER0 overflow?	
	goto no_timer0_overflow						
	bcf INTCON,T0IF							
	incfsz freqcount+1, F					
	goto no_overflow_freqcount_1			
	incf freqcount+0, F						
	goto overflow_freqcount_1				
no_timer0_overflow
	nop
	nop										
	nop											
no_overflow_freqcount_1
	nop										
	nop										
overflow_freqcount_1							
	
	incf counter1, F						
	btfsc STATUS,Z							
	incf counter2, F						
	btfss STATUS,Z							
	goto loop_250_ms						
	
loop_250_ms_done
	;movfw TMR0			; get timer0 as least significant byte
	movwf freqcount+2	

	;multiple by 128 by rotating everything right 7 times
	movlw d'7'
	movwf count
multiply_loop
	bcf STATUS,C
	rlf freqcount+2, F
	rlf freqcount+1, F
	rlf freqcount+0, F
	decfsz count,F
	goto multiply_loop
	
	;convert 24 bit freqcount to 8 digit BCD
	bcf STATUS,C                ; clear the carry bit
	movlw d'24'
	movwf count
	clrf BCD0	; most significant 
	clrf BCD1
	clrf BCD2
	clrf BCD3	; least significant
loop24  
	rlf freqcount+2, F
	rlf freqcount+1, F
	rlf freqcount+0, F
	rlf BCD3,F
	rlf BCD2,F
	rlf BCD1,F
	rlf BCD0,F
	decfsz count, F
	goto adjDEC24
	goto calculate_the_reading
;	goto send_the_digits_in_cw
adjDEC24  
	movlw BCD3
	movwf FSR
	call adjBCD24
	movlw BCD2
	movwf FSR
	call  adjBCD24
	movlw BCD1
	movwf FSR
	call adjBCD24
	movlw BCD0
	movwf FSR
	call adjBCD24
	goto loop24
adjBCD24  
	movlw 0x03
	addwf INDF,W
	movwf temp_memory
	btfsc temp_memory,3         ; test if result > 7
	movwf INDF
	movlw 0x30
	addwf INDF,W
	movwf temp_memory
	btfsc temp_memory,7         ; test if result > 7
	movwf INDF               	; save as MSD
	return

calculate_the_reading

	; calculate the reading using the offset
	btfsc eight_digit_mode			; if we are in eight digit mode, there is no offset applied
	goto send_the_digits_in_cw		
	btfsc offset0
	goto offset0_is_1
	btfss offset1
	goto send_the_digits_in_cw		; there is no offset activated, skip over
	pagesel calc_offset_minus_measurement
	errorlevel -306
	goto calc_offset_minus_measurement
	errorlevel +306
offset0_is_1
	pagesel calc_measurement_plus_offset
	errorlevel -306
	btfsc offset1
	goto calc_measurement_plus_offset
	pagesel calc_measurement_minus_offset
	goto calc_measurement_minus_offset
	errorlevel +306

send_the_digits_in_cw

	btfss eight_digit_mode	; are we in eight digit mode ?
	goto skip_BCD0			; no, skip over BCD0
	;send BCD0 H
	swapf BCD0,F
	movfw BCD0
	andlw b'00001111'
	btfsc STATUS,Z
	goto skip_BCD0H
	errorlevel -306
	pagesel send_w_BCD_in_cw
	call send_w_BCD_in_cw
	errorlevel +306
	
skip_BCD0H
	;send BCD0 L
	swapf BCD0,F
	movfw BCD0
	andlw b'00001111'
	errorlevel -306
	pagesel send_w_BCD_in_cw
	call send_w_BCD_in_cw
	errorlevel +306

skip_BCD0
	btfsc eight_digit_mode
	goto no_BCD1H_zero_suppress
	movfw BCD1
	btfsc STATUS,Z
	goto BCD1_is_zeros
no_BCD1H_zero_suppress
	;send BCD1 H
	swapf BCD1,F
	movfw BCD1
	andlw b'00001111'
	pagesel send_w_BCD_in_cw
	btfss eight_digit_mode	; skip over zero suppresion if we're in 8 digit mode
	btfss STATUS,Z			; zero suppression
	errorlevel -306
	call send_w_BCD_in_cw
	errorlevel +306
skip_BCD1H
	;send BCD1 L
	swapf BCD1,F
	movfw BCD1
	andlw b'00001111'
	errorlevel -306
	pagesel send_w_BCD_in_cw
	call send_w_BCD_in_cw
	errorlevel +306

BCD1_is_zeros
	
	swapf BCD2,F
	movfw BCD2
	andlw b'00001111'
	errorlevel -306
	pagesel send_w_BCD_in_cw
	call send_w_BCD_in_cw
	errorlevel +306
	btfss eight_digit_mode
	return						; if we are in three digit mode, blow out of here
	swapf BCD2,F
	movfw BCD2
	andlw b'00001111'
	errorlevel -306
	pagesel send_w_BCD_in_cw
	call send_w_BCD_in_cw
	errorlevel +306
	
	swapf BCD3,F
	movfw BCD3
	andlw b'00001111'
	errorlevel -306
	pagesel send_w_BCD_in_cw
	call send_w_BCD_in_cw
	errorlevel +306
	swapf BCD3,F
	movfw BCD3
	andlw b'00001111'
	errorlevel -306
	pagesel send_w_BCD_in_cw
	call send_w_BCD_in_cw
	errorlevel +306

	return

	; these routines calculate the frequency readout when the offset is configured

	;   0     7  ,  0    4     0   ,  0     0    0		hertz
	;+-----+-----+-----+-----+-----+-----+-----+-----+
	;|BCD0H|BCD0L|BCD1H|BCD1L|BCD2H|BCD2L|BCD3H|BCD3L|
	;+-----+-----+-----+-----+-----+-----+-----+-----+
	;				|     |     |
	; 			 +-----+-----+-----+
	;			 |    freq_offset  |
	;            +-----+-----+-----+

calc_offset_minus_measurement
	
	clrf BCD0						; in this offset mode we the 10 Mhz and 1 Mhz digits
	errorlevel -306
	pagesel rotate_BCD012_right
	call rotate_BCD012_right		; rotate the BCD digits one position to the right
	errorlevel +306
	clrf BCD0
	errorlevel -306
	pagesel binary2bcd
	call bcd2binary					; convert the BCD measurement to binary
	errorlevel +306
	movfw freq_offset_binary+1		; store offset in temp registers
	movwf L_temp
	movfw freq_offset_binary+0
	movwf H_temp
	movfw bcd2binary_binary_out+1
	subwf L_temp,F
	btfss STATUS,C
	decf H_temp,F
	movfw binary2bcd_binary_in+0
	subwf H_temp,F
	movfw L_temp
	movwf binary2bcd_binary_in+1
	movfw H_temp
	movwf binary2bcd_binary_in+0
	errorlevel -306
	pagesel binary2bcd
	call binary2bcd
	pagesel rotate_BCD012_left
	call rotate_BCD012_left	
	pagesel send_the_digits_in_cw
	goto send_the_digits_in_cw
	errorlevel +306

calc_measurement_minus_offset
	call rotate_BCD012_right		; rotate the BCD digits one position to the right
	errorlevel -306
	pagesel binary2bcd
	call bcd2binary					; convert the BCD measurement to binary
	errorlevel +306
	movfw freq_offset_binary+1
	subwf bcd2binary_binary_out+1,F
	btfss STATUS,C
	decf bcd2binary_binary_out+0,F
	movfw freq_offset_binary+0
	subwf bcd2binary_binary_out+0,F	;bcd2binary_binary_out becomes input parameters for binary2bcd routine
	errorlevel -306
	pagesel binary2bcd
	call binary2bcd
	pagesel rotate_BCD012_left
	call rotate_BCD012_left
	pagesel send_the_digits_in_cw
	goto send_the_digits_in_cw
	errorlevel +306

calc_measurement_plus_offset
	call rotate_BCD012_right		; rotate the BCD digits one position to the right
	errorlevel -306
	pagesel binary2bcd
	call bcd2binary					; convert the BCD measurement to binary
	errorlevel +306
	movfw freq_offset_binary+1
	addwf bcd2binary_binary_out+1,F
	btfsc STATUS,C
	incf bcd2binary_binary_out+0,F
	movfw freq_offset_binary+0
	addwf bcd2binary_binary_out+0,F	;bcd2binary_binary_out becomes input parameters for binary2bcd routine
	errorlevel -306
	pagesel binary2bcd
	call binary2bcd
	pagesel rotate_BCD012_left
	call rotate_BCD012_left
	pagesel send_the_digits_in_cw
	goto send_the_digits_in_cw
	errorlevel +306
	endif ;include_freq_counter_code

; ------------------

	ifdef include_freq_counter_code

rotate_BCD012_right
	clrf temp_w
	movlw d'4'
	movwf count
rotate_BCD012_right_loop
	bcf STATUS,C
	rrf BCD0,F
	rrf BCD1,F
	rrf BCD2,F
	rrf temp_w,F
	decf count,F
	btfss STATUS,Z
	goto rotate_BCD012_right_loop
	movfw temp_w		; store the original lower nibble of BCD2 in upper nibble of BCD0
	addwf BCD0,F
	return

	endif ;include_freq_counter_code

	ifdef include_rotateBCD012_left

; ------------------

rotate_BCD012_left

	;this rotates

	; BCD0 <- BCD1 <- BCD2 <-
	;   |                    |
	;   +--------------------+

	; four bits

	clrf temp_w
	movlw d'4'
	movwf count
rotate_BCD012_left_loop
	bcf STATUS,C
	rlf BCD2,F
	rlf BCD1,F
	rlf BCD0,F
	rlf temp_w,F
	decf count,F
	btfss STATUS,Z
	goto rotate_BCD012_left_loop
	movfw temp_w		; store the upper nibble of BCD0 in lower nibble of BCD2
	addwf BCD2,F
	return

	endif ;include_rotateBCD012_left

; ------------------

	ifdef include_say_hi_code

say_hi
	;say hi
	bcf key_tx_active
	movlw b'01010101'			; H
	movwf cw_char_to_send+0
	movlw b'00000000'
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	movlw b'01010000'			; I
	movwf cw_char_to_send+0
	movlw b'00000000'
	movwf cw_char_to_send+1
	errorlevel -306
	pagesel send_cw
	call send_cw
	errorlevel +306
	bsf key_tx_active
	return

    endif ;include_say_hi_code

; Main Program ------------------------------------------------------------

main_program_start						; this is where the whole shebang starts

	errorlevel -306
	pagesel initialize
	call initialize						; initialize registers, read from eeprom, etc.
	pagesel check_startup_modes
	call check_startup_modes			; see if the user is invoking a special startup mode
	errorlevel +306

	ifdef include_say_hi_code
	pagesel say_hi
    call say_hi
    endif ;include_say_hi_code

	ifdef include_funky_beeps_code		; this is for debugging / programming purposes	
	ifdef include_debug_code			; do a beep at the beginning to make sure the programming worked
	errorlevel -306
	pagesel low_beep
	call beep_boop						; this is needed when using in circuit serial programming
	;call low_beep						; as there's no way to tell if code was successfully loaded
	;pagesel high_beep
	;call high_beep
	errorlevel +306
	endif ;include_debug_code
	endif ;include_funky_beeps_code	

										; now we're done with all the startup stuff, let's send some CW
main_loop

	banksel PORTA						; for good measure

	errorlevel -306
	pagesel check_mode_button
	call check_mode_button				; check if the mode button is pressed and take care of it

	pagesel check_paddles
	call check_paddles					; check if the paddles are active and fill buffers as needed

	pagesel send_buffer
	call send_buffer					; send any dits and dahs if the buffers have stuff

	errorlevel +306
	
	ifdef include_function_button_code
	pagesel check_function_buttons
	errorlevel -306
	call check_function_buttons			; check if any of the function buttons are pressed and take care of them
	errorlevel +306
	endif ;include_function_button_code
	
	ifdef include_txwake_line_code	
	pagesel check_txwake_clear_time
	errorlevel -306
	call check_txwake_clear_time
	errorlevel +306
	endif

	ifdef include_serial_cw_keyboard
	pagesel check_serial_cw_keyboard
	errorlevel -306
	call check_serial_cw_keyboard
	errorlevel +306
	endif ;include_serial_cw_keyboard

	pagesel main_loop
	goto main_loop						; lather, rinse, repeat

	end								


; End Main Program --------------------------------------------------------

; code graveyard


; this tests 100 ms time loop routine
;	bsf sending_dit
;test_loop
;	call send_dit_or_dah
;	movlw d'10'
;	call pause_w_100ms
;	goto test_loop



