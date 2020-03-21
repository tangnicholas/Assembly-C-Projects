; Talking_Stop_Watch.asm:  The name says it all!
; P2.6 is the START push button
; P3.0 is the STOP push button.  Pressing this button plays the ellapsed time.
; P0.3 is the CLEAR push button.
; The SPI flash memory is assumed to be loaded with 'stop_watch.wav'
; The state diagram of the playback FSM is available as 'Stop_Watch_FSM.pdf'
;
; Copyright (C) 2012-2019  Jesus Calvino-Fraga, jesusc (at) ece.ubc.ca
; 
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by the
; Free Software Foundation; either version 2, or (at your option) any
; later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
; 
; Connections:
; 
; P89LPC9351  SPI_FLASH
; P2.5        Pin 6 (SPI_CLK)
; P2.2        Pin 5 (MOSI)
; P2.3        Pin 2 (MISO)
; P2.4        Pin 1 (CS/)
; GND         Pin 4
; 3.3V        Pins 3, 7, 8
;
; P0.4 is the DAC output which should be connected to the input of an amplifier (LM386 or similar)
;
; P2.6, P3.0, and P0.3 are connected to push buttons
;
; LCD uses pins P0.5, P0.6, P0.7, P1.2, P1.3, P1.4, P1.6
; WARNING: P1.2 and P1.3 need each a 1k ohm pull-up resistor to VCC (according to the datasheet!).
;
; P2.7 is used (with a transistor) to turn the speaker on/off so it doesn't have a clicking sound.  Use a NPN BJT
; like the 2N3904 or 2N2222A.  The emitter is connected to GND.  The base is connected to a 330 ohm resistor
; and pin P2.7; the other pin of the resistor is connected to 5V.  The collector is connected to the '-'
; terminal of the speaker.
;



CCU_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
CCU_RELOAD  EQU ((65536-((CLK/(2*CCU_RATE)))))



FLASH_CE    EQU P2.4
SOUND       EQU P2.7

; Commands supported by the SPI flash memory according to the datasheet
WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:0
ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite

dseg at 30H
w:             ds 3 ; 24-bit play counter.  Decremented in CCU ISR.
;minutes:       ds 1
;seconds:       ds 1
degrees:	   ds 1
T2S_FSM_state: ds 1
Count5ms:      ds 1

BSEG
T2S_FSM_start: dbit 1
;seconds_flag:  dbit 1
fsm_done:      dbit 1 ;flag used to detect whether fsm has done

; Connect pushbuttons to this pins to, start, stop, or clear the stop watch
;START equ P2.6
;STOP  equ P3.0
;CLEAR equ P0.3

cseg
;org 0x0000 ; Reset vector
;    ljmp Initialize

;org 0x0003 ; External interrupt 0 vector (not used in this code)
;	reti

;org 0x000B ; Timer/Counter 0 overflow interrupt vector (not used in this code)
;	reti

;org 0x0013 ; External interrupt 1 vector (not used in this code)
;	reti

;org 0x001B ; Timer/Counter 1 overflow interrupt vector
;	ljmp Timer1_ISR

;org 0x0023 ; Serial port receive/transmit interrupt vector (not used in this code)
;	reti



;---------------------------------;
; Routine to initialize the CCU.  ;
; We are using the CCU timer in a ;
; manner similar to the timer 2   ;
; available in other 8051s        ;
;---------------------------------;
CCU_Init:
	mov TH2, #high(CCU_RELOAD)
	mov TL2, #low(CCU_RELOAD)
	mov TOR2H, #high(CCU_RELOAD)
	mov TOR2L, #low(CCU_RELOAD)
	mov TCR21, #10000000b ; Latch the reload value
	mov TICR2, #10000000b ; Enable CCU Timer Overflow Interrupt
	setb ECCU ; Enable CCU interrupt
	setb TMOD20 ; Start CCU timer
	ret

;---------------------------------;
; ISR for CCU.  Used to playback  ;
; the WAV file stored in the SPI  ;
; flash memory.                   ;
;---------------------------------;
CCU_ISR:
	mov TIFR2, #0 ; Clear CCU Timer Overflow Interrupt Flag bit. Actually, it clears all the bits!
	
	; The registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Check if the play counter is zero.  If so, stop playing sound.
	mov a, w+0
	orl a, w+1
	orl a, w+2
	jz stop_playing
	
	; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
	mov a, #0xff
	dec w+0
	cjne a, w+0, keep_playing
	dec w+1
	cjne a, w+1, keep_playing
	dec w+2
	
keep_playing:

	lcall Send_SPI ; Read the next byte from the SPI Flash...
	mov AD1DAT3, a ; and send it to the DAC
	
	sjmp CCU_ISR_Done

stop_playing:
	clr TMOD20 ; Stop CCU timer
	setb FLASH_CE  ; Disable SPI Flash
	clr SOUND ; Turn speaker off

CCU_ISR_Done:	
	clr ORANGE_LED
	pop psw
	pop acc
	reti

;---------------------------------;
; Initial configuration of ports. ;
; After reset the default for the ;
; pins is 'Open Drain'.  This     ;
; routine changes them pins to    ;
; Quasi-bidirectional like in the ;
; original 8051.                  ;
; Notice that P1.2 and P1.3 are   ;
; always 'Open Drain'. If those   ;
; pins are to be used as output   ;
; they need a pull-up resistor.   ;
;---------------------------------;
Ports_Init:
    ; Configure all the ports in bidirectional mode:
    mov P0M1, #00H
    mov P0M2, #00H
    mov P1M1, #00H
    mov P1M2, #00H ; WARNING: P1.2 and P1.3 need 1 kohm pull-up resistors if used as outputs!
    mov P2M1, #00H
    mov P2M2, #00H
    mov P3M1, #00H
    mov P3M2, #00H
	ret

;---------------------------------;
; Initialize ADC1/DAC1 as DAC1.   ;
; Warning, the ADC1/DAC1 can work ;
; only as ADC or DAC, not both.   ;
; The P89LPC9351 has two ADC/DAC  ;
; interfaces.  One can be used as ;
; ADC and the other can be used   ;
; as DAC.  Also configures the    ;
; pin associated with the DAC, in ;
; this case P0.4 as 'Open Drain'. ;
;---------------------------------;
InitDAC1:
    ; Configure pin P0.4 (DAC1 output pin) as open drain
	orl	P0M1,   #00010000B
	orl	P0M2,   #00010000B
    mov ADMODB, #00101000B ; Select main clock/2 for ADC/DAC.  Also enable DAC1 output (Table 25 of reference manual)
	mov	ADCON1, #00000100B ; Enable the converter
	mov AD1DAT3, #0x80     ; Start value is 3.3V/2 (zero reference for AC WAV file)
	ret

;---------------------------------;
; Change the internal RC osc. clk ;
; from 7.373MHz to 14.746MHz.     ;
;---------------------------------;
Double_Clk:
    mov dptr, #CLKCON
    movx a, @dptr
    orl a, #00001000B ; double the clock speed to 14.746MHz
    movx @dptr,a
	ret

;---------------------------------;
; Initialize the SPI interface    ;
; and the pins associated to SPI. ;
;---------------------------------;
Init_SPI:
	; Configure MOSI (P2.2), CS* (P2.4), and SPICLK (P2.5) as push-pull outputs (see table 42, page 51)
	anl P2M1, #low(not(00110100B))
	orl P2M2, #00110100B
	; Configure MISO (P2.3) as input (see table 42, page 51)
	orl P2M1, #00001000B
	anl P2M2, #low(not(00001000B)) 
	; Configure SPI
	mov SPCTL, #11010000B ; Ignore /SS, Enable SPI, DORD=0, Master=1, CPOL=0, CPHA=0, clk/4
	ret

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	mov SPDAT, a
Send_SPI_1:
	mov a, SPSTAT 
	jnb acc.7, Send_SPI_1 ; Check SPI Transfer Completion Flag
	mov SPSTAT, a ; Clear SPI Transfer Completion Flag
	mov a, SPDAT ; return received byte via accumulator
	ret

;---------------------------------;
; SPI flash 'write enable'        ;
; instruction.                    ;
;---------------------------------;
Enable_Write:
	clr FLASH_CE
	mov a, #WRITE_ENABLE
	lcall Send_SPI
	setb FLASH_CE
	ret

;---------------------------------;
; This function checks the 'write ;
; in progress' bit of the SPI     ;
; flash memory.                   ;
;---------------------------------;
Check_WIP:
	clr FLASH_CE
	mov a, #READ_STATUS
	lcall Send_SPI
	mov a, #0x55
	lcall Send_SPI
	setb FLASH_CE
	jb acc.0, Check_WIP ;  Check the Write in Progress bit
	ret
	
; Display a binary number in the LCD (must be less than 99).  Number to display passed in accumulator.
LCD_number:
	push acc
	mov b, #10
	div ab
	orl a, #'0'
	lcall ?WriteData
	mov a, b
	orl a, #'0'
	lcall ?WriteData
	pop acc
	ret

; Sounds we need in the SPI flash: 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 30; 40; 50; minutes; seconds;
; Approximate index of sounds in file 'stop_watch.wav'
; This was generated using: computer_sender -Asw_index.asm -S2000 stop_watch.wav
sound_index:
    db 0x00, 0x0a, 0xe0 ; 0 
    db 0x00, 0x64, 0x78 ; 1 
    db 0x00, 0xa0, 0xbd ; 2 
    db 0x00, 0x20, 0xc9 ; 3 
    db 0x01, 0x72, 0x05 ; 4 
    db 0x01, 0xd8, 0x86 ; 5 
    db 0x02, 0x58, 0x05 ; 6 
    db 0x02, 0xa8, 0xa6 ; 7 
    db 0x03, 0x18, 0x6a ; 8 
    db 0x03, 0x78, 0x04 ; 9 
    db 0x03, 0xb2, 0x05 ; 10 
    db 0x04, 0x2a, 0x05 ; 11 
    db 0x04, 0x9a, 0xb5 ; 12 
    db 0x04, 0xff, 0xb5 ; 13 
    db 0x05, 0x73, 0xb5 ; 14 
    db 0x05, 0xff, 0xb5 ; 15 
    db 0x06, 0x5a, 0xb5 ; 16 
    db 0x06, 0xcc, 0x40 ; 17 
    db 0x07, 0x54, 0xb5 ; 18 
    db 0x07, 0xba, 0x5d ; 19 
    db 0x08, 0x3a, 0xb5 ; 20 -------------change made by steven 
    db 0x08, 0x8e, 0xee ; 21 - 30
    db 0x09, 0x01, 0x10 ; 22 - 40 
    db 0x09, 0x4a, 0x00 ; 23 - 50 
    db 0x09, 0xca, 0x99 ; 24 - 60 
    db 0x0a, 0x1a, 0x00 ; 25 - 70 
    db 0x0a, 0x99, 0x00 ; 26 - 80 
    db 0x0a, 0xfd, 0x00 ; 27 - 90 
    db 0x0b, 0x5a, 0x00 ; 28 - 100 
    db 0x0b, 0xca, 0x00 ; 29 - 200 
    db 0x0c, 0x3f, 0x00 ; 30 - the current state is  
    db 0x0c, 0xc0, 0x00 ; 31 - the current temperature is 
    db 0x0d, 0x34, 0x00 ; 32 - too hot 
    db 0x0d, 0x75, 0x00 ; 33 - ramp to soak
    db 0x0e, 0x00, 0x00 ; 34 - soaking
    db 0x0e, 0x65, 0x00 ; 35 - ramp to peak
    db 0x0f, 0x00, 0x00 ; 36 - reflow
    db 0x0f, 0x50, 0x00 ; 37 - cooling
    db 0x0f, 0xa5, 0x00 ; 38 - degrees celsius---------change made by magan

; Size of each sound in 'sound_index'
; Generated using: computer_sender -Asw_index.asm -S2000 stop_watch.wav
Size_Length:
    db 0x00, 0x61, 0xa8 ; 0
    db 0x00, 0x61, 0xa8 ; 1
    db 0x00, 0x61, 0xa8 ; 2
    db 0x00, 0x61, 0xa8 ; 3
    db 0x00, 0x61, 0xa8 ; 4
    db 0x00, 0x61, 0xa8 ; 5
    db 0x00, 0x61, 0xa8 ; 6
    db 0x00, 0x61, 0xa8 ; 7
    db 0x00, 0x61, 0xa8 ; 8
    db 0x00, 0x61, 0xa8 ; 9
    db 0x00, 0x61, 0xa8 ; 10
    db 0x00, 0x61, 0xa8 ; 11
    db 0x00, 0x61, 0xa8 ; 12
    db 0x00, 0x61, 0xa8 ; 13
    db 0x00, 0x61, 0xa8 ; 14
    db 0x00, 0x61, 0xa8 ; 15
    db 0x00, 0x61, 0xa8 ; 16
    db 0x00, 0x61, 0xa8 ; 17
    db 0x00, 0x61, 0xa8 ; 18
    db 0x00, 0x61, 0xa8 ; 19
    db 0x00, 0x61, 0xa8 ; 20
    db 0x00, 0x61, 0xa8 ; 21 - 30
    db 0x00, 0x61, 0xa8 ; 22 - 40
    db 0x00, 0x61, 0xa8 ; 23 - 50
    db 0x00, 0x61, 0xa8 ; 24 - 60
    db 0x00, 0x61, 0xa8 ; 25 - 70
    db 0x00, 0x61, 0xa8 ; 26 - 80
    db 0x00, 0x61, 0xa8 ; 27 - 90
    db 0x00, 0x75, 0x30 ; 28 - 100
    db 0x00, 0x75, 0x30 ; 29 - 200
    db 0x00, 0x75, 0x30 ; 30 - "Current state"
    db 0x00, 0x79, 0x81 ; 31 - "Current temperature"
    db 0x00, 0x61, 0xa8 ; 32 - too hot 
    db 0x00, 0x75, 0x30 ; 33 - ramp to soak
    db 0x00, 0x75, 0x30 ; 34 - soaking
    db 0x00, 0x75, 0x30 ; 35 - ramp to peak
    db 0x00, 0x75, 0x30 ; 36 - reflow
    db 0x00, 0x75, 0x30 ; 37 - cooling
    db 0x00, 0x75, 0x30 ; 38 - degrees celsius
    

; The sound and its length from the two tables above is passed in the accumulator.
Play_Sound_Using_Index:
	clr TMOD20 ; Stop the CCU from playing previous request
	setb FLASH_CE
	
	clr FLASH_CE ; Enable SPI Flash
	mov a, #READ_BYTES
	lcall Send_SPI
	; Set the initial position in memory where to start playing
	mov a, #0
	lcall Send_SPI
	mov a, #0
	lcall Send_SPI
	mov a, #0xff
	lcall Send_SPI
	; How many bytes to play? All of them!  Asume 4Mbytes memory
	mov w+2, #0x3f
	mov w+1, #0xff
	mov w+0, #0x00
	
	mov a, #0x00 ; Request first byte to send to DAC
	lcall Send_SPI
	
	setb SOUND
	setb TMOD20 ; Start playback by enabling CCU timer
  ret
	ret

;---------------------------------------------------------------------------------;
; This is the FSM that plays minutes and seconds after the STOP button is pressed ;
; The state diagram of this FSM is available as 'Stop_Watch_FSM.pdf'              ;
;---------------------------------------------------------------------------------;
; Variables
; degrees - temperature reading from sensor

T2S_FSM:
	mov a, T2S_FSM_state
	clr fsm_done ; Make sure fsm_done flag cleared
;-----------------------------------------------------13---------------------------------------------	
T2S_FSM_State13:
    cjne a, #13, T2S_FSM_State14
    cpl BLUE_LED
	mov a, #31 ; Index 31 has "Current temperature" audio clip
	lcall Play_Sound_Using_Index
	jb TMOD20, $
    mov T2S_FSM_State, #14
    ret
;------------------------------------------------------14--------------------------------------------
T2S_FSM_State14: ;Stay in this state until State1 finishes playing
	cjne a, #14, T2S_FSM_State0
    mov T2S_FSM_State, #0
T2S_FSM_State14_Done:
	ret
;------------------------------------------------------0---------------------------------------------	
T2S_FSM_State0: ; Checks for start signal (T2S_FSM_Start==1)
	cjne a, #0, T2S_FSM_State1
	jnb T2S_FSM_Start, T2S_FSM_State0_Done
	; Check if degrees is larger than 100
	clr c
	mov a, degrees
	subb a, #100
	jnc degrees_gt_100
	; Check if degrees is larger than 20
	clr c
	mov a, degrees
	subb a, #20
	jnc degrees_gt_20
	mov T2S_FSM_State, #1
	sjmp T2S_FSM_State0_Done
degrees_gt_20:
	mov T2S_FSM_state, #3
	sjmp T2S_FSM_State0_Done
degrees_gt_100:
	mov T2S_FSM_state, #6
T2S_FSM_State0_Done:
	ret
;-------------------------------------------------------1----------------------------------------------
T2S_FSM_State1: ; Play degrees when degree is less than 20
	cjne a, #1, T2S_FSM_State2
	mov a, degrees ; Index 28 has "100" audio clip
	lcall Play_By_Index
	mov T2S_FSM_state, #2
	ret
;-------------------------------------------------------2-----------------------------------------------
T2S_FSM_State2: ; Stay in this state until State1 finishes playing
	cjne a, #2, T2S_FSM_State3
    jb TMOD20, T2S_FSM_State2_Done
    mov T2S_FSM_State, #11
T2S_FSM_State2_Done:
	ret
;-------------------------------------------------------3-----------------------------------------------   
T2S_FSM_State3: ; play the tens(21~99)
	cjne a, #3, T2S_FSM_State4
	mov a, degrees
	mov b, #10
	div ab
	add a, #18
	lcall Play_By_Index
	mov T2S_FSM_State, #4
	ret
;-------------------------------------------------------4------------------------------------------------	
T2S_FSM_State4: ; Stay in this state until State2 finishes playing
	cjne a, #4, T2S_FSM_State5
	jb TMOD20, T2S_FSM_State4_Done
	mov T2S_FSM_state, #5
T2S_FSM_State4_Done:
	ret
;-------------------------------------------------------5-------------------------------------------------
T2S_FSM_State5: ; play the remains
	cjne a, #5, T2S_FSM_State6
	mov a, degrees
	mov b, #10
	div ab
	mov a, b
	jz T2S_FSM_State5_Done ;for preventing to playing like forty zero
	lcall Play_By_Index
T2S_FSM_State5_Done:
	mov T2s_FSM_State, #2
	ret
;--------------------------------------------------------6-------------------------------------------------
T2S_FSM_State6: ; Play the hundreds(101~240)
	cjne a, #6, T2S_FSM_State7
	mov a, degrees
	mov b, #100
	div ab
	add a, #27
	lcall Play_By_Index
	mov T2S_FSM_state, #7
	ret
;--------------------------------------------------------7--------------------------------------------------
T2S_FSM_State7: ; Stay in this state until State2 finishes playing
	cjne a, #7, T2S_FSM_State8
	jb TMOD20, T2S_FSM_State4_Done
	mov T2S_FSM_state, #8
T2S_FSM_State7_Done:
	ret
;--------------------------------------------------------8---------------------------------------------------
T2S_FSM_State8: ; Play the tens(101~240)
	cjne a, #8, T2S_FSM_State9
	mov a, degrees
	mov b, #100
	div ab
	mov a, b
	mov b, a
	
	clr c
	subb a, #20
	jnc tens_gt_20
	mov a, b
	sjmp T2S_FSM_State8_Done
tens_gt_20:	
    mov a, b
	mov b, #10
	div ab
	add a, #18
	lcall Play_By_Index
T2S_FSM_State8_Done:
	mov T2S_FSM_state, #9
	ret
;--------------------------------------------------------9---------------------------------------------------
T2S_FSM_State9: ; Stay in this state until State2 finishes playing
	cjne a, #9, T2S_FSM_State10
	jb TMOD20, T2S_FSM_State9_Done
	mov T2S_FSM_state, #10
T2S_FSM_State9_Done:
	ret
;--------------------------------------------------------10-------------------------------------------------
T2S_FSM_State10: ; Play the remains
	cjne a, #10, T2S_FSM_State11
	mov a, degrees
	mov b, #100
	div ab
	mov a, b
	mov b, a
	
	clr c
	subb a, #20
	jnc remains_gt_20
	mov a, b
	jz T2S_FSM_State10_Done ;for preventing to playing like forty zero
	lcall Play_By_Index
	sjmp T2S_FSM_State10_Done
remains_gt_20:	
	mov b, #10
	div ab
	mov a, b
	jz T2S_FSM_State10_Done ;for preventing to playing like forty zero
	lcall Play_By_Index
T2S_FSM_State10_Done:
	mov T2S_FSM_State, #2
	ret
;---------------------------------------------------------11-------------------------------------------------
T2S_FSM_State11: ; play the word 'degrees'
	cjne a, #11, T2S_FSM_State12
	mov a, #38 ;the index 38 has the word 'degree celcius'
	lcall Play_By_Index
	mov T2S_FSM_state, #12
	ret
;---------------------------------------------------------12-------------------------------------------------
T2S_FSM_State12: ; Stay in this state until sounds finishes playing
	cjne a, #12, T2S_FSM_Error
	jb TMOD20, T2S_FSM_State12_Done
	clr T2S_FSM_Start
	mov T2S_FSM_state, #13
T2S_FSM_State12_Done:
	setb fsm_done
	ret

T2S_FSM_Error: ; If this state is reached, there is an error in FSM- restart it
	mov T2S_FSM_state, #13
	clr T2S_FSM_Start
	ret
; End of FSM

Line1: db 'Stop watch', 0
Line2: db '00:00', 0
	
;------------------------------------;
; Play sound by index, set the index ;
; want to play to the parameter 'a'  ;
; before use.                        ;
;------------------------------------;  
Play_By_Index:  
	lcall Play_Sound_Using_Index
	jb TMOD20, $ ; Wait for sound to finish playing
	ret

;---------------------------------;
; Hardware initialization.                          
;---------------------------------;
Initialize:
;   mov SP, #0x7F
    
    lcall Ports_Init ; Default all pins as bidirectional I/O. See Table 42.
    lcall Double_Clk
	lcall InitDAC1 ; Call after 'Ports_Init'
	lcall CCU_Init
	lcall Init_SPI
	
	clr TMOD20 ; Stop CCU timer
	setb EA ; Enable global interrupts.
	clr fsm_done
	clr SOUND ; Turn speaker off

	; Initialize variables
	clr T2S_FSM_Start
	mov T2S_FSM_state, #13
	mov degrees, #0
	
	ret

;-----------------------------------;
; Forever loop to call temperature  ;
; Can lcall check_reflow_start	    ;
;
; For states, can mov a, #[index]   ;
;				  lcall Play_Sound..;
;-----------------------------------;
check_state:
	
temperature_loop:
	mov degrees, temp ; Move temperature reading to degrees variable
	mov T2S_FSM_State, #13
	setb T2S_FSM_Start ; Make sure that start flag is on
fsmloop:
	lcall T2S_FSM ; Run the state machine that plays minutes:seconds
	jb fsm_done, done_fsm
    ljmp fsmloop	
done_fsm:
	;ljmp lalala
	
stop_audio:
	ret
END