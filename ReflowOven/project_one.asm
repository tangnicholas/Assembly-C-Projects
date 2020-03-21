$MOD9351

  TIMER0_RELOAD_L DATA 0xf2
  TIMER1_RELOAD_L DATA 0xf3
  TIMER0_RELOAD_H DATA 0xf4
  TIMER1_RELOAD_H DATA 0xf5

  XTAL EQU 7373000
  BAUD EQU 115200
  BRVAL EQU ((XTAL/BAUD)-16)
 

  ;---------------;
  ;  Constants    ;
  ;---------------;
  CLK           EQU 14746000 ; Microcontroller system crystal frequency in Hz
  TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
  TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
  TIMER1_RATE   EQU 200    ; 1000Hz, for a timer tick of 1ms
  TIMER1_RELOAD EQU ((65536-(CLK/2*TIMER1_RATE)))
  



  ; Reset vector
  org 0x0000
      ljmp main

  ; External interrupt 0 vector (not used in this code)
  org 0x0003
    reti

  ; Timer/Counter 0 overflow interrupt vector
  org 0x000B
    ljmp Timer0_ISR

  ; External interrupt 1 vector (not used in this code)
  org 0x0013
    reti

  ; Timer/Counter 1 overflow interrupt vector (not used in this code)
  org 0x001B
    ljmp Timer1_ISR

  ; Serial port receive/transmit interrupt vector (not used in this code)
  org 0x0023 
    reti

  ; Timer/Counter 2 overflow interrupt vector
  org 0x002B
   reti
  
  
  org 0x005b ; CCU interrupt vector.  Used in this code to replay the wave file.
	ljmp CCU_ISR


  ;  ljmp Timer2_ISR

  ; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
  dseg at 0x30
  Count_5ms:     	ds 2; Used to determine when half second has passed
  BCD_counter: 		ds 1; The BCD counter incrememted in the ISR and displayed in the main loop
  cold_temp: 		ds 1;
  hot_temp:			ds 1;

  Reflow_time:		ds 2;
  Reflow_temp:		ds 2;
  Soak_time:		ds 1;
  Soak_temp:		ds 1;
  thermo_temp:		ds 1;
  room_temp:		ds 1;

  Mode_sel:     	ds 2;
  state:			ds 1;
  sec:				ds 2;
  temp:				ds 1;

  ReflowTemp_UB:	ds 2;
  ReflowTemp_LB:	ds 2;
  ReflowTime_UB:	ds 2;
  ReflowTime_LB:	ds 2;
  SoakTemp_UB:		ds 2;
  SoakTemp_LB:		ds 2;
  SoakTime_UB:		ds 2;
  SoakTime_LB:		ds 2;
  pwm:				ds 7;

  Result: 			ds 2;
  x:				ds 4;
  y:				ds 4;
  bcd:				ds 5;

 five_seconds_count: ds 1 ;

  ; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
  ; instructions with these variables.  This is how you define a 1-bit variable:
  bseg
  error_flag:       dbit 1 ; Set to one in the ISR every time 1000 ms had passed 
  speak_flag:       dbit 1 ;
  activation_flag:  dbit 1 ;
  soaking_flag:     dbit 1 ;
  rampUp_flag:      dbit 1 ;
  reflow_flag:      dbit 1 ;
  coolDown_flag:    dbit 1 ;
  finished_flag:    dbit 1 ;
  one_seconds_flag:	dbit 1 ;
  PB0:        		dbit 1 ;time inc
  PB1:				dbit 1 ;time dec
  PB2:				dbit 1 ;temp inc
  PB3:				dbit 1 ;temp dec
  PB4:				dbit 1 ;increment mode from continueReflowSetting
  PB5:				dbit 1 ;increment mode from continueSoakSetting
  PB6:				dbit 1 ;start button 
  mf:				dbit 1 ;
  five_seconds_flag:dbit 1

  cseg
  ;CE_ADC    EQU  P2.0 
  ;MY_MOSI   EQU  P2.1  
  ;MY_MISO   EQU  P2.2 
  ;MY_SCLK   EQU  P2.3 

  ;----------------;
  ;button variables;
  ;----------------;
  BLUE_LED 			equ P0.1
  ;RED_LED  			equ P0.1
  ORANGE_LED 		equ P0.2
  YELLOW_LED 		equ P0.3
  ;SOUND_LONGGREEN 	equ P0.4

  LCD_RS equ P0.5
  LCD_RW equ P0.6
  LCD_E  equ P0.7
  LCD_D4 equ P3.0
  LCD_D5 equ P3.1
  LCD_D6 equ P1.2
  LCD_D7 equ P1.3

  ;ADC_7PB				equ P1.7
  MODE_BUTTON    		equ P1.6 ;mode
  ;OP_VOUT				equ P1.4 ;for temp
  OVEN					equ P0.0
  ALARM					equ P2.1
  SOUND_OUT				EQU P2.2


  $NOLIST
  $include(LCD_4bit_LPC9351.inc) ; A library of LCD related functions and utility macros
  $include(math32.inc)
  $include(macros.inc)
  $include(LEDchecker.inc)
  $include(Sound_Final.inc)
  $LIST

  ;                    	1234567890123456    <- This helps determine the location of the counter
  DONEZO: 			db 'd o n e .', 0
  Select_Language:	db 'Select language:', 0
  Error:			db 'ERROR', 0
  Time:				db 'Time:', 0

  TP:				db 'Tp:', 0
  Celsius:			db 'C', 0  
  ReflowMessage:	db 'Reflow Settings:', 0

  ConfirmStart:		db 'Begin?', 0
  blank:			db '     ', 0
  ;rfl, sk, rps, rpp, coo

  Reflow:			db 'REFLOW', 0
  Cool_Down:		db 'COOL DOWN', 0
  RTP:				db 'RAMP 2 PEAK', 0

  ;                     1234567890123456    <- This helps determine the location of the counter
  Initial_Message:  db 'Welcome! To cont', 0
  ToContinueClick:  db 'pls click mode  ', 0

  SoakMessage:      db 'Soak Settings:  ', 0
  OvenDisplay:      db 't=   s tmp=    C', 0
  OvenDisplay2:     db 'st:             ', 0
  RPS:				db 'RAMP 2 SOAK          ', 0
  SK:				db 'SOAKING            ', 0
  HexAscii: 		db '0123456789ABCDEF', 0
  User:				db 'USER', 0
  Terminated:		db 'TERMINATED', 0
  ToRestart:		db 'To restart', 0
  InitialMessage: db '\r\nDisplaying temperature on puTTy!  Input pin is P2.0 \r\n', 0     


;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P2.2 ;
;---------------------------------;
Timer0_ISR:
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	cpl SOUND_OUT ; Connect speaker to this pin
	reti
  ;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 1                   ;
;---------------------------------;
Timer1_Init:
	clr TR1
	
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 1
	orl a, #0x01 ; Configure timer 1 as 16-timer
	mov TMOD, a
	
	mov a, TAMOD
	anl a, #0xf0
	mov TAMOD, a
	clr a
	
	mov TH1, #high(TIMER1_RELOAD)
  	mov TL1, #low(TIMER1_RELOAD)
 
	
	; Enable the timer and interrupts
    setb ET1  ; Enable timer 1 interrupt
    setb TR1  ; Start timer 1
	ret

;---------------------------------;
; ISR for timer 1                 ;
;---------------------------------;
Timer1_ISR:
  	push acc
  	push psw
  	
  	mov TL1, #low(TIMER1_RELOAD)
  	mov TH1, #high(TIMER1_RELOAD)
  	
  ; Increment the 16-bit one mili second counter
	inc Count_5ms+0
	mov a, Count_5ms+0
	jnz Inc_Done
	inc Count_5ms+1
		
Inc_Done:
	mov a, Count_5ms+0
	cjne a, #low(1000), Timer1_ISR_done
	mov a, Count_5ms+1
	cjne a, #high(1000), Timer1_ISR_done
	clr a
	
	mov Count_5ms+0, #0
	mov Count_5ms+1, #0
	
	setb one_seconds_flag
	jnb error_flag, continueNOALARM
	cpl TR0
    
continueNOALARM:
	
	mov a, five_seconds_count
	cjne a, #5, cont_inc_five
	setb five_seconds_flag
	;lcall check_state
	clr a
	mov five_seconds_count, #0
		lcall LEDflickerer
	sjmp cont_Inc_Done
	
cont_inc_five:
	mov a, five_seconds_count
	inc a
	mov five_seconds_count, a
	
cont_Inc_Done:
	lcall read_temperature
	lcall hex2bcd
	lcall shiftBCDdown
	lcall SendTemp

	inc BCD_counter+0
	mov a, BCD_counter
	jnz Timer1_ISR_done
	inc BCD_counter+1


Timer1_ISR_done:
	pop psw
	pop acc
	reti

    
;-----------------------------------------

  ;---------------;
  ;	ADC_TO_PB   ;
  ;---------------;

  ADC_to_PB:
    setb PB6
    setb PB5
    setb PB4
    setb PB3
    setb PB2
    setb PB1
    setb PB0
    ; Check PB6
    clr c
    mov a, AD0DAT2
    subb a, #(216-10) ; 2.8V=216*(3.3/255); the -10 is to prevent false readings
    jc ADC_to_PB_L6
    clr PB6
    ret
  ADC_to_PB_L6:
    ; Check PB5
    clr c
    mov a, AD0DAT2
    subb a, #(185-10) ; 2.4V=185*(3.3/255); the -10 is to prevent false readings
    jc ADC_to_PB_L5
    clr PB5
    ret
  ADC_to_PB_L5:
    ; Check PB4
    clr c
    mov a, AD0DAT2
    subb a, #(154-10) ; 2.0V=154*(3.3/255); the -10 is to prevent false readings
    jc ADC_to_PB_L4
    clr PB4
    ret
  ADC_to_PB_L4:
    ; Check PB3
    clr c
    mov a, AD0DAT2
    subb a, #(123-10) ; 1.6V=123*(3.3/255); the -10 is to prevent false readings
    jc ADC_to_PB_L3
    clr PB3
    ret
ADC_to_PB_L3:
    ; Check PB2
    clr c
    mov a, AD0DAT2
    subb a, #(92-10) ; 1.2V=92*(3.3/255); the -10 is to prevent false readings
    jc ADC_to_PB_L2
    clr PB2
    ret
ADC_to_PB_L2:
    ; Check PB1
    clr c
    mov a, AD0DAT2
    subb a, #(61-10) ; 0.8V=61*(3.3/255); the -10 is to prevent false readings
    jc ADC_to_PB_L1
    clr PB1
    ret
ADC_to_PB_L1:
    ; Check PB1
    clr c
    mov a, AD0DAT2
    subb a, #(30-10) ; 0.4V=30*(3.3/255); the -10 is to prevent false readings
    jc ADC_to_PB_L0
    clr PB0
    ret
ADC_to_PB_L0:
    ; No pusbutton pressed	
    ret

  ;---------------;
  ; SPI and init  ;
  ;---------------;

  ; Configure the serial port and baud rate
InitSerialPort:
    mov	BRGCON,#0x00
    mov	BRGR1,#high(BRVAL)
    mov	BRGR0,#low(BRVAL)
    mov	BRGCON,#0x03 ; Turn-on the baud rate generator
    mov	SCON,#0x52 ; Serial port in mode 1, ren, txrdy, rxempty
    
	mov	P1M1,#0x00 ; Enable pins RxD and TXD
	mov	P1M2,#0x00 ; Enable pins RxD and TXD
    ret

InitADC0:
    ;orl P1M1, #10000000b
    ;anl P1M2, #01111111b
    ;configuring 2.0 and 2.1 as inputs 
    orl P2M1, #00000011b
    anl P2M2, #11111100b
    
	; Setup ADC0
	setb BURST0 ; Autoscan continuos conversion mode
	mov	ADMODB,#0x20 ;ADC0 clock is 7.3728MHz/2
	mov	ADINS,#0x0c ; Select two channels of ADC0 for conversion
	mov	ADCON0,#0x05 ; Enable the converter and start immediately
	; Wait for first conversion to complete
  InitADC0_L1:
    mov	a,ADCON0
    jnb	acc.3,InitADC0_L1
    ret
;-------------------------------------------

  Delay_one_second:
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    ret



  ; Send a constant-zero-terminated string using the serial port

  ;---------------------------------;
  ; USEFUL FUCNTIONS
  ;---------------------------------;

  ; Display a 3-digit BCD number in the LCD
  LCD_3BCD:
    mov a, bcd+1
    anl a, #0x0f
    orl a, #'0'
    lcall ?WriteData
    mov a, bcd+0
    swap a
    anl a, #0x0f
    orl a, #'0'
    lcall ?WriteData
    mov a, bcd+0
    anl a, #0x0f
    orl a, #'0'
    lcall ?WriteData
    ret
    
 LCD_3BCD_4temp:
    mov a, bcd+2
    anl a, #0x0f
    orl a, #'0'
    lcall ?WriteData
    mov a, bcd+1
    swap a
    anl a, #0x0f
    orl a, #'0'
    lcall ?WriteData
    mov a, bcd+1
    anl a, #0x0f
    orl a, #'0'
    lcall ?WriteData
    ret

  Display_ADC_Values:
    ; Analog input to pin P2.1
    mov x+0, AD0DAT2
    mov x+1, #0
    mov x+2, #0
    mov x+3, #0
    lcall Hex2BCD
    Set_Cursor(1, 13)
    lcall LCD_3BCD
    ; Some delay so the LCD looks ok
    Wait_Milli_Seconds(#250)
    ret
  
	
SendString:
    clr a
    movc a, @a+dptr
    jz SendString_L1
    lcall putchar
    inc dptr
    sjmp SendString  
SendString_L1:
	ret

Wait10us:
    mov R0, #18
    djnz R0, $ ; 2 machine cycles-> 2*0.27126us*18=10us
	ret
	
Wait1S:
	mov R2, #40
LA:	mov R1, #250
LB:	mov R0, #184
LC:	djnz R0, LC ; 2 machine cycles-> 2*0.27126us*184=100us
	djnz R1, LB ; 100us*250=0.025s
	djnz R2, LA ; 0.025s*40=1s
	ret

	
sendTemp:
	Send_BCD2(bcd+1)
	Send_BCD(bcd)
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar

ret


  ;---------------------------------;
  ; Main program. Includes hardware ;
  ; initialization and 'forever'    ;
  ; loop.                           ;
  ;---------------------------------;
main:
      mov SP, #0x7F
	  setb OVEN
      ;Configure all the ports in bidirectional mode:
      mov P0M1, #00H
      mov P0M2, #00H
      mov P1M1, #00H
      mov P1M2, #00H ; WARNING: P1.2 and P1.3 need 1kohm pull-up resistors!
      mov P2M1, #00H
      mov P2M2, #00H
      mov P3M1, #00H
      mov P3M2, #00H
      ;Configure input pins
      lcall InitADC0
      ;lcall Initialize
      lcall LCD_4BIT

	  ;THE BELOW VALUES ARE IN   H E X 
  	  mov Soak_temp, 	#140
      mov Soak_time, 	#60 ; Soak Time 60
      mov Reflow_temp, 	#220 ; Reflow Temp 220
      mov Reflow_time, 	#30 ; Reflow Time 30
      mov BCD_counter, 	#0x00
      mov five_seconds_count, #0


      lcall defaultMessageDisplay
      Wait_Milli_Seconds(#50)

      lcall setReflow
      Wait_Milli_Seconds(#50)
      WriteCommand(#1)
      Wait_Milli_Seconds(#50)
      
      lcall activateOven  ;technically our 'state 0' 
      Wait_Milli_Seconds(#50)
      WriteCommand(#1)
      Wait_Milli_Seconds(#50)
      lcall Timer0_Init
      lcall Timer1_Init
      setb EA
	    ;Configure putty
      lcall InitSerialPort
      
	  ;ljmp start ;original
		ljmp start
	  
	 

shiftBCDdown:
	mov bcd+0, bcd+1
	mov bcd+1, bcd+2
	mov bcd+2, bcd+3
	mov bcd+3, bcd+4
	mov bcd+4, #0
ret

start:
	Set_Cursor(1,1)
   	Send_Constant_String(#OvenDisplay)
    Set_Cursor(2,1)
    Send_Constant_String(#OvenDisplay2)
    
    ;state sound
	;mov a, #30
	;lcall Play_By_Index
	;mov a, #33
	;lcall Play_By_Index
	
	
    Set_Cursor(2,4)				;display what state currently in
    Send_Constant_String(#RPS)
    clr OVEN ;power = 100%
    
    mov BCD_counter+0, #0
    mov BCD_counter+1, #0
    mov a, #1
    mov state, a
    clr YELLOW_LED

    
;---------------------------------------

state1:
 	lcall ADC_to_PB
	jnb PB6, done_jump
	
state1_1:
	ljmp system_terminate ;check if pre-soak takes too long 
	
cont2_state1:
	;display time and temp
	Set_Cursor(1,3)			
    _convert_time
    lcall LCD_3BCD
    	
	lcall read_temperature
	lcall shiftBCDdown
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,12)
    lcall LCD_3BCD 
    
    jnb one_seconds_flag, state1 ;if a second has not passed, return to state3
	clr one_seconds_flag 
	lcall read_temperature
	lcall shiftBCDdown
	lcall bcd2hex
    clr mf
    mov a, Soak_temp-2
    mov y+0, a
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
	
    lcall x_gt_y
    jnb mf, state1  ;if current temp > soak_temp, mf = 1, if mf = 1, then go to state 2
    clr mf 
	
    sjmp initialize_state2  ;should be initialize_state2 but for debugging purposes....
	
done_jump:
	ljmp you_is_done2

initialize_state2:
	;audio to indicate soak time
	;mov a, #30
	;lcall Play_Sound_Using_Index
	;mov a, #34
	;lcall Play_Sound_Using_Index
	
	Set_Cursor(1,1)		;will clear previous screen
   	Send_Constant_String(#OvenDisplay)
    Set_Cursor(2,1)
    Send_Constant_String(#OvenDisplay2)
	Set_Cursor(2,4)
    Send_Constant_String(#SK)
    mov a, #2
    mov state, a
    
state2:
	lcall ADC_to_PB
	jnb PB6, done_jump ;did user press abort?
	lcall twenty_percent
	lcall twenty_percent2
	
state2_1:
	;display time and temp
	Set_Cursor(1,3)	
	_convert_to_bcd(Soak_time)	
    lcall LCD_3BCD
	lcall read_temperature
	lcall shiftBCDdown
	Set_Cursor(1,12)
    lcall LCD_3BCD
    

	jnb one_seconds_flag, jumpy ;if a second has not passed, return to state2
	clr one_seconds_flag
	
	dec Soak_time ;1 second has passed so decrement time
	mov a, Soak_time 
	jnz jumpy ;if Soak_time is not 0, return to state2
	ljmp initialize_state3

jumpy:
	ljmp state2

initialize_state3:
	;audio to indicate soak time
	;mov a, #30
	;lcall Play_Sound_Using_Index
	;mov a, #35
	;lcall Play_Sound_Using_Index
	
	Set_Cursor(1,1)		;will clear previous screen
   	Send_Constant_String(#OvenDisplay)
    Set_Cursor(2,1)
    Send_Constant_String(#OvenDisplay2)
	Set_Cursor(2,4)
    Send_Constant_String(#RTP)
    clr OVEN ;power = 100%
    mov BCD_counter, #0
    mov a, #3
    mov state, a
    
state3:
	lcall ADC_to_PB
	jnb PB6, done_jump2 ;did user press abort?
	sjmp state3_1

done_jump2:
	ljmp you_is_done2
	
state3_1:
		;display time and temp
		Set_Cursor(1,3)			
   		_convert_time
    	lcall LCD_3BCD
    
		lcall read_temperature
		lcall shiftBCDdown
		Wait_Milli_Seconds(#250)
		Set_Cursor(1,12)
	    lcall LCD_3BCD  
	    
		jnb one_seconds_flag, state3 ;if a second has not passed, return to state3
		clr one_seconds_flag
		
		lcall read_temperature
		lcall shiftBCDdown
		lcall bcd2hex
		clr mf
		mov a, Reflow_temp
		mov y+0, a
		mov y+1, #0
    	mov y+2, #0
    	mov y+3, #0
    	lcall x_gt_y
    	jnb mf, state3  
    	clr mf 

    
initialize_state4:
	;audio to indicate soak time
	;mov a, #30
	;lcall Play_Sound_Using_Index
	;mov a, #36
	;lcall Play_Sound_Using_Index
	
	Set_Cursor(1,1)		;will clear previous screen
   	Send_Constant_String(#OvenDisplay)
    Set_Cursor(2,1)
    Send_Constant_String(#OvenDisplay2)
	Set_Cursor(2,4)
    Send_Constant_String(#Reflow)
    mov a, #4
    mov state, a
    ;lcall LEDflickerer

    
state4:
	lcall ADC_to_PB
	jnb PB6, done_jump3 ;did user press abort?
	
	lcall twenty_percent3
	lcall twenty_percent4  
	lcall LEDs4
	sjmp state4_1

	
done_jump3:
	ljmp you_is_done2
	

state4_1:
	;display time and temp
	Set_Cursor(1,3)			
    _convert_to_bcd(Reflow_time)
    Display_BCD(bcd+0)
	lcall read_temperature
	lcall hex2bcd
	lcall shiftBCDdown
	Set_Cursor(1,12)
    lcall LCD_3BCD
	
	jnb one_seconds_flag, state4 ;if a second has not passed, return to state2
	clr one_seconds_flag
	
	dec Reflow_time ;1 second has passed so decrement time
	mov a, Reflow_time 
	jnz state4 ;if Soak_time is not 0, return to state2

initialize_state5:
	;audio to indicate soak time
	;mov a, #30
	;lcall Play_Sound_Using_Index
	;mov a, #37
	;lcall Play_Sound_Using_Index
	
	Set_Cursor(1,1)		;will clear previous screen
   	Send_Constant_String(#OvenDisplay)
    Set_Cursor(2,1)
    Send_Constant_String(#OvenDisplay2)
	Set_Cursor(2,4)
    Send_Constant_String(#Cool_Down)
    setb OVEN
    mov BCD_counter, #0
    mov a, #5
    mov state, a
    ;lcall LEDflickerer
    
state_5:
	;display time and temp
	Set_Cursor(1,3)			
    _convert_time
    lcall LCD_3BCD
	lcall read_temperature
	lcall shiftBCDdown
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,12)
    lcall LCD_3BCD
    
    jnb one_seconds_flag, state_5 ;if a second has not passed, return to state3
	clr one_seconds_flag
    lcall read_temperature
    lcall shiftBCDdown
    lcall bcd2hex
    clr mf
    mov a, #50
    ;compare temp
    mov y+0, a  ;hex 1388 = 5000 or 50 degrees 
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall x_lt_y
    jnb mf, state_5
    clr mf  
    ljmp defaultMessageDisplay 

;-------------------------------------

you_is_done2:
    
user_terminated:
	setb OVEN
	WriteCommand(#1)
	Wait_Milli_Seconds(#2)
	Set_Cursor(1,6)
	Send_Constant_String(#User)
	Set_Cursor(2,3)
	Send_Constant_String(#Terminated)
	
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	WriteCommand(#1)
	Wait_Milli_Seconds(#2)
	
	Set_Cursor(1,1)
	Send_Constant_String(#ToRestart)
	Set_Cursor(2,1)
	Send_Constant_String(#ToContinueClick)
	
wait_restart:
	jb MODE_BUTTON, wait_restart
	Wait_Milli_Seconds(#50)
	jb MODE_BUTTON, wait_restart
	jnb MODE_BUTTON, $
	
	ljmp defaultMessageDisplay
;--------------------------------------
twenty_percent:
	
    lcall read_temperature
    lcall shiftBCDdown
    lcall bcd2hex
    clr mf
    mov a, Soak_temp
   	mov y+0, a    ;hex for 140 degrees (using 2 bits) supposed to be Soak_temp 
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall x_gt_y
    jb mf, turn_off ;if mf = 1, or current temp > goal temp, turn off
    ljmp go_back	;else return
	clr mf
twenty_percent2:
    lcall read_temperature
    lcall shiftBCDdown
    lcall bcd2hex
    clr mf
    mov a, Soak_temp
    mov y+0, a    ;hex for 140 degrees (using 2 bits) supposed to be Soak_temp 
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall x_lt_y
    jb mf, turn_on ;if mf = 1, or current temp > goal temp, turn off
    ljmp go_back	;else return
	clr mf

twenty_percent3:
	
    lcall read_temperature
    lcall shiftBCDdown
    lcall bcd2hex
    clr mf
    mov a, Reflow_temp
   	mov y+0, a    ;hex for 140 degrees (using 2 bits) supposed to be Soak_temp 
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall x_gt_y
    jb mf, turn_off ;if mf = 1, or current temp > goal temp, turn off
    ljmp go_back	;else return
	clr mf
twenty_percent4:
    lcall read_temperature
    lcall shiftBCDdown
    lcall bcd2hex
    clr mf
    mov a, Reflow_temp
    mov y+0, a    ;hex for 140 degrees (using 2 bits) supposed to be Soak_temp 
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall x_lt_y
    jb mf, turn_on ;if mf = 1, or current temp > goal temp, turn off
    ljmp go_back	;else return
	clr mf

go_back1:
    ret
    
turn_on:
    clr OVEN
    ret

turn_off:
    setb OVEN
    ret 

go_back:
    ret
	
;--------------------------------------
system_terminate:
	clr mf
	_convert_time
	mov y+0, #59
	mov y+1, #0
	mov y+2, #0
	mov y+3, #0
	lcall x_gt_y
	jnb mf, cont_state1 ;if condition (mf=1, current time IS less than 60) is met, continue )
	clr mf 
	
	;otherwise
	lcall read_Temperature
	lcall hex2bcd
	lcall shiftBCDdown
	lcall bcd2hex
	clr mf
	mov y+0, #50
	mov y+1, #0
	mov y+2, #0
	mov y+3, #0
	lcall x_lt_y
	jnb mf, cont_state1
	clr mf
	
	setb error_flag
	ljmp you_is_done2
	
cont_state1:
	ljmp cont2_state1

;---------------------------------
	
defaultMessageDisplay:
	mov a, #0
	mov state, a
	lcall LEDflickerer
    WriteCommand(#1)
    Wait_Milli_Seconds(#2)

    Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#2)
    Set_Cursor(2, 1)
    Send_Constant_String(#ToContinueClick)
	

waiting_to_continue:
	jb MODE_BUTTON, waiting_to_continue
	Wait_Milli_Seconds(#50)
	jb MODE_BUTTON, waiting_to_continue
	jnb MODE_BUTTON, $
	

setSoak:
	;configuring display
    Set_Cursor(1, 1)
    Send_Constant_String(#SoakMessage)
	Set_Cursor(2,1)
    Send_Constant_String(#TP)
    mov a, Soak_temp
    mov x+0, a
    mov x+1, #0
    mov x+2, #0
    mov x+3, #0
    lcall hex2bcd
    ;Display_bcd(bcd+1)
    lcall LCD_3BCD
	WriteData(#0b11011111)
    Send_Constant_String(#Celsius)
    WriteData(#' ')
    WriteData(#' ')
    WriteData(#' ')
    mov x+0, Soak_time+0
    mov x+1, #0
    mov x+2, #0
    mov x+3, #0
    lcall hex2bcd
    lcall LCD_3BCD
    WriteData(#'s')


checkSoakTimeINC:
    lcall ADC_to_PB
    jb PB0, checkSoakTimeDEC

    mov a, Soak_time
    cjne a, #0x50, jumpINCSoakTime

checkSoakTimeDEC:
    jb PB1, checkSoakTempINC

    mov a, Soak_time
    cjne a, #0x60, jumpDECSoakTime

setSoakJump:		;can't reach branch
    ljmp setSoak

checkSoakTempINC:
    jb PB2, checkSoakTempDEC
    mov a, Soak_temp
    cjne a, #205, jumpINCSoakTemp

checkSoakTempDEC:
    jb PB3, continueSoakSetting
    mov a, Soak_temp
    cjne a, #140, jumpDECSoakTemp

continueSoakSetting:
    jb MODE_BUTTON, jmpstsoak
	Wait_Milli_Seconds(#50)
	jb MODE_BUTTON, jmpstsoak
	jnb MODE_BUTTON, $
    ret
      

jmpstsoak:
    ljmp setSoak
  ;--------------------------------

  jumpINCSoakTime:
      ljmp INCSoakTime

  jumpDECSoakTime:
      ljmp DECSoakTime

  jumpINCSoakTemp:
      ljmp INCSoakTemp

  jumpDECSoakTemp:
      ljmp DECSoakTemp
  ;----------------------------------------------------------------------------------------------------------
  setReflow:
      	;configuring display
    Set_Cursor(1, 1)
    Send_Constant_String(#ReflowMessage)
	Set_Cursor(2,1)
    Send_Constant_String(#TP)
    mov x+0, Reflow_temp+0
    mov x+1, #0
    mov x+2, #0
    mov x+3, #0
    lcall hex2bcd
    lcall LCD_3BCD
	WriteData(#0b11011111)
    Send_Constant_String(#Celsius)
    WriteData(#' ')
    WriteData(#' ')
    WriteData(#' ')
    mov x+0, Reflow_time+0
    mov x+1, #0
    mov x+2, #0
    mov x+3, #0
    lcall hex2bcd
    lcall LCD_3BCD
    WriteData(#'s')

  checkReflowTimeINC:
    lcall ADC_TO_PB
      jb PB0, checkReflowTimeDEC
      mov a, Reflow_time
      cjne a, #0x90, jumpINCReflowTime

  checkReflowTimeDEC:

    jb PB1, checkReflowTempINC
      mov a, Reflow_time
      cjne a, #0x00, jumpDECReflowTime

  setReflowJump:
    ljmp setReflow

  checkReflowTempINC:
      jb PB2, checkReflowTempDEC
      mov a, Reflow_temp
      cjne a, #0x90, jumpINCReflowTemp

  checkReflowTempDEC:
      jb PB3, continueReflowSetting
      mov a, Reflow_temp
      cjne a, #0x00, jumpDECReflowTemp

  continueReflowSetting:
	jb MODE_BUTTON, setReflowJump
	Wait_Milli_Seconds(#50)
	jb MODE_BUTTON, setReflowJump
	jnb MODE_BUTTON, $

    ret
  ;----------------------------------------------
  jumpINCReflowTime:
      ljmp INCReflowTime

  jumpDECReflowTime:
      ljmp DECReflowTime

  jumpINCReflowTemp:
      ljmp INCReflowTemp

  jumpDECReflowTemp:
      ljmp DECReflowTemp

  replacerdonzo:
    setb OVEN ;setb turns oven OFF (reverse logic), clr turns oven ON 
    WriteCommand(#0x01)
	Wait_Milli_Seconds(#3)
      Set_Cursor(1,1)
      Send_Constant_String(#DONEZO)

  donzo_loop:
  	Wait_Milli_Seconds(#250)
  	Wait_Milli_Seconds(#250)
	cpl OVEN
    sjmp donzo_loop


  ;----------------------------------------------------------------------------------------------------------
activateOven:
    mov a, #0
    mov state, a  ;state=0 for fsm 
    Set_Cursor(1, 1)
    Send_Constant_String(#ConfirmStart)

wait_activate:
	jb MODE_BUTTON, wait_activate
	Wait_Milli_Seconds(#50)
	jb MODE_BUTTON, wait_activate
	jnb MODE_BUTTON, $

    ret
  ;----------------------------------------------------------------------------------------------------------

  INCSoakTime:
      mov a, Soak_time
      cjne a, #0x5A, contINCSoakTime ; 0x5A = 90 in decimal
      
      Wait_Milli_Seconds(#200)
      ljmp setSoak
  
  contINCSoakTime:
      add a, #0x05
      mov Soak_time, a
      Wait_Milli_Seconds(#200)
      ljmp setSoak
      
      

  DECSoakTime:
      mov a, Soak_time
      cjne a, #0x3C, contDECSoakTime ; 0x3C = 60 in decimal

      Wait_Milli_Seconds(#200)
      ljmp setSoak
      
  contDECSoakTime:
      add a, #0xFB ; 0xFB = 251 in decimal
      mov Soak_time, a
      Wait_Milli_Seconds(#200)
      ljmp setSoak
      
      
      

INCSoakTemp:
      mov a, Soak_temp
      cjne a, #0xC8, contINCSoakTemp ; 0xC8 = 200 in decimal
      
      Wait_Milli_Seconds(#200)
      ljmp setSoak
      
  contINCSoakTemp:
      add a, #0x05
      mov Soak_temp, a
      Wait_Milli_Seconds(#200)
      ljmp setSoak

  DECSoakTemp:
      mov a, Soak_temp
      cjne a, #0x8C, contDECSoakTemp ; 0x8C = 140 in decimal
      
      Wait_Milli_Seconds(#200)
      ljmp setSoak
  
  contDECSoakTemp:
      add a, #0xFB
      mov Soak_temp, a
      Wait_Milli_Seconds(#200)
      ljmp setSoak

  ;-------------------------
  INCReflowTime:
      mov a, Reflow_time
      cjne a, #0x3C, contINCReflowTime ; 0x3C = 60 in decimal
      
      Wait_Milli_Seconds(#200)
      ljmp setReflow
  
  contINCReflowTime:
      add a, #0x05
      mov Reflow_time, a  
      Wait_Milli_Seconds(#200)
      ljmp setReflow

  DECReflowTime:
      mov a, Reflow_time
      cjne a, #0x1E, contDECReflowTime ; 0x1E = 30 in decimal

      Wait_Milli_Seconds(#200)
      ljmp setReflow
      
  contDECReflowTime:
      add a, #0xFB ; 0xFB = 251 in decimal
      mov Reflow_time, a
      Wait_Milli_Seconds(#200)
      ljmp setReflow

  INCReflowTemp:
      mov a, Reflow_temp
      cjne a, #0xE6, contINCReflowTemp ; 0xE6 = 230 in decimal
      
      Wait_Milli_Seconds(#200)
      ljmp setReflow
      
  contINCReflowTemp:
      add a, #0x05
      mov Reflow_temp, a
      Wait_Milli_Seconds(#200)
      ljmp setReflow

  DECReflowTemp:
      mov a, Reflow_temp
      cjne a, #0xDC, contDECReflowTemp ; 0xDC = 220 in decimal
      
      Wait_Milli_Seconds(#200)
      ljmp setReflow
  
  contDECReflowTemp:
      add a, #0xFB
      mov Reflow_temp, a
      Wait_Milli_Seconds(#200)
      ljmp setReflow




      END
