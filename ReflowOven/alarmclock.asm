; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
BUTTON0       equ P0.0
BUTTON1       equ P0.1
BUTTON2       equ P0.2
BUTTON3       equ P0.3
BUTTON4       equ P0.4
BUTTON5       equ P0.5
BUTTON6       equ P0.6

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
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_counter2: ds 1 ;
BCD_counter3: ds 1 ;
Hour:         ds 1 ;
Hour2:        ds 1 ;
Hour3:        ds 1 ;
Minute:       ds 1 ;
Minute2:      ds 1 ;
Minute3:      ds 1 ;
Second:       ds 1 ;
Second2:      ds 1 ;
Second3:      ds 1 ;
Half_Day:     ds 1 ;
Day:          ds 1 ;
Day2:         ds 1 ;
Day3:         ds 1 ;
Mode_sel:     ds 1 ;

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
error_flag:        dbit 1 ; Set to one in the ISR every time 1000 ms had passed 
speak_flag:        dbit 1 ;
activation_flag:   dbit 1 ;
soaking_flag:      dbit 1 ;
rampUp_flag:       dbit 1 ;
reflow_flag:       dbit 1 ;
coolDown_flag:     dbit 1 ;
finished_flag:     dbit 1 ;


cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'xx:xx:xx xM, xxx', 0
Clock_Message:    db 'Mode: Clock     '
Alarm1_Message:   db 'Mode: Alarm Wday ', 0
Alarm2_Message:   db 'Mode: Alarm Wknd ', 0
Monday:           db 'Mon', 0
Tuesday:          db 'Tue', 0
Wednesday:        db 'Wed', 0
Thursday:         db 'Thu', 0
Friday:           db 'Fri', 0
Saturday:         db 'Sat', 0
Sunday:           db 'Sun', 0
 
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
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    clr ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), offset10 ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), offset10
	sjmp millisecond_cnt

offset10:
	ljmp Timer2_ISR_done

millisecond_cnt:
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know 1 second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
check_Alarm:
	mov a, Day
	cjne a, #0x07, Weekday_Alarm
	cjne a, #0x06, Weekday_Alarm
	mov a, Hour
	cjne a, Hour3, Main_counter
	mov a, Minute
	cjne a, Minute3, Main_counter
	mov a, AM_flag
	cjne a, AM_flag3, Main_counter
	ljmp Alarm_on
	
Weekday_Alarm:
	mov a, Hour
	cjne a, Hour2, Main_counter
	mov a, Minute
	cjne a, Minute2, Main_counter
	mov a, AM_flag
	cjne a, AM_flag2, Main_counter
	ljmp Alarm_on
	
Alarm_on:
	clr a
	setb ET0 
	
Main_counter:	
	; Increment the BCD counter
	mov a, BCD_counter
	cjne a, #0x59, Increment_Sec ; Checks if 60 seconds has passed 
	mov a, #0x00
	mov BCD_counter, a
	
	clr a
	mov a, Minute 
	add a, #0x01 ; Increments 1 minute if 60 seconds passed
	da a
	mov Minute, a  
	
	clr a
	mov a, Minute
    cjne a, #0x60, Not_increment ; Checks if 60 minutes has passed
	mov a, #0x00
	mov Minute, a
	
	clr a
	mov a, Hour 
	add a, #0x01 ; Increments 1 hour if 60 seconds passed
	da a
	mov Hour, a

	clr a
	mov a, Hour 
	cjne a, #0x13, Hour_increment ; Checks if 12 hours has passed
	mov a, #0x01 ;----
	mov Hour, a ;-----
	
Hour_increment:
	clr a
	mov a, Hour 
	cjne a, #0x12, Not_increment ; Checks if 12 hours has passed
	mov a, #0x01 ;----
	cpl AM_Flag ;'nots' the AM_Flag if 12
	jb AM_Flag, Day_increment
	sjmp Not_increment
	
Day_increment:	
	clr a
	mov a, Day
	add a, #0x01 ; Increments 1 full-day if AM->PM
	da a
	mov Day, a
	
	clr a
	mov a, Day
	cjne a, #0x08, Not_increment ; Checks if 7 days have passed 
	mov a, #0x01
	mov Day, a
	
	clr a	
	
Increment_Sec:
	mov a, BCD_counter
	add a, #0x01
	da a
	mov BCD_counter, a
	clr a
	
Not_increment:
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
	
    setb seconds_flag
    clr  AM_flag
	
	mov BCD_counter,  #0x50
	mov BCD_counter2, #0x00
	mov BCD_counter3, #0x00
    mov Hour,         #0x11
    mov Hour2,        #0x01
    mov Hour3,        #0x01
    mov Minute,       #0x59
    mov Minute2,      #0x01
    mov Minute3,      #0x01
    mov Day,          #0x01
    mov Day2,         #0x01
    mov Day3,         #0x06
    mov Mode_sel,     #0x01 
	
	; After initialization the program stays in this 'forever' loop
	
loop:
	jb BOOT_BUTTON, offset1      ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	      ; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, offset1      ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		      ; Wait for button release.  The '$' means: jump to same instruction.
	
	cpl Clock_pause
	jb Clock_pause, pause
	setb TR2                      ; Stop timer 2
	ljmp offset1
	
offset1:
	ljmp mode_set;
	
pause: 
	clr TR2
	ljmp mode_set;

mode_set:
	jb BUTTON3, offset2
	Wait_Milli_Seconds(#50)
	jb BUTTON3, offset2
	jnb BUTTON3, $		          ; Wait for button release.  The '$' means: jump to same instruction.
	clr a
	mov a, Mode_sel
	add a, #0x01
	da a 
	cjne a, #0x04, Mode_sel_reset
	mov a, #0x01
	mov Mode_sel, a
	clr a
	sjmp offset2
Mode_sel_reset:
	mov Mode_sel, a
	clr a
	sjmp offset2
offset2:
	mov a, Mode_sel
	cjne a, #0x01, offset3
	clr a
	ljmp hour_set
offset3:
	cjne a, #0x02, offset4
	clr a
	ljmp Mode2_hour_set
offset4:
	cjne a, #0x03, loop
	clr a
	ljmp Mode3_hour_set

hour_set:
	jb BUTTON0, minute_set
	Wait_Milli_Seconds(#50)
	jb BUTTON0, minute_set
	jnb BUTTON0, $
	clr a
	mov a, Hour
	add a, #0x01
	da a
	cjne a, #0x13, Reset_Hour 
	mov a, #0x01
	mov Hour, a
	clr a
	sjmp minute_set
Reset_Hour:
	mov Hour, a
	clr a
	sjmp minute_set
	
minute_set:
	jb BUTTON4, second_set
	Wait_Milli_Seconds(#50)
	jb BUTTON4, second_set
	jnb BUTTON4, $
	clr a
	mov a, Minute
	add a, #0x01
	da a
	cjne a, #0x60, Reset_Min 
	mov a, #0x00
	mov Minute, a
	clr a
	sjmp second_set
Reset_Min:
	mov Minute, a
	clr a
	sjmp second_set
	
second_set:
	jb BUTTON6, AM_PM_set
	Wait_Milli_Seconds(#50)
	jb BUTTON6, AM_PM_set
	jnb BUTTON6, $
	clr a
	mov a, BCD_counter
	add a, #0x01
	da a
	cjne a, #0x60, Reset_Sec ; Checks if 60 seconds has passed 
	mov a, #0x00
	mov BCD_counter, a
	clr a
	sjmp AM_PM_set
Reset_Sec:
	mov BCD_counter, a
	clr a
	sjmp AM_PM_set

AM_PM_set:
	jb BUTTON1, Day_set
	Wait_Milli_Seconds(#50)
	jb BUTTON1, Day_set
	jnb BUTTON1, $
	cpl AM_Flag
	sjmp Day_Set
	
Day_set:
	jb BUTTON2, offset5
	Wait_Milli_Seconds(#50)
	jb BUTTON2, offset5
	jnb BUTTON2, $
	clr a
	mov a, Day
	add a, #0x01
	da a
	cjne a, #0x8, Reset_Day ; Checks if 60 seconds has passed 
	mov a, #0x01
	mov Day, a
	clr a
	ljmp loop_b
offset5:
	clr a
	ljmp loop_b
Reset_Day:
	mov Day, a
	clr a
	ljmp loop_b
	
Mode2_hour_set:	
	jb BUTTON0, Mode2_minute_set
	Wait_Milli_Seconds(#50)
	jb BUTTON0, Mode2_minute_set
	jnb BUTTON0, $
	clr a
	mov a, Hour2
	add a, #0x01
	da a
	cjne a, #0x13, Mode2_Reset_Hour 
	mov a, #0x01
	mov Hour2, a
	clr a
	sjmp Mode2_minute_set
Mode2_Reset_Hour:
	mov Hour2, a
	clr a
	sjmp Mode2_minute_set

Mode2_minute_set:
	jb BUTTON4, Mode2_second_set
	Wait_Milli_Seconds(#50)
	jb BUTTON4, Mode2_second_set
	jnb BUTTON4, $
	clr a
	mov a, Minute2
	add a, #0x01
	da a
	cjne a, #0x60, Mode2_Reset_Min 
	mov a, #0x00
	mov Minute2, a
	clr a
	sjmp Mode2_second_set
Mode2_Reset_Min:
	mov Minute2, a
	clr a
	sjmp Mode2_second_set

Mode2_second_set:
	jb BUTTON6, Mode2_AM_PM_set
	Wait_Milli_Seconds(#50)
	jb BUTTON6, Mode2_AM_PM_set
	jnb BUTTON6, $
	clr a
	mov a, BCD_counter2
	add a, #0x01
	da a
	cjne a, #0x60, Mode2_Reset_Sec ; Checks if 60 seconds has passed 
	mov a, #0x00
	mov BCD_counter2, a
	clr a
	sjmp Mode2_AM_PM_set
Mode2_Reset_Sec:
	mov BCD_counter2, a
	clr a
	sjmp Mode2_AM_PM_set

Mode2_AM_PM_set:
	jb BUTTON1, Mode2_Day_set
	Wait_Milli_Seconds(#50)
	jb BUTTON1, Mode2_Day_set
	jnb BUTTON1, $
	cpl AM_flag2
	sjmp Mode2_Day_Set
	
Mode2_Day_set:
	jb BUTTON2, offset6
	Wait_Milli_Seconds(#50)
	jb BUTTON2, offset6
	jnb BUTTON2, $
	clr a
	mov a, Day2
	add a, #0x01
	da a
	cjne a, #0x8, Mode2_Reset_Day ; Checks if 60 seconds has passed 
	mov a, #0x01
	mov Day2, a
	clr a
	ljmp loop_c
offset6:
	clr a
	ljmp loop_c
Mode2_Reset_Day:
	mov Day, a
	clr a
	ljmp loop_c


Mode3_hour_set:	
	jb BUTTON0, Mode3_minute_set
	Wait_Milli_Seconds(#50)
	jb BUTTON0, Mode3_minute_set
	jnb BUTTON0, $
	clr a
	mov a, Hour3
	add a, #0x01
	da a
	cjne a, #0x13, Mode3_Reset_Hour 
	mov a, #0x01
	mov Hour3, a
	clr a
	sjmp Mode3_minute_set
Mode3_Reset_Hour:
	mov Hour3, a
	clr a
	sjmp Mode3_minute_set

Mode3_minute_set:
	jb BUTTON4, Mode3_second_set
	Wait_Milli_Seconds(#50)
	jb BUTTON4, Mode3_second_set
	jnb BUTTON4, $
	clr a
	mov a, Minute3
	add a, #0x01
	da a
	cjne a, #0x60, Mode3_Reset_Min 
	mov a, #0x00
	mov Minute3, a
	clr a
	sjmp Mode3_second_set
Mode3_Reset_Min:
	mov Minute3, a
	clr a
	sjmp Mode3_second_set

Mode3_second_set:
	jb BUTTON6, Mode3_AM_PM_set
	Wait_Milli_Seconds(#50)
	jb BUTTON6, Mode3_AM_PM_set
	jnb BUTTON6, $
	clr a
	mov a, BCD_counter3
	add a, #0x01
	da a
	cjne a, #0x60, Mode3_Reset_Sec ; Checks if 60 seconds has passed 
	mov a, #0x00
	mov BCD_counter3, a
	clr a
	ljmp Mode3_AM_PM_set
Mode3_Reset_Sec:
	mov BCD_counter3, a
	clr a
	sjmp Mode3_AM_PM_set

Mode3_AM_PM_set:
	jb BUTTON1, Mode3_Day_set
	Wait_Milli_Seconds(#50)
	jb BUTTON1, Mode3_Day_set
	jnb BUTTON1, $
	cpl AM_flag3
	sjmp Mode3_Day_Set
	
Mode3_Day_set:
	jb BUTTON2, offset7
	Wait_Milli_Seconds(#50)
	jb BUTTON2, offset7
	jnb BUTTON2, $
	clr a
	mov a, Day3
	add a, #0x01
	da a
	cjne a, #0x8, Mode3_Reset_Day ; Checks if 60 seconds has passed 
	mov a, #0x01
	mov Day3, a
	clr a
	ljmp loop_d
offset7:
	clr a
	ljmp loop_d
Mode3_Reset_Day:
	mov Day, a
	clr a
	ljmp loop_d
	 
loop_b:
    clr seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
   	Set_Cursor(2, 1)
   	Send_Constant_String(#Clock_Message)
   	Set_Cursor(1, 7)
	Display_BCD(BCD_Counter)
	Set_Cursor(1, 4)
	Display_BCD(Minute)
	Set_Cursor(1, 1)
	Display_BCD(Hour)
	Set_Cursor(1, 10)
	
	mov a, Day
	cjne a, #0x01, Tue_c
	Set_Cursor(1, 14)
	Send_Constant_String(#Monday)
	ljmp day_c
Tue_c:
	cjne a, #0x02, Wed_c
	Set_Cursor(1, 14)
	Send_Constant_String(#Tuesday)
	ljmp day_c
Wed_c:
	cjne a, #0x03, Thurs_c
	Set_Cursor(1, 14)
	Send_Constant_String(#Wednesday)
	ljmp day_c
Thurs_c:
	cjne a, #0x04, Fri_c
	Set_Cursor(1, 14)
	Send_Constant_String(#Thursday)
	ljmp day_c
Fri_c:
	cjne a, #0x05, Sat_c
	Set_Cursor(1, 14)
	Send_Constant_String(#Friday)
	ljmp day_c
Sat_c:
	cjne a, #0x06, Sun_C
	Set_Cursor(1, 14)
	Send_Constant_String(#Saturday)
	ljmp day_c
Sun_c:
	cjne a, #0x07, day_c
	Set_Cursor(1, 14)
	Send_Constant_String(#Sunday)
day_c:
	jb AM_flag, AM2_Output
	Set_Cursor(1, 10)
	Display_char(#'P')
	ljmp loop
AM2_Output:
	Set_Cursor(1, 10)
	Display_char(#'A')
    ljmp loop

loop_c:
	Set_Cursor(2, 1)
	Send_Constant_String(#Alarm1_Message)
	Set_Cursor(1, 7)
	Display_BCD(BCD_Counter2)
	Set_Cursor(1, 4)
	Display_BCD(Minute2)
	Set_Cursor(1, 1)
	Display_BCD(Hour2)
	Set_Cursor(1, 10)
	
	mov a, Day2
	cjne a, #0x01, Tue_c2
	Set_Cursor(1, 14)
	Send_Constant_String(#Monday)
	ljmp day_c2
Tue_c2:
	cjne a, #0x02, Wed_c2
	Set_Cursor(1, 14)
	Send_Constant_String(#Tuesday)
	ljmp day_c2
Wed_c2:
	cjne a, #0x03, Thurs_c2
	Set_Cursor(1, 14)
	Send_Constant_String(#Wednesday)
	ljmp day_c2
Thurs_c2:
	cjne a, #0x04, Fri_c2
	Set_Cursor(1, 14)
	Send_Constant_String(#Thursday)
	ljmp day_c2
Fri_c2:
	cjne a, #0x05, Sat_c2
	Set_Cursor(1, 14)
	Send_Constant_String(#Friday)
	ljmp day_c2
Sat_c2:
	cjne a, #0x06, Sun_c2
	Set_Cursor(1, 14)
	Send_Constant_String(#Saturday)
	ljmp day_c2
Sun_c2:
	cjne a, #0x07, day_c2
	Set_Cursor(1, 14)
	Send_Constant_String(#Sunday)
day_c2:
	jb AM_flag2, AM2_Output2
	Set_Cursor(1, 10)
	Display_char(#'P')
	ljmp loop
AM2_Output2:
	Set_Cursor(1, 10)
	Display_char(#'A')
    ljmp loop
    
loop_d:
	Set_Cursor(2, 1)
	Send_Constant_String(#Alarm2_Message)
	Set_Cursor(1, 7)
	Display_BCD(BCD_Counter3)
	Set_Cursor(1, 4)
	Display_BCD(Minute3)
	Set_Cursor(1, 1)
	Display_BCD(Hour3)
	Set_Cursor(1, 10)
	
	mov a, Day3
	cjne a, #0x01, Tue_c3
	Set_Cursor(1, 14)
	Send_Constant_String(#Monday)
	ljmp day_c3
Tue_c3:
	cjne a, #0x02, Wed_c3
	Set_Cursor(1, 14)
	Send_Constant_String(#Tuesday)
	ljmp day_c3
Wed_c3:
	cjne a, #0x03, Thurs_c3
	Set_Cursor(1, 14)
	Send_Constant_String(#Wednesday)
	ljmp day_c3
Thurs_c3:
	cjne a, #0x04, Fri_c3
	Set_Cursor(1, 14)
	Send_Constant_String(#Thursday)
	ljmp day_c3
Fri_c3:
	cjne a, #0x05, Sat_c3
	Set_Cursor(1, 14)
	Send_Constant_String(#Friday)
	ljmp day_c3
Sat_c3:
	cjne a, #0x06, Sun_c3
	Set_Cursor(1, 14)
	Send_Constant_String(#Saturday)
	ljmp day_c3
Sun_c3:
	cjne a, #0x07, day_c3
	Set_Cursor(1, 14)
	Send_Constant_String(#Sunday)
day_c3:
	jb AM_flag2, AM2_Output3
	Set_Cursor(1, 10)
	Display_char(#'P')
	ljmp loop
AM2_Output3:
	Set_Cursor(1, 10)
	Display_char(#'A')
    ljmp loop
END