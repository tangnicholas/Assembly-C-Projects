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

SET_SEC0	  equ P0.1
UPDOWN        equ P0.3
SET_HR		  equ P0.5
SET_MIN		  equ P4.5
TIMER_SPD	  equ P2.5
DISPLAY_MODE  equ P2.3 
TURN_OFF	  equ P2.0
DISMISS       equ P2.7
SNOOZE_TOG	  equ P0.2


SOUND_OUT     equ P3.7


; Reset vector
org 0x0000
    ljmp ClkInit

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
sec:          ds 1
min:          ds 1
hr:           ds 1
min_alarm:    ds 1
hr_alarm:     ds 1
clkMode:	  ds 1
snoozeMode:   ds 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
min_flag:		   dbit 1
hr_flag: 		   dbit 1
pm_flag:		   dbit 1
alarm_on_flag:     dbit 1
alarm_pm_flag:     dbit 1
snooze_flag:	   dbit 1

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
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
   	cpl SOUND_OUT
	;clr TF0  ; According to the data sheet this is done for us already.
 ; Connect speaker to P3.7!
    jnb alarm_on_flag, notOn
    reti
notOn:
	clr SOUND_OUT
	reti
;------
;TIMER 1 STUFF
;Timer1_Init:
;	mov a, TMOD
;	anl a, #0xf0 ; Clear the bits for timer 0
;	orl a, #0x01 ; Configure timer 0 as 16-timer
;	mov TMOD, a
;	mov TH0, #high(TIMER0_RELOAD)
;	mov TL0, #low(TIMER0_RELOAD)
;	; Set autoreload value
;	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
;	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
;	; Enable the timer and interrupts
 ;   setb ET1  ; Enable timer 0 interrupt
 ;   setb TR1  ; Start timer 0
 
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
	; Check if 1 second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a

	
updateTime:	
	;Check the second counter
	mov a, sec
	cjne a, #0x59, Oto59sec
	setb min_flag
	mov a, #0x0 ;set sec to 0
	mov sec, a

updateMin:
	;Check the minute counter
	mov a, min
	cjne a, #0x59, Oto59min
	setb hr_flag
	mov a, #0x0
	mov min, a
	
updateHr:
	;Check if 11am/pm
	mov a, hr
	cjne a, #0x11, Oto11hr
	cpl pm_flag
	sjmp Oto11hr
	
	;Check alarm (to be added later)
	ljmp Timer2_ISR_done
	
Oto59sec:
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov sec, a
	sjmp Timer2_ISR_done

Oto59min:
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov min, a
	sjmp Timer2_ISR_done
	

Oto11hr:
	cjne a, #0x12, really0to11hr
	mov hr, #0x1
	sjmp Timer2_ISR_done


really0to11hr:
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov hr, a
	sjmp Timer2_ISR_done

	
	
Timer2_ISR_decrement:
	mov a, min
	add a, #0x01 ; Adding the 10-complement of -1 is like subtracting 1.
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov min, a
	sjmp Timer2_ISR_done

	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Initializing....', 0
Show_Date:		  db '22/1/2020  Thurs', 0
Alarm_Clock:	  db 'Alarm Clock  ',0
Setting_Alarm:    db 'Setting Alarm',0

; These custom characters copied from https://cdn.instructables.com/ORIG/FGY/5J1E/GYFYDR5L/FGY5J1EGYFYDR5L.txt
Custom_Characters:
	WriteCommand(#40h) ; Custom characters are stored starting at address 40h
; Custom made character 0
	WriteData(#00111B)
	WriteData(#01111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 1
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
; Custom made character 2
	WriteData(#11100B)
	WriteData(#11110B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 3
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#01111B)
	WriteData(#00111B)
; Custom made character 4
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 5
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11110B)
	WriteData(#11100B)
; Custom made character 6
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 7
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	ret

; For all the big numbers, the starting column is passed in register R1
Draw_big_0:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)  
	WriteData(#1) 
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)  
	WriteData(#4)  
	WriteData(#5)
	WriteData(#' ')
	ret
	
Draw_big_1:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand
	WriteData(#' ') 
	WriteData(#255)
	WriteData(#' ')
	ret

Draw_big_2:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#7)
	WriteData(#' ')
	ret

Draw_big_3:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_4:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#4)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	WriteData(#' ')
	ret

Draw_big_5:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#255)
	WriteData(#6)
	WriteData(#6)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_6:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#6)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_7:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#1)
	WriteData(#1)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#0)
	WriteData(#' ')
	ret

Draw_big_8:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_9:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	WriteData(#' ')
	ret
	
Draw_colon:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#1)
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#4)
	ret

; The number to display is passed in accumulator.  The column where to display the
; number is passed in R1. This works only for numbers 0 to 9.
Display_big_number:
	; We need to multiply the accumulator by 3 because the jump table below uses 3 bytes
	; for each 'ljmp' instruction.
	mov b, #3
	mul ab
	mov dptr, #Jump_table
	jmp @A+dptr
Jump_table:
	ljmp Draw_big_0 ; This instruction uses 3 bytes
	ljmp Draw_big_1
	ljmp Draw_big_2
	ljmp Draw_big_3
	ljmp Draw_big_4
	ljmp Draw_big_5
	ljmp Draw_big_6
	ljmp Draw_big_7
	ljmp Draw_big_8
	ljmp Draw_big_9
; No 'ret' needed because we are counting of on the 'ret' provided by the Draw_big_x functions above

; Takes a BCD 2-digit number passed in the accumulator and displays it at position passed in R0
Display_Big_BCD:
	push acc	
	
	; Display the most significant decimal digit
	mov b, R0
	mov R1, b
	swap a
	anl a, #0x0f
	lcall Display_big_number

	; Display the least significant decimal digit, which starts 4 columns to the right of the most significant digit
	mov a, R0
	add a, #4
	mov R1, a
	pop acc
	anl a, #0x0f
	lcall Display_big_number
	ret

; Takes a BCD 2-digit number passed in the accumulator and displays it at position passed in R0
Display_Big_BCD_hr:
	push acc	
	;anl a, #0x10
	;cjne a, #0x10, oneDigitHr
	; Display the most significant decimal digit
	mov a, hr
	mov b, R0
	mov R1, b
	swap a
	anl a, #0x0f
	lcall Display_big_number
	;sjmp forTwoDigit

oneDigitHr:
	; Display the least significant decimal digit, which starts 4 columns to the right of the most significant digit
	;mov a, hr
	
forTwoDigit:
	mov a, R0
	add a, #3
	mov R1, a
	pop acc
	anl a, #0x0f
	lcall Display_big_number
	ret

ClkDis_AMPM:
 	jb pm_flag, wrPM
 	jnb pm_flag, wrAM
    
wrPM:
    WriteData(#'P')
    WriteData(#'M')
    ljmp abreak
    
wrAM:
	WriteData(#'A')
    WriteData(#'M')
    ljmp abreak

AlrmDis_AMPM:    
	jb alarm_pm_flag, wrPM_alarm
 	jnb alarm_pm_flag, wrAM_alarm

wrPM_alarm:
    WriteData(#'P')
    WriteData(#'M')
    ret
    
wrAM_alarm:
	WriteData(#'A')
    WriteData(#'M')
    ret

Wait_1_Sec:
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	ret
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
ClkInit:  ; Initialization of the clk time (alarm is 12:00am by default)
    mov SP, #0x7F
    lcall Timer0_Init
   ; lcall Timer1_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0 configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    lcall Custom_Characters ; Custom characters are needed to display big numbers.  This call generates them.
    
    ;We assume that people are smart enough to setup alarm first time when it is initialized.
    mov BCD_counter, #0x00
    mov sec, 		 #0x00
    mov min, 		 #0x00
    mov hr, 		 #0x01
    mov	min_alarm,   #0x00
	mov hr_alarm,	 #0x00
    mov snoozeMode,	 #0x00
    mov clkMode,	 #0x00
    clr alarm_on_flag
    clr hr_flag
	clr pm_flag
	clr alarm_pm_flag
	clr half_seconds_flag
	clr snooze_flag
	
	Set_Cursor(1, 4)
	WriteData(#0b10110001)
	WriteData(#0b11010111)
	WriteData(#0b10110000)
	WriteData(#0b11010001)
	WriteData(#0b10100101)
	WriteData(#0b10111001)
	WriteData(#0b11011011)
	WriteData(#0b11000010)
	WriteData(#0b10111001)
	Set_Cursor(2, 3)
	Send_Constant_String(#Alarm_Clock)
	lcall Wait_1_Sec
	lcall Wait_1_Sec
	WriteCommand(#0x1)
	Wait_Milli_Seconds(#2)
	
	
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_Cursor(2, 1)
    Send_Constant_String(#Show_Date)
    lcall Wait_1_Sec
    lcall Wait_1_Sec
	WriteCommand(#0x1)
	Wait_Milli_Seconds(#2)
      
main:
    setb half_seconds_flag
	; After initialization the program stays in this 'forever' loop
loop:
;Something to modify clk time (sec, min, hr, up/down)
;Something to modify alarm time (min, hr)
;A button to toggle alarm on/off (toggle)
;Snooze button (5 min timer, use timer 1)
theActualClk:
	ljmp turnOffAlarm
	
wow2s:
	jb DISPLAY_MODE, ActualClk2ndPt
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DISPLAY_MODE, ActualClk2ndPt
	jnb DISPLAY_MODE, $		; Wait for button release.  The '$' means: jump to same instruction
	
goToAlarm:
	WriteCommand(#0x1)
	Wait_Milli_Seconds(#2)
	ljmp setAlarmTime

ActualClk2ndPt:
	jb SET_SEC0, checkClkMin  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SET_SEC0, checkClkMin   ; if the 'BOOT' button is not pressed skip
	jnb SET_SEC0, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov sec, a
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value
	
checkClkMin:
	Wait_Milli_Seconds(#50)
	jb SET_MIN, checkClkHr  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SET_MIN, checkClkHr  ; if the 'BOOT' button is not pressed skip
	jnb SET_MIN, $		; Wait for button release.  The '$' means: jump to same instruction.
	
setClkMin:
	mov a, min
	add a, #0x01
	mov min, a
	cjne a, #0x60, checkClkHr
	mov a, #0x0
	mov min, a
	sjmp checkClkHr
loop_a:
	jnb half_seconds_flag, loop
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    
    mov R0, #0 ; Column where to display the big font 2-digit number
    mov a, hr ; The number to display using big font
	lcall Display_Big_BCD_hr
	
	mov R0, #7 ; Column where to display the big font 2-digit number
    mov a, min ; The number to display using big font
	lcall Display_Big_BCD
	
	Set_Cursor(1,15)
	ljmp ClkDis_AMPM
abreak:
	Set_Cursor(2,15)
	Display_BCD(sec)
	
    ljmp loop
    
checkClkHr:	
	jb SET_HR, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SET_HR, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb SET_HR, $		; Wait for button release.  The '$' means: jump to same instruction.

setClkHr:
	mov a, hr
	cjne a, #0x11, Oto11hrSet
	cpl pm_flag
	ljmp Oto11hrSet

Oto11hrSet:
	cjne a, #0x12, really0to11hrSet
	mov hr, #0x1
	ljmp loop_a

really0to11hrSet:
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov hr, a
	ljmp loop_a

setAlarmTime:  
    jb SNOOZE_TOG, checkMinAlarm
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SNOOZE_TOG, checkMinAlarm
	jnb SNOOZE_TOG, $		; Wait for button release.  The '$' means: jump to same instruction

setAlarmHr:
	mov a, hr_alarm
	cjne a, #0x11, Oto11HrAlarm
	cpl alarm_pm_flag

Oto11HrAlarm:
	cjne a, #0x12, really0to11hr_alarm
	mov a, #0x1
	mov hr_alarm, a
	ljmp checkMinAlarm
	
really0to11hr_alarm:
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov hr_alarm, a
	
checkMinAlarm:
	jb DISMISS, checkModeShift
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DISMISS, checkModeShift
	jnb DISMISS, $		; Wait for button release.  The '$' means: jump to same instruction	
	
setAlarmMin:
	mov a, min_alarm
	cjne a, #0x59, Oto59minAlarm
	mov min_alarm, #0x0
	da a
	ljmp checkModeShift

Oto59minAlarm:
	mov a, min_alarm
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov min_alarm, a
	ljmp checkModeShift
	
checkModeShift:
	jb DISPLAY_MODE, displayAlarmTime
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DISPLAY_MODE, displayAlarmTime
	jnb DISPLAY_MODE, $		; Wait for button release.  The '$' means: jump to same instruction
		
goToActualClktime:
	WriteCommand(#0x1)
	Wait_Milli_Seconds(#2)
	ljmp theActualClk
	
displayAlarmTime:
	Set_Cursor(1,1)
	Send_Constant_String(#Setting_Alarm)
	Set_Cursor(2,1)
	Display_BCD(hr_alarm)
	WriteData(#':')
	Display_BCD(min_alarm)
	Set_Cursor(2,7)
	lcall AlrmDis_AMPM
	ljmp setAlarmTime
	
turnOffAlarm:
	mov a, min 
	cjne a, min_alarm, noAlarm
	mov a, hr
	cjne a, hr_alarm, noAlarm	

	setb alarm_on_flag
	
noAlarm:
	jb TIMER_SPD, jumpBack
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb TIMER_SPD, jumpBack
	jnb TIMER_SPD, $		; Wait for button release.  The '$' means: jump to same instruction
	clr alarm_on_flag
	
jumpBack:
	ljmp wow2s
	
END
