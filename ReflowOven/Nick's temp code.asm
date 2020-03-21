$NOLIST
$MODLP51
$include(LCD_4bit.inc)
$include(math32.inc)
$LIST
org 0000H
   ljmp MainProgram

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

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Welcome! To cont', 0
ToContinueClick:  db 'pls click mode  ', 0

SoakMessage:      db 'Soak Settings:  ', 0
OvenDisplay:      db 't=   s tmp=   °C', 0
OvenDisplay2:     db 's:     otmp=  °C', 0

;rfl, sk, rps, rpp, coo


Outside_temp: 	ds 1;
Oven_temp:	ds 1;
Reflow_time:	ds 1;
Reflow_temp:	ds 1;
Soak_time:	ds 1;
Soak_temp:	ds 1;
Mode_sel:     	ds 1 ;

    defaultMessageDisplay:
        lcall LCD_4BIT
        WriteCommand(#0x01)
        Wait_Milli_Seconds(#2)
        Set_Cursor(1, 0)
        Send_Constant_String(#InitMessage)
        Set_Cursor(2, 0)
        Send_Constant_String(#ToContinueClick)

checkContinue:
    jb THE MODE BUTTON, checkContinue  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb THE MODE BUTTON, checkContinue   ; if the 'BOOT' button is not pressed repeat
	jnb THE MODE BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a

;selectLanguage: To Be added later

setSoak:
    Set_Cursor(1, 0)
	Send_Constant_String(#SoakMessage)

    Set_Cursor(2,0)
    WriteData(#Soak_temp)
    WriteData(#0b11011111)
    WriteData(#'C   ')
    WriteData(#Soak_time)
    WriteData(#'s')

checkSoakTimeINC:
    jb INCREMENT_BUT, checkSoakTimeDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkSoakTimeDEC   ; if the button is not pressed jump
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Soak_time, #0x120, jumpINCSoakTime

checkSoakTimeDEC:
    jb INCREMENT_BUT, checkSoakTempINC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkSoakTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Soak_time, #0x60, jumpDECSoakTime

checkSoakTempINC:
    jb INCREMENT_BUT2, checkSoakTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkSoakTempDEC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Soak_temp, #0x200, jumpINCSoakTemp

checkSoakTempDEC:
    jb INCREMENT_BUT2, checkSoakTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkSoakTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Soak_temp, #0x140, jumpDECSoakTemp

continueSoakSetting:
    jb THE MODE BUTTON, setSoak  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb THE MODE BUTTON, setSoak   ; if the 'BOOT' button is not pressed repeat
	jnb THE MODE BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ljmp setReflow
----------------------------------------------
jumpINCSoakTime:
    ljmp INCSoakTime

jumpDECSoakTime:
    ljmp DECSoakTime

jumpINCSoakTemp:
    ljmp INCSoakTemp

jumpDECSoakTemp:
    ljmp DECSoakTemp
----------------------------------------------------------------------------------------------------------
setSoak:
    Set_Cursor(1, 0)
	Send_Constant_String(#ReflowMessage)

    Set_Cursor(2,0)
    WriteData(#'Tp:')
    WriteData(#Reflow_temp)
    WriteData(#0b11011111)
    WriteData(#'C ')
    WriteData(#Reflow_time)
    WriteData(#'s')

checkReflowTimeINC:
    jb INCREMENT_BUT, checkReflowTimeDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkReflowTimeDEC   ; if the button is not pressed jump
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_time, #0x60, jumpINCReflowTime

checkReflowTimeDEC:
    jb INCREMENT_BUT, checkReflowTempINC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkReflowTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_time, #0x30, jumpDECReflowTime

checkReflowTempINC:
    jb INCREMENT_BUT2, checkReflowTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkReflowTempDEC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_temp, #0x260, jumpINCReflowTemp

checkReflowTempDEC:
    jb INCREMENT_BUT2, checkReflowTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkReflowTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_temp, #0x230, jumpDECReflowTemp

continueReflowSetting:
    jb THE MODE BUTTON, setReflow  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb THE MODE BUTTON, setReflow   ; if the 'BOOT' button is not pressed repeat
	jnb THE MODE BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ljmp activateOven
----------------------------------------------
jumpINCReflowTime:
    ljmp INCReflowTime

jumpDECReflowTime:
    ljmp DECReflowTime

jumpINCReflowTemp:
    ljmp INCReflowTemp

jumpDECReflowTemp:
    ljmp DECReflowTemp
----------------------------------------------------------------------------------------------------------

activateOven:





    
----------------------------------------------------------------------------------------------------------

INCSoakTime:
    mov a, Soak_time
    add a, #0x05
    mov Soak_time, a
    ljmp setSoak

DECSoakTime:
    mov a, Soak_time
    sub a, #0x05
    mov Soak_time, a
    ljmp setSoak

INCSoakTemp:
    mov a, Soak_temp
    add a, #0x05
    mov Soak_temp, a
    ljmp setSoak

DECSoakTime:
    mov a, Soak_temp
    sub a, #0x05
    mov Soak_temp, a
    ljmp setSoak

-------------------------
INCReflowTime:
    mov a, Reflow_time
    add a, #0x05
    mov Reflow_time, a
    ljmp setReflow

DECReflowTime:
    mov a, Reflow_time
    sub a, #0x05
    mov Reflow_time, a
    ljmp setReflow

INCReflowTemp:
    mov a, Reflow_temp
    add a, #0x05
    mov Reflow_temp, a
    ljmp setReflow

DECReflowTime:
    mov a, Reflow_temp
    sub a, #0x05
    mov Reflow_temp, a
    ljmp setReflow