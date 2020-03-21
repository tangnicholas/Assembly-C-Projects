$MODLP51
org 0000H
   ljmp MainProgram

CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER0_RELOAD_H DATA 0xf4
TIMER0_RELOAD_L DATA 0xf2

dseg at 0x30
Result:          ds 4
CTemp:	 		 ds 4
FTemp:    		 ds 4
x:				 ds 4
y:				 ds 4
bcd:			 ds 5		

CSEG
; These ’EQU’ must match the wiring between the microcontroller and ADC
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3
SOUND_OUT EQU P3.7

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

bseg
over100flag:	 dbit 1
mf:				 dbit 1


$NOLIST
$include(LCD_4bit.inc)
$include(math32.inc)
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
    jnb over100flag, notOn
    reti
notOn:
	clr SOUND_OUT
	reti

INIT_SPI:
	setb MY_MISO ; Make MISO an input pin
	clr MY_SCLK ; For mode (0,0) SCLK is zero
	ret
DO_SPI_G:
	push acc
	mov R1, #0 ; Received byte stored in R1
	mov R2, #8 ; Loop counter (8-bits)
DO_SPI_G_LOOP:
	mov a, R0 ; Byte to write is in R0
	rlc a ; Carry flag has bit to write
	mov R0, a
	mov MY_MOSI, c
	setb MY_SCLK ; Transmit
	mov c, MY_MISO ; Read received bit
	mov a, R1 ; Save received bit in R1
	rlc a
	mov R1, a
	clr MY_SCLK
	djnz R2, DO_SPI_G_LOOP
	pop acc
	ret

; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret


TurnVintoTEMP:
	mov x+0, Result+0
	mov x+1, Result+1
	mov x+2, #0
	mov x+3, #0
	
	load_y(410)
	lcall mul32
	load_y(50)
	lcall div32
	load_y(22)
	lcall add32
	load_y(273) ;T = Vout-273
	lcall sub32
	lcall hex2bcd
	
	Send_BCD2(bcd+1)
	Send_BCD(bcd)
	; Send new line / carriage return
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
	

	load_y(30)
	lcall x_gteq_y
	lcall highTemp
	
	lcall printCFtemp
	ret

highTemp:
	jbc mf, alarmOn
	clr over100flag
doneTemp:	
	ret
	
alarmOn:
	setb over100flag
	sjmp doneTemp
	
printCFtemp:
	Set_Cursor(2, 7)
	WriteData(#'C')
	Set_Cursor(2, 14)
	WriteData(#'F')
	
	Set_Cursor(2, 3)
	Display_BCD2(bcd+1)
	Display_BCD(bcd)
	WriteData(#0b11011111)
	
	lcall convertFarenheit		
	ret

convertFarenheit:
	load_y(9)
	lcall mul32
	load_y(5)
	lcall div32
	load_y(32)
	lcall add32
	lcall hex2bcd
	Set_Cursor(2, 10)
	Display_BCD2(bcd+1)
	Display_BCD(bcd)
	WriteData(#0b11011111)
	ret

wait1sec:
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#100)
	ret


; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
 
Hello_World:
    DB  'Hello, World!', '\r', '\n', 0
                 ;01234567890123456
                                   
InitMessage: DB  '  Current Temp: ', 0


MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    lcall Timer0_Init
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    Set_Cursor(2, 0)
	Send_Constant_String(#InitMessage)
    
    mov bcd, #0
    lcall InitSerialPort
    lcall INIT_SPI
    clr over100flag
    
    
Forever:
	clr CE_ADC
	mov R0, #00000001B ; Start bit:1
	lcall DO_SPI_G
	
	mov R0, #10000000B ; Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1 ; R1 contains bits 8 and 9
	anl a, #00000011B ; We need only the two least significant bits
	mov Result+1, a ; Save result high.
	
	mov R0, #55H ; It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result, R1 ; R1 contains bits 0 to 7. Save result low.
	setb CE_ADC
	
	lcall wait1sec
	lcall TurnVintoTEMP
	sjmp Forever	    
END
