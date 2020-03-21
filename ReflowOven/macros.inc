
		
  ; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret
;--------------------------------------------
_convert_time mac
	mov x+0, BCD_counter+0
	mov x+1, BCD_counter+1
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
endmac
 
_convert_to_bcd mac
	mov x+0, %0
	mov x+1, #0
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
endmac		


read_temp mac
	lcall readADC
	lcall calcTemp
	mov x+0, temp+0
	mov x+1, temp+1
	mov x+2, temp+2
	mov x+3, temp+3	
	lcall hex2bcd

endmac


;------------Retreive temp readings from adc---------------------;
read_temperature:
	; Take 256 (4^4) consecutive measurements of ADC0 channel 0 at about 10 us intervals and accumulate in x
	Load_x(0)
    mov x+0, AD0DAT3
	mov R7, #255
    lcall Wait10us
accumulate_loop:
    mov y+0, AD0DAT3
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall add32
    lcall Wait10us
	djnz R7, accumulate_loop
	
	; Now divide by 16 (2^4)
	Load_Y(16)
	lcall div32
	; x has now the 12-bit representation of the temperature
	
	; Convert to temperature (C)
	Load_Y(33000) ; Vref is 3.3V
	lcall mul32
	Load_y(((1<<12)-1)) ; 2^12-1
	lcall div32
	Load_y(255)
	lcall mul32
	Load_y(100)
	lcall div32
	Load_y(2200)
	lcall add32
	
	mov temp+0, x+0			;store result of thermocouple in temp
	mov temp+1, x+1
	mov temp+2, x+2
	mov temp+3, x+3
	lcall hex2bcd
ret	

Send_BCD2 mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD2
	pop ar0
endmac

?Send_BCD2:
	push acc
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret

;---------------------------------;
; Send a BCD number to PuTTY      ;
;---------------------------------;
Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	; Write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret


END