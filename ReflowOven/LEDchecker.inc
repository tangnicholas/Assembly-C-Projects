LEDflickerer:
    mov a, state
    cjne a, #1, LEDs2
    cpl YELLOW_LED
    ljmp finishLED

LEDs2:
    cjne a, #2, LEDs3
    setb YELLOW_LED
    cpl ORANGE_LED
    ljmp finishLED

LEDs3:
    cjne a, #3, LEDs4
    setb YELLOW_LED
    setb ORANGE_LED
    cpl BLUE_LED
    ljmp finishLED

LEDs4:
    cjne a, #4, LEDs5
    cpl YELLOW_LED
    cpl ORANGE_LED
    cpl BLUE_LED
    ljmp finishLED

LEDs5:
    cjne a, #0, finishLED
    setb YELLOW_LED
    setb ORANGE_LED
    setb BLUE_LED

finishLED:
    ret
END