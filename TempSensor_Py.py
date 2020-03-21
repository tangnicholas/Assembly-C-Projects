import numpy as np
import cv2
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math
import serial
import serial.tools.list_ports
import plotly.graph_objects as go
import time
from gtts import gTTS
import os
import threading

try:
    ser.close() # try to close the last opened port
except:
    print('')

portlist=list(serial.tools.list_ports.comports())
print ('Available serial ports (will try to open the last one):')
for item in portlist:
    print (item[0])

# configure the serial port
ser = serial.Serial(
    port=item[0],
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_TWO,
    bytesize=serial.EIGHTBITS
)
ser.isOpen()

xsize=60
cap = cv2.VideoCapture(0)
fourcc = cv2.VideoWriter_fourcc(*'XVID')
out = cv2.VideoWriter('output.avi',fourcc, 20.0, (640,480))

# def printit():
#   threading.Timer(5.0, printit).start()
#   strin = ser.readline()
#   val4=float(strin)
# tts = gTTS(text='The current temperature is', lang='en')
# tts.save("good.mp3")
# os.system("good.mp3")
# tts = gTTS(text=val4, lang='en')
# tts.save("good1.mp3")
# os.system("good1.mp3")
# tts = gTTS(text='degrees celcius', lang='en')
# tts.save("good2.mp3")
# os.system("good2.mp3")
#     return


def data_gen():
    t = data_gen.t
    while True:
        t+=1
        strin = ser.readline()
        val1=float(strin)
        val2=(9.0/5.0*float(strin)+32.0)
        val3=273.0+float(strin)
        yield t, val1, val2, val3

def run(data):
    # update the data
    t,y1,y2,y3 = data
    if t>-1 or t:
        xdata.append(t)
        ydata1.append(y1)
        ydata2.append(y2)
        ydata3.append(y3)
        if t>xsize: # Scroll to the left.
        ax.set_xlim(t-xsize, t)
        line1.set_data(xdata, ydata1)
        line2.set_data(xdata, ydata2)
        line3.set_data(xdata, ydata3)

        ret, frame = cap.read()
        frame = cv2.flip(frame,1)
        out.write(frame)
        cv2.imshow('frame',frame)

        tts = gTTS(text='The current temperature is', lang='en')
        tts.save("good.mp3")
        tts = gTTS(text=y1, lang='en')
        tts.save("good1.mp3")
        tts = gTTS(text='degrees celcius', lang='en')
        tts.save("good2.mp3")
        os.system("good.mp3")
        os.system("good1.mp3")
        os.system("good2.mp3")

    return line1, line2, line3

def on_close_figure(event):
    sys.exit(0)



data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line1, = ax.plot([], [], lw=2, label='Celcius')
line2, = ax.plot([], [], lw=2, label='Farenheit')
line3, = ax.plot([], [], lw=2, label='Kelvin')
ax.set_ylim(10, 500)
ax.set_xlim(0, xsize)
ax.grid()
ax.legend()
xdata, ydata1, ydata2, ydata3 = [], [], [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=20, 
repeat=False)
plt.show()

# Release everything if job is finished
cap.release()
out.release()
cv2.destroyAllWindows()
