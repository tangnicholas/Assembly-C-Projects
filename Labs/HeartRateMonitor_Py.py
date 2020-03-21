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

def data_gen():
    t = data_gen.t
    while True:
        t+=1
        strin = ser.readline()
        val1=float(strin)
        yield t, val1

def run(data):
    # update the data
    t,y1 = data
    if t>-1 or t:
        xdata.append(t)
        ydata1.append(y1)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
        line1.set_data(xdata, ydata1)

        ret, frame = cap.read()
        frame = cv2.flip(frame,1)
        out.write(frame)
        cv2.imshow('frame',frame)
    return line1

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line1, = ax.plot([], [], lw=2, label='BPM')
ax.set_ylim(10, 150)
ax.set_xlim(0, xsize)
ax.grid()
ax.legend()
xdata, ydata1, = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, 
repeat=False)
plt.show()

# Release everything if job is finished
cap.release()
out.release()
cv2.destroyAllWindows()

      
