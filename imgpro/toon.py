#!/usr/bin/python3
import sys
import cv2

line_size = 7
blur_value = 7
filename = sys.argv[1]
img = cv2.imread(filename)
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
edges = cv2.adaptiveThreshold(gray, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, line_size, blur_value)
cv2.imwrite('{}-toon.png'.format(filename.split('.')[-2]), edges)
