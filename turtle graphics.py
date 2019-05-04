# -*- coding: utf-8 -*-
"""
Created on Sat May  4 14:13:17 2019

@author: Prateek
"""
from turtle import 
shape(turtle)
color('red', 'yellow')
begin_fill()
while True:
    forward(200)
    left(170)
    if abs(pos()) < 1:
        break
end_fill()
done()