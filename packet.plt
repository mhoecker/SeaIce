reset
d = 5000;
g = 9.81;
accel = 100;
t=0;
k0 = .1
dk = .002*k0;
omega(k) = sqrt(g*k*tanh(k*d))
dt = .5/omega(k0);
T = 500*dt;
w(x,k) = sin(k*x-t*omega(k))/(20*k);
waves(x) = w(x,k0+dk)+w(x,k0-dk)+w(x,k0+2*dk)/2+w(x,k0-2*dk)/2+w(x,k0+3*dk)/3+w(x,k0-3*dk)/3+w(x,k0+4*dk)/4+w(x,k0-4*dk)/4+w(x,k0+5*dk)/5+w(x,k0-5*dk)/5+w(x,k0+6*dk)/6+w(x,k0-6*dk)/6+w(x,k0+7*dk)/7+w(x,k0-7*dk)/7+w(x,k0+8*dk)/8+w(x,k0-8*dk)/8+w(x,k0+9*dk)/9+w(x,k0-9*dk)/9+w(x,k0+10*dk)/10+w(x,k0-10*dk)/10+w(x,k0+11*dk)/11+w(x,k0-11*dk)/11
set xrange [-1/dk:1/dk]
set samples 2048
set yrange [-1/k0:1/k0]
plot waves(x) not
load '~/Documents/gnuplot/looper.plt'
# packet.plt
# 
#
# Created by Martin Hoecker-Martinez on 11/17/10.
# Copyright 2010 Oregon State University. All rights reserved.


