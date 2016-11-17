reset
dat = '/Users/mhoecker/Downloads/accumulation.dat'
set palette rgbformulae 3,0,-3
set grid
splot dat u 1:2:($5/$3):3 lc pal pt 4
theta = 0
phi = 0
load '/Users/mhoecker/Documents/gnuplot/rotate.plt'