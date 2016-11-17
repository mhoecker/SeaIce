reset
k=0.1897/.25
g(x,y) = -x**3+(3*pi*log(10)*log(y)/(k*y)+1)*x**2+x-1
set xrange [0.05:.25]
set yrange [1e1:1e6]
set zrange [-1:1]
set logscale y
set view map
set cbtics ("K-H Billows" -.5, "No Billows" .5)
N = 384
set isosamples N,N
set samples N
set contour
set cntrparam level 1
set grid
unset surface
set cbtics nomirror
set cbtics out
set xtics .025
set xlabel "Richardson number"
set ylabel offset character -2,0
set ylabel "Reynolds number"
set format y "10^{%1.0T}"
set term aqua
set pm3d
set pm3d corners2color min
set palette  defined (-1 "gray",1 "white")
set palette  maxcolors 2
unset key
splot sgn(g(x/.25,y)) lt -2 lw 2
set term postscript enhanced eps
set output "allowedRi.eps"
replot
set term aqua
