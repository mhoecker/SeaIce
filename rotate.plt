dt = .02
theta = theta+dt
theta = theta-floor(theta)
phi = phi+dt*dt
phi = phi - floor(phi)
a = 180*phi
b = 360*theta
set view a,b
a = floor(a)
b = floor(b) 
set title ''.a.','.b 
replot
pause 1/24.
if((theta>0)+(phi>0)) reread;