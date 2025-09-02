

a = 1
b = 1
c = 0.1
p = 0.75
beta = 0.2

N0 = 1

yx(x) = N0 - x - (c*x)/(a*(1-p) + beta*x)
yy(x) = a*p*(N0-x)/(a*p+b)

set xlab 'x'
set ylab 'y'

plot [0:1][0:1] yx(x),yy(x)
