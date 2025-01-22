

a = 1
b = 1
c = 0.1
p = 0.2
beta = 0.2

Nn(x,y)=(b*x + c*y)/(a + beta * y)
Nx(x,y)=b*x/(a*p)
Ny(x,y)=c*y/(a*(1-p) + beta*y)

splot [0:10][0:10][0:10] Nn(x,y),Nx(x,y),Ny(x,y)





