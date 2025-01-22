

a = 1
b = 1
c = 0.1
p = 0.75
beta = 0.2


set hidden3d


Nn(x,y)=(b*x + c*y)/(a + beta * y)
Nx(x,y)=b*x/(a*p)
Ny(x,y)=c*y/(a*(1-p) + beta*y)

splot [0:1][0:1][0:1] Nn(x,y) , Nx(x,y) , Ny(x,y) 

# splot [0:1][0:1][0:1] Nn(x,y) w pm3d , Nx(x,y) w pm3d ,Ny(x,y) w pm3d





