#!/usr/bin/Rscript


source("parameters.r")

nv <- function(x,parameters) {
   with( as.list(c(parameters) ), {
   	 nv = mad * (betavv + mjuv) / (betavv * rv * kxn * x)
   })
}

yv <- function(x,parameters) {
   with( as.list(c(parameters) ), {
   	 yv = rx*(1-x/(AT*Kp))/kxn
   })
}



xr <- seq(0.1,15000, by=1.0)
df <- data.frame(x = xr,
   nv = c(nv(xr,parameters)),
   yv = c(yv(xr,parameters))
   )

print(df)

pdf("phase_plane.pdf")
matplot(df[,"x"],df[2:3], xlab = "x", type="l", ylab = "nv",col="black")
