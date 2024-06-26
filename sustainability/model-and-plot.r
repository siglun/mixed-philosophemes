#!/usr/bin/Rscript

library("deSolve")

## ==============================================================================
## Numerical solution of a far too complex model, a
## system of ordinary differential equations
## ==============================================================================

source("./parameters.r")

cat(parameters)

state <- c(Av = Atotal - 1,
           Aa = 1,
           Ai = 0.0,
           Aw = 0.0,
           Rv = Rtotal,
           Rm = 0.0,
           Rw = 0.0,
           x  = 10,
	   nv = 50,	
           yv = 50,
           na = 50,
           ya = 0.5,
           ni = 5,
           yi = 0.5)

## the ODE system

worldmodel <- function(t, state, parameters) {
    with( as.list(c(state, parameters) ), {

# rates of changes

#
# Areas
#

# virgin land

        dAv <- -( am * ni + aa * na ) * Av + awv * Aw

# agricultural land

        dAa <- aa * na * Av - aw * Aa

# industrial land

        dAi <- am * ni * Av - ai * Ai

# land that are no longer used which will be recycled

        dAw <- aw * Aa + ai * Ai - awv * Aw

# resource dynamics
# Virgin resource which is mined by the industrial population

        dRv <- -krm * (RT - Rm - Rw) * ni

# material in use
        dRm <- krm * (RT - Rm - Rw) * ni - krw * Rm  + kwm * ni * Rw

# waste resource
        dRw <- krw * Rm - kwm * ni *Rw

# coal or energy. not implemented yet
#        dCf <- 1

# hunter & gatherer
# these utilize a natural resource with population dynamics

        dx  <- rx*x*(1-x/Av/Kp ) - kxn*x*nv

# adults and youth, respectively

        dnv <- (betavv*yv + betaav*ya + betaiv*yi - mad)*nv
        dyv <- rv*kxn*x*nv               - (betavv*nv + betava*na + betavi*ni + mjuv)*yv

# farming population

        dna <- (betava*yv + betaaa*ya + betaia*yi  - mad)*na
        dya <- ra*na*(1-(na+ni)/Aa/(Kc+kmc*Rm)) - (betaav*nv + betaaa*na + betaai*ni + mjuv)*ya

# industrial population

        dni <- (betavi*yv + betaai*ya + betaii*yi - mad)*ni                
        dyi <- ri*ni*(1-(na+ni)/Aa/(Kc+kmc*Rm)) - (betaiv*nv + betaia*na + betaii*ni + mjuv)*yi




# return the rate of change

        list(c(dAv, dAa, dAi, dAw, dRv, dRm, dRw, dx, dnv, dyv, dna, dya, dni, dyi))

    })

}

step <- 1
time <- seq(0, 1000, by = step)



# method = "ode45",

table <- lsoda(y = state, times = time,  func = worldmodel, parms = parameters)
out <- as.data.frame(table)

print(out, max.levels=10)


write.table(out, file = "model_data.txt", sep = "\t")

pdf("time_series.pdf")

matplot(out[,"time"],out[2:5], xlab = "Time ", type="l", ylab = "Numbers",col="black")
matplot(out[,"time"],out[6:8], xlab = "Time ", type="l", ylab = "Numbers",col="black")
matplot(out[,"time"],out[9:11], xlab = "Time ", type="l", ylab = "Numbers",col="black")
matplot(out[,"time"],out[12:13], xlab = "Time ", type="l", ylab = "Numbers",col="black")
matplot(out[,"time"],out[14:15], xlab = "Time ", type="l", ylab = "Numbers",col="black")

plot(table)

# legend("topright", inset=0, legend=c("X (susceptible)", "Y (infected)", "Z (immune)"),  lty=1:3,  col="black",  bty = "n", horiz=FALSE)
# mtext(outer = TRUE, side = 3, "SIR Model", cex = 1.5)
