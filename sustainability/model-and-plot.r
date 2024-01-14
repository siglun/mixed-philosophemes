#!/usr/bin/Rscript

library(deSolve)

## ==============================================================================
## Numerical solution of a far too complex model, a
## system of ordinary differential equations
## ==============================================================================
Rtotal <- 10
Atotal <- 100
parameters <- c(AT = Atotal,
                RT = Rtotal,
                am = 0.01,
                aa = 0.011,
                awv = 0.01,
                aw = 0.001,
                ai = 0.001,
                
                krm = 0.01,
                krw = 0.01,
                kxn = 0.01,
                kwm = 0,05,
                
                betavv = 0.1,
                betaav = 0.08,
                betaiv = 0.08,

                betava = 0.08,
                betaaa = 0.1,
                betaia = 0.08,

                betavi = 0.08,
                betaai = 0.08,
                betaii = 0.1,
                
                mad = 0.05,
                ra  = 0.2,
                rv = 0.3,
                rx  = 1.1,
                KP = 1000,
                kxn  = 0.01,
                mjuv = 0.06,
                ra  = 0.01,
                Kc =100,
                ri = 0.1)

state <- c(Av = Atotal - 1,
           Aa = 1,
           Ai = 0,
           Aw = 0,
           Rv = Rtotal,
           Rm = 0,
           Rw = 0,
           nv = 1,
           x  = 10,
           yv = 1,
           na = 1,
           ya = 1,
           ni = 1,
           yi = 1)

## the ODE system

worldmodel <- function(t, state, parameters) {
    with(as.list(c(state, parameters)),{

# rate of change

        dAv <- - ( am * ni + aa * na ) * (AT - Aa - Ai - Aw) + awv * Aw
        dAa <- aa * na * (AT - Aa - Ai - Aw) - aw * Aa
        dAi <- am * ni * (AT - Aa - Ai - Aw) - ai * Ai
        dAw <- aw * Aa + ai * Ai - awv * Aw
        
        dRv <- -krm * Rv * ni       
        dRm <- krm * Rv * ni - krw * Rm  + kwm * Rw
        dRw <- krw * Rm - kwm * Rw

#        dCf <- 1
        dnv <- (betavv*yv + betaav*ya + betaiv*yi - mad)*nv
        dx  <- rx*x*(1-x/((Av)*KP)) - kxn*x*nv
        dyv <- rv*kxn*x*nv               - (betavv*nv + betava*na + betavi*ni + mjuv)*yv
        
        dna <- (betava*yv + betaaa*ya + betaia*yi  - mad)*na
        dya <- ra*na*(1-(na+ni)/(Aa*Kc)) - (betaav*nv + betaaa*na + betaai*ni + mjuv)*ya
        
        dni <- (betavi*yv + betaai*ya + betaii*yi - mad)*ni                
        dyi <- ri*ni*(1-(na+ni)/(Aa*Kc)) - (betaiv*nv + betaia*na + betaii*ni + mjuv)*yi




# return the rate of change

        list(c(dAv, dAa, dAi, dAw, dRv, dRm, dRw, dnv, dx, dyv, dna, dya, dni, dyi))

    })

}

time <- seq(0, 500, by = 2.0)

out <- ode(y = state, times = time, func = worldmodel, parms = parameters)

head(out)

plot(out)


# matplot(out[,"time"],out, xlab = "Time ", type="l", ylab = "Numbers",col="black")

# legend("topright", inset=0, legend=c("X (susceptible)", "Y (infected)", "Z (immune)"),  lty=1:3,  col="black",  bty = "n", horiz=FALSE)
# mtext(outer = TRUE, side = 3, "SIR Model", cex = 1.5)
