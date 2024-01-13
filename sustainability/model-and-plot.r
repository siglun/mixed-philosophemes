#!/usr/bin/Rscript

require(deSolve)

## ==============================================================================
## Numerical solution of a far too complex model, a
## system of ordinary differential equations
## ==============================================================================

parameters <- c(AT = 100,
                RT = 1,
                am = 0.01,
                aa = 0.01,
                awv = 0.01,
                aw = 0.01,
                ai = 0.01,
                krm = 0.01,
                krw = 0.01,
                kxn = 0.01,
                
                betavv = 0.01,
                betaav = 0.01,
                betaiv = 0.01,

                betava = 0.01,
                betaaa = 0.01,
                betaia = 0.01,

                betavi = 0.01,
                betaai = 0.01,
                betaii = 0.01,
                
                mad = 0.02,
                ra  = 0.01,
                rv = 0.01,
                rx  = 0.01,
                KP = 1000,
                kxn  = 0.01,
                mjuv = 0.05,
                ra  = 0.01,
                Kc =100,
                ri = 0.01)

state <- c(Av = 98,
           Aa = 1,
           Ai = 0,
           Aw = 0,
           Rv = 1,
           Rm = 0,
           Rw = 0,
#           Cf = 1,
           nv = 1,
           yv = 0,
           na = 0,
           ya = 0,
           ni = 0,
           yi = 0,
           x  = 1)

## the ODE system

lorenz <- function(t, state, parameters) {
    with(as.list(c(state, parameters)),{

# rate of change

        dAv <- - ( am * ni + aa * na ) * (AT - Aa - Ai -Aw) + awv * Aw
        dAa <- aa * na * (AT - Aa - Ai) - aw * Aa
        dAi <- am * ni * (AT - Aa - Ai) - ai * Ai
        dAw <- aw * Aa + ai * Ai - awv * Aw        
        dRv <- -krm * (RT - Rm - Rw)*ni        
        dRm <- krm * (RT - Rm - Rw)*ni - krw * Rm
        dRw <- krw * Rm

#        dCf <- 1
        dnv <- (betavv*yv + betaav*ya + betaiv*yi - mad)*nv
        dx  <- rx*x*(1-x/(Av*KP)) - kxn*x*nv
        dyv <- rv*kxn*x*nv               - (betavv*nv + betava*na + betavi*ni + mjuv)*yv
        
        dna <- (betava*yv + betaaa*ya + betaia*yi  - mad)*na
        dya <- ra*na*(1-(na+ni)/(Aa*Kc)) - (betaav*nv + betaaa*na + betaai*ni + mjuv)*ya
        
        dni <- (betavi*yv + betaai*ya + betaii*yi - mad)*ni                
        dyi <- ri*ni*(1-(na+ni)/(Aa*Kc)) - (betaiv*nv + betaia*na + betaii*ni + mjuv)*yi




# return the rate of change

        list(c(dX, dY, dZ))

    })

}

times <- seq(0, 100, by = 0.01)

out <- ode(y = state, times = times, func = lorenz, parms = parameters)

head(out)

par(oma = c(0, 0, 3, 0))
matplot(out[,"time"],out[,2:4], xlab = "Time ", type="l", ylab = "Numbers",col="black")
legend("topright", inset=0, legend=c("X (susceptible)", "Y (infected)", "Z (immune)"),  lty=1:3,  col="black",  bty = "n", horiz=FALSE)
mtext(outer = TRUE, side = 3, "SIR Model", cex = 1.5)
