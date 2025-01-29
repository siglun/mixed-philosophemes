#!/usr/bin/Rscript

library("deSolve")

## =============================================================================
## Numerical solution of a the coffee machine model.
## A system of ordinary differential equations
## =============================================================================

source("./parameters.r")

cat(parameters)

state <- c(n = 1, x = 0, y = 0)

## the ODE system

worldmodel <- function(t, state, parameters) {
    with( as.list(c(state, parameters) ), {

# rates of changes

#  Customers

   dn  <- -a * n + b * y  + c * x - beta * n * x

#  Those who hate the coffee

   dx <- a * (1-p) * n - c * x  + beta * n * x

#  Those who like the coffee

   dy <- a * p * n - b * y 

# return the rate of change

        list(c(dn, dx, dy))

    })

}

step <- 0.1
time <- seq(0, 20, by = step)

# method = "ode45",

table <- lsoda(y = state, times = time,  func = worldmodel, parms = parameters)

out <- as.data.frame(table)

# print(out, max.levels=10)


write.table(out, file = "shit.text", sep = "\t")

pdf("time_series.pdf")

matplot(out[,"time"],out[2:4], xlab = "Time ", type="l", ylab = "n, x and y",col="black")

# plot(table)

