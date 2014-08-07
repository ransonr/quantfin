library(quantmod)

source("GitHub/quantfin/R/VarianceRatio.R")

getSymbols(c("SPY", "TLT"))

vr.spy <- VarianceRatio(Ad(SPY), max.period = 100)
vr.tlt <- VarianceRatio(Ad(TLT), max.period = 100)

par(mfrow = c(2, 1))

plot(vr.spy, main = "SPY Variance Ratio")
plot(vr.tlt, main = "TLT Variance Ratio")