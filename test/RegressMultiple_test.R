require(quantmod)

source("GitHub/quantfin/R/RegressMultiple.R")

# Lazy way to get the data we need - model doesn't mean anything
mdl <- specifyModel(Delt(Ad(SPY)) ~ Delt(Ad(IWM)) + Delt(Ad(HYG)) + Delt(Ad(TLT)) + Delt(Ad(VXX)))
mdl.data <- modelData(mdl)

outputs <- data.frame(mdl.data$Delt.Ad.SPY, mdl.data$Delt.Ad.IWM)
inputs <- data.frame(mdl.data$Delt.Ad.HYG, mdl.data$Delt.Ad.TLT, mdl.data$Delt.Ad.VXX)

res <- RegressMultiple(outputs, inputs)

print(summary(res[[1]]))