require(quantmod)

source("GitHub/quantfin/R/RegressMultiple.R")

# Lazy way to get the data we need - model doesn't mean anything
mdl <- specifyModel(Ad(SPY) ~ Ad(IWM) + Ad(HYG) + Ad(TLT) + Ad(VXX))
mdl.data <- modelData(mdl)

outputs <- data.frame(mdl.data$Ad.SPY, mdl.data$Ad.IWM)
inputs <- data.frame(mdl.data$Ad.HYG, mdl.data$Ad.TLT, mdl.data$Ad.VXX)

res <- RegressMultiple(outputs, inputs)

print(summary(res[[1]]))