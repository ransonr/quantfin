source("GitHub/quantfin/R/VarianceRatio.R")

data <- read.csv("GitHub/quantfin/test/TLT_5M.csv", header = TRUE, stringsAsFactors = FALSE)

vr <- VarianceRatio(data$TLT, max.period = 200)

which(vr == min(vr))

plot(vr)

a <- acf(diff(data$TLT), lag.max = 200)
p <- pacf(diff(data$TLT), lag.max = 200)

ar.mdl <- ar(diff(data$TLT), order.max = 80)