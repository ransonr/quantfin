require(quantmod)

getSymbols("SPY")

returns <- na.omit(Delt(Ad(SPY)))

chains <- prefixTree(returns, maxTreeDepth=5)