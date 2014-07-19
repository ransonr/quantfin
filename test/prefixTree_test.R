require(quantmod)

getSymbols("SPY")

returns <- na.omit(Delt(Ad(SPY)))

chains <- PrefixTree(returns, max.tree.depth = 5)