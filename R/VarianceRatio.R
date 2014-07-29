VarianceRatio <- function(x, max.period = 10) {
  # Calculates the variance ratio of the log returns of x
  #
  # Args:
  #   x:          Numeric vector of prices
  #   max.period: Number of periods to use for the model-building phase
  #
  # Returns:
  #   Vector of variance ratios from aggregation period of 2 up to max.period
  #
  # TODO:
  #   Error checking
  
  # Validate inputs
  
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  x <- as.vector(x) # should use a try/catch
  
  result <- c()
  
  var.1 <- var(log(tail(x, -1) / head(x, -1)))
  
  for (i in 2:max.period) {
    result <- c(result, var(log(tail(x, -i) / head(x, -i))) / (i * var.1))
  }
  
  return (result)
}