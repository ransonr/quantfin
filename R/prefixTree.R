PrefixTree <- function(x, max.tree.depth=3) {
  # Discretizes x into up/down values and checks if certain patterns have predictive power
  #
  # Args:
  #   x:              1-dimensional numeric data series
  #   max.tree.depth: max pattern length to analyze
  #
  # Returns:
  #   List of pattern statistics (result$DDD -> what happens after 3 negative observations?)
  #
  # Call:
  #   PrefixTree(rnorm(100))
  #
  # TODO:
  #   Validate inputs; error handling
  
  discrete <- ifelse(x > 0, "U", "D")
  
  result <- list()
  
  for (i in (max.tree.depth+1):length(discrete)) {
    for (j in 1:max.tree.depth) {
      pattern <- paste(discrete[(i - j):(i - 1)], sep = "", collapse = "")
      
      if (is.null(result[[pattern]])) {
        result[[pattern]]$count <- 0
        result[[pattern]]$up <- 0
      }
      
      result[[pattern]]$count <- result[[pattern]]$count + 1
      
      if (discrete[i] == "U") result[[pattern]]$up = result[[pattern]]$up + 1
    }
  }
  
  for (name in names(result)) {
    result[[name]]$up.percent <- result[[name]]$up / result[[name]]$count
    result[[name]]$t.stat <- (result[[name]]$up.percent - 0.5) / sqrt(0.25 / result[[name]]$count)
    result[[name]]$p.value <- binom.test(x = result[[name]]$up, n = result[[name]]$count, alternative = "two.sided")$p.value
  }
  
  return(result)
}