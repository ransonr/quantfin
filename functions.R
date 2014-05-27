# require(quantmod)
# require(party)

prefixTree <- function(x, maxTreeDepth=3) {
  discrete <- ifelse(x > 0, "U", "D")
  
  result <- list()
  
  for (i in (maxTreeDepth+1):length(discrete)) {
    for (j in 1:maxTreeDepth) {
      pattern <- paste(discrete[(i-j):(i-1)], sep="", collapse="")
      
      if (is.null(result[[pattern]])) {
        result[[pattern]]$count <- 0
        result[[pattern]]$up <- 0
      }
      
      result[[pattern]]$count <- result[[pattern]]$count + 1
      
      if (discrete[i] == "U") result[[pattern]]$up = result[[pattern]]$up + 1
    }
  }
  
  #result$up.percent <- lapply(result, function(x) { x$up / x$count })
  
  for (name in names(result)) {
    result[[name]]$up.percent <- result[[name]]$up / result[[name]]$count
    result[[name]]$t.stat <- (result[[name]]$up.percent - 0.5) / sqrt(0.25 / result[[name]]$count)
    result[[name]]$p.value <- pt(result[[name]]$t.stat, df=result[[name]]$count-1, lower.tail=FALSE)
  }
  
  return(result)
}