RegressMultipleSummary <- function(lm.list) {
  # Returns a data frame of statistics for each lm object
  #
  # Args:
  #   lm.list:  List of lm objects
  #
  # Returns:
  #   Data frame with summary statistics from each lm object
  #
  # TODO:
  #   Error checking
  
  # Validate inputs
  if (!is.list(lm.list)) {
    stop("lm.list must be a list of lm objects")
  }
  
  df <- data.frame(Y=names(lm.list))
  
  df$adj.r.squared <- sapply(lm.list, function(mdl) { summary(mdl)$adj.r.squared })
  df$p.value <- sapply(lm.list, function(mdl) {
      f <- summary(mdl)$fstat; 
      pf(f[1], f[2], f[3], lower.tail = FALSE)
    })
  df$std.error <- sapply(lm.list, function(mdl) { summary(mdl)$sigma })
  
  return(df)
}