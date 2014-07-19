RegressMultiple <- function(y, x) {
  # Regresses each column of y on all columns of x
  #
  # Args:
  #   y:  Data frame with 1 or more columns of endogenous variables
  #   x:  Data frame 1 or more columns of exogenous variables that explain columns of y
  #
  # Returns:
  #   List of the lm models
  #
  # TODO:
  #   Error checking
  
  # Validate inputs
  if (!is.data.frame(y)) {
    stop("y must be a data frame")
  } else if (!is.data.frame(x)) {
    stop("x must be a data frame")
  } else if (nrow(y) != nrow(x)) {
    stop("x and y must have the same number of rows")
  }
  
  result <- lapply(y, function(z) { lm(z ~ ., x) } )
}