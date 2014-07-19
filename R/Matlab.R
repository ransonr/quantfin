# Variety of functions to imitate useful MATLAB functions

tic <- function(gc.first = TRUE, type=c("elapsed", "user.self", "sys.self")) {
  # Begins a timer; used in conjunction with toc()
  #
  # Call:
  #   tic(); factorial(21); toc()
  #
  # Returns:
  #   N/A
  
  type <- match.arg(type)
  assign(".type", type, envir = baseenv())
  if (gc.first) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir = baseenv())
  invisible(tic)
}

toc <- function() {
  # Ends a timer and prints elapsed time; used in conjunction with tic()
  #
  # Call:
  #   tic(); factorial(21); toc()
  #
  # Returns:
  #   Elapsed time in seconds
  
  type <- get(".type", envir = baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir = baseenv())
  print(toc - tic)
  invisible(toc)
}