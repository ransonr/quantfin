RollingPCAPrediction <- function(x, lookback = 100, variance.explained = 0.95) {
  # Calculates the principal components of the data over the lookback period and
  # regresses all columns of x on the top principal components that explain the
  # desired amount of variance in the data. The in-sample rotation matrix is used
  # to calculate the 1-period ahead out-of-sample principal components and these,
  # along with the regression models, are used to predict the 1-period ahead 
  # values of x. This ensures that all forecasts use only out-of-sample data and
  # the prediction models are updated every period with the most recent data.
  #
  # Args:
  #   x:                  Data frame of data to be used for PCA and prediction
  #   lookback:           Number of periods to use for the model-building phase
  #   variance.explained: Minimum variance explained by chosen principal components
  #
  # Returns:
  #   Predictions:  Data frame of predictions for each series and prediction step
  #   Actual:       Data frame of values for each series and prediction step
  #   Errors:       Data frame of errors for each series and prediction step
  #   MSE:          Mean squared error for each series
  #
  # TODO:
  #   Error checking
  
  # Validate inputs
  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  } else if (lookback < 2) {
    stop("lookback must be greater than or equal to 2")
  } else if (lookback > nrow(x) - 1) {
    stop("lookback must be less than or equal to nrow(x) - 1")
  } else if (variance.explained <= 0 | variance.explained >= 1) {
    stop("variance.explained must be between 0 and 1")
  }
  
  prediction.error <- data.frame()
  factors.needed <- data.frame()
  
  # Yes, I know loops are bad in R -> maybe I'll optimize this someday
  for (i in (lookback + 1):nrow(x)) {
    temp <- x[(i - lookback):(i - 1), ]
    
    # Run pca and regress all columns of x on top principal components
    pca <- prcomp(temp)
    num.factors <- match(TRUE, cumsum(pca$sdev ^ 2) / sum(pca$sdev ^ 2) > variance.explained)
    betas <- apply(temp, 2, function(z) { summary(lm(z ~ pca$x[, 1:num.factors]))$coef[, 1] })
    
    # Calculate current principal component values
    current.values.scaled <- x[i, ] - pca$center
    prin.comps <- as.matrix(current.values.scaled) %*% as.matrix(pca$rotation[, 1:num.factors])
    
    predictions <- (c(1, prin.comps) %*% betas) #+ pca$center
    prediction.error <- rbind(prediction.error, predictions - x[i, ])
    factors.needed <- rbind(factors.needed, num.factors)
  }
  
  rownames(factors.needed) <- rownames(prediction.error)
  
  list(Predictions = x[(lookback + 1):nrow(x), ] + prediction.error, 
       Actual = x[(lookback + 1):nrow(x), ], 
       PredictionError = prediction.error,
       FactorsNeeded = factors.needed)
}