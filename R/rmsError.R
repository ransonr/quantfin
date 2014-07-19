rmsError <- function(mdl, train, test, yval) {
  # Calculates the root mean squared error for a given model on training data and test data
  #
  # Args:
  #   mdl:    Model object to be used in prediction
  #   train:  Data frame used for training the model
  #   test:   Data frame with same columns as train used for verifying the model
  #   yval:   Data frame with single column and nrow(yval)=nrow(train)+nrow(test)
  #
  # Returns:
  #   List of the training RMS error and the testing RMS error
  
  train_yhat <- predict(object=mdl, newdata=train)
  test_yhat <- predict(object=mdl, newdata=test)
  
  train_y <- with(train, get(yval))
  test_y <- with(test, get(yval))
  
  train_err <- sqrt(mean((train_yhat - train_y) ^ 2))
  test_err <- sqrt(mean((test_yhat - test_y) ^ 2))
  
  c(train_err=train_err, test_err=test_err)
}