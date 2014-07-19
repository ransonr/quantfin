RMSError <- function(mdl, train, test, y.val) {
  # Calculates the root mean squared error for a given model on training data and test data
  #
  # Args:
  #   mdl:    Model object to be used in prediction
  #   train:  Data frame used for training the model
  #   test:   Data frame with same columns as train used for verifying the model
  #   y.val:  Data frame with single column and nrow(y.val)=nrow(train)+nrow(test)
  #
  # Returns:
  #   List of the training RMS error and the testing RMS error
  #
  # TODO:
  #   Error checking
  
  train.yhat <- predict(object = mdl, newdata = train)
  test.yhat <- predict(object = mdl, newdata = test)
  
  train.y <- with(train, get(y.val))
  test.y <- with(test, get(y.val))
  
  train.err <- sqrt(mean((train.yhat - train.y) ^ 2))
  test.err <- sqrt(mean((test.yhat - test.y) ^ 2))
  
  c(train.err = train.err, test.err = test.err)
}