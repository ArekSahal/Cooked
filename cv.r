#### Cross Validation

cross_validation = function(data, k) {
  
  #### Get our k-splits
  
  n = length(data[,1])
  oda = sample(1:n)
  splits = c()
  
  for (i in 0:(k-1)) {
    splits = cbind(splits, oda[(i*n/k + 1):((i+1)*n/k + 1)])
  }
          ### splits is now a matrix where each col has the index values of the spesific sample
  
  
  #### Perform corss validation
  
  ers = c()
  
  for (i in 1:k) {
    training_data = c()
    test_data = splits[,i]
    
    for (j in 1:k) {
      if (j != i) {
        training_data = c(training_data, splits[,j])
      }
    }
    
    model = lm(formula(data), data=data[training_data,])
    
    preds = predict(model, newdata=data[test_data,])
    
    res = (preds - data[test_data,][1])**2
    
    ers = c(ers , mean(as.matrix(res)))
  }
  plot(ers)
  ers
  
}
