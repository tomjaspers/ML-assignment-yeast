library("randomForest") # Random Forest

# Define the range of values for K that we wish to examine
ls = c(1)
successRates = data.frame()

for(counter in 1:length(ls)){
  l = ls[counter]
  validationSuccessRates = c()
  for(i in 1:folds){
    # Partition the data (9 parts training, 1 part validation)
    begin = (i-1)*foldsize + 1
    end = begin+foldsize-1
    indices = c(begin:end)
    # Non discrete
    set.training = dta.training[-indices, ]
    set.validation = dta.training[indices, ]
    
    # Make a model
    model = randomForest(location ~ ., 
                        data = set.training,
                        ntree=500,
                        importance=TRUE,
                        proximity=TRUE)
    
    # Make a prediction
    prediction  = predict(model, set.validation[,-9], type="class")
    
    validationSuccessRates[i] = 1 - CalculateMisclassificationRate(table(True=set.validation$location, Prediction=prediction))
  }
  successRates[counter, "l"] = l
  successRates[counter, "avg"] = mean(validationSuccessRates)
  successRates[counter, "std"] = sd(validationSuccessRates)
  
}

# Plot the results of the success rates 
# // TODO: add std error bars?
plot(successRates$l, successRates$avg, type = "b", 
     xlab = "Value controlling Laplace smoothing", ylab = "Avg. success rate",
     main = "Average success rates for different laplace smoothing values")

# Best smoothing factor for NaiveBayes
cat(sprintf("Best k-value: %d with success rate of: %f", which.max(successRates$avg), max(successRates$avg)))

