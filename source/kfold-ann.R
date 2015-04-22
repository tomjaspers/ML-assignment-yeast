library("nnet")

run_ann<- function(data, features, sizes){
  successRates <- data.frame()
  pb <- txtProgressBar(min = 1, max = length(sizes), style = 3)
  for(counter in 1:length(sizes)){
    size = sizes[counter]
    performance <- KFoldCv(dta.training,
                           features,
                           function(set.training, set.validation, features){
                             formula <- as.formula(paste("location~",paste(features,collapse="+")))
                             model <- nnet(formula = formula, 
                                           data = set.training,
                                           size = size,
                                           trace = FALSE
                             )
                             return(model)
                           },
                           function(model, set.validation){
                             return(predict(model, set.validation[,-9], type="class"))
                           })
    successRates[counter, "size"] <- size
    successRates[counter, "avg_accuracy"] <- mean(performance$accuracies)
    successRates[counter, "std_accuracy"] <- sd(performance$accuracies)
    successRates[counter, "avg_auc"] <- mean(performance$auc)
    successRates[counter, "std_auc"] <- sd(performance$auc)
    
    setTxtProgressBar(pb, size)
  }
  close(pb)
  
  # Plot the results of the success rates 
  plot(successRates$size, successRates$avg_auc, type = "b", 
       xlab = "Number of units in hidden layer", ylab = "Avg. AUC",
       main = "Average AUC for different number of units in hidden layer")
  
  cat(sprintf("Best size-value: %d with AUC of: %f\n", 
              which.max(successRates$avg_auc), max(successRates$avg_auc)))
  
  
  return(successRates)
}