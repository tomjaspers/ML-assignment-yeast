library("e1071") # Naive Bayes, function "naiveBayes"

run_nb <- function(data, features, ls){
  successRates <- data.frame()
  pb <- txtProgressBar(min = 1, max = length(ls), style = 3)
  for(counter in 1:length(ls)){
    l = ls[counter]
    performance <- KFoldCv(dta.training,
                           features,
                           function(set.training, set.validation, features){
                             model <- naiveBayes(x = set.training[,features],
                                                 y = set.training[,'location'],
                                                 laplace = l
                             )
                             return(model)
                           },
                           function(model, set.validation){
                             return(predict(model, set.validation, type="class"))
                           })
    successRates[counter, "l"] <- l
    successRates[counter, "avg_accuracy"] <- mean(performance$accuracies)
    successRates[counter, "std_accuracy"] <- sd(performance$accuracies)
    successRates[counter, "avg_auc"] <- mean(performance$auc)
    successRates[counter, "std_auc"] <- sd(performance$auc)
    
    setTxtProgressBar(pb, l)
  }
  close(pb)
  
  # Plot the results of the success rates 
  plot(successRates$l, successRates$avg_auc, type = "b", 
       xlab = "Value controlling Laplace smoothing", ylab = "Avg. AUC",
       main = "Average AUC for different laplace smoothing values")
  
  # Best smoothing factor for NaiveBayes
  which.max(successRates$avg_auc)
  
  return(successRates)
}
