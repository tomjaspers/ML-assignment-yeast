library("class") # k Nearest Neighbors, function "knn"
library("plyr")

run_knn <- function(data, features, ks){
  successRates = data.frame()
  pb <- txtProgressBar(min = 1, max = length(ks), style = 3)
  for(counter in 1:length(ks)){
    k <- ks[counter]
    performance <- KFoldCv(data,
                          features,
                          function(set.training, set.validation, features){
                            library("class")
                            model <- knn(train = set.training[,features],
                                         test  = set.validation[,features],
                                         cl = set.training$location,
                                         k = k)
                            return(model)
                          },
                          function(model, set.validation){
                            return(model)
                          },
                          k=5,
                          shuffle=F)
    successRates[counter, "k"] = k
    successRates[counter, "avg_accuracy"] = mean(performance$accuracies)
    successRates[counter, "std_accuracy"] = sd(performance$accuracies)
    successRates[counter, "avg_auc"] = mean(performance$auc)
    successRates[counter, "std_auc"] = sd(performance$auc)
    setTxtProgressBar(pb, counter)
  }
  close(pb)

  # Plot the results of the success rates  // TODO: add std error bars?
  plot(successRates$k, 
       successRates$avg_auc,
       type = "b", 
       xlab = "k-value for KNN", ylab = "Area under ROC curve",
       main = "Average AUC for different k-values")

  
  # Best K value for KNN:
#   cat(sprintf("Best k-value: %d with accuracy of: %f\n", which.max(successRates$avg_accuracy), max(successRates$avg_accuracy)))
  cat(sprintf("Best k-value: %d with AUC of: %f\n", 
              which.max(successRates$avg_auc), max(successRates$avg_auc)))
  
  return(successRates)
}