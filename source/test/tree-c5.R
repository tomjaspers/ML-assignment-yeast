source("util.R")
source("kfoldcv.R")
library("C50")   # C5.0 decission tree, function "C5.0"

run_c5tree <- function(data, features, cs){
  successRates <- data.frame()
  pb <- txtProgressBar(min = 1, max = length(cs), style = 3)
  for(counter in 1:length(cs)){
    c = cs[counter]
    performance <- KFoldCv(dta.training,
                           features,
                           function(set.training, set.validation, features){
                             C5.0(x = set.training[, features],
                                  y = set.training$location,
                                  trials = 5, # if earlyStopping = T, this doesnt go higher than 5
                                  rules = F,
                                  control = C5.0Control(
                                    subset = T,
                                    winnow = F,
                                    minCases = c,
                                    fuzzyThreshold = T,
                                    earlyStopping = T,
                                  ))
                             return(model)
                           },
                           function(model, set.validation){                           
                             return(predict(model, set.validation[, features]))
                           })
    
    successRates[counter, "c"] <- c
    successRates[counter, "avg_accuracy"] <- mean(performance$accuracies)
    successRates[counter, "std_accuracy"] <- sd(performance$accuracies)
    successRates[counter, "avg_auc"] <- mean(performance$auc)
    successRates[counter, "std_auc"] <- sd(performance$auc)
    
    setTxtProgressBar(pb, c)
  }
  close(pb)
  
  # Plot the results of the success rates 
  plot(successRates$c, successRates$avg_auc, type = "b", 
       xlab = "Value for min cases", ylab = "Avg. AUC",
       main = "Average AUC for different laplace smoothing values")
  
  # Best minCases for c5.0 tree
  which.max(successRates$avg_auc)
  
  return(successRates)
}


