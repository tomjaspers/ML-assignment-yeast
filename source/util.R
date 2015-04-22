# library(plyr)
library("pROC") # Area under ROC curve


CalculateMisclassificationRate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  return(signif(1-num1/denom1, digits = 5))
}

KFoldCv <- function(data, features, trainer, predictor, k=5, shuffle=FALSE){
  accuracies <- c()
  aucs <- c()
  foldsize <- floor(nrow(data) / k)
  if(shuffle){
    data <- data[sample(nrow(data)),]
  }
  for(i in 1:k){
    # Partition the data (9 parts training, 1 part validation)
    begin <- (i-1)*foldsize + 1
    end   <- begin+foldsize-1
    indices <- c(begin:end)
    set.training   <- data[-indices, ]
    set.validation <- data[indices, ]
    
    # Train and test model
    model <- trainer(set.training, set.validation, features)
    preds <- predictor(model, set.validation)
    
    # Transform prediction and true value to numeric
    y <- as.numeric(factor(set.validation$location, levels=levels(set.validation$location)))
    preds <- as.numeric(factor(preds, levels=levels(set.validation$location)))
    
    # Calculate performance
    confusion.matrix <- table(True=y, Prediction=preds)
    accuracies[i]  <- 1 - CalculateMisclassificationRate(confusion.matrix)
    aucs[i] <- multiclass.roc(y,  preds)$auc
  }
  performance = data.frame("accuracies" = accuracies,
                           "aucs" = aucs)
  return(performance)
}

CalculatePerformance <- function(set.validation, pred.values){
  true.values <- as.numeric(factor(set.validation$location, levels=levels(set.validation$location)))
  pred.values <- as.numeric(factor(pred.values, levels=levels(set.validation$location)))
  
  accuracy  <- 1 - CalculateMisclassificationRate(table(True=true.values, Prediction=pred.values))
  auc <- multiclass.roc(true.values,  pred.values)$auc
  
  return (data.frame("accuracy"=accuracy, "auc"=auc))
  
}
