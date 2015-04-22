LoadData <- function(scale=TRUE, discretize=FALSE, randomize=FALSE){ 
  library("discretization")
  # Read the data
  dta <- read.table("yeast.data",header=T) # First row acts as a header, containing the column names
  dta <- dta[,-1] # First column contains protein names and is irrelevant, and can thus be removed
  dta <- unique(dta) # Remove duplicates
  
  # Split the dataset into test set, and training set (test set >= 300 instances)
  if(!randomize){
    # Manually set seed so we always get same result
    set.seed(314) 
  }
  indices = sample(seq_len(nrow(dta)), size = floor(0.25 * nrow(dta)))
  dta.testing <- dta[indices, ]
  dta.training <- dta[-indices, ]
  rm(indices)
  if(scale){
    # Scale the data to [0 1] (seperately, else data snooping !)
    dta.training[, 1:8] = apply(dta.training[, 1:8], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
    dta.testing[, 1:8] = apply(dta.testing[, 1:8], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
  }
  if(discretize){
    # Make discreitzed version of the data based on Chi2 algorithm
    dta.training = chi2(dta.training)$Disc.data
    dta.testing  = chi2(dta.testing)$Disc.data
  }
  return(list(dta.training, dta.testing))
}
