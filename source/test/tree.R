library("tree") # Decision trees, function "tree"
library("C50")   # C5.0 decission tree, function "C5.0"



### Decision tree
#model <- C5.0()
model = C5.0(x = dta.training[, features],
             y = dta.training$location,
             trials = 5, # if earlyStopping = T, this doesnt go higher than 5
             rules = F,
             control = C5.0Control(
               subset = T,
               winnow = F,
               minCases = 5,
               fuzzyThreshold = T,
               earlyStopping = T,
             ))
  
