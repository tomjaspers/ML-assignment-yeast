library("FSelector") 
library("CORElearn")
library("tree") # Decision trees, function "tree"
library("class") # k Nearest Neighbors, function "knn"
library("nnet") # artificial neural network, function "nnet"
library("C50")   # C5.0 decission tree, function "C5.0"
# Load utility libraries
source("dta.R") 
source("util.R")
# Load our learning algorithms wrapped with kfold cv
source("kfold-knn.R")
source("kfold-ann.R")
#source("kfold-bayes.R") 


# Folder to save plots
plots.dir <- "/Users/tjs/Desktop/results/" # make sure to end with a slash

## Load the data
dta.list <- LoadData(scale=T, discretize=F, randomize=F)
dta.training <- dta.list[[1]]
# dta.testing <- dta.list[[2]] # Comment since we don't touch this till we are finished
all.classes <- levels(dta.training$location)
full.features <- colnames(dta.training)[-9]



## Try and evaluate the Decision Tree (using full featureset)

# Build a simple tree model
tree.model.simple <- tree(location~., dta.training)
summary(tree.model.simple)
plot(tree.model.simple, type="u")
text(tree.model.simple)
dev.copy2pdf(file = paste0(plots.dir, "simple tree.pdf"))


# Build a more complex tree model, allowing overfitting to happen
tree.model.complex <- tree(location~., dta.training, control=tree.control(nobs=nrow(dta.training), 
                                                                          mincut=2, minsize=5, mindev=0.0025))
summary(tree.model.complex)

# Prune the more complex tree model
tree.model.complex.cv <- cv.tree(tree.model.complex, FUN=prune.tree, K=5)
plot(tree.model.complex.cv)
dev.copy2pdf(file = paste0(plots.dir, "complex tree cv.pdf"))
best.tree.size <- tree.model.complex.cv$size[which.min(tree.model.complex.cv$dev)]
cat(sprintf("Lowest dev (%g) at tree size %g \n", 
            min(tree.model.complex.cv$dev),
            best.tree.size))
tree.model.pruned <- prune.tree(tree.model.complex,best=best.tree.size)
summary(tree.model.pruned)
plot(tree.model.pruned, type="u")
text(tree.model.pruned)
dev.copy2pdf(file = paste0(plots.dir, "pruned complex tree.pdf"))


# We want to try out C5.0 as well
c5tree.model <- C5.0(x = dta.training[, full.features], y = dta.training$location,   
                     rules = F, control = C5.0Control(subset = T, winnow = F, earlyStopping = T))



## Tune KNN and naiveBayes with full feature set

# Run k-fold cv KNN to select best K values
results.knn <- run_knn(dta.training, full.features, ks=c(1:30))
dev.copy2pdf(file = paste0(plots.dir, "knn-full.pdf"))
best.k.features.all <- which.max(results.knn$avg_auc)

# Run k-fold cv ANN to select best Size values
results.ann <- run_ann(dta.training, full.features, sizes=c(1:20))
dev.copy2pdf(file = paste0(plots.dir, "ann-full.pdf"))
best.size.features.all <- which.max(results.ann$avg_auc)


## Apply feature selection
features.by.tree <- length(summary(tree.model.simple)$used)

# Relief-F
relieff.weights <- relief(location ~ ., data = dta.training)
relieff.subset <- cutoff.k(relieff.weights, features.by.tree)
# relieff.subset <- cutoff.biggest.diff(relieff.weights) # alternative
relieff.subset

# Minimum Description Length (MDL)
mdl.weights <- attrEval(location ~., dta.training, estimator="MDL")
mdl.subset <- names(sort(mdl.weights, decreasing=TRUE)[1:features.by.tree])
mdl.subset

## Tune kNN and ANN with features selected via Relief-f
# Run k-fold cv KNN to select best K values
results.knn <- run_knn(dta.training, relieff.subset, ks=c(1:30))
dev.copy2pdf(file = paste0(plots.dir, "knn-relieff.pdf"))
best.k.features.relieff <- which.max(results.knn$avg_auc)

# Run k-fold cv ANN to select best Size values
results.ann <- run_ann(dta.training, relieff.subset, sizes=c(1:20))
dev.copy2pdf(file = paste0(plots.dir, "ann-relieff.pdf"))
best.size.features.relieff <- which.max(results.ann$avg_auc)


## Tune kNN and ANN with features selected via [other]
# Run k-fold cv KNN to select best K values
results.knn <- run_knn(dta.training, mdl.subset, ks=c(1:30))
dev.copy2pdf(file = paste0(plots.dir, "knn-mdl.pdf"))
best.k.features.mdl <- which.max(results.knn$avg_auc)

# Run k-fold cv ANN to select best Size values
results.ann <- run_ann(dta.training, mdl.subset, sizes=c(1:20))
dev.copy2pdf(file = paste0(plots.dir, "ann-mdl.pdf"))
best.size.features.mdl <- which.max(results.ann$avg_auc)




## Train and test on the split data set (held out till paper-writing-day)
dta.testing <- dta.list[[2]] # Comment since we don't touch this till we are finished

# Decision trees: predict
simple.tree.preds <- predict(tree.model.simple, dta.testing[,-9], type="class")
pruned.tree.preds <- predict(tree.model.pruned, dta.testing[,-9], type="class")
c5tree.preds <- predict(c5tree.model, dta.testing[,-9], type="class")

# kNN: train
knn.model.all <- knn(train = dta.training[ ,full.features], test  = dta.testing[, full.features],
                     cl = dta.training$location,  k = best.k.features.all)
knn.model.relieff <- knn(train = dta.training[, relieff.subset], test  = dta.testing[, relieff.subset],
                         cl = dta.training$location, k = best.k.features.relieff)
knn.model.mdl <- knn(train = dta.training[, mdl.subset], test  = dta.testing[, mdl.subset],
                     cl = dta.training$location, k = best.k.features.mdl)
# kNN: 'predict'
knn.preds.all <- knn.model.all
knn.preds.relieff <- knn.model.relieff
knn.preds.mdl <- knn.model.mdl

# ANN: train
ann.model.all <- nnet(formula =  as.formula(paste("location~",paste(full.features,collapse="+"))), 
                      data = dta.training,
                      size = best.size.features.all,
                      trace = FALSE)
ann.model.relieff <- nnet(formula =  as.formula(paste("location~",paste(relieff.subset,collapse="+"))), 
                          data = dta.training,
                          size = best.size.features.relieff,
                          trace = FALSE)
ann.model.mdl <- nnet(formula =  as.formula(paste("location~",paste(mdl.subset,collapse="+"))), 
                      data = dta.training,
                      size = best.size.features.mdl,
                      trace = FALSE)
# ANN: predict
ann.preds.all <- predict(ann.model.all, dta.testing[,-9], type="class")
ann.preds.relieff <- predict(ann.model.relieff, dta.testing[,-9], type="class")
ann.preds.mdl <- predict(ann.model.mdl, dta.testing[,-9], type="class")


# All outputs
CalculatePerformance(dta.testing, simple.tree.preds)
CalculatePerformance(dta.testing, pruned.tree.preds)
CalculatePerformance(dta.testing, c5tree.preds)

CalculatePerformance(dta.testing, knn.model.all)
CalculatePerformance(dta.testing, knn.model.relieff)
CalculatePerformance(dta.testing, knn.model.mdl)

CalculatePerformance(dta.testing, ann.preds.all)
CalculatePerformance(dta.testing, ann.preds.relieff)
CalculatePerformance(dta.testing, ann.preds.mdl)

