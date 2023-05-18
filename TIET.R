#libraries
library(dplyr)
library(leaps)
library(glmnet)
library(tree)
library(randomForest)
library(gbm)
library(BART)

#Call data
SAPS.Irish.Data <- read.csv("~/Desktop/Irish/SAPS Irish Data.csv")

#Separate the target variable from the data set
gaeilgeorí = as.data.frame(SAPS.Irish.Data[,"All.Irish.speakers...Males"]
                           + SAPS.Irish.Data[,"All.Irish.speakers...Females"])
colnames(gaeilgeorí) = c("gaeilgeorí")


#Separate out the pop. of each obs.
population = apply(SAPS.Irish.Data[, 3:ncol(SAPS.Irish.Data)], 1, max)

#take out answers to polls, any data that is a total of male/female, and other
#language based data to avoid multicollinearity
rm.terms = c("Very.well",	"Well",	"Not.well",	"Not.at.all", "Not.stated",
             "Total",	"Yes",	"No",	"Maybe", "Not.stated",	"Males", "Females","Male",
             "Female", "speaks", "GEOGID", "GUID", 
             "All.Irish.speakers...Females", "All.Irish.speakers...Males",
             "education.system", "Don't.Know", "hour", ":")

for (i in rm.terms){
  SAPS.Irish.Data = select(SAPS.Irish.Data, -contains(i))
}

#get per capita numbers
SAPS.Irish.Data.pc = cbind((SAPS.Irish.Data/population), (gaeilgeorí/population))

#check to makre sure no values in df are over 1
any(SAPS.Irish.Data.pc > 1)

#create training and testing variables
set.seed(32)
is.true <- sample(c(TRUE, FALSE), nrow(SAPS.Irish.Data.pc),
                  replace=TRUE, prob=c(0.75,0.25))
train  <- SAPS.Irish.Data.pc[is.true, ]
test   <- SAPS.Irish.Data.pc[!is.true, ]

#Create grid, model matrix etc.
lambda.grid = as.numeric(10^seq(10, -2, length = 100))
x.train = model.matrix(gaeilgeorí ~.,train)
y.train = train$gaeilgeorí

x.test = model.matrix(gaeilgeorí~., test)
y.test = test$gaeilgeorí

#fit the model and cv to find best lambda
ridge.mod = glmnet(x.train, y.train, alpha = 0, lambda = lambda.grid)
ridge.cv = cv.glmnet(x.train, y.train, alpha = 0)
best.lam.r = ridge.cv$lambda.min
one.se.lam.r = ridge.cv$lambda.1se

plot(ridge.mod, xvar = "lambda")
abline(v = log(ridge.cv$lambda.1se), col = 'red', lty = 'dashed')
abline(v = log(ridge.cv$lambda.min), col = 'red', lty = 'dashed')
plot(ridge.cv, xvar = "lambda")

best.pred.r  = predict(ridge.mod, s = best.lam.r, newx = x.test)
one.se.pred.r = predict(ridge.mod, s = one.se.lam.r, newx = x.test)

mse.best.r = mean((best.pred.r - y.test)^2)
mse.one.se.r = mean((one.se.lam.r - y.test)^2)

#Try lasso
lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = lambda.grid)
lasso.cv = cv.glmnet(x.train, y.train, alpha = 1)
best.lam.l = lasso.cv$lambda.min
one.se.lam.l = lasso.cv$lambda.1se

plot(lasso.mod, xvar = "lambda")
abline(v = log(lasso.cv$lambda.1se), col = 'red', lty = 'dashed')
abline(v = log(lasso.cv$lambda.min), col = 'red', lty = 'dashed')

best.pred.l  = predict(lasso.mod, s = best.lam.l, newx = x.test)
one.se.pred.l = predict(lasso.mod, s = one.se.lam.l, newx = x.test)

mse.best.l = mean((best.pred.l - y.test)^2)
mse.one.se.l = mean((one.se.pred.l - y.test)^2)

#was double the test error


#Tree methods
#problem 2

#grow a tree fit and get training and test error
tree.fit = tree(gaeilgeorí ~ ., train, 
                control = tree.control(nobs = nrow(train), mindev = 0.01))
summary(tree.fit)
plot(tree.fit)
text(tree.fit)

#train/test errors
tree.pred = predict(tree.fit, newdata = as.data.frame(x.test))
mse.tree= mean((tree.pred - y.test)^2)

#cv tree
tree.fit.cv = cv.tree(tree.fit)
plot(tree.fit.cv$size, tree.fit.cv$dev, type = "b")

#prune tree
tree.fit.prune = prune.tree(tree.fit, best = 7)
summary(tree.fit.prune)
plot(tree.fit.prune)
text(tree.fit.prune)
tree.prune.pred = predict(tree.fit.prune, newdata = as.data.frame(x.test)) 
mse.test.prune = mean((tree.prune.pred - y.test)^2)

#BART
bart.fit = gbart(x.train[,2:ncol(x.train)], y.train, 
                 as.data.frame(x.test)[,2:ncol(as.data.frame(x.test))])
mse.test.bart = mean((bart.fit$yhat.test.mean - y.test)^2)
plot(bart.fit$sigma)

###############
## SELECT VARS ##
##############

SAPS.Irish.Data.pc.new = cbind((gaeilgeorí/population),SAPS.Irish.Data.pc[,1:19])
set.seed(32)
is.true <- sample(c(TRUE, FALSE), nrow(SAPS.Irish.Data.pc.new),
                  replace=TRUE, prob=c(0.75,0.25))
train  <- SAPS.Irish.Data.pc.new[is.true, ]
test   <- SAPS.Irish.Data.pc.new[!is.true, ]

#Create grid, model matrix etc.
lambda.grid = as.numeric(10^seq(10, -2, length = 100))
x.train = model.matrix(train[,1] ~.,train)
y.train = train$gaeilgeorí

x.test = model.matrix(test[,1]~., test)
y.test = test$gaeilgeorí

#fit the model and cv to find best lambda
ridge.mod = glmnet(x.train, y.train, alpha = 0, lambda = lambda.grid)
ridge.cv = cv.glmnet(x.train, y.train, alpha = 0)
best.lam.r = ridge.cv$lambda.min
one.se.lam.r = ridge.cv$lambda.1se

plot(ridge.mod, xvar = "lambda")
abline(v = log(ridge.cv$lambda.1se), col = 'red', lty = 'dashed')
abline(v = log(ridge.cv$lambda.min), col = 'red', lty = 'dashed')
plot(ridge.cv, xvar = "lambda")

best.pred.r  = predict(ridge.mod, s = best.lam.r, newx = x.test)
one.se.pred.r = predict(ridge.mod, s = one.se.lam.r, newx = x.test)

mse.best.r = mean((best.pred.r - y.test)^2)
mse.one.se.r = mean((one.se.lam.r - y.test)^2)

#Try lasso
lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = lambda.grid)
lasso.cv = cv.glmnet(x.train, y.train, alpha = 1)
best.lam.l = lasso.cv$lambda.min
one.se.lam.l = lasso.cv$lambda.1se

plot(lasso.mod, xvar = "lambda")
abline(v = log(lasso.cv$lambda.1se), col = 'red', lty = 'dashed')
abline(v = log(lasso.cv$lambda.min), col = 'red', lty = 'dashed')

best.pred.l  = predict(lasso.mod, s = best.lam.l, newx = x.test)
one.se.pred.l = predict(lasso.mod, s = one.se.lam.l, newx = x.test)

mse.best.l = mean((best.pred.l - y.test)^2)
mse.one.se.l = mean((one.se.pred.l - y.test)^2)

#Tree methods
#problem 2

#grow a tree fit and get training and test error
tree.fit = tree(gaeilgeorí ~ ., train, 
                control = tree.control(nobs = nrow(train), mindev = 0.01))
summary(tree.fit)
plot(tree.fit)
text(tree.fit)

#train/test errors
tree.pred = predict(tree.fit, newdata = as.data.frame(x.test))
mse.tree= mean((tree.pred - y.test)^2)

#cv tree
tree.fit.cv = cv.tree(tree.fit)
plot(tree.fit.cv$size, tree.fit.cv$dev, type = "b")

#prune tree
tree.fit.prune = prune.tree(tree.fit, best = 7)
summary(tree.fit.prune)
plot(tree.fit.prune)
text(tree.fit.prune)
tree.prune.pred = predict(tree.fit.prune, newdata = as.data.frame(x.test)) 
mse.test.prune = mean((tree.prune.pred - y.test)^2)

#BART
bart.fit = gbart(x.train[,2:ncol(x.train)], y.train, 
                 as.data.frame(x.test)[,2:ncol(as.data.frame(x.test))])
mse.test.bart = mean((bart.fit$yhat.test.mean - y.test)^2)
plot(bart.fit$sigma)
