# Clear global workspace
rm( list = ls( envir = globalenv() ), envir = globalenv() )
library('dplyr') # data manipulation
library('ggplot2') # Data Visualization
library('ggthemes') # Data Visualization
library(glmnet);
# start load
getwd();
setwd("/Users/YoshinoKai/Documents/R/Data/titanic");
getwd();
options(warn = -1)
# load train.csv
full <- read.csv('train.csv', stringsAsFactors = F)
#=================
age <- full$Age
n = length(age)
# replace missing value with a random sample from raw data
set.seed(123)
missed = 0
for(i in 1:n){
  if(is.na(age[i])){
    age[i] = as.integer(sample(na.omit(full$Age),1))
    missed=missed+1
  }
}
str(missed)
str(age)
#========================
class <- full$Pclass
missed = 0
for(i in 1:n){
  if(is.na(class[i])){
    class[i] = as.integer(sample(na.omit(full$class),1))
    missed=missed+1
  }
}
combined<-cbind(age,class)
#========================
sex <- full$Sex
for(i in 1:n){
  if(identical(all.equal(sex[i], 'female'), TRUE)){
    sex[i] = 1
  }
  else{
    sex[i] = 0
  }
}
combined<-cbind(combined,sex)
#=============================
sibsp <- full$SibSp
n = length(sibsp)
missed = 0
for(i in 1:n){
  if(is.na(sibsp[i])){
    sibsp[i] = 0
    missed=missed+1
  }
}
combined<-cbind(combined,sibsp)
#=============================
parch <- full$Parch
missed = 0
for(i in 1:n){
  if(is.na(parch[i])){
    parch[i] = 0
    missed=missed+1
  }
}
combined<-cbind(combined,parch)
#=============================
fare <- full$Fare
missed = 0
for(i in 1:n){
  if(is.na(fare[i])){
    fare[i] = 0
    missed=missed+1
  }
}
combined<-cbind(combined,fare)
#=============================
embarked <- full$Embarked
for(i in 1:n){
  if(identical(all.equal(embarked[i], 'C'), TRUE)){
    embarked[i] = 1
  }
  else if(identical(all.equal(embarked[i], 'Q'), TRUE)){
    embarked[i] = 2
  }
  if(identical(all.equal(embarked[i], 'S'), TRUE)){
    embarked[i] = 3
  }
  else{
    embarked[i] = 0
  }
}
combined<-cbind(combined,embarked)
#=============================
survived <- full$Survived
n = length(survived)
for(i in 1:n){
  if(identical(all.equal(survived[i], 1), TRUE)){
    survived[i] = TRUE
  }
  else{
    survived[i] = FALSE
  }
}
#================================
exp_vars <- data.matrix(combined)
target_var <- data.matrix(survived)
nr <- nrow(exp_vars)
set.seed(1234)
row.train <- sample(nr, floor(0.95 * nr))
exp_vars.train <- exp_vars[row.train,]
target_var.train <- target_var[row.train,]
exp_vars.valid <- exp_vars[-row.train,]
target_var.valid <- target_var[-row.train,]
#===============================
nlambda <- 100;
# L2 or L1 penalty
#alpha <- 0.0; # L2 penalty
#alpha <- 1.0; # L1 penalty (sparse)
alpha <- 0.5; # L1 + L2 penalty
fit1 <- glmnet(exp_vars.train,target_var.train,family = "binomial",alpha = alpha,nlambda = nlambda,standardize = TRUE,intercept = TRUE);
plot(fit1, xvar = "lambda", label = TRUE)
#===========================================
fit1 <- glmnet(exp_vars.train,target_var.train,
               family = "gaussian", # for regression, family = "gaussian"
               alpha = alpha,nlambda = nlambda,standardize = TRUE,intercept = TRUE);
plot(fit1, xvar = "lambda", label = TRUE)
#print(fit1);
validYHat <- predict(fit1, newx = as(exp_vars.valid, "dgCMatrix"), s = 0.001, type = "class")
validRate <- mean(target_var.valid == validYHat);
validYHatPath <- predict(fit1, newx = as(exp_vars.valid, "dgCMatrix"), type = "class");
validRatePath <- colMeans(target_var.valid == validYHatPath[,]);
plot(validRatePath);
bestLambda <- fit1$lambda[which.max(as.vector(validRatePath))];
#===========================================
# load test.csv
test  <- read.csv('test.csv', stringsAsFactors = F)
test$Survived <- NA
age2 <- test$Age
n = length(age2)
set.seed(123)
missed = 0
for(i in 1:n){
  if(is.na(age2[i])){
    age2[i] = as.integer(sample(na.omit(test$Age),1))
    missed=missed+1
  }
}
#========================
class2 <- test$Pclass
missed = 0
for(i in 1:n){
  if(is.na(class2[i])){
    class2[i] = as.integer(sample(na.omit(test$class),1))
    missed=missed+1
  }
}
combined2<-cbind(age2,class2)
#=================
sex2 <- test$Sex
for(i in 1:n){
  if(identical(all.equal(sex2[i], 'female'), TRUE)){
    sex2[i] = 1
  }
  else{
    sex2[i] = 0
  }
}
combined2<-cbind(combined2,sex2)
#=============================
sibsp2 <- test$SibSp
missed = 0
for(i in 1:n){
  if(is.na(sibsp2[i])){
    sibsp2[i] = 0
    missed=missed+1
  }
}
combined2<-cbind(combined2,sibsp2)
#=============================
parch2 <- test$Parch
missed = 0
for(i in 1:n){
  if(is.na(parch2[i])){
    parch2[i] = 0
    missed=missed+1
  }
}
combined2<-cbind(combined2,parch2)
#=============================
fare2 <- test$Fare
missed = 0
for(i in 1:n){
  if(is.na(fare2[i])){
    fare2[i] = 0
    missed=missed+1
  }
}
combined2<-cbind(combined2,fare2)
#=============================
embarked2 <- test$Embarked
for(i in 1:n){
  if(identical(all.equal(embarked2[i], 'C'), TRUE)){
    embarked2[i] = 1
  }
  else if(identical(all.equal(embarked2[i], 'Q'), TRUE)){
    embarked2[i] = 2
  }
  if(identical(all.equal(embarked2[i], 'S'), TRUE)){
    embarked2[i] = 3
  }
  else{
    embarked2[i] = 0
  }
}
combined2<-cbind(combined2,embarked2)
#=============================
test_vars <- data.matrix(combined2)
validYHat <- predict(fit1, newx = as(test_vars, "dgCMatrix"), s = 0.001, type = "class")
# prediction for all lambda
validYHatPath <- predict(fit1, newx = as(test_vars, "dgCMatrix"), type = "class");
validRatePath <- colMeans(target_var.valid == validYHatPath[,]);
# just for checking
plot(validRatePath);
# find the best lambda
bestLambda <- fit1$lambda[which.max(as.vector(validRatePath))];
# just for checking
plot(validYHatPath);
print(bestLambda)
