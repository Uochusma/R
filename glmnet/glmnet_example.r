# ------------------------------------------------------------
# change directory (folder)
# ------------------------------------------------------------

setwd("C:/Path to the folder where this script exits/"); # e.g., "C:/User/your user name/Downloads/"
getwd();

# ------------------------------------------------------------
# install and load glmnet package
# ------------------------------------------------------------

install.packages("glmnet"); # necessary only for the first time

library(glmnet);

# ------------------------------------------------------------
# load data for binary classification
# ------------------------------------------------------------

# load data
data <- as.matrix(read.table("data.csv", sep = ",", skip = 0));

# get size of the data set
n <- nrow(data);
d <- ncol(data) - 1;

# ------------------------------------------------------------
# Divide data into training and validation sets
# ------------------------------------------------------------

# data ID from 1 to n
dataId <- c(1:n);
X <- data[,1:d];
Y <- data[,(d + 1)];

# check X and Y
head(X);
tail(X);
X[2,];
X[,5];
head(Y);
tail(Y);

# shuffle
randomizedDataId <- sample(dataId);

# get trainId, validId, testId

trainId <- randomizedDataId[1:ceiling(n/2)];
nTrain <- length(trainId);

validId <- randomizedDataId[(ceiling(n/2) + 1): ceiling(3*n/4)];
nValid <- length(validId);

testId <- randomizedDataId[(ceiling(3*n/4) + 1):n];
nTest <- length(testId);

# trainX, trainY, validX, validY, testX, testY

trainX <- data[trainId, 1:d];
trainY <- data[trainId, (d + 1)];

validX <- data[validId, 1:d];
validY <- data[validId, (d + 1)];

testX <- data[testId, 1:d];
testY <- data[testId, (d + 1)];

# ------------------------------------------------------------
# set parameters
# ------------------------------------------------------------

# the number of regularization parameter lambdas
nlambda <- 100;

# L2 or L1 penalty
#alpha <- 0.0; # L2 penalty
#alpha <- 1.0; # L1 penalty (sparse)
alpha <- 0.5; # L1 + L2 penalty

# ------------------------------------------------------------
# glmnet
# ------------------------------------------------------------

# ----- fit for model selection -----

fit1 <- glmnet(trainX,
               trainY,
               family = "binomial", # for regression, family = "gaussian"
               alpha = alpha,
               nlambda = nlambda,
               standardize = TRUE,
               intercept = TRUE);

# plot parameter sequence
plot(fit1, xvar = "lambda", label = TRUE)

# print glmnet
print(fit1);

# prediction on validation data with lambda = 0.02
validYHat <- predict(fit1, validX, s = 0.001, type = "class")
validRate <- mean(validY == validYHat);

# prediction for all lambda
validYHatPath <- predict(fit1, validX, type = "class");
validRatePath <- colMeans(validY == validYHatPath[,]);

# just for checking
plot(validRatePath);

# find the best lambda
bestLambda <- fit1$lambda[which.max(as.vector(validRatePath))];

# ----- fit for final model -----

trainValidX <- rbind(trainX, validX);
trainValidY <- append(trainY, validY);

fit2 <- glmnet(trainValidX,
               trainValidY,
               family = "binomial", # for regression, family = "gaussian"
               alpha = alpha,
               lambda = bestLambda,
               standardize = TRUE,
               intercept = TRUE);

# plot parameter sequence
plot(fit2, xvar = "lambda", label = TRUE);

# print glmnet
print(fit2);

# prediction on test data with lambda = bestLambda
testYHat <- predict(fit2, testX, s = bestLambda, type = "class");
testRate <- mean(testY == testYHat);
cat("Test Accuracy =", testRate, "\n");

# final model
finalModel <- coef(fit2, s = bestLambda);
write.table(as.matrix(finalModel), "finalModel.dat", sep = ",", quote=F, col.names=F);

