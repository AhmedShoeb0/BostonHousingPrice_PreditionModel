#Reading training dataset
training.df<-read.csv("train.csv")

#Display the structure of the data frame
str(training.df)

#To get more detailed statistical information from each column
summary(training.df)

#scatterplot of some of the important variables....
#....(based on intuition) with the outcome variable MEDV.

suppressMessages(library("ggplot2"))
suppressMessages(library("ggpubr"))

plot(training.df[,c(4,6,7,12,14,15)],pch=3)

#1 visualizing the relationship between “crim” and “medv”
ggscatter(training.df, x = "crim", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "crim", ylab = "medv")
#2 visualizing the relationship between “indus” and “medv”
ggscatter(training.df, x = "indus", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "indus", ylab = "medv")
#3 visualizing the relationship between “zn” and “medv”
ggscatter(training.df, x = "zn", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "zn", ylab = "medv")
#4 visualizing the relationship between “chas” and “medv”
ggscatter(training.df, x = "chas", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "chas", ylab = "medv")
#5 visualizing the relationship between “nox” and “medv”
ggscatter(training.df, x = "nox", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "nox", ylab = "medv")
#6 visualizing the relationship between “rm” and “medv”
ggscatter(training.df, x = "rm", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "rm", ylab = "medv")
#7 visualizing the relationship between “age” and “medv”
ggscatter(training.df, x = "age", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "age", ylab = "medv")
#8 visualizing the relationship between “dis” and “medv”
ggscatter(training.df, x = "dis", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "dis", ylab = "medv")
#9 visualizing the relationship between “rad” and “medv”
ggscatter(training.df, x = "rad", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "rad", ylab = "medv")
#10 visualizing the relationship between “tax” and “medv”
ggscatter(training.df, x = "tax", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "tax", ylab = "medv")
#11 visualizing the relationship between “ptratio” and “medv”
ggscatter(training.df, x = "ptratio", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ptratio", ylab = "medv")
#12 visualizing the relationship between “black” and “medv”
ggscatter(training.df, x = "black", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "black", ylab = "medv")
#13 visualizing the relationship between “istat” and “medv”
ggscatter(training.df, x = "lstat", y = "medv",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "lstat", ylab = "medv")


#check if any feature has near zero variance (values not varying much within the column).
suppressMessages(library(caret))
# Correlation of each independent variable with the dependent variable
cor(training.df,training.df$MEDV)
# Calulate near zero variance
nzv <- nearZeroVar(training.df, saveMetrics = TRUE)
sum(nzv$nzv)

#Reading testing dataset
testing.df<-read.csv("test.csv")

#Display the stucture of the data frame
str(testing.df)

#Building The Regression Model (Random Forest)
#Setting a seed in R means to initialize a pseudorandom number generator.
suppressMessages(library(randomForest))
set.seed(12345)
#fit function creates a trained model (an object) of class regression.
fit.rf <- randomForest(formula = medv~zn+chas+rm+dis+black
                       , data = training.df)

#predict on test set
set.seed(12345)
pred.rf <- predict(fit.rf, testing.df)

#Display the summary of the predicted list of values
summary(pred.rf)

#Adding the predicted values to the testing data frame
testing.df$medv<-pred.rf

#Printing All dataset values
print(testing.df)

# Root-mean squared error
rmse.rf <- sqrt(sum(((training.df$MEDV) - testing.df$MEDV)^2)/
                  length(testing.df$MEDV))
c(RMSE = rmse.rf, pseudoR2 = mean(fit.rf$rsq))


