library(sparklyr)
data(longley)
longley
names(longley)

#CART
{##RMSE 0.691151
  fit <- rpart(Employed~., data=longley, control=rpart.control(minsplit=5))
  # summarize the fit
  summary(fit)
  # make predictions
  predictions <- predict(fit, longley[,1:6])
  # summarize accuracy
  mse <- mean((longley$Employed - predictions)^2)
  print(mse)
  rmse(longley$Employed,predictions)
  fit$variable.importance
}
library(party)
#Conditional D.Trees 
{##RMSE 0.6911508
  fit <- ctree(Employed~., data=longley, controls=ctree_control(minsplit=2,minbucket=2,testtype="Univariate"))
  # summarize the fit
  summary(fit)
  # make predictions
  predictions <- predict(fit, longley[,1:6])
  # summarize accuracy
  mse <- mean((longley$Employed - predictions)^2)
  print(mse)
  rmse(longley$Employed,predictions)
}

library(RWeka)
??RWeka
{
  fit <- M5P(Employed~., data=longley)
  # summarize the fit
  summary(fit)
  # make predictions
  predictions <- predict(fit, longley[,1:6])
  # summarize accuracy
  mse <- mean((longley$Employed - predictions)^2)
  print(mse)
}

library(ipred)
#Bagging CART
{##RMSE=0.6099433
  fit <- bagging(Employed~., data=longley, control=rpart.control(minsplit=5))
  # summarize the fit
  summary(fit)
  # make predictions
  predictions <- predict(fit, longley[,1:6])
  # summarize accuracy
  mse <- mean((longley$Employed - predictions)^2)
  print(mse)
}
sum(is.na(q[[5]]))
library(randomForest)
{# RMSE=0.5585226
  fit <- randomForest(target~., data=q,ntree = 200)
  # summarize the fit
  summary(fit)
  # make predictions
  predictions <- predict(fit, longley[,1:6])
  # summarize accuracy
  mse <- mean((longley$Employed - predictions)^2)
  print(mse)
  rmse(longley$Employed,predictions)
}
?randomForest
#Gradient Boosted Machine
library(gbm)
q<-tsamp
q$target<-ttarg
q<-q[1:20000,]
{##????
  fit <- gbm(target~., data=q, distribution="gaussian",n.trees = 100)
  # summarize the fit
  summary(fit)
  # make predictions
  predictions <- predict(fit, q)
  # summarize accuracy
  mse <- mean((longley$Employed - predictions)^2)
  print(mse)
}
fit$

library(Cubist)
{##RMSE=0.41926
  fit <- cubist(longley[,1:6], longley[,7])
  # summarize the fit
  summary(fit)
  # make predictions
  predictions <- predict(fit, longley[,1:6])
  # summarize accuracy
  mse <- mean((longley$Employed - predictions)^2)
  print(mse)
  rmse(longley$Employed,predictions)
}
