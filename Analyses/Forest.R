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

wdat<-clean.data(wtr.data)
wdat<-clean.data2(wdat)
out<-data.split(wdat)
for(i in 1:length(wdat)){
  cat("(",i,": ",sum(is.na(wdat[[i]])),")\n",sep="")
}

{
  tsamp<-rem.target(out[[1]])
  vsamp<-rem.target(out[[2]])
  ttarg<-tsamp[[2]]
  vtarg<-vsamp[[2]]
  tsamp<-rem.col(tsamp[[1]])
  vsamp<-rem.col(vsamp[[1]])
  tsamp<-add.col(tsamp)
  vsamp<-add.col(vsamp)
  par<-norm.param(tsamp)
  tsamp<-norm.data(data=tsamp,average =par[[1]],std.dev = par[[2]])
  vsamp<-norm.data(data=vsamp,average =par[[1]],std.dev = par[[2]])
  rfin<-tsamp
  rfin$target<-ttarg
  fit2 <- randomForest(target~.,data=rfin,ntree=200)
  
  pred1<-predict(fit2,tsamp)
  print(rmse(pred1,ttarg))
  pred2<-predict(fit2,vsamp)
  print(rmse(pred2,vtarg))
}
