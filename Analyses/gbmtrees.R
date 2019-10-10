prog.bar<-function(value,len=50){
  if(value>1){
    value=1
  } else if(value<0){
    value=0
  }
  nl<-floor(value*len)
  nr<-floor((1-value)*len)
  nm<-len-nl-nr
  cat("\r|",rep("=",nl),rep(".",nm),rep(" ",nr),"| ",floor(value*100)," %",sep = "")
}


prog.bar(.22)

rep(x = "1",1)
prog.bar(.2)
nrow(out[[2]])
names(tsamp)
names(testdat)
library(gbm)

wdat<-clean.data(wtr.data)
wdat<-clean.data2(wdat)
out<-data.split(wdat)

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
  gbmin<-tsamp
  gbmin$target<-ttarg
  fit2 <- gbm(target~.,data=gbmin,distribution = "gaussian",shrinkage = 0.4,n.trees = 150)
  
  pred1<-predict(fit2,tsamp,n.trees = 150)
  print(rmse(pred1,ttarg))
  pred2<-predict(fit2,vsamp,n.trees = 150)
  print(rmse(pred2,vtarg))
}

gbm.perf(fit2,method = "OOB")
pretty.gbm.tree(fit2, i.tree = 150)
plot(fit2,i.var=7)
relative.influence(fit2,n.trees = 150)
hist(vtarg-pred2,breaks = 200,xlim = c(-5,5))

#plot(ttarg,pred1)
# plot(vsamp[[24]],vtarg-pred2)
# fit2$coefficients
{
  testdat<-rem.col(wtestdat)
  testdat<-add.col(testdat)
  testdat<-norm.data(data=testdat,average =par[[1]],std.dev = par[[2]])
  tar.pred<-predict(fit2,testdat)
  summary(tar.pred)
  ind<- is.na(tar.pred)|tar.pred>60 | tar.pred<40 
  summary(ind)
  tar.pred[ind]<-53.5
  {
    ind<- is.na(tar.pred) 
    tar.pred[ind]<-53.5
    ind<- tar.pred>60 
    tar.pred[ind]<-60
    ind<-  tar.pred<40 
    tar.pred[ind]<-40
  }
  summary(tar.pred)
  my.summary(tar.pred)
  tar.pred<-data.frame(target=tar.pred)
  write.csv(tar.pred,file="submissionCTcorr1.csv")
  
}

x<-wdat$DIC88023.PV
y<-wdat$target
mean((x-y)^2)
hist(y-x,breaks = 400,xlim=c(-3,3))
boxplot(y-x,ylim=c(-2,2))
my.summary(x-y)
summary(x-y)
plot(x,y-x)
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

z<-data.frame(x=1:10,y=1:10)
names(z)<-c("xx","yy")
z
