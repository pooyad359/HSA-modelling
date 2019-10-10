#FUNCTIONS
{
  my.summary<-function(data){
    mltp<-2
    q1<-quantile(data,.25,na.rm = T)
    q3<-quantile(data,.75,na.rm = T)
    md<-median(data,na.rm = T)
    lb<-md-mltp*(md-q1)
    ub<-md+mltp*(q3-md)
    ind<-data<q3 & data>q1 & !is.na(data)
    summary(data[ind])
  }
  clean.data<-function(data){
    ind<-data[[20]]<65 & data[[20]]>30 & !is.na(data[[20]]) & data[[29]]<65 & data[[29]]>30 & !is.na(data[[29]])
    print(sum(ind)/nrow(data)*100)
    data2<-data[ind,]
    return(data2)
  }
  clean.data2<-function(data){
    for(j in 1:length(data)){
      ind.na<-is.na(data[[j]])
      if(sum(ind.na)==0){
        next()
      }
      ni<-1:nrow(data)
      for(i in ni[ind.na]){
        if(is.na(data[i,j])){
          data[i,j]<-data[i-1,j]
        }
      }
      cat("\r",floor(j/length(data)*100),"%")
    }
    cat("\r",100,"%")
    return(data)
  }
  rem.target<-function(data){
    return(list(data[,c(-20,-29)],data[,29]))
  }
  rem.col<-function(data){
    
    data[[22]]<-data[[22]]+data[[23]]
    temp<-data[,c(-1,-2,-20,-21,-23,-25,-26,-27)]
    return(temp)
  }
  norm.param<-function(data){
    avg<-c()
    sdev<-c()
    for(i in 1:length(data)){
      avg[i]<-mean(data[[i]],na.rm = T)
      sdev[i]<-sd(data[[i]],na.rm = T)
    }
    output<-list(avg,sdev)
    return(output)
  }
  norm.data<-function(data,average,std.dev){
    data2<-data
    for(i in length(data2)){
      data2[[i]]<-(data2[[i]]-average[i])/std.dev[i]
    }
    return(data2)
  }
  data.split<-function(data,fraction=.5){
    # split data into training and validating set
    valid.frac=1-fraction
    valid.count<-floor(valid.frac*nrow(data))
    valid.ind<-sample(seq(nrow(data)),valid.count)
    data.val<-data[valid.ind,]
    
    data.train<-data[-valid.ind,]
    out=list(data.train,data.val)
    return(out)
  }
  add.col<-function(data){
    output<-data
    output$DenEst<-data[[14]]/data[[15]]
    output$totin<-data[[6]]+data[[10]]+data[[7]]/1000
    output$msolin<-data[[6]]*data[[5]]
    output$saccu<-output$msolin-data[[19/3.6]]
    output$maccu<-output$totin-data[[19]]
    return(output)
  }
  norm.time<-function(data,npoint=180){
    new.names<-paste(names(data),"norm")
    new.names<-c(names(data),new.names)
    counter<-0
    numcol<-length(data)
    numrow<-nrow(data)
    nmax<-numrow*numcol
    data<-cbind(data,data)
    names(data)<-new.names
    for(j in 1:numcol){
      for(i in 1:numrow){
        counter<-counter+1
        if(i<npoint){
          k<-i-1
        } else{
          k<-npoint-1
        }
        avg<-mean(data[(i-k):i,j],na.rm = T)
        sdev<-sd(data[(i-k):i,j],na.rm = T)
        if(is.na(sdev)|sdev==0){
          sdev=1
        }
        data[i,j+numcol]<-(data[i,j]-avg)/sdev
        if(floor(counter/nmax*1000)>floor((counter-1)/nmax*1000)){
          prog.bar(counter/nmax)
        }
      }
    }
    return(data)
  }
  prog.bar<-function(value,len=50){
    if(value>1){
      value=1
    } else if(value<0){
      value=0
    }
    nl<-floor(value*len)
    nr<-floor((1-value)*len)
    nm<-len-nl-nr
    cat("\r|",rep("=",nl),rep(".",nm),rep(" ",nr),"| ",floor(value*1000)/10," %",sep = "")
  }
}
x<-wtr.data[1:5000,20]
y<-wtr.data[1:5000,7]
df<-data.frame(x=x,y=y)
df

df2<-norm.time(df,npoint = 180)
df2
?system.time
t1<-Sys.time()
t2<-Sys.time()
t2-t1
plot(df)
wdat<-clean.data(wtr.data)
wdat<-clean.data2(wdat)
out<-data.split(wdat)
nrow(out[[2]])
names(tsamp)
names(testdat)
names(wdat)

library(Cubist)
{
  #tsamp<-rem.target(out[[1]])
  #vsamp<-rem.target(out[[2]])
  #ttarg<-tsamp[[2]]
  #vtarg<-vsamp[[2]]
  ##
  tsamp<-wdat[1:400000,c(-20,-29)]
  vsamp<-wdat[400001:nrow(wdat),c(-20,-29)]
  ttarg<-wdat[1:400000,29]
  vtarg<-wdat[400001:nrow(wdat),29]
  ##
  tsamp<-rem.col(tsamp)
  vsamp<-rem.col(vsamp)
  tsamp<-add.col(tsamp)
  vsamp<-add.col(vsamp)
  
  #par<-norm.param(tsamp)
  #tsamp<-norm.data(data=tsamp,average =par[[1]],std.dev = par[[2]])
  #vsamp<-norm.data(data=vsamp,average =par[[1]],std.dev = par[[2]])
  tsamp2<-norm.time(tsamp)
  vsamp2<-norm.time(vsamp)##<------------------------
  cubistControl(extrapolation = 10)
  fit2 <- cubist(tsamp, ttarg,committees = 1)#,neighbors=1,composite=10
  
  pred1<-predict(fit2,tsamp2)
  print(rmse(pred1,ttarg))
  pred2<-predict(fit2,vsamp2)
  print(rmse(pred2,vtarg))
}
hist(vtarg-pred2,breaks = 200,xlim = c(-5,5))
my.summary(wtr.data[[29]])
summary(wtr.data[[29]])
#plot(ttarg,pred1)
# plot(vsamp[[24]],vtarg-pred2)
# fit2$coefficients
{
  testdat<-rem.col(wtestdat)
  testdat<-clean.data2(testdat)
  testdat<-add.col(testdat)
  testdat<-norm.data(data=testdat,average =par[[1]],std.dev = par[[2]])
  tar.pred<-predict(fit2,testdat)
  summary(tar.pred)
  sum(is.na(tar.pred))
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
  tar.pred<-data.frame(target=tar.pred)
  write.csv(tar.pred,file="submissionCTcorr1.csv",row.names = F)
  
}
