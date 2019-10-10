###PREDICTING CURRENT CONCENTRATION
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
    return(list(data[,c(-20,-29)],data[,20]))
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
    for(i in 1:length(data2)){
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
    output$DenEst<-data$PIC88007.PV/data$LIC88006.PV
    x<-data$MQI88024.CPV/data$FIC88022.PV
    output$Cest<-4.6333+76.876*x-20.776*x^2
    output$totin<-data$FX87211.CPV1+data$FI87208.PV+data$FIC87211.PV/1000
    output$msolin<-data$FX87211.CPV1*data$XI84125.PV
    output$saccu<-output$msolin-data$MQI88024.CPV/3.6
    output$maccu<-output$totin-data$MQI88024.CPV
    return(output)
  }
  norm.time<-function(data,npoint=180){
    new.names<-paste(names(data),"norm")
    data2<-data
    names(data2)<-new.names
    
    counter<-0
    numcol<-length(data)
    numrow<-nrow(data)
    nmax<-numrow*numcol
    
    # data<-cbind(data,data)
    # names(data)<-new.names
    for(i in 1:numrow){
      if(i<npoint){
        k<-i-1
      } else{
        k<-npoint-1
      }
      avg<-sapply(data[(i-k):i,], mean)
      sdev<-sapply(data[(i-k):i,],sd)
      for(j in 1:length(sdev)){
        if(is.na(sdev[[j]])|sdev[[j]]==0){
          sdev[[j]]=1
        }
      }
      data2[i,]<-(data[i,]-avg)/sdev
      if(floor(i/numrow*1000)>floor((i-1)/numrow*1000)){
        prog.bar(i/numrow)
      }
      
    }
    
    #names(data2)<-new.names
    return(data2)
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
    cat("\r|",rep("=",nl),rep("-",nm),rep(" ",nr),"| ",floor(value*1000)/10," %",sep = "")
  }
  mult.mov.avg<-function(data,radius=5,preserve.size=F,rep=1){
    res<-data
    for(i in 1:rep){
      res<-mov.avg(res,radius = radius,preserve.size = preserve.size )
    }
    return(res)
  }
  mov.avg<-function(data,radius=5,preserve.size=F){
    res<-data
    len<-length(data)
    for(i in 1:len){
      if(i<(radius+1)){
        k<-i-1
      } else if(i>(len-radius)){
        k<-len-i
      }
      else{
        k<-radius
      }
      res[i]<-mean(data[(i-k):(i+k)],na.rm = T)
      if(floor(i/len*1000)>floor((i-1)/len*1000)){
        prog.bar(i/len)
      }
    }
    if(!preserve.size){
      res<-res[(radius+1):(len-radius)]
      
    } 
    return(res)
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
# out<-data.split(wdat)
out<-list(wdat[1:400000,],wdat[400001:521354,])
nrow(out[[2]])
names(tsamp)
names(testdat)
names(wdat)

library(Cubist)
{
  tsamp<-rem.target(out[[1]])
  vsamp<-rem.target(out[[2]])
  ttarg<-tsamp[[2]]
  vtarg<-vsamp[[2]]
  tsamp<-tsamp[[1]]
  vsamp<-vsamp[[1]]
  ##
  # tsamp<-wdat[1:400000,c(-20,-29)]
  # vsamp<-wdat[400001:nrow(wdat),c(-20,-29)]
  # ttarg<-wdat[1:400000,29]
  # vtarg<-wdat[400001:nrow(wdat),29]
  ##
  tsamp<-rem.col(tsamp)
  vsamp<-rem.col(vsamp)
  tsamp<-add.col(tsamp)
  vsamp<-add.col(vsamp)
  
  par<-norm.param(tsamp)
  tsamp<-norm.data(data=tsamp,average =par[[1]],std.dev = par[[2]])
  vsamp<-norm.data(data=vsamp,average =par[[1]],std.dev = par[[2]])
  tsamp2<-norm.time(tsamp,npoint = 360)
  vsamp2<-norm.time(vsamp,npoint = 360)
  cubistControl(extrapolation = 10)
  xtr<-cbind(tsamp,tsamp2)
  fit2 <- cubist(x=xtr,y =  ttarg[[1]],committees = 5)#,neighbors=1,composite=10
  summary(ttarg)
  library(Cubist)
  pred1<-predict(fit2,cbind(tsamp,tsamp2))
  print(rmse(pred1,ttarg))
  pred2<-predict(fit2,cbind(vsamp,vsamp2))
  print(rmse(pred2,vtarg))
  hist(pred1,breaks=500,xlim=c(30,60))
  sum(pred2<48)
  plot(pred2,vtarg[[1]])
}
save(list = c("fit2","tsamp","vsamp","tsamp2","vsamp2","ttarg","vtarg","wdat"),file = "CubistTree.RData")
names(testdat)
hist(vtarg-pred2,breaks = 200,xlim = c(-5,5))
my.summary(pred2)
my.summary(vtarg)
summary(wtr.data[[29]])
#plot(ttarg,pred1)
# plot(vsamp[[24]],vtarg-pred2)
# fit2$coefficients
names(testdat)
names(tsamp)
{
  testdat<-rem.col(wtestdat)
  testdat<-clean.data2(testdat)
  testdat$MQI88024.CPV.1<-testdat$MQI88024.CPV/testdat$FIC88022.PV
  testdat<-add.col(testdat)
  
  testdat1<-norm.data(data=testdat,average =par[[1]],std.dev = par[[2]])
  testdat2<-norm.time(testdat,npoint = 360)
  tar.pred<-predict(fit2,cbind(testdat1,testdat2))
  summary(tar.pred)
  sum((tar.pred)<45)
  tar.pred2<-tar.pred
  ind<- is.na(tar.pred)|tar.pred>58 
  tar.pred2[ind]<-55.5
  ind<- tar.pred<48 
  tar.pred2[ind]<-51
  #summary(tar.pred2)
  #tar.pred[ind]<-median(tar.pred[!ind],na.rm = )
  #hist(tar.pred2,breaks = 500)
  #mod(tar.pred)
  # {
  #   ind<- is.na(tar.pred) 
  #   tar.pred[ind]<-53.5
  #   ind<- tar.pred>60 
  #   tar.pred[ind]<-60
  #   ind<-  tar.pred<40 
  #   tar.pred[ind]<-40
  # }
  #summary(tar.pred)
  tar.df<-data.frame(target=tar.pred)
  write.csv(tar.df,file="submissionCTcorr2.csv",row.names = F)
  write.csv(tar.df,file="submissionCT2.csv",row.names = F)
}
hist(tar.pred2,breaks = 500,xlim = c(30,65))
############
#TEMP
{
  n<-25
  hist(wdat[,n],breaks = 400,main=names(wdat[n]))
}
plot(wdat[[7]],wdat[[20]])
par(pch=".")
plot(rnorm(20))

{
  i<- wdat[,19]>0
  hist(wdat[i,19],breaks = 400,main=names(wdat[6]))
  hist(wdat[i,20],breaks = 400,main=names(wdat[20]))
}
