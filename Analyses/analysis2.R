library(readr)
library(ggplot2)
{
  samp<-wtr.data[1:50000,c(-1,-21,-22,-26,-27,-28,-30)]
  t<-wtr.data[1:50000,1]
  samp[[21]]<-samp[[21]]+samp[[22]]
  samp<-samp[,-22]
  }
nm<-names(samp)
nm
length(samp)
is.numeric(wtr.data[1,])
for(i in 1:length(samp)){
  cat("\n+",nm[i])
}
ind<-samp[[22]]>30 & samp[[22]]<65 & !is.na(samp[[22]])
lin.mod<-lm(data=samp[ind,],target~ WQI8100XCL1.CPV
            + XI84201.PV
            + XI84202.PV
            + XI84123.PV
            + XI84124.PV
            + XI84125.PV
            + FX87211.CPV1
            + FIC87211.PV
            + FIC87211.SV
            + FX87211.P01
            + FI87208.PV
            + AIC88049.PV
            + ZI88001.PV
            + NIC88002.PV
            + PIC88007.PV
            + LIC88006.PV
            + AIC88055.PV
            + FIC88022.PV
            + DIC88023.PV
            + SI88033.PV
            + SI88034.PV)
summary(lin.mod)
plot(lin.mod$residuals)
sqrt(mean(lin.mod$residuals^2))

temp<-samp[ind,]
ind<-wtr.data[[20]]>45 & wtr.data[[20]]<60 & !is.na(wtr.data[[20]]) & wtr.data[[2]]>100 &wtr.data[[8]]>100
samp<-wtr.data[ind,]
summary(samp[[7]]*samp[[8]]/100-samp[[25]]/3.6)
summary(samp[[8]])
cor(wtr.data[[2]],wtr.data[[8]],use = "na.or.complete")
{
  plot(c(1,2),c(1,2),type='l')
  xtemp<-temp[[6]]*temp[[7]]/(temp[[7]]+temp[[11]])/100
  ytemp<-temp[[22]]
  plot(x,y,main=cor(xtemp,ytemp,use = "na.or.complete"))
}

##Trees
library(rpart)
{
  tsamp<-wtr.data[3000:130000,]
  ind<-tsamp[[20]]<60 & tsamp[[20]]>30 & !is.na(tsamp[[20]]) & tsamp[[29]]<60 & tsamp[[29]]>30 & !is.na(tsamp[[29]])
  tsamp<-tsamp[ind,]
  tsamp<-tsamp[,c(-1,-21,-22,-26,-27,-28)]
  names(tsamp)
}
{#CART
  fit1<-rpart(target~.,data = tsamp,control = rpart.control(minsplit = 10))
  #summary(fit1)
  #fit1$variable.importance
  pred<-predict(fit1,tsamp[,-29])
  rmse(pred,tsamp$target)
  fit1$variable.importance
}
names(vsamp)
summary(wtr.data$MQI88024.CPV.1)

{
  n<-nrow(tsamp)
  tsamp<-clean.data(wtr.data[1:300000,])
  vsamp<-clean.data(wtr.data[300000:n,])
  fit2 <- cubist(tsamp[,-22], tsamp[,22])
  pred1<-predict(fit2,tsamp[,-22])
  print(rmse(pred1,tsamp$target))
  pred2<-predict(fit2,vsamp[,-22])
  print(rmse(pred2,vsamp$target))
}
clean.data<-function(data){
  ind<-data[[20]]<60 & data[[20]]>30 & !is.na(data[[20]]) & data[[29]]<60 & data[[29]]>30 & !is.na(data[[29]])
  data2<-data[ind,]
  data2<-data2[,c(-1,-20,-21,-22,-26,-27,-28)]
  return(data2)
}
norm.data<-function(data,average,std.dev){
  data2<-data
  for(i in length(data2)){
    data2[[i]]<-(data2[[i]]-average[i])/std.dev[i]
  }
  return(data2)
}
sd(wtr.data[,2],na.rm = T)
###Functions
{
  my.diff<-function(data,radius=1){
    len<-length(data)
    res<-data[(1+2*radius):len]
    if(radius==1){
      for(i in 1:length(res)){
        res[i]<-(data[i+2]-data[i])/2
      }
    } else if(radius==2){
      for(i in 1:length(res)){
        res[i]<-(-data[i+4]+8*data[i+3]-8*data[i+1]+data[i])/12
      }
    } else if(radius==3){
      for(i in 1:length(res)){
        res[i]<-(data[i+6]-9*data[i+5]+45*data[i+4]-45*data[i+2]+9*data[i+1]-data[i])/60
      }
    }
    return(res)
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
      
    }
    if(!preserve.size){
      res<-res[(radius+1):(len-radius)]
      
    } 
    return(res)
  }
  lag.cor<-function(x,y,lag=5,show.graph=F){
    if(length(x)>length(y)){
      x<-x[1:(length(y))]
    } else if(length(y)>length(x)){
      y<-y[1:(length(x))]
    }
    # if(length(lag)==1){
    #   lag=c(-lag,lag)
    # }
    
    res<-data.frame(lag=NULL,Correlation=NULL)
    for(lg in lag){
      len<-length(x)
      if(lg>=0){
        cr<-cor(x[1:(len-lg)],y[(1+lg):len],use = "na.or.complete")
      } else{
        cr<-cor(x[(1-lg):len],y[1:(len+lg)],use = "na.or.complete")
      }
      res<-rbind(res,data.frame(lag=lg,Correlation=cr))
    }
    if (show.graph) {
      i=abs(res[,2])==max(abs(res[,2]))
      res[,1]
      tit<-paste("Lag =", res[i,1],"  MAX correlation =",res[i,2])
      plot(res$lag,res$Correlation,main = tit)
    }
    
    return(res)
  }
  const.rmse<-function(data,val){
    rmse=0
    counter=0
    for(i in 1:length(data)){
      if(is.na(data[i])){
        next()
      }
      counter=counter+1
      rmse=rmse+(data[i]-val)^2
    }
    rmse=sqrt(rmse/counter)
    return(rmse)
  }
  df.fun<-function(data){
    n<-nrow(data)
    df<-data[1:n-1,1]
    for(i in 2:length(data)){
      if(is.character(data[1,i])){
        next()
      }
      df[[i]]<-diff(data[[i]])
    }
    
  }
  numplot<-function(data,bounds){
    xf<-bounds[1]:bounds[2]
    yf<-xf
    for(i in 1:length(xf)){
      yf[i]<-nrow(subset(data,x>xf[i]))
    }
    plot(xf,yf)
  }
  numplot<-function(data,bounds){
    xf<-bounds[1]:bounds[2]
    yf<-xf
    for(i in 1:length(xf)){
      yf[i]<-nrow(subset(data,x>xf[i]))
    }
    plot(xf,yf)
  }
  compar.plot<-function(data,col1,col2){
    par(mfrow=c(1,1))
    plot(data[[col1]],data[[col2]])
  }
  par.plot<-function(data,col1,col2,progress=0,len.ratio=.002){
    nd<-nrow(data)
    if(progress<0){
      progress=0
    } 
    if(len.ratio>1){
      len.ratio=1
    }
    if(len.ratio<0){
      len.ratio=.002
    }
    if(progress>1-len.ratio){
      progress<-1-len.ratio
    }
    nm<-names(data)
    i1<-round(progress*nd)+1
    i2<-i1+round(len.ratio*nd)-1
    daty1<-data[i1:i2,col1]
    daty2<-data[i1:i2,col2]
    datx<-data[i1:i2,1]
    par(mfrow=c(2,1))
    length(datx)
    length(daty1)
    plot(datx[[1]],daty1[[1]],xlab = " ",ylab = nm[col1],type = 'l')
    plot(datx[[1]],daty2[[1]],xlab = " ",ylab = nm[col2],type = 'l')
    par(mfrow=c(1,1))
  }
  par.plot3<-function(data,col1,col2,col3,progress=0,len.ratio=.002){
    nd<-nrow(data)
    if(progress<0){
      progress=0
    } 
    if(len.ratio>1){
      len.ratio=1
    }
    if(len.ratio<0){
      len.ratio=.002
    }
    if(progress>1-len.ratio){
      progress<-1-len.ratio
    }
    nm<-names(data)
    i1<-round(progress*nd)+1
    i2<-i1+round(len.ratio*nd)-1
    daty1<-data[i1:i2,col1]
    daty2<-data[i1:i2,col2]
    daty3<-data[i1:i2,col3]
    datx<-data[i1:i2,1]
    par(mfrow=c(3,1))
    length(datx)
    length(daty1)
    plot(datx[[1]],daty1[[1]],xlab = " ",ylab = nm[col1],type = 'l')
    plot(datx[[1]],daty2[[1]],xlab = " ",ylab = nm[col2],type = 'l')
    plot(datx[[1]],daty3[[1]],xlab = " ",ylab = nm[col2],type = 'l')
    par(mfrow=c(1,1))
  }
  rmse<-function(y1,y2){
    out<-sqrt(mean((y1-y2)^2,na.rm = T))
    return(out)
    
  }
}