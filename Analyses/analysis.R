library(readr)
library(ggplot2)

wtr.data<-train
wtr.data[1:100,1]
names(wdattr)
par(mfrow=c(2,1))
samp<-wtr.data[1:2000,]
plot(wtr.data[[20]],wtr.data[[24]])
plot(samp[[1]],samp[[20]])
samp[1,1]
timestamp(samp[1,1])
time(x = samp[1,1])
summary(wtr.data[[29]])
wtr.data[[28]]<-factor(wtr.data[[28]])
boxplot(wtr.data[[29]])
mavg<-median(x=wtr.data[[24]],na.rm = T)
mavg
targn=wtr.data[[20]]
targf=wtr.data[[24]]
ind=1:length(targf)
i=targn>30 & targn<65 & targf>30 & targf<65
{
  targest=targn
  targest[targest>58]=55
  targest[targest<48]=52
}
dat2<-wtr.data[i,]
dat.tr<-dat2[1:400000,]
dat.val<-dat2[400001:560000,]
library(FNN)
knn.reg(dat.tr[,c(-20,-24)],dat.val[,c(-20,-24)],y=dat.tr[,20],k=10)




rmse(targest[i],targf[i])
rmse(targn[i],targf[i])
names(wtr.data)
hist(wtr.data$MassCalc[i]/wtr.data$DischFlow[i],xlim=c(0.5,1),breaks = 8000)
plot(wtr.data$MassCalc[i]/wtr.data$DischFlow[i],wtr.data$DischDensity[i],xlim=c(0,1),pch='.')
j=ind[abs(targn-targf)>5&targn>30 & targn<65 & targf>30 & targf<65]
hist(j,breaks = 200)
sqrt(mean((wtr.data[[24]]-mavg)^2,na.rm = T))
line(x = samp[[1]],y=samp[[29]])
x=wtr.data[[1]]
y=wtr.data[[20]]
i=20200:21500
plot(wtr.data[[7]],wtr.data[[20]],pch='.')
plot.lagged(wtr.data[[20]],wtr.data[[7]],lag=180)
#lag.cor(wtr.data[[7]],wtr.data[[20]],lag = seq(0,500,5),show.graph = T)
(y[2500]-y[2495])/5
ggplot(aes(wtr.data[,2],wtr.data[,29]))+geom_line()
names(wtr.data)<-c("time","floatfeed","per80size","persize106","perCu","perFe","perSolid","SolidRate",
                   "FlocAdd","FlocAddSP","flocAddGT","FlocWater","pH",
                   "RakeHeight","RakeTorque","BedPressure","BedLevel","SettlingRate","DischFlow",
                   "DischDensity","Pump1","Pump2","MassCalc","target")
names(wtr.data)
par.plot(wtr.data,col1=19,col2=20,progress = 0.30756,len.ratio = .0003)
par.plot(wtr.data,col1=2,col2=20,progress = .1192,len.ratio = .001)
par.plot(wtr.data,col1=15,col2=20,progress = 0.69,len.ratio = .02)
par.plot(wtr.data,col1=16,col2=20,progress = 0.50,len.ratio = .02)
par.plot(wtr.data,col1=2,col2=20,progress = 0.20,len.ratio = .015)
mean(wtr.data[[20]],na.rm = T)

plot.lagged<-function(x,y,lag=0){
  n<-length(x)
  plot(x[(lag+1):n],y[1:(n-lag)],pch='.')
}
n=4000
i1=1000
i2=i1+n
samp<-wtr.data[i1:i2,]
plot(samp[[8]]/samp[[2]],type = 'l',ylim = c(0,2))
plot(samp[[20]],type = 'l')
plot(samp[[8]],type = 'l')

##Subsetting
{
  samp<-wtr.data[1:44913,]
  samp<-wtr.data
  wtr.data[,30]<-wtr.data[,25]/wtr.data[,19]
  #ind<-wtr.data[,20]>51 & wtr.data[,20]<59 & !is.na(wtr.data[,20])
  targ<-samp[,19]
  summary(wtr.data[,30])
  summary(targ)
  #changes in two parameters
  {
    col1<-30
    col2<-20
    col3<-19
    #par.plot(samp,col1=col1,col2=col2,progress = 0.054,len.ratio = .003)
    #par.plot(samp,col1=col1,col2=col2,progress = 0.0560,len.ratio = .00005)
    #par.plot(samp,col1=col1,col2=col2,progress = 0.056015,len.ratio = .00005)
    #par.plot3(samp,col1=col1,col2=col2,col3=col3,progress = 0.055915,len.ratio = .0005)
    par.plot3(samp,col1=col1,col2=col2,col3=col3,progress = 0.2330,len.ratio = .0051)
  }
  ind<- !is.na(targ) & targ>550 & targ<1000
  summary(ind)
  wtr.data[2,]
  #max(samp[[20]],na.rm = T)
  col1<-20
  #moving average smoothing
  {
    marad<-10
    reps<-5
    #x<-samp[[col1]]
    #y<-samp[[col2]]
    x<-mult.mov.avg(samp[[col1]],radius = marad,preserve.size = T,rep=reps)
    #x<-diff(x)
    y<-mult.mov.avg(samp[[col2]],radius = marad,preserve.size = T,rep=reps)
    #y<-diff(y)
    par(mfrow=c(2,1))
    plot(x,type='l')
    plot(y,type='l')
    par(mfrow=c(1,1))
  }
  {
    n<-23000
    i1<-55000
    i2<-i1+n
    par(mfrow=c(2,1))
    plot(x[i1:i2]*64.3984,type='l')
    plot(y[i1:i2],type='l')
    cor(x[i1:i2],y[i1:i2],use = "na.or.complete")
    
  }
  summary(x[i1:i2]*64.3984)
  summary(y[i1:i2])
  xnew<-x[i1:i2]
  ynew<-y[i1:i2]
  xavg<-mean(x[i1:i2],na.rm = T)
  yavg<-mean(y[i1:i2],na.rm = T)
  yavg/xavg
  lmxy<-lm(ynew~xnew)
  summary(lmxy)
  par(mfrow=c(1,1))
  plot(xnew,ynew)
  plot(lmxy)
  plot(x,y)
  ?lm
  m<-y/x
  summary(y)
  hist(y,breaks = 100)
  ind<- y>51 & y<56 & x<.92 & x>.75
  plot(x[ind],y[ind])
  lmxy<-lm(y[ind]~x[ind])
  summary(lmxy)
  ind.err<-(abs(y-24.26482-36.19603*x)>2)
  plot(x[ind.err],y[ind.err])
  lines(lmxy,color="red")
  # correlation between parameters with different lags
  lag.cor(x,y,lag = seq(-0,500,5),show.graph = T)
  #calculating derivatives
  {
    x<-my.diff(x,radius=3)
    y<-my.diff(y,radius = 3)
  }
  
  # correlation berween derivatives
  lag.cor(x,y,lag = seq(0,500,5),show.graph = T)
  
  lag.cor(y-55.84345,y-55.84345,lag = c(0,500),show.graph = T)
  mean(y)
  #lag.cor(x[2:length(x)],y,max.lag = 25)
  len<-length(x)
  len
  lg<-5
  plot(x[1:(len-lg)],y[(lg+1):len])
  {
    is0<-wtr.data[,2]<100
    i<-1:nrow(wtr.data)
    i<-i[is0]
    x<-wtr.data[i,2]
    i<-i-65
    y<-wtr.data[i,20]
    plot(x[[1]],y[[1]])
    
  }
  
  ysec<-wtr.data[[20]]
  xsec<-wtr.data[[25]]/wtr.data[[19]]
  ind<- ysec>51 & ysec<56 & xsec<.92 & xsec>.75
  plot(xsec[ind],ysec[ind])
  lm1<-lm(ysec[ind]~xsec[ind])
  summary(lm1)
  newi<-abs(ysec[ind]-xsec[ind]*37.31201+23.28856)>1
  plot(xsec[ind][newi],ysec[ind][newi])
}
summary(ysec)
### testing models
model1<-function(xv,xm){
  x<-xm/xv
  y<- -26.4224*x^2+85.8416*x+1.1114
  y[xv<1 & xv>0 &xm<1 & xm>0]<-77.7
  y[xv<1 & xv>0 &xm<0 ]<-0
  y[y<52]<-52
  y[y>77.8]<-77.8
  return(y)
}
model2<-function(xv,xm){
  x<-xm/xv
  y<- 48.79853*x+12.67621
  y[xv<1 & xv>0 &xm<1 & xm>0]<-77.7
  y[xv<1 & xv>0 &xm<0 ]<-0
  y[y<52]<-52
  y[y>77.8]<-77.8
  return(y)
}
{
  xm<-mult.mov.avg(wtr.data[[25]],radius = 5,preserve.size = T,rep=5)
  xv<-mult.mov.avg(wtr.data[[19]],radius = 5,preserve.size = T,rep=5)
  yest<-model2(xv,xm)
  rmse(yest,wtr.data[[20]])
}

plot(yest,type="l")

tempdat<-samp[ind,]
par.plot3(tempdat,col1=col1,col2=col2,col3=col3,progress = 0,len.ratio = 1)

###Cleaning
{
  rowna<-is.na(wtr.data[,2])
  for(i in 3:20){
    rowna<- rowna | is.na(wtr.data[,i])
  }
  summary(rowna)
}

###Test Data Analysis
{
  c1<-2
  c2<-19
  par.plot(wtestdat,col1=c1,col2=c2,progress = 0,len.ratio = 1)
}
wtestdat[1,1]
tail(wtr.data[1])
{
  n<-nrow(wtr.data)
  df<-wtr.data[1:n-1,1]
  df$c2<-diff(wtr.data[[2]])
  df$c3<-diff(wtr.data[[3]])
  df$c4<-diff(wtr.data[[4]])
  df$c5<-diff(wtr.data[[5]])
  df$c6<-diff(wtr.data[[6]])
  df$c7<-diff(wtr.data[[7]])
  df$c8<-diff(wtr.data[[8]])
  df$c9<-diff(wtr.data[[9]])
  df$c10<-diff(wtr.data[[10]])
  df$c11<-diff(wtr.data[[11]])
  df$c12<-diff(wtr.data[[12]])
  df$c13<-diff(wtr.data[[13]])
  df$c14<-diff(wtr.data[[14]])
  df$c15<-diff(wtr.data[[15]])
  df$c16<-diff(wtr.data[[16]])
  df$c17<-diff(wtr.data[[17]])
  df$c18<-diff(wtr.data[[18]])
  df$c19<-diff(wtr.data[[19]])
  df$c20<-diff(wtr.data[[20]])
  df$c25<-diff(wtr.data[[25]])
  df$c29<-diff(wtr.data[[29]])
}
{
  compar.plot(df,8,20)
}
{
  df<-data.frame(target=1)
}
marad<-10
x<-mov.avg(wtr.data[[15]],radius = marad,preserve.size = T)
#x<-diff(x)
y<-mov.avg(wtr.data[[20]],radius = marad,preserve.size = T)
#y<-diff(y)
lag.cor(x,y,max.lag = 25)
lag.cor(x[2:length(x)],y,max.lag = 25)
summary(wtr.data[[20]])
par(mfrow=c(1,1))
boxplot(wtr.data[[20]],ylim=c(50,60))
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
summary(wtr.data[[20]])
