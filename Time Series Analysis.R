
install.packages(readxl)
library(readxl)
spMerge <- read_excel("spMerge.xlsx",sheet = "Sheet1")
View(spMerge)
spData<-spMerge
dim(spData)
names(spData)

spdf<-spMerge



spdf$date
spdf$year<-as.numeric(substring(spdf$date,7,10))


library("YRmisc")

unique(spdf$tkr)


names(spdf)
tsdf<-spdf[spdf$tkr=="AMD",c("tkr","price","eps","bvps","cr","dta","year")]
tsdf
tsdf<-df.sortcol(tsdf,"year",FALSE)
dim(tsdf)
names(tsdf)
tsdf$obs<-1:23
names(tsdf)
tsdf[,2:5]<-round(tsdf[,2:5],2)

par(mfrow=c(3,3))
	hist(tsdf$price,xlab="Price",ylab="Freq",main="Fig. 1 Hist of Price",col="green")		
	hist(tsdf$eps,xlab="Eps",ylab="Freq",main="Fig. 2 Hist of Eps",col="green")
	hist(tsdf$bvps,xlab="Bvps",ylab="Freq",main="Fig. 3 Hist of Bvps",col="green")	
	
par(mfrow=c(3,3))
	ts.plot(tsdf$price,xlab="Time",ylab="Price",main="Fig. 4 TS Plot Price",col="blue")
	ts.plot(tsdf$eps,xlab="Time",ylab="Eps",main="Fig. 5 TS Plot Eps",col="blue")
	ts.plot(tsdf$bvps,xlab="Time",ylab="Bvps",main="Fig. 6 TS Plot Bvps",col="blue")

par(mfrow=c(3,3))
	scatter.smooth(tsdf$eps,tsdf$price,xlab="Price",ylab="Eps",main="Fig. 7 Scatterplot Eps/Price",col="red")
	scatter.smooth(tsdf$bvps,tsdf$price,xlab="Price",ylab="Bvps",main="Fig. 8 Scatterplot Bvps/Price",col="red")
	

#analytical methods
ds.summ(tsdf[,c("price","obs","eps","bvps","cr","dta")],2)
round(cor(na.omit(tsdf[,c("price","obs","eps","bvps","cr","dta")])),3)
fit<-lm(price~eps+bvps+obs,na.action=na.omit,data=tsdf)
summary(fit)

# Residual Plots

par(mfrow=c(3,3))
  hist(fit$residuals,xlab="Price",ylab="Freq",main="Fig.9 Hist of Residules",col='orange')
  scatter.smooth(tsdf$price,fit$fitted.values,xlab="Actual",ylab="Fitted",main="Fig. 10 Scatter Plot of Residules",col = 'orange')
  ts.plot(cbind(tsdf$price, fit$fitted.values),
          col = c("orange", "black"), 
          xlab = "Time", 
          ylab = "Values",
          main = "Fig. 11: TS Plot - Actual vs. Fitted")


  
  install.packages("gam")
  library("gam")
  # nonparametric NON-LINEAR REGRESSION
  fit2<-gam(price~s(eps)+s(bvps)+s(obs),na.action=na.omit,data=tsdf)
  cor(tsdf$price,fit2$fitted.values)^2  # R-squared
  summary(fit2)  # this does not really display useful information
  par(mfrow=c(2,2)); plot(fit2)


