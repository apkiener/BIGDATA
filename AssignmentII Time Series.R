library(fBasics)
library(forecast) 
#Time Series
# Set the folder (path) that contains this R file as the working directory

data <-read.csv("/Users/apkiener/Desktop/Datos CO2.csv")
y<-data[,4]


#### Plotting the original data ####
par(mar=c(3,1))
par(mfrow=c(3,1))
ts.plot(y)

nlags=12     # play with this parameter..

par(mfrow=c(2,1))
acf(y,nlags)
pacf(y,nlags)  


nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences?

z <- diff(y)
ts.plot(z)
par(mfrow = c(2,1))

acf(z)


ndiffs(z, alpha=0.05, test=c("adf"))

#### Plotting the data after taking the differences ####
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

par(mfrow=c(1,1))
ts.plot(fit$residuals)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?


Box.test(fit$residuals,lag=12)
shapiro.test(fit$residuals)  

#### Plotting the first model (0,1,0)x(0,1,2) S
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,2),period=s)) 
fit

par(mfrow=c(1,1))
ts.plot(fit$residuals)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit$residuals,lag=12)
shapiro.test(fit$residuals)

#### Plotting the second model (2,1,0)x(0,1,2) S = 4 ####
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(0,1,2),period=s)) 
fit

par(mfrow=c(1,1))
ts.plot(fit$residuals)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit$residuals,lag=12)
shapiro.test(fit$residuals)  

#### Plotting the third model (2,1,0)x(0,1,1) S = 4 ####
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s)) 
fit

par(mfrow=c(1,1))
ts.plot(fit$residuals)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit$residuals,lag=30)
shapiro.test(fit$residuals)

#### Plotting the fourth model (0,1,0)x(1,1,0) S = 4 ####
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit

par(mfrow=c(1,1))
ts.plot(fit$residuals)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit$residuals,lag=30)
shapiro.test(fit$residuals) 

#### Plotting the fifth model (2,1,0)x(1,1,0) S = 4 ####
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit

par(mfrow=c(1,1))
ts.plot(fit$residuals)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit$residuals,lag=30)
shapiro.test(fit$residuals) 


#### RECURSIVE/WINDOW METHOD #####

#### Make Predictions arima (2,1,0)x(1,1,0)s=4 ####

n<-length(y)
n.estimation<-83 # 
n.forecasting<-n-n.estimation # 198 observations
horizontes<-4 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=2)
MAPE<-matrix(0,nrow=horizontes,ncol=2)

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead,1]<-mean(error^2);
  MAPE[Periods_ahead,1]<-mean(abs(error/real)*100);
}

#### Make Predictions arima (2,1,0)x(0,1,1)s=4 ####

n<-length(y)
n.estimation<-83 # 
n.forecasting<-n-n.estimation # 198 observations
horizontes<-4 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead,2]<-mean(error^2);
  MAPE[Periods_ahead,2]<-mean(abs(error/real)) *100;
}
colnames(MSFE)[1] <- "(2,1,0)x(1,1,0)s=4"
colnames(MAPE)[1] <- "(2,1,0)x(1,1,0)s=4"
colnames(MSFE)[2] <- "(2,1,0)x(0,1,1)s=4"
colnames(MAPE)[2] <- "(2,1,0)x(0,1,1)s=4"

mean(MAPE[,1]);
mean(MAPE[,2]);

colnames(MAPE) <- c("(2,1,0)x(1,1,0)s=4","(2,1,0)x(0,1,1)s=4")
colnames(MSFE) <- c("(2,1,0)x(1,1,0)s=4","(2,1,0)x(0,1,1)s=4")

#### ROLLING METHOD #####

BoxMatrixMAPE <- matrix(0,nrow=59,ncol=3)
colnames(BoxMatrixMAPE) <- c("box","MAPEM1","MAPEM2")

BoxMatrixMSFE <- matrix(0,nrow=59,ncol=3)
colnames(BoxMatrixMSFE) <- c("box","MSFEM1","MSFEM2")

for (boxes in 20:78){
  
  #Load Splits
  
  n<-length(y)
  n.estimation<-83 # 
  n.forecasting<-n-n.estimation # 198 observations
  horizontes<-4 # number of periods ahead
  boxes
  
  #Create Matrix
  
  predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
  real<-matrix(0,nrow=n.forecasting,ncol=1)
  real<-y[(n.estimation+1):length(y)] 
  RMSFE<-matrix(0,nrow=horizontes,ncol=2)
  RMAPE<-matrix(0,nrow=horizontes,ncol=2)
  colnames(RMAPE) <- c("(2,1,0)x(1,1,0)s=4","(2,1,0)x(0,1,1)s=4")
  colnames(RMSFE) <- c("(2,1,0)x(1,1,0)s=4","(2,1,0)x(0,1,1)s=4")
  
#### Make Predictions arima (2,1,0)x(1,1,0)s=4 ####
  
  for (Periods_ahead in 1:horizontes) {
    for (i in 1:n.forecasting) {
      aux.y<-y[(n.estimation-Periods_ahead+i-boxes):(n.estimation-Periods_ahead+i)];
      fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s));
      y.pred<-predict(fit,n.ahead=Periods_ahead);
      predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
    }
    error<-real-predicc[,Periods_ahead];
    RMSFE[Periods_ahead,1]<-mean(error^2);
    RMAPE[Periods_ahead,1]<-mean(abs(error/real)*100);
  }
  
  #Make Predictions arima (2,1,0)x(1,1,0)s=4
  
  n<-length(y)
  n.estimation<-83 # 
  n.forecasting<-n-n.estimation # 198 observations
  horizontes<-4 # number of periods ahead
  
  predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
  real<-matrix(0,nrow=n.forecasting,ncol=1)
  real<-y[(n.estimation+1):length(y)] 
  
  for (Periods_ahead in 1:horizontes) {
    for (i in 1:n.forecasting) {
      aux.y<-y[(n.estimation-Periods_ahead+i-boxes):(n.estimation-Periods_ahead+i)];
      fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s));
      y.pred<-predict(fit,n.ahead=Periods_ahead);
      predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
    }
    error<-real-predicc[,Periods_ahead];
    RMSFE[Periods_ahead,2]<-mean(error^2);
    RMAPE[Periods_ahead,2]<-mean(abs(error/real)) *100;
  }
  
  MeanMAPE1 <-mean(RMAPE[,1]);
  MeanMAPE2 <-mean(RMAPE[,2]);
  MeanMSFE1 <-mean(RMSFE[,1]);
  MeanMSFE2 <-mean(RMSFE[,2]);
  
  BoxMatrixMAPE[(boxes-19),1] <- boxes
  BoxMatrixMAPE[(boxes-19),2] <- MeanMAPE1
  BoxMatrixMAPE[(boxes-19),3] <- MeanMAPE2
  
  BoxMatrixMSFE[(boxes-19),1] <- boxes
  BoxMatrixMSFE[(boxes-19),2] <- MeanMSFE1
  BoxMatrixMSFE[(boxes-19),3] <- MeanMSFE2
  
}

bestboxingModel1 <- BoxMatrixMAPE[order(BoxMatrixMAPE[,2]),][1,]
bestboxingModel2 <- BoxMatrixMAPE[order(BoxMatrixMAPE[,3]),][1,]

bestboxingModel1
bestboxingModel2

# Lets test our best Box Methods for each and compare


n<-length(y)
n.estimation<-83 # 
n.forecasting<-n-n.estimation # 198 observations
horizontes<-4 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
RMSFE<-matrix(0,nrow=horizontes,ncol=2)
RMAPE<-matrix(0,nrow=horizontes,ncol=2)
colnames(RMAPE) <- c("(2,1,0)x(1,1,0)s=4","(2,1,0)x(0,1,1)s=4")
colnames(RMSFE) <- c("(2,1,0)x(1,1,0)s=4","(2,1,0)x(0,1,1)s=4")

#Make Predictions arima (2,1,0)x(1,1,0)s=4

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[(n.estimation-Periods_ahead+i-bestboxingModel1[1]):(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  RMSFE[Periods_ahead,1]<-mean(error^2);
  RMAPE[Periods_ahead,1]<-mean(abs(error/real)*100);
}

#### Make Predictions arima (2,1,0)x(0,1,1)s=4 ####

n<-length(y)
n.estimation<-83 # 
n.forecasting<-n-n.estimation # 198 observations
horizontes<-4 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[(n.estimation-Periods_ahead+i-bestboxingModel2[1]):(n.estimation-Periods_ahead+i)];
    fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  RMSFE[Periods_ahead,2]<-mean(error^2);
  RMAPE[Periods_ahead,2]<-mean(abs(error/real)) *100;
}

MeanMAPE1 <-mean(RMAPE[,1]);
MeanMAPE2 <-mean(RMAPE[,2]);
MeanMSFE1 <-mean(RMSFE[,1]);
MeanMSFE2 <-mean(RMSFE[,2]);

RMAPE
RMSFE
