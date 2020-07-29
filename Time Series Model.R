

data("AirPassengers")
AirPassengers
class(AirPassengers)

head(AirPassengers)
windows(9,5)
plot(AirPassengers,xlab="Year", ylab="Volume", main="Air Passengers")

#Using decompose()
windows(9,5)
tsDecompose <- decompose(AirPassengers,type="multiplicative")
plot(tsDecompose)
tsDecompose

#Splitting into train test set
ts_train <- window(AirPassengers, start=c(1949,1), end=c(1957, 12), freq=12)
head(ts_train)
tail(ts_train)

length(ts_train)

windows(9,5)
plot(ts_train,xlab="Year", ylab="Volume", main="Air Passengers")

ts_test <- window(AirPassengers, start=c(1958,1), freq=12)

windows(9,5)
plot(ts_test,xlab="Year", ylab="Volume", main="Air Passengers")
length(ts_test)
###
tsDecompose_train_log <- stl(log10(ts_train),s.window = 'p')
tsDecompose_train_log

#Random Walk Drift
ts_train_stl <- forecast::forecast(tsDecompose_train_log, method = 'rwdrift', h = 36)
ts_train_stl


windows(9,5)
plot(ts_train_stl)
head(ts_train_stl)
#Accuracy measure
vec2 <- 10^(cbind(log10(ts_test), as.data.frame(forecast::forecast(tsDecompose_train_log, method='rwdrift', h=36))[,1]))
windows(9,5)
ts.plot(vec2,col=c('blue','red'),main='Air Passenger: Actual vs Forecast')

RMSE2 <- round(sqrt(sum(((vec2[,1]-vec2[,2])^2)/length(vec2[,1]))),4)
MAPE2 <- round(mean(abs(vec2[,1]-vec2[,2])/vec2[,1]),4)
paste("Accuracy Measures: RMSE:", RMSE2, "and MAPE", MAPE2)

#Single exponential Smoothing
ts_train_SES <- forecast::ses(log10(ts_train), h=36, initial='simple', seasonal = 'multiplicative')
windows(8,6)
plot(ts_train_SES)

#Holt Winter's Smoothing
ts_train_HW <- HoltWinters(ts_train, seasonal = 'multiplicative')
#?HoltWinters
windows(9,5)
plot(ts_train_HW)

windows(9,5)
plot(forecast::forecast(ts_train_HW, h=36))

#Accuracy Measures
vec <- cbind(ts_test, as.data.frame(forecast::forecast(ts_train_HW, method = 'Holtwinters', h=36))[,1])
windows(9,5)
ts.plot(vec,col=c('blue','red'),main='Air Passenger: Actual vs Forecast')

RMSE2 <- round(sqrt(sum(((vec[,1]-vec[,2])^2)/length(vec[,1]))),4)
MAPE2 <- round(mean(abs(vec[,1]-vec[,2])/vec[,1]),4)
paste("Accuracy Measures: RMSE:", RMSE2, "and MAPE", MAPE2)

split.screen(figs=c(2,2))
screen(1)
plot(ts_train,xlab="Year", ylab="Volume", main="Air Passengers")
screen(2)
plot(ts_train_stl)
screen(3)
plot(forecast::ses(log10(ts_train), h=36, initial='simple', seasonal = 'multiplicative'))
screen(4)
plot(forecast::forecast(ts_train_HW, h=36))

#According to the Model accuracy measure, the Random Walk drift was the best forecast for the model
#With RMSE: 31.3836 and MAPE: 0.065
