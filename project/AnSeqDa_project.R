library(Mcomp)
library(ggplot2)
library(MASS)
set.seed(123)
all_data <- subset(M3, 'quarterly')[[300]]
all_data
train <- all_data$x
test <- all_data$xx

str(all_data)
# exploratory analysis
##########################################################################

# autoplot
autoplot(all_data) + ylab('Volume Indeces') + xlab('Year') + ggtitle('SWITZERLAND Gross Domestic Product')
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'autoplot.jpeg',  device='jpeg')

# seasonal plot
ggseasonplot(train, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Volume index") +
  ggtitle("Seasonal plot: SWITZERLAND Gross Domestic Product")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'seasonal.jpeg',  device='jpeg')

# seasonal subseries plot
ggsubseriesplot(train) + ylab("Volume index") + ggtitle("Seasonal subseries plot")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'subseriesplot.jpeg',  device='jpeg')

# autocorrelation plot
ggAcf(train) + ggtitle("Autocorrelation plot")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'autocorrelation.jpeg',  device='jpeg')

# lag plot
gglagplot(train)+ ylab("Volume index") + ggtitle("Lag Plot")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'lagplot.jpeg',  device='jpeg')
##########################################################################
# Indicators?

# last measurement
length(test)
last_train <- as.numeric(tail(train, n=1))
last_train
max_mae <- last_train * 0.05
max_mae
max_rsme <-  sqrt((8*(last_train - (last_train * 1.05))^2)/8)
max_rsme


# simple models
##########################################################################
#train_decompose <- decompose(train)
#train_decompose$trend
#rw_drift_mean <- mean(na.omit(train_decompose$trend))-as.numeric(head(train, n = 1))
#rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = rw_drift_mean, sd=sd(train))

#rw_drift <- arima.sim(model= list(order = c(0, 1, 0)), n=100, mean=1)
#plot(resid(rw_drift))
#ts.plot(rw_drift)
#ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'rw_drift.jpeg',  device='jpeg')

plot.ts(RW_drift_diff, col=4, main="First Order Difference")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'rw_drift_first_order_diff.jpeg',  device='jpeg')

rw_drift_diff <-diff(rw_drift)
ggAcf(rw_drift_diff) + ggtitle("Autocorrelation plot")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'acf_rw_drift.jpeg',  device='jpeg')




fits <- fitted(naive(train))

autoplot(train, series="Data") +
  autolayer(fits, series="Fitted") +
  xlab('Year') + ylab('Volume Indeces') +
  ggtitle("SWITZERLAND Gross Domestic Product")

res <- residuals(rw_drift(train))

# ACF of residuals
ggAcf(res) + ggtitle("ACF of residuals")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'acf_of_residuals.jpeg',  device='jpeg')

# Histogram of residuals
gghistogram(res, add.normal=TRUE) + ggtitle("Histogram of residuals")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'histogram_of_residuals.jpeg',  device='jpeg')

# Ljung-Box test
Box.test(res, lag=10, fitdf=0, type="Lj")

##########################################################################
# exponential smoothing

### SES
ses_fc <- ses(train, h=5)
summary(ses_fc)
ses_resid <- resid(ses_fc)
plot(ses_resid)
ggAcf(ses_resid)

### Holt lin
holt_fc <- holt(train, h=5)
summary(holt_fc)
holt_resid <- resid(holt_fc)
plot(holt_resid)
ggAcf(holt_resid)

### Holt dampend
holt_d_fc <- holt(train, damped = TRUE, h=5)
summary(holt_d_fc)
holt_d_resid <- resid(holt_d_fc)
plot(holt_d_resid)
ggAcf(holt_d_resid)

###### ETS
?ets
ets_holt <- ets(y = train, model = "AAN", damped = FALSE)
ets_holt
plot(resid(ets_holt))
ggAcf(resid(ets_holt))
gghistogram(resid(ets_holt), add.normal=TRUE) + ggtitle("Histogram of residuals")

ets_holt_d <- ets(y = train, model = "AAN", damped = TRUE)
ets_holt_d
plot(resid(ets_holt_d))
ggAcf(resid(ets_holt_d))
gghistogram(resid(ets_holt_d), add.normal=TRUE) + ggtitle("Histogram of residuals")

##########################################################################
# ARIMA models

##########################################################################

