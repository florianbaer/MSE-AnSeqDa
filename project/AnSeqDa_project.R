library(Mcomp)
library(ggplot2)
library(MASS)
library(xtable)
library(forecast)
library(ggpubr)

set.seed(42)
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
rw_model <- rwf(train, drift=TRUE, h=4)
rw_resid <- residuals(rw_model)

ggarrange(autoplot(rw_resid) + ylab("Residuals") + ggtitle("Residuals from a random walk with drift model"), 
          ggAcf(rw_resid) + ggtitle("Autocorrelation plot"), 
          forecast::gghistogram(rw_resid, add.normal=TRUE) + ggtitle("Histogram of residuals")+ xlab("Residuals"), 
          widths = c(2, 1, 1),
          ncol = 3, nrow = 1)
ggsave(unit= "px", width = 3500, height = 1500, path = 'workspace/project/', filename = 'simple_model_resid.jpeg',  device='jpeg')

autoplot(rw_resid) + ylab("Residuals") + ggtitle("Residuals from a random walk with drift model")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'resid_from_rw_drift.jpeg',  device='jpeg')

ggAcf(rw_resid) + ggtitle("Autocorrelation plot")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'acf_rw_drift.jpeg',  device='jpeg')

forecast::gghistogram(rw_resid, add.normal=TRUE) + ggtitle("Histogram of residuals")
ggsave(unit= "px", width = 2500, height = 1500, path = 'workspace/project/', filename = 'hist_resid.jpeg',  device='jpeg')

# Ljung-Box test
Box.test(rw_resid, lag=10, fitdf=0, type="Lj")

# Eval on train and test set
acc <- xtable(accuracy(rw_model, test))
acc
xtable::print.xtable(acc, type = "latex", file = "metrics.tex")
##########################################################################
# exponential smoothing

### SES
ses_fc <- ses(train, h=5)
summary(ses_fc)
ses_resid <- resid(ses_fc)

autoplt <- autoplot(ses_resid)
acf <- ggAcf(ses_resid)
hist <- forecast::gghistogram(ses_resid)

ggarrange(autoplt, ggarrange(acf, hist, ncol=2, nrow=1),
          ncol = 1, nrow = 2)

### Holt lin
holt_fc <- holt(train, h=5)
summary(holt_fc)
holt_resid <- resid(holt_fc)

autoplt <- autoplot(holt_resid)
acf <- ggAcf(holt_resid)
hist <- forecast::gghistogram(holt_resid)

ggarrange(autoplt, ggarrange(acf, hist, ncol=2, nrow=1),
          ncol = 1, nrow = 2)

### Holt dampend
holt_d_fc <- holt(train, damped = TRUE, h=5)
summary(holt_d_fc)
holt_d_resid <- resid(holt_d_fc)

autoplt <- autoplot(holt_d_resid)
acf <- ggAcf(holt_d_resid)
hist <- forecast::gghistogram(holt_d_resid)
ggarrange(autoplt, ggarrange(acf, hist, ncol=2, nrow=1),
          ncol = 1, nrow = 2)

###### ETS
?ets
ets_holt <- ets(y = train, model = "AAN", damped = FALSE)
ets_holt
plot(resid(ets_holt))
ggAcf(resid(ets_holt))
forecast::gghistogram(resid(ets_holt), add.normal=TRUE) + ggtitle("Histogram of residuals")

ets_holt_d <- ets(y = train, model = "AAN", damped = TRUE)
ets_holt_d
plot(resid(ets_holt_d))
ggAcf(resid(ets_holt_d))
forecast::gghistogram(resid(ets_holt_d), add.normal=TRUE) + ggtitle("Histogram of residuals")

##########################################################################
# ARIMA models
#autoplot of diff log 
autoplot(diff(log(train)))

auto_arima <- auto.arima(train)
summary(auto_arima)
autoplot(forecast(auto_arima))

arima_resid <- resid(auto_arima)

autoplt <- autoplot(arima_resid)
acf <- ggAcf(arima_resid)
hist <- forecast::gghistogram(arima_resid)
ggarrange(autoplt, ggarrange(acf, hist, ncol=2, nrow=1),
          ncol = 1, nrow = 2)

##########################################################################

