library(Mcomp)
library(ggplot2)
all_data <- subset(M3, 'quarterly')[[300]]
all_data
train <- all_data$x
test <- all_data$xx

str(all_data)
# exploratory analysis
##########################################################################

# autoplot
autoplot(all_data) + ylab('Volume Indeces') + xlab('Year') + ggtitle('SWITZERLAND Gross Domestic Product')
ggsave(width = 2500, height = 1500, path = 'workspace/project/', filename = 'autoplot.jpeg',  device='jpeg')

# seasonal plot
ggseasonplot(train, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Volume index") +
  ggtitle("Seasonal plot: SWITZERLAND Gross Domestic Product")
ggsave(width = 2500, height = 1500, path = 'workspace/project/', filename = 'seasonal.jpeg',  device='jpeg')

# seasonal subseries plot
ggsubseriesplot(train) + ylab("Volume index") + ggtitle("Seasonal subseries plot")
ggsave(width = 2500, height = 1500, path = 'workspace/project/', filename = 'subseriesplot.jpeg',  device='jpeg')

# autocorrelation plot
ggAcf(train) + ggtitle("Autocorrelation plot")
ggsave(width = 2500, height = 1500, path = 'workspace/project/', filename = 'autocorrelation.jpeg',  device='jpeg')

# lag plot
gglagplot(train)+ ylab("Volume index") + ggtitle("Lag Plot")
ggsave(width = 2500, height = 1500, path = 'workspace/project/', filename = 'lagplot.jpeg',  device='jpeg')
##########################################################################
# Indicators?




# simple models
##########################################################################
fits <- fitted(naive(train))

autoplot(train, series="Data") +
  autolayer(fits, series="Fitted") +
  xlab('Year') + ylab('Volume Indeces') +
  ggtitle("SWITZERLAND Gross Domestic Product")

res <- residuals(naive(train))

# ACF of residuals
ggAcf(res) + ggtitle("ACF of residuals")
ggsave(width = 2500, height = 1500, path = 'workspace/project/', filename = 'acf_of_residuals.jpeg',  device='jpeg')

# Histogram of residuals
gghistogram(res, add.normal=TRUE) + ggtitle("Histogram of residuals")
ggsave(width = 2500, height = 1500, path = 'workspace/project/', filename = 'histogram_of_residuals.jpeg',  device='jpeg')

# Ljung-Box test
Box.test(res, lag=10, fitdf=0, type="Lj")

##########################################################################

# exponential smoothing
##########################################################################


##########################################################################

