library(Mcomp)
library(ggplot2)
all_data <- subset(M3, 'quarterly')[[300]]
all_data
train <- all_data$x
test <- all_data$xx

str(all_data)

# autoplot
autoplot(all_data) + ylab('Volume Indeces') + xlab('Year') + ggtitle('SWITZERLAND Gross Domestic Product')


# seasonal plot
ggseasonplot(train, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Volume index") +
  ggtitle("Seasonal plot: SWITZERLAND Gross Domestic Product")

# seasonal subseries plot
