# PEMBACAAN DATA
setwd("S:/kuliah/classification_cleaned_data/daming_project")
data_set <- read.csv("train.csv", header = TRUE, sep = ",")
mobile_price <- data.frame(data_set)

# EKSPLORASI DATA
summary(mobile_price)
View(mobile_price)
str(mobile_price)

# PRAPROSES
price_range <- c(0,1,2,3)
col_order <- c(2,4,6,18:20,1,3,5,7:17,21)

mobile_price <- mobile_price[with(mobile_price, order(price_range)), ]
mobile_price <- mobile_price[,col_order]

## kategorisasi kolom 1-6 dan kelas
mobile_price$blue <- as.factor(mobile_price$blue)
mobile_price$dual_sim <- as.factor(mobile_price$dual_sim)
mobile_price$four_g <- as.factor(mobile_price$four_g)
mobile_price$three_g <- as.factor(mobile_price$three_g)
mobile_price$touch_screen <- as.factor(mobile_price$touch_screen)
mobile_price$wifi <- as.factor(mobile_price$wifi)
mobile_price$price_range <- as.factor(mobile_price$price_range)

categorized <- mobile_price
View(categorized)

# REGRESI px_height ~ px_width
categorized[categorized$px_height < 100, 16:15]
more_than_100 <- which(categorized$px_height >= 100)

y <- categorized$px_height[more_than_100] # px_height
x <- categorized$px_width[more_than_100] # px_width

scatter.smooth(x=x, y=y, main="y ~ x")

par(mfrow=c(1, 2))  # divide graph area in 2 columns

boxplot(x, main="x", sub=paste("Outlier rows: ", boxplot.stats(x)))
boxplot(y, main="y", sub=paste("Outlier rows: ", boxplot.stats(y)))

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(x), main="Density Plot: px_width", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(x), 2)))
plot(density(y), main="Density Plot: px_height", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(y), 2)))

cor(x, y)
height_width <- lm(y ~ x)
print(height_width) # height = 0.522*width + 31.858

less_than_100 <- which(mobile_price$px_height < 100)
categorized$px_height[less_than_100] = round(0.522*(categorized$px_width[less_than_100]) + 31.858)
categorized[less_than_100, 15:16]

# REGRESI sc_w ~ sc_h
categorized[categorized$sc_w < 4, 18:19]
more_than_4 <- which(categorized$sc_w >= 4)
y <- categorized$sc_w[more_than_4]
x <- categorized$sc_h[more_than_4]

scatter.smooth(x=x, y=y, main="y ~ x")

par(mfrow=c(1, 2))  # divide graph area in 2 columns

boxplot(x, main="x", sub=paste("Outlier rows: ", boxplot.stats(x)))  # box plot for 'speed'
boxplot(y, main="y", sub=paste("Outlier rows: ", boxplot.stats(y)))

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(x), main="Density Plot: px_width", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(x), 2)))  # density plot for 'speed'
polygon(density(x), col="blue")
plot(density(y), main="Density Plot: px_height", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(y), 2)))  # density plot for 'dist'
polygon(density(y), col="green")

cor(x, y)
w_h <- lm(y ~ x)
print(w_h) # w = 0.5272*h + 1.2627

less_than_4 <- which(categorized$sc_w < 4)
categorized$sc_w[less_than_4] = round(0.5272*categorized$sc_h[less_than_4] + 1.2627)
categorized[less_than_4, 18:19]

# PRAPROSES (lanj.)
#binning
library(infotheo)
binned <- categorized
binned$battery_power <- cut(mobile_price$battery_power, 15, include.lowest = TRUE)
binned$clock_speed <- cut(mobile_price$clock_speed, 5, include.lowest = TRUE)
binned$fc <- cut(mobile_price$fc, 5, include.lowest = TRUE)
binned$int_memory <- cut(mobile_price$int_memory, 10, include.lowest = TRUE)
binned$m_dep <- cut(mobile_price$m_dep, 5, include.lowest = TRUE)
binned$mobile_wt <- cut(mobile_price$mobile_wt, 10, include.lowest = TRUE)
binned$n_cores <- cut(mobile_price$n_cores, 4, include.lowest = TRUE)
binned$pc <- cut(mobile_price$pc, 5, include.lowest = TRUE) #
binned$px_height <- cut(mobile_price$px_height, 5, include.lowest = TRUE) # kok ada 0 nya?
binned$px_width <- cut(mobile_price$px_width, 10, include.lowest = TRUE)
binned$ram <- cut(mobile_price$ram, 10, include.lowest = TRUE)
binned$sc_h <- cut(mobile_price$sc_h, 5, include.lowest = TRUE)
binned$sc_w <- cut(mobile_price$sc_w, 5, include.lowest = TRUE) # kok ada 0 nya?
binned$talk_time <- cut(mobile_price$talk_time, 6, include.lowest = TRUE)

View(binned)
str(binned)
