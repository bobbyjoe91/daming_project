# PEMBACAAN DATA
setwd("~/Documents/Daming/Project")
data_set <- read.csv("train.csv", header = TRUE, sep = ",")
mobile_price <- data.frame(data_set)

View(mobile_price)

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

summary(mobile_price)
#test_data
#train_data

