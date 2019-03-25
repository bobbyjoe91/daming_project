# PEMBACAAN DATA
setwd("S:/kuliah/daming_project")
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

categorized <- mobile_price
summary(categorized)
View(categorized)

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

#test_data
#train_data

