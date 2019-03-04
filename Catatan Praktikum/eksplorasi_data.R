# Input data
data <- airquality
View(data)

# cek apakah missing value
is.na(data)

# summary data
str(data)
summary(data)

# distribusi missing value
install.packages("mice")
library(mice)
md.pattern(data)

# isi missing value dengan rataan
data$Solar.R[is.na(data$Solar.R)] <- mean(data$Solar.R, na.rm = TRUE)
View(data)

# isi missing value dgn modus
mode = names(sort(-table(data$Ozone)))[1]
data$Ozone[is.na(data$Ozone)] <- mode
View(data)

which(data$Ozone == 23) # mengembalikan index baris
which(data == 223) # mengembalikan index yg dihitung scr. berurutan dari atas ke bawah per kolom

# diskretisasi data
# fungsi cut
# 
# install.packages("infotheo")
# library(infotheo)
# fungsi discretize
#
# install.packages("discretization")
# library(discretization)
