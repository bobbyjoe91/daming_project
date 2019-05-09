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

scatter.smooth(x=x, y=y, main="px_height ~ px_width")

par(mfrow=c(1, 2))  # divide graph area in 2 columns

boxplot(x, main="px_width")
boxplot(y, main="px_height")

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

par(mfrow=c(1, 1))
scatter.smooth(x=x, y=y, main="sc_w ~ sc_h")

par(mfrow=c(1, 2))  # divide graph area in 2 columns

boxplot(x, main="sc_h")  # box plot for 'speed'
boxplot(y, main="sc_w")

cor(x, y)
w_h <- lm(y ~ x)
print(w_h) # sc_w = 0.5272*sc_h + 1.2627

less_than_4 <- which(categorized$sc_w < 4)
categorized$sc_w[less_than_4] = round(0.5272*categorized$sc_h[less_than_4] + 1.2627)
categorized[less_than_4, 18:19]

# KLASIFIKASI
library(rpart)
library(rpart.plot)
#library(party)
#set.seed(1234)
index <- sample(2, nrow(binned), replace=TRUE, prob=c(0.7,0.3))
train <- categorized[index==1, ]
test <- categorized[index==2, ]

phone_ctree <- rpart(price_range ~., train)
#phone_ctree <- ctree(model, data=train)

# cetak model
print(phone_ctree)
rpart.plot(phone_ctree, type=3, cex=0.5) #masih hrs diatur lg...
text(phone_ctree, use.n=TRUE)

# testing
result <- predict(phone_ctree)
conf_matrix <- table(colnames(result)[max.col(result,ties.method='first')], train$price_range)
plot(conf_matrix)
str(conf_matrix)

# accuracy
library(caret)
confusionMatrix(conf_matrix)
