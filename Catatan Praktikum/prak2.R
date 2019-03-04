setwd("~/Desktop")
insurance <- read.csv(file="insurance.csv",sep=",",header=TRUE)

# ringkasan data
str(insurance)

# summary data
summary(insurance)

# boxplot
boxplot(insurance$bmi ~ insurance$smoker, xlab="bmi")

# histogram
hist(insurance$bmi, xlab="bmi", main="distribution of bmi")

# bar chart utk mendeskripsikan frekuensi data kategorik
# histogram utk melihat distibusi frekuensi data numerik

# scatterplot sederhana
plot(x=insurance$bmi, y=insurance$charges)

# scatterplot dengan jitter
plot(jitter(insurance$bmi), jitter(insurance$charges))

# scatterplot multivariable
with(insurance, plot(x=insurance$bmi, y=insurance$charges, col=insurance$smoker, pch=as.numeric(insurance$smoker)))

# scatterplot matriks
pairs(insurance)

# scatterplot 3d
install.packages("scatterplot3d")
library("scatterplot3d")
scatterplot3d(insurance$age, insurance$bmi, insurance$charges)

# scatterplot 3d interaktif
install.packages("rgl")
library("rgl")
plot3d(insurance$age, insurance$bmi, insurance$charges)

# scatterplot with ggplot2
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)

# plot sebaran 'bmi'
plot(density(insurance$bmi))

# kovarian dan korelasi
cov(insurance$bmi, insurance$charges)
cor(insurance$bmi, insurance$charges)

# aggregate
aggregate(insurance$bmi ~ insurance$smoker, summary, data=insurance)

# I R I S
# heat map
distMatrix <- as.matrix(dist(iris[,1:4]))
heatmap(distMatrix)

# level plot
install.packages("lattice")
library(lattice)
levelplot(Petal.Width ~ Sepal.Length*Sepal.Width, data=iris, cuts=9, col.regions=rainbow(10))

# contour
filled.contour(volcano, color=terrain.colors, asp=1, plot.axes=contour(volcano, add=T))
View(volcano)

# 3d surface
persp(volcano, theta = 25, phi = 30, expand = 0.5, col="green")

# Paralell coordinates
library(MASS)
parcoord(iris[1:4], col=iris$Species)

library(lattice)
parallelplot(~iris[,1:4] | Species, data=iris)
