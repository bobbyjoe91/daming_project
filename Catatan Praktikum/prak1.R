#buat data
var1 <- 1:5
var2 <- (1:5)/10
var3 <- c("Kuda", "Ayam", "Kambing", "Sapi", "Kocheeng")
dataset <- data.frame(var1, var2, var3)
rownames(dataset) <- c("a", "b", "c", "d", "e")
colnames(dataset) <- c(1, 2, 3)
View(dataset)

# VISUALISASI DATA

## pie chart
JumlahMobil <- c(1, 2, 5, 7, 9)
warna <- c("blue", "red", "green", "yellow", "purple") # warna2
labelmobil <- round(JumlahMobil/sum(JumlahMobil)*100,1) # Persentase
labelmobil <- paste(labelmobil, "%", sep = "") # convert ke string
# pie(x = data, main = judul, col = vector dari warna, labels = persentase, cex = ukuran labels dari 0 - 1)
pie(JumlahMobil, main = "presentase mobil di parkiran", col = warna, labels = labelmobil, cex = 1) # buat pie
legend(1.3, 0.5, JumlahMobil, cex = 0.7, fill = warna) # legenda
# notes: data, warna, label harus pd indeks yg sama

## line chart
cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)

#hitung range dari 0 ke nilai max dari cars dan trucks
g_range <- range(0, cars, trucks) # hasilnya g_range = [0, 12] dgn 0 nilai min dan 12 nilai max

plot(cars, type="o", col="red", ylim=g_range, axes=FALSE, ann=FALSE)

axis(1, at=1:5, lab=c("Mon", "Tue", "Wed", "Thu", "Fri"))

axis(2, las=1, at=4*0:g_range[2])

box()

lines(trucks, type="o", pch=20, lty = 1, col="blue")

title(main="autos", col.main="red", font.main = 4)

title(xlab="Days", col.lab=rgb(0, 0.5, 0))
title(ylab="Total", col.lab=rgb(0, 0.5, 0))

legend(1, g_range[2], c("Cars", "Trucks"), cex=0.8, col=c("blue", "red"), pch=21:22, lty=1:2)

library(lattice) # add library 'lattice'
attach(iris) # attach 'iris' dataset
View(iris) # view table of dataset
summary(iris) # view data summary

# S C A T T E R P L O T
# draw pairwise comparison of column 1-4
# based on column 5 with 3 colors
splom( ~ iris[,1:4], col = c("black","blue","red")[iris[,5]])


# B O X P L O T
boxplot(Sepal.Length ~ Species, data = iris, main="B O X P L O T")
boxplot(Sepal.Width ~ Species, data = iris, main="B O X P L O T")
