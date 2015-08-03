# Cousera Data Visualization Week 2.
# by Cristina Moody
# cimoody@purdue.edu  


## Libraries to install and source (so it is done once) for VNL
# Installing Gaussian Process Library
install.packages("tgp");
library(tgp);
# Installing package to compare the two data tables
install.packages("compare");
library(compare);
install.packages("plyr");
library(plyr);
install.packages("Hmisc", dependencies = T);
library(Hmisc);
# Package for classification
library(rpart);
install.packages('rattle');
library(rattle);
install.packages('rpart.plot');
library(rpart.plot);
# install.packages('RColorBrewer');
library(RColorBrewer);
install.packages('randomForest');
library(randomForest);
install.packages('party');
library(party);
# Additional package for categorical graphics
install.packages("vcd");
library(vcd);
library(lmtest);
library(sandwich);
library(car);
library(zoo);
library(scales);
install.packages('matrixStats');
library(matrixStats);
####

wDir <- getwd();
dDir <- sprintf("%s%s", wDir, "/");
listfiles <- list.files(path = dDir, pattern = "\\.csv");
data2 <- read.csv(listfiles[1], header = TRUE, stringsAsFactors = FALSE);
data1 <- read.csv(listfiles[2], header = TRUE, stringsAsFactors = FALSE);
data1$Jun <- as.numeric(data1$Jun);
data1$Jul <- as.numeric(data1$Jul);
data1$Aug <- as.numeric(data1$Aug);
data1$Sep <- as.numeric(data1$Sep);
data1$Oct <- as.numeric(data1$Oct);
data1$Nov <- as.numeric(data1$Nov);
data1$Dec <- as.numeric(data1$Dec);
data1$J.D <- as.numeric(data1$J.D);
data1$D.N <- as.numeric(data1$D.N);
data1$DJF <- as.numeric(data1$DJF);
data1$MAM <- as.numeric(data1$MAM);
data1$JJA <- as.numeric(data1$JJA);
data1$SON <- as.numeric(data1$SON);
plot(DJF ~ Year, data = data1, type = 'l', col = 'blue')
points(SON ~ Year, data = data1, type = 'l', col = 'purple')
points(JJA ~ Year, data = data1, type = 'l', col = 'orange')
points(MAM ~ Year, data = data1, type = 'l', col = 'dark green')

intVars <- c("Year", "DJF", "MAM", "JJA", "SON")
data <- data1[intVars]
data$YearlyMean <- rowMeans(data[,2:5], na.rm = TRUE);
data$YearlyMean2 <- rowMeans(data1[,2:13], na.rm = TRUE);

m <- transform(as.matrix(data[,2:5]), SD=rowSds(as.matrix(data[,2:5]), na.rm=TRUE));
data$SD <- m$SD
m2 <- transform(as.matrix(data1[,2:13]), SD=rowSds(as.matrix(data1[,2:13]), na.rm=TRUE));
data$SD2 <- m2$SD

par(mfrow=c(5,1));
plot(YearlyMean ~ Year, data = data, col = 'purple', type = 'l', ylim = range(-80:100), 
     ylab = "", xlab = "", xlim = range(1880:2015), lwd = 3)
points(YearlyMean2 ~ Year, data = data, col = 'orange', type = 'l', ylim = range(-80:100), 
       ylab = "", xlab = "", xlim = range(1880:2015), lwd = 2)
legend(x = "topleft",inset =0, bty='n',
       legend = c("Year Mean by Quarter", "Yearly Mean by Month"),
       col=c("purple", "orange"), lwd=5, cex=0.6, horiz = TRUE)
plot(DJF ~ Year, data = data, col = 'cyan', type = 'l', ylim = range(-80:100), 
     ylab = "", xlab = "", xlim = range(1880:2015), lwd = 2)
legend(x = "topleft",inset =0,bty='n',
       legend = c("Dec-Feb"),
       col=c("cyan"), lwd=5, cex=0.6, horiz = TRUE)
plot(MAM ~ Year, data = data, col = 'blue', type = 'l', ylim = range(-80:100), 
     ylab = "Deviation, 1/100 Deg. C", xlab = "", xlim = range(1880:2015), lwd = 2)
legend(x = "topleft",inset =0, bty='n',
       legend = c("Mar-May"),
       col=c("blue"), lwd=5, cex=0.6, horiz = TRUE)
plot(JJA ~ Year, data = data, col = 'dark green', type = 'l', ylim = range(-80:100), 
     ylab = "", xlab = "", xlim = range(1880:2015), lwd = 2)
legend(x = "topleft",inset =0, bty='n',
       legend = c("June-Aug"),
       col=c("dark green"), lwd=5, cex=0.6, horiz = TRUE)
plot(SON ~ Year, data = data, col = 'black', type = 'l', ylim = range(-80:100), 
     ylab = "", xlab = "Year", xlim = range(1880:2015), lwd = 2)
legend(x = "topleft",inset =0, bty='n',
       legend = c("Sept-Nov"),
       col=c("black"), lwd=5, cex=0.6, horiz = TRUE)
# 
# legend(x = "topleft",inset =0,
#        legend = c("Year Mean by Quarter", "Yearly Mean by Month", "Dec-Feb","Mar-May", "June-Aug", "Sept-Nov"),
#        col=c("purple", "orange", "cyan", "blue", "dark green", "black"), lwd=5, cex=0.6, horiz = TRUE)

