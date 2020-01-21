library('readxl')
library('lattice')
library('Rmisc')
library('plyr')
library('grid')
library('ggplot2')
library('ggfortify')
require(grid)
a = read_xlsx('C:\\Users\\Administrator\\Desktop\\CME_Mstock3.xlsx')
a <- as.data.frame(a)
# hist(a$Esm0301,
#      col='red',
#      xlab = 'P/E',
#      main = '农林牧渔业市盈率')
# lines(density(a$Esm0301),col='blue',lwd=1)
# 
# dotchart(a$Esm0301,labels = row.names(a),cox=.7)
mean <- apply(a[4:16],2,mean)
mean
var <- apply(a[4:16], 2, var)
var
max <- apply(a[,4:16], 2, max)
max
min <- apply(a[,4:16], 2, min)
min
d <-as.table(summary(a[4:16]))
d
e <- as.data.frame(d)


par(mfrow=c(1,2))
box1<- boxplot(a$Esm0301,main='农林牧渔业',ylab='P/E',col='gray')
box2<- boxplot(a$Esm0302,main='采掘业',ylab='P/E',col='gray')
box3<- boxplot(a$Esm0303,main='制造业',ylab='P/E',col='gray')
box4<- boxplot(a$Esm0304,main='电力、煤气及水的生产和供应业',ylab='P/E',col='gray')
boxplot(a$Esm0305,main='建筑业',ylab='P/E',col='gray')
boxplot(a$Esm0306,main='交通运输、仓储业',ylab='P/E',col='gray')
boxplot(a$Esm0307,main='信息技术业',ylab='P/E',col='gray')
boxplot(a$Esm0308,main='批发和零售贸易业',ylab='P/E',col='gray')
boxplot(a$Esm0309,main='金融、保险业',ylab='P/E',col='gray')
boxplot(a$Esm0310,main='房地产业',ylab='P/E',col='gray')
boxplot(a$Esm0311,main='社会服务业',ylab='P/E',col='gray')
boxplot(a$Esm0312,main='传播与文化产业',ylab='P/E',col='gray')
boxplot(a$Esm0313,main='综合类',ylab='P/E',col='gray')

# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2,2)))
# vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)} 
# print(box1,vp = vplayout(1,1:2))


c<- colnames(a)
mydataframe <-data.frame(mean,var,max,min)
mydataframe

# 构造出一个新的便于ggplot的dataframe
f <- c(a$Esm0301,a$Esm0302,a$Esm0303,a$Esm0304,a$Esm0305,a$Esm0306,a$Esm0307,a$Esm0308,a$Esm0309,a$Esm0310,a$Esm0311,a$Esm0312,a$Esm0313)
marker1 <- rep('农林牧渔业',22)
marker2 <- rep('采掘业',22)
marker3 <- rep('制造业',22)
marker4 <- rep('电力、煤气及水的生产和供应业',22)
marker5 <- rep('建筑业',22)
marker6 <- rep('交通运输、仓储业',22)
marker7 <- rep('信息技术业',22)
marker8 <- rep('批发和零售贸易业',22)
marker9 <- rep('金融、保险业',22)
marker10 <- rep('房地产业',22)
marker11 <- rep('社会服务业',22)
marker12 <- rep('传播与文化产业',22)
marker13 <- rep('综合类',22)

marker <- c(marker1, marker2, marker3,marker4,marker5,marker6,marker7,marker8,marker9,marker10,marker11,marker12,marker13)
period <- 1:22
g <- data.frame(period,f,marker,stringsAsFactors = TRUE)

ggplot(data = g,aes(x=period,y=f, fill=marker,colour=factor(marker))) + geom_point(show.legend = FALSE)+geom_line()
# newa <- t(a)
# newa <- as.data.frame(newa)
# k <- newa[4:16,1:22]
# k <- as.data.frame(k)
# rownames(k)=NULL



