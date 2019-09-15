#generating a binomial distribution plot
rbinom(30,100,0.6)
n <- 1000
obs <- rbinom(n, 1, 1/2)
sm_obs <- rep(0:n)   #generate a vector to preempt


for (i in 1:n){
  sm_obs[i]<-sum(obs[1:i])/i
}

plot(1:n, sm_obs, type = "1", xlab = "n",
     ylab = "(Y1+Y2+... Yn/n)",
     abline(0.5,0,lty = 2))

x <- matrix(1:100,10,10)
y <- matrix(1:100,10,10)

n <- 10
matmul <- function(x,y){
  n<- nrow(x)
  z <- matrix(rep(0,n^2),n,n)
  for (i in 1:n){
    for(j in 1:n){
      for (k in 1:n){
        z[i,j]<-z[i,j]+x[i,k]*y[k,j]
      }
    }
  }
  return(z)
}

matmul(x,y)


jpeg("mygraphy")
attach(mtcars)
#col means colors 
plot(wt, mpg, col=1)
# abline is for plotting the line
abline(lm(mpg~wt),col = 2)
title("This is a title")
detach(mtcars)
dev.off()

dose <- c(20,30,40,65,21)
drugA <- c(45,91,45,65,89)
drugB <- c(15, 18, 25, 31, 40)

plot(dose, drugA, type = "b")

opar <- par(no.readonly=TRUE)
#par is used to set parameters of the graph
# pch=?  the type of a symbol
# cex=? the magnitude of a symbol
# lty=?  the type of a line
# lwd=? the width of a line
# pin = c(a,b) setting the length and the width of the graph
# mai=c(a, b, d, f): the marginal size of a graph in inch, with bottom=a, left=b, up=d, right=f
# font= 1,2,3,4,5
# font.axis
# font.lab
# font.main 
# font.sub
# ps is the size of the font
# family is the family of fonts. e.g. serif, snas, mono

par(pin=c(4,4)) #changing the size of the graph
par(lty=1, pch=0, lwd = 12, cex = 0.5,col.axis = "yellow",col.lab = "red")
par(lty=2, pch=1, lwd = 12, cex = 0.5,col.axis = "yellow",col.lab = "red")
par(lty=3, pch=5, lwd = 12, cex = 0.5,col.axis = "yellow",col.lab = "red")
par(lty=4, pch=7, lwd = 12, cex = 0.5,col.axis = "yellow",col.lab = "red")
par(lty=5, pch=9, lwd = 12, cex = 0.5,col.axis = "yellow",col.lab = "red")
par(lty=6, pch=24, lwd = 12, cex = 0.5,col.axis = "yellow",col.lab = "red", fg = "pink",bg = "black")
plot(dose, drugA, type="b")
# opening a new graph window
dev.new()
plot(dose, drug, type = "b", pch = 23, lty = 6, col = "Blue")

par(opar)
dev.off()


par(font.lab=3, cex.lab=1.5, font.main=4, ps = 12,cex.main=2)
plot(dose, drugA, type="b")

