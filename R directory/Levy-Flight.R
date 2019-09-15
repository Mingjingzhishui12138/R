alpha=2
n=1000
x=rep(0,n)
y=rep(0,n)

for (i in 2:n){
  theta=runif(1)*2*pi
  f=runif(1)^(-1/alpha)
  x[i]=x[i-1]+f*cos(theta)
  y[i]=y[i-1]+f*sin(theta)
}
plot(x, y, type = "l",col='magenta')
#lines(y,x,cex=.5,col='blue')
