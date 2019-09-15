for (i in 1:10){
  print("Hello")
}

vector <- c(seq(2,200,2))
for (i in vector){
  print(i)
}

for (j in 1:100){
  print(2*j)
}

a <- 1
for (i in 1:5){
  a <- a*i
  print(a)
}

i <- 10
while (i>0){
  print("Hello")
  i <- i-1
}

x<- rnorm(100)
y<- matrix(x,10,10)
z <- rep(0,ncol(y))
for (i in 1:ncol(y)){
  z[i] = mean(y[i])
}

w <- apply(y,2,mean)

score <- 80
ifelse(score > 60, print("Passed"), print("Failed"))
outcome <- ifelse(score >60, "Passed", "Failed")

score <- 80
if(score > 60){print("Passed")}else{print("Failed")}

score <- 90
if (score > 80){
  print("Excellent")
}else if(score > 75){
  print("Good")
}else{
  print("Failed")
}

feelings <- c("sad", "afraid")
for (i in feelings){
  print(
    switch(i,
           happy = "Iam glad you are happy",
           afraid ="There is nothing to fear",
           sad = "Cheer up",
           angry = "Calm down now"
    )
  )
}


