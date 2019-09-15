x <- c(0,10,30,20,67,55,44,72,83);
y <- c(0,50,30,93,45,21,75,11,68);

library(ggplot2)
attribute <- c('Customer','Customer','Customer','Customer','Depot','Customer','Customer','Customer','Customer')
mydata<- data.frame(x_col=x,y_col=y,attribute=attribute)
mydata
ggplot(mydata, mapping=aes(x=x_col,y=y_col,fill=attribute,col=factor(attribute)))+
  geom_point(aes(size=10,shape = factor(attribute)))+
  theme_bw()+
theme(legend.position = 'none')+
theme(plot.title = element_text(hjust=.5))+
labs(title='Positions of Customers and Depot')

