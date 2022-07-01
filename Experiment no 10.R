#Programming in R
#We can also find the sample Auto covariance and Auto correlation function by using the following R-program-
library('ggplot2')
ut=c(200, 202, 208, 204, 202, 201, 200, 199, 201, 198, 200, 203, 202, 211, 204, 206, 203, 203, 204, 207, 206, 207, 206, 200, 207, 205, 200, 195, 202, 204, 203, 198, 200)
N=length(ut)
N
Mean=mean(ut)
Mean
c0=var(ut)*((N-1)/N)
c0
c=mat.or.vec(5,1)
r=mat.or.vec(5,1)
for(i in 1:5){
  c[i]=(sum((ut[1:(33-i)]-Mean)*(ut[(i+1):33]-Mean)))/N
  r[i]=c[i]/c0
}
c
r
lag=c(1,2,3,4,5)
lag
Table_1 = data.frame(c,lag)
Table_1
View(Table_1)
Table_2 = data.frame(r,lag)
Table_2
View(Table_2)
ggp = ggplot(NULL, mapping = aes(x = lag,y = ACF_and_AVF)) +
  geom_point(data = Table_1, mapping = aes(x=lag,y=c), col = "black") + geom_line(data = Table_1, mapping = aes(x=lag,y=c), col = "orange", size = 1) + 
  geom_point(data = Table_2, mapping = aes(x=lag,y=r), col = "blue") + geom_line(data = Table_2, mapping = aes(x=lag,y=r), col = "green", size = 1) + 
  labs(
    title = paste("Autocorrelation Vs Autocovariance"),
    subtitle = paste("orange_line=Autocovariance and green_line=Autocorrelation"),
    caption = "Data from Temperature",
    x = "Lags",
    y = "ACF_and_AVF"
  )
ggp  
