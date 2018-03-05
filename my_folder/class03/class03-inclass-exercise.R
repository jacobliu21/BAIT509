library(tidyverse)
library(knitr)
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1),
              z=abs(x-0))
kable(head(dat))
plot(dat)
ggplot(dat, aes(x,y)) + 
  geom_point(colour=my_accent) +
  theme_bw() + 
  rotate_y
##2.75
dat
#add_column(dat,z=-0,w=0)
##kNN
dat<-arrange(dat,z)
subsetdat<-dat[1:10, ]
subsetdat
mean(subsetdat$y)
##loess,r=1
datloess<-filter(dat,z<1)
mean(datloess$y)
##Small variance if the parameter becomes too large(k and r)/ Bias is high meanwhile
##Bias is small if choosing reasonable amount of data
##large variance occurs

#####################
#Regression Curve
xgrid <- seq(-5, 4, length.out=1000)
dat1 <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))

kNN_estimates <- map_dbl(xgrid, function(x){
  dat1$z<-abs(dat1$x-x)
  dat1<-arrange(dat1,z)
  subsetdat<-dat1[1:10, ]
  yhat<-mean(subsetdat$y)
  return(yhat)
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
loess_estimates <- map_dbl(xgrid, function(x){
  dat1$z<-abs(dat1$x-x)
  dat1<-arrange(dat1,z)
  datloess<-filter(dat1,z<1)
  yhat<-mean(datloess$y)
  return(yhat)
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat1, mapping=aes(x,y)) +
  geom_line(data=est, size=1
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()

