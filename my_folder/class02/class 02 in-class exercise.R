install.packages("tidyverse")
install.packages("ISLR")
install.packages("dplyr")
install.packages('tidyverse')
install.packages('ggplot2')
library(pillar)
library(tidyverse)
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}
genreg(100)
dat<-genreg(1000)
dat<-mutate(dat,
       yhat=0,
       yhat1=5-x1,
       yhat2=5+2*x2,
       yhat12=5-x1+2*x2)
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}
##x=1
pA<-0.2
pB <- 0.8/(1+exp(-1))
pC<-1-0.2-pB
pB
pC
##generate
dat2<-gencla(100)
dat2<-mutate(dat2,yhat=sapply(x,function(x_),if(X<0)"C"else"B")
1-mean(dat2$yhat==dat2$y)
plot()