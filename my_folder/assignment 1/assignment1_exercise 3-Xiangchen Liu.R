#3(a) Scaling
traindata <- read.csv(file.choose(), header=TRUE)
traindata$Fare<-log(traindata$Fare)
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
traindata<-delete.na(traindata)
str(traindata$Age)
traindata$Fare <- ifelse((traindata$Fare=='-Inf'),0,traindata$Fare)
traindata$Fare
traindata$Age
sd(traindata$Fare)
sd(traindata$Age)

#scaling
traindata$Age<-(traindata$Age-mean(traindata$Age))/sd(traindata$Age)
traindata$Fare<-(traindata$Fare-mean(traindata$Fare))/sd(traindata$Fare)
trainfemale<-subset(traindata,traindata$Sex=='female')
trainmale<-subset(traindata,traindata$Sex=='male')

## 3b Regression
#training and validation set
library(dplyr)
library(ggplot2)
library(tidyr)
#female subset
n = nrow(trainfemale)
trainIndex = sample(1:n, size = round(0.6*n), replace=FALSE)
trainfe = trainfemale[trainIndex ,]
valfe = trainfemale[-trainIndex ,]

tibble(r = seq(0.1, 2, length.out=100)) %>% 
  group_by(r) %>% 
  do({
    this_r <- .$r
    fit <- loess(Survived ~ Age+Fare , data=trainfe, span=this_r)
    yhat_tr  <- predict(fit)
    yhat_val <- predict(fit, newdata = valfe)
    data.frame(
      r = this_r,
      training = mean((yhat_tr - trainfe$Survived)^2),
      validation = mean((yhat_val - valfe$Survived)^2, na.rm = TRUE)
    )
  }) %>% 
  gather(key="set", value="mse", training, validation) %>% 
  ggplot(aes(r, mse)) +
  geom_line(aes(group=set, colour=set)) +
  theme_bw()
#male subset
n1 = nrow(trainmale)
trainIndex1 = sample(1:n1, size = round(0.6*n1), replace=FALSE)
trainma = trainmale[trainIndex1 ,]
valma = trainmale[-trainIndex1 ,]

tibble(r = seq(0.1, 1.5, length.out=100)) %>% 
  group_by(r) %>% 
  do({
    this_r <- .$r
    fit <- loess(Survived ~ Age+Fare , data=trainma, span=this_r)
    yhat_tr  <- predict(fit)
    yhat_val <- predict(fit, newdata = valma)
    data.frame(
      r = this_r,
      training = mean((yhat_tr - trainma$Survived)^2),
      validation = mean((yhat_val - valma$Survived)^2, na.rm = TRUE)
    )
  }) %>% 
  gather(key="set", value="mse", training, validation) %>% 
  ggplot(aes(r, mse)) +
  geom_line(aes(group=set, colour=set)) +
  theme_bw()

## 3c classification
#female subset
n = nrow(trainfemale)
trainIndex = sample(1:n, size = round(0.6*n), replace=FALSE)
trainfe = trainfemale[trainIndex ,]
valfe = trainfemale[-trainIndex ,]

tibble(r = seq(0.1, 2, length.out=100)) %>% 
  group_by(r) %>% 
  do({
    this_r <- .$r
    fit <- loess(Survived ~ Age+Fare , data=trainfe, span=this_r)
    tab1 <- table(trainfe$Survived, predict(fit) > 0.5)
    tab2 <- table(valfe$Survived, predict(fit, newdata = valfe) > 0.5)
    yhat_tr  <- predict(fit)
    yhat_val <- predict(fit, newdata = valfe)
    data.frame(
      r = this_r,
      training = 1-(tab1[2,2][1]+tab1[1,1][1])/(tab1[2,1][1]+tab1[2,2][1]+tab1[1,1][1]+tab1[1,2][1]),
      validation = 1-(tab2[2,2][1]+tab2[1,1][1])/(tab2[2,1][1]+tab2[2,2][1]+tab2[1,1][1]+tab2[1,2][1])
    )
  }) %>% 
  gather(key="set", value="error_rate", training, validation) %>% 
  ggplot(aes(r, error_rate)) +
  geom_line(aes(group=set, colour=set)) +
  theme_bw()

# male subset
n1 = nrow(trainmale)
trainIndex1 = sample(1:n1, size = round(0.6*n), replace=FALSE)
trainma = trainmale[trainIndex1 ,]
valma = trainmale[-trainIndex1 ,]

tibble(r = seq(0.1, 2.5, length.out=100)) %>% 
  group_by(r) %>% 
  do({
    this_r <- .$r
    fit <- loess(Survived ~ Age+Fare , data=trainma, span=this_r)
    tab3 <- table(trainma$Survived, predict(fit) > 0.5)
    tab4 <- table(valma$Survived, predict(fit, newdata = valma) > 0.5)
    yhat_tr  <- predict(fit)
    yhat_val <- predict(fit, newdata = valma)
    data.frame(
      r = this_r,
      training = 1-(tab3[2,2][1]+tab3[1,1][1])/(tab3[2,1][1]+tab3[2,2][1]+tab3[1,1][1]+tab3[1,2][1]),
      validation = 1-(tab4[2,2][1]+tab4[1,1][1])/(tab4[2,1][1]+tab4[2,2][1]+tab4[1,1][1]+tab4[1,2][1])
    )
  }) %>% 
  gather(key="set", value="error_rate", training, validation) %>% 
  ggplot(aes(r, error_rate)) +
  geom_line(aes(group=set, colour=set)) +
  theme_bw()

## 3d kNN
library(class)
n = nrow(trainfemale)
trainIndex = sample(1:n, size = round(0.6*n), replace=FALSE)
trainfe = trainfemale[trainIndex ,]
valfe = trainfemale[-trainIndex ,]
myvars <- c("Age", "Fare")
fetrain <- trainfe[myvars]
feval <- valfe[myvars]

tibble(k = seq(1, 50, length.out=50)) %>% 
  group_by(k) %>% 
  do({
    this_k <- .$k
    knn1 <-  knn(fetrain, fetrain,trainfe$Survived, k=this_k)
    knn2 <-  knn(fetrain, feval,trainfe$Survived, k=this_k)
    tab1 <- table(trainfe$Survived, knn1)
    tab2 <- table(valfe$Survived, knn2)
    data.frame(
      k = this_k,
      training = 1-(tab1[2,2][1]+tab1[1,1][1])/(tab1[2,1][1]+tab1[2,2][1]+tab1[1,1][1]+tab1[1,2][1]),
      validation = 1-(tab2[2,2][1]+tab2[1,1][1])/(tab2[2,1][1]+tab2[2,2][1]+tab2[1,1][1]+tab2[1,2][1])
    )
  }) %>% 
  gather(key="set", value="error_rate", training, validation) %>% 
  ggplot(aes(k, error_rate)) +
  geom_line(aes(group=set, colour=set)) +
  theme_bw()

#male subset
n = nrow(trainmale)
trainIndex = sample(1:n, size = round(0.6*n), replace=FALSE)
trainma = trainmale[trainIndex ,]
valma = trainmale[-trainIndex ,]
myvars <- c("Age", "Fare")
matrain <- trainma[myvars]
maval <- valma[myvars]

tibble(k = seq(1, 50, length.out=50)) %>% 
  group_by(k) %>% 
  do({
    this_k <- .$k
    knn1 <-  knn(matrain, matrain,trainma$Survived, k=this_k)
    knn2 <-  knn(matrain, maval,trainma$Survived, k=this_k)
    tab1 <- table(trainma$Survived, knn1)
    tab2 <- table(valma$Survived, knn2)
    data.frame(
      k = this_k,
      training = 1-(tab1[2,2][1]+tab1[1,1][1])/(tab1[2,1][1]+tab1[2,2][1]+tab1[1,1][1]+tab1[1,2][1]),
      validation = 1-(tab2[2,2][1]+tab2[1,1][1])/(tab2[2,1][1]+tab2[2,2][1]+tab2[1,1][1]+tab2[1,2][1])
    )
  }) %>% 
  gather(key="set", value="error_rate", training, validation) %>% 
  ggplot(aes(k, error_rate)) +
  geom_line(aes(group=set, colour=set)) +
  theme_bw()


