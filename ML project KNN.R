install.packages('tidyverse')
install.packages('dplyr')
install.packages('purrr')
install.packages('mice')
install.packages("e1071", dep = TRUE) 
library(tidyverse)
library(readr)
library(readxl)
library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn)

ts <- read_excel("survey.xlsx")

#table(techsurvey['work_interfere']) #show the number of yes and no if they are having their mental illness interfere with work

#ts$mental_illness <- ts$work_interfere #create a new variable with work interfere data

#ts$mental_illness <- ifelse(ts$mental_illness == 'NA', 0, 1) #changed all the NAs to 0

data_new <- ts %>%                          
  mutate(benefits = replace(benefits, benefits == "Yes", 1)) %>%
  mutate(benefits = replace(benefits, benefits =="No", -1)) %>%
  mutate(benefits = replace(benefits, benefits == "Don\'t know", 0)) %>%
  
  mutate(anonymity = replace(anonymity, anonymity == "Yes", 1)) %>%
  mutate(anonymity = replace(anonymity, anonymity =="No", -1)) %>%
  mutate(anonymity = replace(anonymity, anonymity == "Don\'t know", 0)) %>%
  
  mutate(care_options = replace(care_options, care_options == "Yes", 1)) %>%
  mutate(care_options = replace(care_options, care_options =="No", -1)) %>%
  mutate(care_options = replace(care_options, care_options == "Not sure", 0)) %>%
  
  mutate(wellness_program = replace(wellness_program, wellness_program == "Yes", 1)) %>%
  mutate(wellness_program = replace(wellness_program, wellness_program =="No", -1)) %>%
  mutate(wellness_program = replace(wellness_program, wellness_program == "Don't know", 0)) %>%
  
  mutate(seek_help = replace(seek_help, seek_help == "Yes", 1)) %>%
  mutate(seek_help = replace(seek_help, seek_help =="No", -1)) %>%
  mutate(seek_help = replace(seek_help, seek_help == "Don't know", 0)) %>%
  
  mutate(mental_health_consequence = replace(mental_health_consequence, mental_health_consequence == "Yes", 1)) %>%
  mutate(mental_health_consequence = replace(mental_health_consequence, mental_health_consequence =="No", -1)) %>%
  mutate(mental_health_consequence = replace(mental_health_consequence, mental_health_consequence == "Maybe", 0)) %>%
  
  mutate(supervisor = replace(supervisor, supervisor == "Yes", 1)) %>%
  mutate(supervisor = replace(supervisor, supervisor =="No", -1)) %>%
  mutate(supervisor = replace(supervisor, supervisor == "Some of them", 0)) %>%
  
  mutate(coworkers = replace(coworkers, coworkers == "Yes", 1)) %>%
  mutate(coworkers = replace(coworkers, coworkers =="No", -1)) %>%
  mutate(coworkers = replace(coworkers, coworkers == "Some of them", 0)) %>%
  
  mutate(mental_vs_physical = replace(mental_vs_physical, mental_vs_physical == "Yes", 1)) %>%
  mutate(mental_vs_physical = replace(mental_vs_physical, mental_vs_physical =="No", -1)) %>%
  mutate(mental_vs_physical = replace(mental_vs_physical, mental_vs_physical == "Don't know", 0)) %>%
  
  mutate(obs_consequence = replace(obs_consequence, obs_consequence == "Yes", 1)) %>%
  mutate(obs_consequence = replace(obs_consequence, obs_consequence =="No", 0)) %>%
  
  mutate(remote_work = replace(remote_work, remote_work == "Yes", 1)) %>%
  mutate(remote_work = replace(remote_work, remote_work =="No", 0)) %>%
  
  mutate(leave = replace(leave, leave == "Very difficult", 0)) %>%
  mutate(leave = replace(leave, leave =="Somewhat difficult", 1)) %>%
  mutate(leave = replace(leave, leave == "Don\'t know", 2)) %>%
  mutate(leave = replace(leave, leave =="Somewhat easy", 3)) %>%
  mutate(leave = replace(leave, leave =="Very easy", 4)) %>%
  
  mutate(work_interfere = replace(work_interfere, work_interfere == "NA", 0)) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Never", 1)) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "Rarely", 2)) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Sometimes", 3)) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Often", 4))

  data_new[1:8] <- list(NULL) #taking out variables we are not considering for our regression
  data_new[2] <- NULL
  data_new[3] <- NULL
  data_new[10] <- NULL
  data_new[12:13] <- list(NULL)
  data_new[14:15] <- list(NULL)
  data_new = as.data.frame(lapply(data_new, as.numeric))
  View(data_new)

  
##########################
#### knn
##########################

# library(maps)
# library(kknn) ## knn library
  
attach(data_new)
n = dim(data_new)[1]

plot(work_interfere, mental_health_consequence)

train = data.frame(work_interfere, mental_health_consequence)
test = data.frame(work_interfere, mental_health_consequence)
ind = order(test[,1])
test =test[ind,]

MSE = NULL

# kk = c(120,150,180,250,300,400,505)
kk = c(20,120, 150, 170, 180, 190, 400, 505)

for(i in kk){
  
  near = kknn(mental_health_consequence~work_interfere,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  MSE = c(MSE,aux)
  
  plot(work_interfere,mental_health_consequence,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  cat ("Press [enter] to continue")
  #line <- readline()
}

plot(kk,MSE,type="b",xlab="kk",col="blue",ylab="MSE",lwd=2,cex.lab=1.2)
plot(kk,sqrt(MSE),type="b",xlab="kk",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
text(kk[3],sqrt(MSE[3]),paste("k=",kk[3]),col=2,cex=1.2)
MSE[3]
sqrt(MSE[3])

sqrt(MSE[5])
plot(log(1/kk),sqrt(MSE),type="b",xlab="Complexity (log(1/k))",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
text(log(1/kk[1]),sqrt(MSE[1]),paste("k=",kk[1]),col=2,cex=1.2)
#text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[6]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)

plot(kk,MSE,type="b",xlab="kk",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
plot(kk,sqrt(MSE),type="b",xlab="kk",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)

text(kk[3],sqrt(MSE[3]),paste("k=",kk[3]),col=2,cex=1.2)
sqrt(MSE[3])


near = kknn(mental_health_consequence~work_interfere,train,test,k=20,kernel = "rectangular")

for(i in seq(1,505,by=100)){
  ii = near$C[i,1:20]
  plot(work_interfere,mental_health_consequence,main=paste("k=",20),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  abline(v=test[i,1],col=2,lty=2)
  points(work_interfere[ii],mental_health_consequence[ii],pch=19,col="blue")
  cat ("Press [enter] to continue")
  line <- readline()
}

print(MSE) #lowest MSE:  0.5706219 k = 180 index3
plot(kk,MSE,type="b",xlab="kk",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)

