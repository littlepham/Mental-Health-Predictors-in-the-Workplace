#install.packages('mice')
#install.packages("e1071", dep = TRUE) 
library(tidyverse)
library(readr)
library(readxl)
library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn)

ts <- read_excel("~/Desktop/R/project/survey.xlsx")
# nrow(ts)
ts<-filter(ts, work_interfere != "NA")
# names(ts)

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
  
  mutate(work_interfere = replace(work_interfere, work_interfere =="Never", "NOT")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "Rarely", "YES")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Sometimes", "YES")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Often", "YES"))

  data_new[1:8] <- list(NULL) #taking out variables we are not considering for our regression
  data_new[2] <- NULL
  data_new[3] <- NULL
  data_new[10] <- NULL
  data_new[12:13] <- list(NULL)
  data_new[14:15] <- list(NULL)
  names(data_new)


  
 
#defining train and test dataset
train = sample (1:nrow(data_new), nrow(data_new)/2)
data_new[-train,]
#(data_new)

#A matrix containing the predictors associated with the training data,labeled train.X 
train.X=cbind(benefits,care_options,leave)[train ,]#give predictors
#train.X
# A matrix containing the predictors associated with the data for which we wish to make predictions.
test.X=cbind(benefits,care_options,leave)[-train ,]
#test.X

train.wi =work_interfere[train]#work_interfere
train.wi
test.wi=work_interfere[-train]
test.wi


knn.pred=knn(train.X,test.X,train.wi,k=2)
table(knn.pred ,test.wi)
mean(knn.pred==test.wi)

kk = c(2,6,10,20,50,200)

ERROR=NULL
for(i in kk){
  set.seed(33)
  knn.pred=knn(train.X,test.X,train.wi ,k=i)
  #table(knn.pred ,test.wi)
  aux=mean(knn.pred==test.wi)
  ERROR<- c(ERROR,aux)
  # cat ("Press [enter] to continue")
  # line <- readline()
}

aux

ERROR

# plotting

plot(kk,ERROR,type="b",xlab="kk",col="blue",ylab="ERROR",lwd=2,cex.lab=1.2)
text(kk[1]+25,ERROR[1],paste("k=",kk[1],'| 0.7550'),col=2,cex=1.0) # Good location k=2
text(kk[4]+23,ERROR[4]-0.0015,paste("k=",kk[4],'| 0.7751'),col=2,cex=1.2)
text(kk[5]+17,ERROR[5]-0.0018,paste("k=",kk[5],'| 0.783'),col=2,cex=1.0)
text(kk[6]-15,ERROR[6]-0.0018,paste("k=",kk[6],'| 0.783'),col=2,cex=1.0)


