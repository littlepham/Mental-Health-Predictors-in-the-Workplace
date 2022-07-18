install.packages('tidyverse')
library(tidyverse)
library(readr)
library(readxl)
library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools

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
dim(data_new)[2]

#Random forest function
library( randomForest)
set.seed(1)
#defining train and test dataset
train = sample (1:nrow(data_new), nrow(data_new)/2)
w_data.test=data_new[-train ,"work_interfere"]

#running with m=p, same as bagging
bag.w_data= randomForest(work_interfere~.,data=data_new,subset=train ,mtry=dim(data_new)[2],importance =TRUE)
bag.w_data
#testing for m=p 
yhat.bag = predict (bag.w_data , newdata=data_new[-train ,])
plot(yhat.bag , w_data.test)
abline (0,1)
mean((yhat.bag - w_data.test)^2)

#changing the number of trees grown as 25 with m=p
bag.w_data= randomForest( work_interfere~.,data=data_new , subset=train ,mtry=dim(data_new)[2],ntree=25)
yhat.bag = predict (bag.w_data , newdata=data_new[-train ,])
mean((yhat.bag-w_data.test)^2)

#changing mtry=6 with n=25
bag.w_data= randomForest( work_interfere~.,data=data_new , subset=train ,mtry=6,ntree=25)
yhat.bag = predict (bag.w_data , newdata=data_new[-train ,])
mean((yhat.bag-w_data.test)^2)

