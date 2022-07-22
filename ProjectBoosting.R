install.packages('tidyverse')
library(tidyverse)
library(readr)
library(readxl)
library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools

ts <- read_excel("/Volumes/GoogleDrive/My Drive/Summer 2022/Intro to Machine Learning/Project Weeks 1-2/survey (1).xlsx")

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
  
  library(gbm)
  set.seed(1)
  boost.test <- data_new[-train, "work_interfere"]
  boost.mentalhealth <- gbm(work_interfere~., data = data_new[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
  #par(mar=c(1,1,1,1))
  summary(boost.mentalhealth) #produces a relative influence plot and also outputs the relative influence statistics
  plot(boost.mentalhealth, i = "care_options") #produce partial dependence plots for care_options, leave, and benefits
  plot(boost.mentalhealth, i = "leave")
  plot(boost.mentalhealth, i = "benefits")
  plot(boost.mentalhealth, i = "coworkers")
  yhat.boost <- predict(boost.mentalhealth, newdata = data_new[-train, ], n.trees = 5000) 
  mean((yhat.boost - boost.test)^2)
  
  boost.mentalhealth <- gbm(work_interfere~., data = data_new[train, ], distribution = "gaussian", n.trees = 10000, interaction.depth =  4, shrinkage = 0.001, verbose = F)
  #changed the shrinkage/lambda above to 0.2 from default 0.001
  yhat.boost <- predict(boost.mentalhealth, newdata = data_new[-train, ], n.trees = 10000)
  mean((yhat.boost - boost.test)^2) 
  
  #best model of a Test MSE of 3rd run - 1.709534
  boost.mentalhealth <- gbm(work_interfere~., data = data_new[train, ], distribution = "gaussian", n.trees = 2000, interaction.depth =  4, shrinkage = 0.001, verbose = F)
  #changed the shrinkage/lambda above to 0.2 from default 0.001
  yhat.boost <- predict(boost.mentalhealth, newdata = data_new[-train, ], n.trees = 2000)
  mean((yhat.boost - boost.test)^2) 

  #creates the confusion matrix with probability > 0.5
  boost.prob <-  predict(boost.mentalhealth, data_new[-train, ], n.trees=10000, type="response")
  boost.pred <-  ifelse(boost.prob >0.5, 1, 0)
  table(boost.test, boost.pred)
