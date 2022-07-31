###################################################################################
# ML Group project
# Title: MENTAL HEALTH IN THE WORKPLACE 
# By: Kavya Angara, Olivia Lee, Monica Martinez, Aarib Mohammed, Nicole Pham-Nguyen

# Outline
#  1. Data Cleaning Steps
#  2. Knn model and Kfold results
#  3. Random Forest and Bagging model along with tuning
#  4. Boosting model
#  5. Logistic regression - BEST MODEL
###################################################################################
library(tidyverse)
library(readr)
library(readxl)
library(caret)
library(nnet)
library(class) 

###Data Cleaning steps

ts <- read_excel("survey.xlsx")
ts<-filter(ts, work_interfere != "NA")
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
  
  mutate(work_interfere = replace(work_interfere, work_interfere =="Never", as.integer(0))) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "Rarely", as.integer(1))) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Sometimes", as.integer(1)))%>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Often", as.integer(1)))


###Data cleaning completed
###taking out variables we are not considering for our regression
data_new[1:8] <- list(NULL) 
data_new[2] <- NULL
data_new[3] <- NULL
data_new[10] <- NULL
data_new[12:13] <- list(NULL)
data_new[14:15] <- list(NULL)

data_new[] <- lapply(data_new, function(x) as.numeric(as.character(x)))
data_new
sapply(data_new, class)

train.idx = sample (1:nrow(data_new), nrow(data_new)/2)
train_data= data_new[train.idx,]
test_data = data_new[-train.idx,]
train_data <- as.data.frame(train_data)

########################
#knn and kfold
########################
data_new <- data_new %>%     
  mutate(work_interfere = replace(work_interfere, work_interfere =="Never", "NOT")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "Rarely", "YES")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Sometimes", "YES")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Often", "YES"))

#defining train and test dataset
train = sample (1:nrow(data_new), nrow(data_new)/2)
data_new[-train,]
#(data_new)
attach(data_new)
#A matrix containing the predictors associated with the training data,labeled train.X 
train.X=cbind(data_new$benefits,data_new$care_options,data_new$leave)[train ,]#give predictors

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
  aux=mean(knn.pred==test.wi)
  ERROR<- c(ERROR,aux)

}


# plotting

plot(kk,ERROR,type="b",xlab="kk",col="blue",ylab="ERROR",lwd=2,cex.lab=1.2)
text(kk[1]+25,ERROR[1],paste("k=",kk[1],'| 0.7550'),col=2,cex=1.0) # Good location k=2
text(kk[4]+23,ERROR[4]-0.0015,paste("k=",kk[4],'| 0.7751'),col=2,cex=1.2)
text(kk[5]+17,ERROR[5]-0.0018,paste("k=",kk[5],'| 0.783'),col=2,cex=1.0)
text(kk[6]-15,ERROR[6]-0.0018,paste("k=",kk[6],'| 0.783'),col=2,cex=1.0)

##############################
#Random forest and bagging
##############################
#Random forest function
library( randomForest)
set.seed(1)
#defining train and test dataset
train = sample (1:nrow(data_new), nrow(data_new)/2)
w_data.test=data_new[-train ,"work_interfere"]
#Divide the screen in 2 columns and 3 rows
par(mfrow=c(2,3))

###########################################################################################
#Bagging with ntree 700
data_new$work_interfere= as.factor(data_new$work_interfere)
rf.w_data= randomForest(work_interfere~.,data=data_new,ntree=700,mtry=12,importance =TRUE,proximity = TRUE)
yhat.rf = predict (rf.w_data , newdata=data_new[-train ,])
plot(rf.w_data$err.rate[,1],xlab="log(1/ntree)",type="s",main="Bagging Model",col="blue",ylab="Error Rate",cex.lab=1.2)
print(rf.w_data)
###########################################################################################
###########################################################################################
#Bagging with ntree 10000
data_new$work_interfere= as.factor(data_new$work_interfere)
rf.w_data= randomForest(work_interfere~.,data=data_new,ntree=10000,mtry=12,importance =TRUE,proximity = TRUE)
yhat.rf = predict (rf.w_data , newdata=data_new[-train ,])
plot(rf.w_data$err.rate[,1],xlab="log(1/ntree)",type="s",main="Bagging Model (ntree 10000)",col="blue",ylab="Error Rate",cex.lab=1.2)
rf.w_data$err.rate
print(rf.w_data)
###########################################################################################
#Random Forests with variable Split=3 and m=sqrt(p)
library(caret)
data_new$work_interfere= as.factor(data_new$work_interfere)
rf.w_data= randomForest(work_interfere~.,data=data_new,ntree=700,mtry=sqrt(12),importance =TRUE,proximity = TRUE)
print(rf.w_data)
yhat.rf = predict (rf.w_data , newdata=data_new[-train ,])

confusionMatrix(data_new[-train ,]$work_interfere,yhat.rf)
par(mfrow=c(1,1))
plot(rf.w_data$err.rate[,1],xlab="log(1/ntree)",type="s",main="Random Forest Model [m=sqrt(12)]",col="dark green",ylab="Error Rate",cex.lab=1.2)
importance(rf.w_data)
###########################################################################################
###########################################################################################
#Random Forests with variable Split=3,max_nodes=2 and m=sqrt(p)
data_new$work_interfere= as.factor(data_new$work_interfere)
rf.w_data= randomForest(work_interfere~.,data=data_new,ntree=700,mtry=5,max_nodes=7,importance =TRUE,proximity = TRUE)
print(rf.w_data)
yhat.rf = predict (rf.w_data , newdata=data_new[-train ,])
plot(rf.w_data$err.rate[,1],xlab="log(1/ntree)",type="s",main="Random Forest Model [m=sqrt(12), max_nodes=7]",col="dark green",ylab="Error Rate",cex.lab=1.2)
importance(rf.w_data)
confusionMatrix(data_new[-train ,]$work_interfere,yhat.rf)
###########################################################################################
###########################################################################################
#Random Forests with variable Split=3,max_nodes=2, replace=FALSE and m=sqrt(p)
data_new$work_interfere= as.factor(data_new$work_interfere)
rf.w_data= randomForest(work_interfere~.,data=data_new,ntree=700,mtry=12)
print(rf.w_data)
yhat.rf = predict (rf.w_data , newdata=data_new[train ,])
plot(rf.w_data$err.rate[,1],xlab="log(1/ntree)",type="s",main="Random Forest Model [m=sqrt(12), replace=FALSE]",col="dark green",ylab="Error Rate",cex.lab=1.2)
importance(rf.w_data)
confusionMatrix(data_new[train ,]$work_interfere,yhat.rf)
###########################################################################################
############################################################################################
#Random Forests with variable Split=3,max_nodes=2, replace=FALSE and m=sqrt(p)
data_new$work_interfere= as.factor(data_new$work_interfere)
rf.w_data= randomForest(work_interfere~.,data=data_new,ntree=10000,mtry=sqrt(12),replace=FALSE,importance =TRUE,proximity = TRUE)
print(rf.w_data)
yhat.rf = predict (rf.w_data , newdata=data_new[-train ,])
plot(rf.w_data$err.rate[,1],xlab="log(1/ntree)",type="s",main="Random Forest Model [ntrees=10000,m=sqrt(12),max_nodes=2,replace=FALSE]",col="dark green",ylab="Error Rate",cex.lab=1.2)
importance(rf.w_data)
###########################################################################################

#Tuning Random forests

library(mlbench)
library(caret)
data_new$work_interfere= as.factor(data_new$work_interfere)
# Create model with default parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(data_new))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(work_interfere~., data=data_new, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(12)
rf_random <- train(work_interfere~., data=data_new, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random,main="Tuning using Random search")

# Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(work_interfere~., data=data_new, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch,main="Tuning using Grid search")

########################
#Boosting Model 
########################
                     
rm(list = ls())
install.packages('tidyverse')
library(tidyverse)
library(readr)
library(readxl)
library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools

ts <- read_excel("/Volumes/GoogleDrive/My Drive/Summer 2022/Intro to Machine Learning/Project Weeks 1-2/survey (1).xlsx")
nrow(ts)
ts<-filter(ts, work_interfere != "NA")
names(ts)

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
  
  mutate(work_interfere = replace(work_interfere, work_interfere =="Never", "0")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "Rarely", "1")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Sometimes", "1")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Often", "1"))
  data_new[1:8] <- list(NULL) #taking out variables we are not considering for our regression
  data_new[2] <- NULL
  data_new[3] <- NULL
  data_new[10] <- NULL
  data_new[12:13] <- list(NULL)
  data_new[14:15] <- list(NULL)
  data_new = as.data.frame(lapply(data_new, as.numeric))

data_new <- data_new %>%                          
  mutate(work_interfere = replace(work_interfere, work_interfere =="NOT", "0")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "YES", "1")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="YES", "1")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="YES", "1"))

library(gbm)
set.seed(1)
train = 1:250
boost.train <- data_new[train, ]
boost.test <- data_new[-train, ]
boost.test
boost.mentalhealth <- gbm(work_interfere~., data = data_new[train, ], distribution = "bernoulli", n.trees = 5000, interaction.depth = 4)
boost.mentalhealth 
#par(mar=c(1,1,1,1))
#par(mar=c(5,6,4,1)+.1)
summary(boost.mentalhealth) #produces a relative influence plot and also outputs the relative influence statistics
yhat.boost <- predict(boost.mentalhealth, newdata = data_new[-train, ], n.trees = 5000) 

mean((yhat.boost - as.numeric(unlist(boost.test))^2))

boost.mentalhealth <- gbm(work_interfere~., data = data_new[train, ], distribution = "bernoulli", n.trees = 2000, interaction.depth =  10, shrinkage = 0.2, verbose = F)
#changed the shrinkage/lambda above to 0.2 from default 0.001
yhat.boost <- predict(boost.mentalhealth, newdata = data_new[-train, ], n.trees = 2000)
mean((yhat.boost - as.numeric(unlist(boost.test))^2))

#best model of a Test MSE of 3rd run - 0.394856
boost.mentalhealth <- gbm(work_interfere~., data = data_new[train, ], distribution = "bernoulli", n.trees = 5000, interaction.depth =  4, shrinkage = 0.001, verbose = F)
#changed the shrinkage/lambda above to 0.2 from default 0.001
yhat.boost <- predict(boost.mentalhealth, newdata = data_new[-train, ], n.trees = 5000)
mean((yhat.boost - as.numeric(unlist(boost.test))^2))

#confusion matrix 
boost.prob <-  predict(boost.mentalhealth, data_new[-train, ], n.trees=5000, type="response")
boost.pred <-  ifelse(boost.prob >0.5, 1, 0)
table(boost.test$work_interfere, boost.pred)

 
########################
#Logical regression
########################

data_new <- data_new %>% 
  mutate(work_interfere = replace(work_interfere, work_interfere =="0", as.integer(0))) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "1", as.integer(1))) 



find_best_model= glm(work_interfere ~. , data = train_data,family=binomial)
summary (find_best_model)
fitted.results <- predict(find_best_model, newdata=test_data ,type='response')
fitted.results <- ifelse(fitted.results > 0.5, 1,0)
misClasificError <- mean(fitted.results != test_data$work_interfere)
print(paste('Accuracy',1-misClasificError))
plot(find_best_model)

# Using above model found care options, coworkers and obs consequences to be best predictors for my model 
#use this to create a new glm model with just these variables

best_model= glm(work_interfere ~ care_options+ leave +obs_consequence, data = train_data,family = binomial)
summary (best_model)
fitted.results <- predict(best_model, newdata=test_data ,type='response')
plot(fitted.results)

fitted.results <- ifelse(fitted.results > 0.5, 1,0)
misClasificError <- mean(fitted.results != test_data$work_interfere)
print(paste('Accuracy',1-misClasificError))



