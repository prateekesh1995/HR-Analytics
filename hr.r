##Import the data##
setwd("G:\\working directory\\HR_Analytics")
getwd()
train<-read.csv("train_LZdllcl.csv",sep = ",",na.strings = c(""))
test<-read.csv("test_2umaH9m.csv",sep = ",",na.strings = c(""))
##Audit Data##
summary(train)

#str(train)
##Modifying the data##
train$education<-as.character(train$education)
train$education<-ifelse(is.na(train$education),median(train$education,na.rm=TRUE),
                        train$education)
sum(is.na(train$education))
train$education<-as.factor(train$education)
train$education<-as.numeric(train$education)
skewness(train$education)
train$ln_education<-log(train$education)
skewness(train$ln_education)
train$previous_year_rating<-as.factor(train$previous_year_rating)
summary(train$previous_year_rating)
sum(is.na(train$previous_year_rating))
train$previous_year_rating<-as.numeric(train$previous_year_rating)
train$previous_year_rating<-ifelse(is.na(train$previous_year_rating),median(train$previous_year_rating,na.rm=TRUE),
                                   train$previous_year_rating)
sum(is.na(train$previous_year_rating))
library(e1071)
#train_1$Log_CoapplicantIncome <- ifelse(train_1$Log_CoapplicantIncome=='-Inf',0,train_1$Log_CoapplicantIncome)
skewness(train$previous_year_rating)
# train$Log_previous_year_rating<-log(train$previous_year_rating)
# skewness(train$Log_previous_year_rating)
#boxplot(train$previous_year_rating)
#skewness(train$education)
skewness(train$previous_year_rating)
summary(train)
skewness(train$no_of_trainings)
skewness(train$avg_training_score)

names(train)[names(train) == "KPIs_met..80."] <- "kpi"
skewness(train$kpi)
summary(train)

#skewness(train$is_promoted)
boxplot(train$no_of_trainings)
train$Log_no_of_training<-log(train$no_of_trainings)
skewness(train$Log_no_of_training)
train$Log_avg_training_score<-log(train$avg_training_score)
skewness(train$avg_training_score)
names(train)
str(train$is_promoted)
summary(train$is_promoted)
train$is_promoted<-as.factor(train$is_promoted)
train$ln_avg_training_score<-log(train$avg_training_score)
skewness(train$ln_avg_training_score)
summary(train$log_avg_training_score)
str(train$ln_avg_training_score)
skewness(train$ln_avg_training_score)
#train$Log_previous_year_rating<-NULL
summary(train$ln_avg_training_score)
skewness(train$age)
summary(train)
train$log_age<-log(train$age)
skewness(train$log_age)
names(train)
##logistic regression##

model_1<-glm(is_promoted~department+region+
               length_of_service+awards_won.+previous_year_rating+
               Log_no_of_training+kpi+log_age+ln_education,family='binomial',data=train)
summary(model_1)
train$preds_prbs<-model_1$fitted.values
train$preds_prb<-ifelse(train$preds_prbs>0.5,'Y','N')
summary(train$preds_prb)
table(train$is_promoted,train$preds_prbs)
###Decision Tree###
library(rpart)
model_dtree<-rpart(is_promoted~department+region+
                     length_of_service+awards_won.+previous_year_rating+
                     Log_no_of_training+kpi+ln_education,data=train,parms=list(split="information"))
summary(model_dtree)
train$preds_model_dtree<-predict(model_dtree,train,type='class')
table(train$is_promoted,train$preds_model_dtree)
#Random Forest
#install.packages("randomForest")
library(randomForest)
model_1_ranforest<-randomForest(is_promoted~department+region+
                                  length_of_service+awards_won.+previous_year_rating+
                                  Log_no_of_training+kpi+log_age+ln_education,data=train,ntree=150)
rfr<-predict(model_1_ranforest,train)
rfr
summary(model_1_ranforest)
##Test Data for Logistic Regression##
summary(test)
library(e1071)
skewness(test$age)
test$log_age<-log(test$age)
skewness(test$log_age)
#boxplot(test$log_age)
test$education<-as.numeric(test$education)
test$education<-ifelse(is.na(test$education),median(test$education,na.rm=TRUE),
                        test$education)
sum(is.na(test$education))
test$education<-as.numeric(test$education)
skewness(test$education)
test$ln_education<-log(test$education)
skewness(test$ln_education)
str(test$previous_year_rating)
summary(test$previous_year_rating)
test$previous_year_rating<-ifelse(is.na(test$previous_year_rating),median(test$previous_year_rating,na.rm=TRUE),
                                  test$previous_year_rating)
sum(is.na(test$previous_year_rating))
test$log_previous_year_rating<-log(test$previous_year_rating)
skewness(test$log_previous_year_rating)
summary(test$log_previous_year_rating)
skewness(test$previous_year_rating)
names(test)[names(test)=="KPIs_met..80."]<-"kpi"
skewness(test$kpi)
test$no_of_trainings<-ifelse(is.na(test$no_of_trainings),
                             median(test$no_of_trainings,na.rm=TRUE),test$no_of_trainings)
sum(is.na(test$no_of_trainings))
skewness(test$no_of_trainings)
test$Log_no_of_training<-log(test$no_of_trainings)
skewness(test$Log_no_of_training)
summary(test$age)
skewness(test$age)
test$ln_age<-log(test$age)
skewness(test$ln_age)
skewness(test$avg_training_score)
test$ln_avg_training_score<-log(test$avg_training_score)
skewness(test$ln_avg_training_score)
names(test)
#Applying regression#
test$pr_is_promoted<-predict(model_1,test,type = "response")
test$pred_is_promoted<-ifelse(test$pr_is_promoted>0.1,'Y','N')
getwd()
#writing the final promotion in the result format
write.csv(test,"result.csv")
#confusion matrix calculation
cm_mat<-table(test$is_promoted,test$pred_is_promoted)
cm_mat

