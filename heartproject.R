#################################################
########### ANN Term Project ####################
########### Heart Disease UCI Dataset ###########

## Importing Data
heart <- read.csv(file.choose())
plot(heart)

## Exploring Data

head(heart)
str(heart)
summary(heart)

## the column names are changed in order to explain them more clearly 
name <- c("age","sex","chest_pain_type","rest_bp","chol","fasting_bloodsugar",
          "rest_ecg","max_heartrate","exercise_angina","ST_depression","slope","n_major_vessel","thal","target")
names(heart) <- name

## The variables that should be factor are converted to factor variable
heart$sex[heart$sex == "1"]= "male"
heart$sex[heart$sex == "0"] = "female"
heart$chest_pain_type[heart$chest_pain_type == "0"]= "typical angina"
heart$chest_pain_type[heart$chest_pain_type == "1"]= "atypical angina"
heart$chest_pain_type[heart$chest_pain_type == "2"]= "non-anginal pain"
heart$chest_pain_type[heart$chest_pain_type == "3"]= "asymptomatic"
heart$fasting_bloodsugar[heart$fasting_bloodsugar == 1]= ">120"
heart$fasting_bloodsugar[heart$fasting_bloodsugar == 0] = "<=120"
heart$rest_ecg[heart$rest_ecg == 0] = "Normal"
heart$rest_ecg[heart$rest_ecg == 1] = "Abnormality"
heart$rest_ecg[heart$rest_ecg == 2] = "Probable or definite"
heart$exercise_angina[heart$exercise_angina == "1"]= "yes"
heart$exercise_angina[heart$exercise_angina == "0"] = "no"
heart$target[heart$target == 1]= "Heart Disease"
heart$target[heart$target == 0] = "Healthy"
heart$slope=as.factor(heart$slope)
heart$thal=as.factor(heart$thal)
heart$target=as.factor(heart$target)
heart$sex=as.factor(heart$sex)
heart$fasting_bloodsugar=as.factor(heart$fasting_bloodsugar)
heart$exercise_angina=as.factor(heart$exercise_angina)
heart$chest_pain_type=as.factor(heart$chest_pain_type)
heart$rest_ecg=as.factor(heart$rest_ecg)

## Missing & Duplicated Values
duplicated(heart) ## check if there is duplicated data 
which(duplicated(heart)) ## there is one duplicated row 
library(dplyr) ## for distinct() function
heart <- distinct(heart) ## remove duplicated row
which(is.na(heart)) ## there is no na value 
summary(heart)
str(heart)


install.packages("DataExplorer")
library(DataExplorer)

plot_histogram(heart)
plot_correlation(heart)

library(ggplot2)
install.packages("ggthemes")
library(ggthemes)

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) 

ggplot(heart,aes(age, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1"))+
  xlab("Age") +
  ylab("Density / Count") +
  ggtitle("Age Histogram")


ggplot(heart,aes(rest_bp, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(90, 200, by=10), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  xlab("Resting Blood Pressure") +
  ylab("Density / Count") +
  ggtitle("Rest ECG Histogram")

## More Heart Disease patients seem to have between 200 and 250 cholestoral

ggplot(heart,aes(chol, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(100, 600, by=25), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  xlab("Serum Cholestoral") +
  ylab("Density / Count") +
  ggtitle("Cholestoral Histogram")

ggplot(heart,aes(max_heartrate, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(70, 205, by=10), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1"))+
  xlab("Maximum Heart Rate Achieved") +
  ylab("Density / Count") +
  ggtitle("Max Heart Rate Histogram")

### Heart Disease patients have higher maximum heart rate than healthy patients

ggplot(heart,aes(ST_depression, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 7, by=0.1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  ggtitle("ST Depression Histogram") +
  xlab("ST Depression Induced by Exercise Relative to Rest") +
  ylab("Density / Count")

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~sex, ncol=2,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +  
  ggtitle("Target on Sex") 

##More females have Heart Disease

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~chest_pain_type, ncol=2,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  ggtitle("Chest Pain Type on Disease") 

## No difference in fasting blood sugar

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~fasting_bloodsugar, ncol=2,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  ggtitle("Fasting Bloodsugar on Disease") 

  ##Patients with Rest ECG 1 have more Heart Diseases

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~rest_ecg, ncol=3,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  ggtitle("Rest ECG on Disease") 

##Patients with no exercise induced angina have more Heart Disease

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~exercise_angina, ncol=1,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  ggtitle("Exercise Agina on Disease") 

  ##Fixed defect thalasemia has more Heart Disease

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~thal, ncol=2,scale="fixed") +
  theme_hc()  +
  scale_fill_manual(values=c("steelblue2","red1")) +
  ggtitle("Thalasemia on Disease") 


log<-glm(target~., data=heart, family=binomial)
summary(log)

step(glm(target ~ .,data=heart,family = "binomial"),direction = "backward")

## creating a new dataset with significant variables sex + chest_pain_type + rest_bp + max_heartrate + exercise_angina + ST_depression + n_major_vessel
newheart <- heart[, -c(1,5,6,7,11,13)]
summary(newheart)

set.seed(1237)
train <- sample(nrow(newheart), .8*nrow(newheart), replace = FALSE)
TrainSet <- newheart[train,]
ValidSet <- newheart[-train,]

install.packages("caret")
library(caret)

install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
update.packages(oldPkgs="caret", ask=FALSE)
update.packages(oldPkgs="rlang", ask=FALSE)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

TrainSet$target<-make.names(TrainSet$target)
set.seed(142)
TrainSet$target<-as.factor(TrainSet$target)

logcross <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "glm", 
                          trControl = fitControl,
                          metric="ROC")
logcross

##Generalized Linear Model 

##241 samples
##7 predictor
##2 classes: 'Healthy', 'Heart.Disease' 

##No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 217, 217, 217, 218, 217, 217, ... 
# Resampling results:
#   
#   ROC        Sens      Spec     
# 0.8926918  0.754697  0.8612179
install.packages("e1071")
library(e1071)

pred <- predict(logcross,ValidSet)
levels(pred)[2] <- "Heart Disease"
t<-table(pred, ValidSet$target)
t.df<-as.data.frame(t)
res<-caret::confusionMatrix(t, positive="Heart Disease")
res

# Confusion Matrix and Statistics
# 
# 
# pred            Healthy Heart Disease
# Healthy            20             5
# Heart Disease       6            30
# 
# Accuracy : 0.8197          
# 95% CI : (0.7002, 0.9064)
# No Information Rate : 0.5738          
# P-Value [Acc > NIR] : 4.229e-05       
# 
# Kappa : 0.6295          
# 
# Mcnemar's Test P-Value : 1               
#                                           
#             Sensitivity : 0.8571          
#             Specificity : 0.7692          
#          Pos Pred Value : 0.8333          
#          Neg Pred Value : 0.8000          
#              Prevalence : 0.5738          
#          Detection Rate : 0.4918          
#    Detection Prevalence : 0.5902          
#       Balanced Accuracy : 0.8132          
#                                           
#        'Positive' Class : Heart Disease   
                                          

ggplot(data = t.df, aes(x = Var2, y = pred, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_manual(c("red1","steelblue2")) +
  theme_hc() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Logistic Regression")

install.packages("randomForest")
library(randomForest)

rfcross <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "rf", 
                          trControl = fitControl,
                          metric="ROC")

rfcross

predrf <- predict(rfcross,ValidSet)
levels(predrf)[2] <- "Heart Disease"
t1<-table(ValidSet$target, predrf)
t1.df<-as.data.frame(t1)
res1<-caret::confusionMatrix(t1, positive="Heart Disease")
res1

# Confusion Matrix and Statistics
# 
# predrf
# Healthy Heart Disease
# Healthy            19             7
# Heart Disease       4            31
# 
# Accuracy : 0.8197          
# 95% CI : (0.7002, 0.9064)
# No Information Rate : 0.623           
# P-Value [Acc > NIR] : 0.0007305       
# 
# Kappa : 0.6258          
# 
# Mcnemar's Test P-Value : 0.5464936       
# 
# Sensitivity : 0.8158          
# Specificity : 0.8261          
# Pos Pred Value : 0.8857          
# Neg Pred Value : 0.7308          
# Prevalence : 0.6230          
# Detection Rate : 0.5082          
# Detection Prevalence : 0.5738          
# Balanced Accuracy : 0.8209          
# 
# 'Positive' Class : Heart Disease   

ggplot(data = t1.df, aes(x = Var1, y = predrf, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="red1", high="steelblue2") +
  theme_hc() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Random Forest")

install.packages("nnet")
library(nnet)

nncross <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "nnet", 
                          trControl = fitControl,
                          metric="ROC")
##100 iterations

prednn <- predict(nncross,ValidSet)
levels(prednn)[2] <- "Heart Disease"
t2<-table(ValidSet$target, prednn)

t2.df<-as.data.frame(t2)
res<-caret::confusionMatrix(t2, positive="Heart Disease")
res
nncross
# Confusion Matrix and Statistics
# 
# prednn
# Healthy Heart Disease
# Healthy            19             7
# Heart Disease       5            30
# 
# Accuracy : 0.8033         
# 95% CI : (0.6816, 0.894)
# No Information Rate : 0.6066         
# P-Value [Acc > NIR] : 0.000848       
# 
# Kappa : 0.5938         
# 
# Mcnemar's Test P-Value : 0.772830       
# 
# Sensitivity : 0.8108         
# Specificity : 0.7917         
# Pos Pred Value : 0.8571         
# Neg Pred Value : 0.7308         
# Prevalence : 0.6066         
# Detection Rate : 0.4918         
# Detection Prevalence : 0.5738         
# Balanced Accuracy : 0.8012         
# 
# 'Positive' Class : Heart Disease  

ggplot(data = t2.df, aes(x = Var1, y = prednn, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="red1", high="steelblue2") +
  theme_hc() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Neural Network")
