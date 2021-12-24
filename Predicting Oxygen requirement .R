


###Necessary libraries####
library(mlbench)
library(caret)           # For Machine Learning Models
library( rpart )         
library( rpart.plot )    
library(rattle) 
library( liver ) 
library(cvms)


#--------------------------------------------------------------------------------------------------------
## Measuring the Run Time of Model##

# sample func, whose exec time will be measured
sleep_func <- function() { Sys.sleep(5) } 
startTime <- Sys.time()

sleep_func()
endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)

#----------------------------------------------------------------------------------------------------------
#####Set Working directory & Read the data#####

setwd( "C:\\Users\\Dropbox\\COVID data-Kerman\\Clean Data&R code" )  # set the working directory (wd)

data = read.csv(file = "COVID-Kerman2-opium.csv")
str( data ) 
dim(data)

sum(is.na(data))  ##check for missing value##

#------------------------------------------------------------------------------------------------------------

#delete some unnessary columns 

data = subset(data, select = -Patient_ID)
data = subset(data, select = -Patient_job)

#-------------------------------------------------------------------------------------------------------------

#####Change the value of Target variable#####
##(merge them and reduce it from 6 to 2 categories)##

data$Type.of.treatment[data$Type.of.treatment == 5] <- 0
data$Type.of.treatment[data$Type.of.treatment == 6] <- 1
data$Type.of.treatment[data$Type.of.treatment == 3] <- 1
data$Type.of.treatment[data$Type.of.treatment == 4] <- 1

#----------------------------------------------------------------------------------------


##change sex to a binary variable##
data$SEX[data$SEX == 2] <- 0


#----------------------------------------------------------------------------------------

######Change the data str of all variables to factor######

data$Type.of.treatment = as.factor(data$Type.of.treatment)
data$SEX = as.factor(data$SEX)
data$Hypothyroidism.or.hyperthyroidism = as.factor(data$Hypothyroidism.or.hyperthyroidism)
data$Diabetes = as.factor(data$Diabetes)
data$blood.pressure = as.factor(data$blood.pressure)
data$Obesity = as.factor(data$Obesity)
data$Lung.disease = as.factor(data$Lung.disease)
data$CD = as.factor(data$CD)
data$Fever = as.factor(data$Fever)
data$Cough = as.factor(data$Cough)
data$Sore.throat = as.factor(data$Sore.throat)
data$runny.nose = as.factor(data$runny.nose)
data$Chest.pain = as.factor(data$Chest.pain)
data$Diarrhea = as.factor(data$Diarrhea)
data$Shortness.of.breath = as.factor(data$Shortness.of.breath)
data$nausea.and.vomiting = as.factor(data$nausea.and.vomiting)
data$Acute.renal.impairment = as.factor(data$Acute.renal.impairment)
data$Cigarettes = as.factor(data$Cigarettes)
#data$influenza.vaccine = as.factor(data$influenza.vaccine)
data$opioid.addiction = as.factor(data$opioid.addiction)

str(data)
glimpse(data)
view(data)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

###Two-Proportions Z-Test (opioid addiction-Type of treatment)###

table(data$opioid.addiction, data$Type.of.treatment)

prop.test(x=c(11,15),n=c(275,97))


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Chi Square Test for the homogeneity of proportions

chisqure= table(data$opioid.addiction , data$Type.of.treatment)

chisq.test( chisqure )

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
###Percentage of opium-addicted and non-addicted hospitalized COVID-19 patients among females and males using ggplot###

ggplot( data = data ) +
  geom_bar( aes( x = factor( opioid.addiction ) ) ) + 
  scale_x_discrete( "opioid addiction" ) 

ggplot( data = data ) +
  geom_bar( aes( x = factor( opioid.addiction ), fill = SEX ), position = "stack" ) + 
  scale_x_discrete(labels = c('Non-addicted','Addicted' )) +
  scale_y_continuous( labels = function(x) paste0(round(x/398*100,1), "%"), breaks = seq(0, 398, 19.9), "Percent") + 
  guides( fill = guide_legend( title = "Sex" ) ) + 
  scale_fill_manual( values = c( "dimgrey", "firebrick" ),labels = c('Female','Male') )+ theme(axis.text.x = element_text(face="bold", 
                                                                                              size=14 ),
                                                                   axis.text.y = element_text(face="bold", 
                                                                                            size=14),
                                                                   
                                                                   axis.title.y = element_text(size = 14, face = "bold"), legend.title=element_text(size=15,face = "bold"), 
                                                                   legend.text=element_text(size=14,face = "bold"),axis.title.x = element_blank())


#Normalized Plot#

ggplot( data = data ) +
  geom_bar( aes( x = factor( opioid.addiction ), fill = SEX ), position = "fill" ) + 
  scale_x_discrete(labels = c('Non-addicted','Addicted') ) + 
  scale_y_continuous("Percent") + 
  guides( fill = guide_legend( title = "Sex" ) ) + 
  scale_fill_manual( values = c( "dimgrey", "firebrick" ),labels = c('Female','Male') )+ theme(axis.text.x = element_text(face="bold", 
                                                                                                                          size=14 ),
                                                                                               axis.text.y = element_text(face="bold", 
                                                                                                                          size=14),
                                                                                               
                                                                                               axis.title.y = element_text(size = 14, face = "bold"), legend.title=element_text(size=15,face = "bold"), 
                                                                                               legend.text=element_text(size=14,face = "bold"),axis.title.x = element_blank() )


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Fatality percentage for opium-addicted and non-addicted hospitalized COVID-19 patients using ggplot###

ggplot( data = data ) +
  geom_bar( aes( x = factor( Type.of.treatment ), fill = opioid.addiction ), position = "stack" ) + 
  scale_x_discrete( "Type of treatment" ) + 
  scale_y_continuous( labels = function(x) paste0(round(x/398*100,1), "%"), breaks = seq(0, 398, 19.9), "Percent") + 
  guides( fill = guide_legend( title = "Opioid Addiction" ) ) + 
  scale_fill_manual( values = c( "dimgrey", "firebrick" ) ) 


#Normalized Plot#
ggplot( data = data ) +
  geom_bar( aes( x = factor( Type.of.treatment ), fill = opioid.addiction ), position = "fill" ) + 
  scale_x_discrete( "Type of treatment" ) + 
  scale_y_continuous("Percent") + 
  guides( fill = guide_legend( title = "Opioid Addiction" ) ) + 
  scale_fill_manual( values = c( "dimgrey", "firebrick" ) ) 

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###### Data partitioning ( train set & test set) ######

set.seed( 42 )

data.sets = partition( data = data, prob = c( 0.80, 0.20 ) )

train = data.sets $ part1
test  = data.sets $ part2

##table of Target variable frequency##
table(data$Type.of.treatment)  # in all data set
table(train$Type.of.treatment) #in train set
table(test$Type.of.treatment)  # in test set

##Proportion of Target variable in train and test##
prop.table((table(train$Type.of.treatment))) #in train
prop.table((table(test$Type.of.treatment)))  # in test

#----------------------------------------------------------------------------------------------------------------------------------------------------

getModelInfo()$glm$type  # to demonstrate model type

#-----------------------------------------------------------------------------------------------------------------------------------------------------

############################################################
#############---Machine learning algorithms---##############
############################################################


#----TRAIN THE MODELS----#

#Train control setting
control <- trainControl(method="repeatedcv", number=10, repeats=3)

metric <- "Accuracy"

seed <- 42

# C5.0
set.seed(seed)
fit.c50 <- train(Type.of.treatment~., data=train, method="C5.0", metric=metric,trControl=control)

# Random Forest
set.seed(seed)
fit.rf <- train(Type.of.treatment~., data=train, method="rf", metric=metric,trControl=control)

# Neural network
set.seed(seed)
fit.nnet <- train(Type.of.treatment~., data=train, method="nnet", trControl=control)

#Logistic Regression
set.seed(seed)
fit.glm <- train(Type.of.treatment~., data=train, method='glm', metric=metric,trControl=control)


#xgbtree
set.seed(seed)
fit.xgbtree <- train(Type.of.treatment~., data=train, method='xgbTree', metric=metric,trControl=control)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Model Summary##
summary(fit.glm)
summary(fit.glm$finalModel)
summary(fit.glm)$coef
plot(fit.glm$finalModel)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##--Visualization the results--##

results <- resamples(list(C5.0=fit.c50, RF=fit.rf,  NNs=fit.nnet,
                          LR=fit.glm, XGBoost=fit.xgbtree))


summary(results)    # Table comparison

bwplot(results)     # boxplot comparison

dotplot(results)    # Dot-plot comparison


#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# estimate variable importance for each model

importance <- varImp(fit.xgbtree, scale=TRUE)

print(importance)  # summarize importance

plot(importance)  #plot importance

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#----PREDICTION----#
seed <- 42

#C50
predictions_c50 <- predict(fit.c50, test)
confusionMatrix(predictions_c50, test$Type.of.treatment)

#Random Forest
predictions_rf <- predict(fit.rf, test)
confusionMatrix(predictions_rf, test$Type.of.treatment)

# Neural Networks
predictions_nn <- predict(fit.nnet, test)
confusionMatrix(predictions_nn, test$Type.of.treatment)

#Logistic Regression
predictions_lr <- predict(fit.glm, test)
confusionMatrix(predictions_lr, test$Type.of.treatment)

#XGBoost
predictions_xgbtree <- predict(fit.xgbtree, test)
confusionMatrix(predictions_xgbtree, test$Type.of.treatment)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----plot confusion matrix------##
s=confusionMatrix(predictions_xgbtree, test$Type.of.treatment)
s$table

#Confusion matrix
fourfoldplot(s$table, main= "XGBoost",color = c("dimgrey", "firebrick") )+ 
  text(-0.4,0.4, "TN", cex=1.2) + 
  text(0.4, -0.4, "TP", cex=1.2) + 
  text(0.5,0.5, "FP", cex=1.2) + 
  text(-0.5, -0.5, "FN", cex=1.2)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------

##------plotting ROC curve-----##

#Train control setting for ROC curve
control <- trainControl(method="repeatedcv", number=10, repeats=3, 
classProbs=T,savePredictions = T)

levels(train$Type.of.treatment) <- c("no", "yes") #change the target variable to character

seed <- 42

metric <- "Accuracy"


# C5.0
set.seed(seed)
fit.c50 <- train(Type.of.treatment~., data=train, method="C5.0", metric=metric,trControl=control)

# Random Forest
set.seed(seed)
fit.rf <- train(Type.of.treatment~., data=train, method="rf", metric=metric,trControl=control)

# Neural network
set.seed(seed)
fit.nnet <- train(Type.of.treatment~., data=train, method="nnet", trControl=control)

#Logistic Regression
set.seed(seed)
fit.glm <- train(Type.of.treatment~., data=train, method='glm', metric=metric,trControl=control)

#xgbtree
set.seed(seed)
fit.xgbtree <- train(Type.of.treatment~., data=train, method='xgbTree', metric=metric,trControl=control)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#----PREDICTION----#

levels(test$Type.of.treatment) <- c("no", "yes")
seed <- 42

#C50
predictions_c50 <- predict(fit.c50, test, type = "prob")
#Random Forest
predictions_rf <- predict(fit.rf, test, type = "prob")
# Neural network
predictions_nn <- predict(fit.nnet, test, type = "prob")
#Logistic Regression
predictions_lr <- predict(fit.glm, test, type = "prob")
#xgbtree
predictions_xgbtree <- predict(fit.xgbtree, test, type = "prob")

###--------ROC Curve--------###
library( pROC )
plot.roc( test$Type.of.treatment,predictions_lr$yes ,legacy.axes = T, col = "black", lwd = 2,
          xlab = "False Positive Rate", ylab = "True Positive Rate", print.auc = T, print.auc.y = 0.45, ci=TRUE, add=FALSE, asp = NA,grid=TRUE)
plot.roc( test$Type.of.treatment, predictions_nn$yes, legacy.axes = T, add = T, col = "red", print.auc = T, print.auc.y = 0.40, ci=TRUE)
plot.roc( test$Type.of.treatment, predictions_rf$yes, legacy.axes = T, add = T, col = "blue", print.auc = T, print.auc.y = 0.35, ci=TRUE)
plot.roc( test$Type.of.treatment, predictions_c50$yes, legacy.axes = T, add = T, col = "purple", print.auc = T, print.auc.y = 0.3, ci=TRUE)
plot.roc( test$Type.of.treatment, predictions_xgbtree$yes, legacy.axes = T, add = T, col = "brown", print.auc = T, print.auc.y = 0.25, ci=TRUE )

?plot.roc

text( x = 1-0.90, y = 0.3 - .06 * 0, "Logistic Regression", col = "black" , cex=1.15)
text( x = 1-0.90, y = 0.3 - .06 * 1, "Neural Networks", col = "red" , cex=1.15)
text( x = 1-0.90, y = 0.3 - .06 * 2, "Random Forest", col = "blue" , cex=1.15)
text( x = 1-0.90, y = 0.3 - .06 * 3, "Decision Tree-C5.0", col = "purple" , cex=1.15)
text( x = 1-0.90, y = 0.3 - .06 * 4, "XGBoost", col = "brown" )





