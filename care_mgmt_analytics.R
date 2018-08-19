#Objective of the Case Study is to:
# - identify among its diabetic patients, 
# - the ones that are at high risk of getting re-admitted to the hospital

library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(doParallel)
library(car)
library(tidyverse)

setwd("~/OneDrive/OneDrive - Atimi Software Inc/Upgrad/case study/care_management_analytics/")
care_mangment_df <- read.csv("~/OneDrive/OneDrive - Atimi Software Inc/Upgrad/case study/care_management_analytics/diabetic_data_original.csv",stringsAsFactors = TRUE)

str(care_mangment_df)

#----------------------------
#----Data preparation
#----------------------------

# Remove redundant variables.
sapply(care_mangment_df , summary)
sapply(care_mangment_df, function(x) sum(is.na(x)))

# there are no duplicate encounters
sum(duplicated(care_mangment_df$encounter_id))

# Identified Redundant variables may not be useful are:
# weight, payer_code, medical speciality, diag_1, diag_2, diag_3, max_glu_serum, A1Cresult, metformin, repaglinide, nateglinide, chlorpropamide, glimepiride, acetohexamide, glipizide, glyburide, tolbutamide, pioglitazone, rosiglitazone, acarbose, miglitol, troglitazone, tolazamide, examide, sitagliptin, insulin, glyburide-metformin, glipizide-metformin, glimepiride-pioglitazone, metformin-rosiglitazone, and metformin-pioglitazone

# 50% of data is stale in medical specialist
round(prop.table(table(care_mangment_df$medical_specialty)),3)

# Weight - 97% is of the data is unknown hence can be remove
round(prop.table(table(care_mangment_df$weight)),3)

# Payer code: 40 % of the data is unknown 
round(prop.table(table(care_mangment_df$payer_code)),3)

# maxglu 95% is have none values hence cannot be used in analysis
round(prop.table(table(care_mangment_df$max_glu_serum)),3)

# majority of the generic medicines had "No" Prescription or "Steady" the status after taking the medicine is "Steady" or no changes observed.

# insulin has significant changes . This medicine is adminsitered to Diabetic patients

# considering only variables that have no significance

diab_vars <- which(names(care_mangment_df) %in% c('weight', 'payer_code', "medical_specialty", 'repaglinide', 'nateglinide', 'chlorpropamide', 'glimepiride', 'acetohexamide', 'tolbutamide', 'pioglitazone', 'rosiglitazone', 'acarbose', 'miglitol', 'troglitazone', 'tolazamide', 'examide', 'citoglipton', "glyburide.metformin", "glipizide.metformin", "glimepiride.pioglitazone", "metformin.rosiglitazone", "metformin.pioglitazone","encounter_id",'admissiont_type_id','discharge_disposition_id','admission_source_id'))

# subsetting the dataframe 
care_df <- care_mangment_df[,-diab_vars] 

#Check for missing values and treat them accordingly.
care_df$readmit_yes_no <- factor(ifelse(care_df$readmitted %in% c("<30",">30"),'yes','no'))

# remove "?" from diag_1 and diag_2
care_df <- care_df[which(!(care_df$diag_1 %in% "?")),]   # remove the unknowns "?" from diag_1 and diag_2
care_df <- care_df[which(!(care_df$diag_2 %in% "?")),]   # remove the unknowns "?" from diag_1 and diag_1

# co-morbidity
View(care_df)

# if diag_1 != 250.XX, diag_2 != 390-459 , comorbidity = 0
# if diag_1 != 250.XX, diag_2 == 390-459 , comorbidity = 1
# if diag_1 == 250.XX, diag_2 != 390-459 , comorbidity = 2
# if diag_1 == 250.XX, diag_2 == 390-459 , comorbidity = 3

# convert the diagnosis to characters
care_df$diag_1 <- as.character(care_df$diag_1) 
care_df$diag_2 <- as.character(care_df$diag_2)
care_df$diag_3 <- as.character(care_df$diag_3)

# comorbidity derieved attribute
ztest1 <- ((as.numeric(care_df$diag_1) - 250) > 0) & ((as.numeric(care_df$diag_1) - 250) < 1)
ztest2 <- (as.numeric(care_df$diag_2) >= 390) & (as.numeric(care_df$diag_2 <= 459))

# creating comorbidity
care_df$comorbidity <-  ifelse(test = (ztest1 == 0) & (ztest2 == 0), 
                               yes = 0,  
                               no = ifelse(test = (ztest1 == 0) & (ztest2 == 1), 
                                           yes = 1,
                                           no = ifelse(test = (ztest1 == 1) & (ztest2 == 0),
                                                       yes = 2,
                                                       no = 3)))

# remove NAs from comorbidity
care_df <- care_df[which(!is.na(care_df$comorbidity)),]

bhp <- care_df 
#care_df <- bhp

# remove unknown races, with "?"
care_df$race <- as.character(care_df$race)
care_df <- care_df[which(!care_df$race %in% "?"),]
care_df$race <- as.factor(care_df$race)

# remove unknow/invalid gender
care_df$gender <- as.character(care_df$gender)
care_df <- care_df[which(!care_df$gender %in% "Unknown/Invalid"),] 

# convert diag_1 and diag_2 and diag_3 to factors
care_df$diag_1 <- as.factor(care_df$diag_1) 
care_df$diag_2 <- as.factor(care_df$diag_2)
care_df$diag_3 <- as.factor(care_df$diag_3)

# remove the readmitted 
care_df <- care_df[,-which(names(care_df) == "readmitted")]

# remove the "diag_2","diag_3","diag_1"
care_df <- care_df[,-which(names(care_df) %in% c("diag_2","diag_3","diag_1"))]

# comorbidity as factor
care_df$comorbidity <- as.factor(care_df$comorbidity)

names(care_df)[length(care_df) - 1] <- "readmit"

# diabetic patients are who had frequently visited:

care_df %>% 
  grouped_df(c('patient_nbr')) %>% 
  summarise( cnt = n()) %>% 
  filter(cnt > 10) %>%  # frequent visitors
  arrange(desc(cnt)) 


####################################
# Exploratory Data analysis
#####################################

# univariate analysis

# insulin, age
ggplot(data = care_df,aes(insulin,fill = age)) + 
  geom_bar() + 
  ggtitle("insulin and age groups")

# insulin, readmit
ggplot(data = care_df,aes(insulin,fill = readmit)) + geom_bar() + 
  ggtitle("insulin readmitted") + 
  theme(legend.position = c(0.9,0.8),
        legend.background = element_blank())

# insulin, comorbidity
ggplot(data = care_df,aes(comorbidity,fill = insulin)) + 
  geom_bar() + ggtitle("comorbditity insulin") + 
  theme(legend.position = c(0.9,0.7),
        legend.background = element_blank()) 
# comments: insluin levels fluctions are high when comorbdity is 0 and 1


# comorbidity and readmit and age
ggplot(data = care_df,aes(comorbidity,fill = readmit)) + geom_bar() + ggtitle("comorbditity/readmitted") + theme(legend.position = c(0.9,0.7),
                                                                                                                 legend.background = element_blank()) 

#Comments: readmit percentage is highest in the patients with comorbdity 2 and 3

# readmit and age, which patient age groups readmit frequenty
ggplot(data = care_df,aes(age,fill = readmit)) + geom_bar() + ggtitle("age / readmitted") + 
  theme(legend.position = c(0.1,0.7),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 90)) 
# memebers in the age group 40-50, 50-60 and 60-70 and 70-80 80-90 are the most that readmitted
names(care_df)

# readmit and diabetesMed
ggplot(data = care_df,aes(readmit, fill = diabetesMed)) + geom_bar() + 
  ggtitle("diabetes /readmitted") + 
  theme(legend.position = c(0.9,0.7),
        legend.background = element_blank()) 
# significant percentage of population number of people taking diabetes medicaton are getting readmitted

# diabetesMed and comorbidity
ggplot(data = care_df, aes(comorbidity,fill = diabetesMed)) + geom_bar() + 
  ggtitle("comorbditity / diabetesMed") + 
  theme(legend.position = c(0.9,0.7),
        legend.background = element_blank()) 
# diabetes medicine usage is highest in patients with comorbidityvbn 1, 2

# gender and admission_typ_id
ggplot(data = care_df,aes(fill = factor(admission_type_id), age)) + geom_bar() + 
  ggtitle("admission type id, readmitted") + 
  theme(legend.position = "bottom", #c(0.9,0.7),
        legend.background = element_blank()) 

# num of diagnosis
ggplot(data = care_df,aes(number_diagnoses, fill = readmit)) + geom_histogram(binwidth = 5)
# 50% of the members with 5 or more dianoses are being readmitted

# num_medications by age, gender, diabetesMed
ggplot(data = care_df,aes(num_medications, fill = gender)) + 
  geom_bar(position = "dodge") + ggtitle("num_medications/gender") + 
  theme(legend.position = "bottom", #c(0.9,0.7),
        legend.background = element_blank()) 
# women appears to be more on medications

# num of medications and diabetesMed
ggplot(data = care_df,aes(num_medications,fill = diabetesMed)) + geom_bar(position = "dodge") + 
  ggtitle("nom_medications / diabetes med") + 
  theme(legend.position = c(0.9,0.7),
        legend.background = element_blank()) 
# A set of memebers are on taking medicines are diabetes medicines

# num_medications, num_diagnosis, num of emergencies, no of lab procedures


# plotting the numeric variables
GGally::ggpairs(care_df[,c("time_in_hospital" ,"num_lab_procedures", "num_procedures", "num_medications", "number_outpatient" ,"number_emergency" ,  "number_inpatient" ,"number_diagnoses")])


# convert the dataframe into dummy variables
care_df$gender <- as.factor(care_df$gender) 
care_df_model <- care_df[,-1] # removing the patient number

# ratio of the readmission label
prop.table(table(care_df$readmit))

#-----------------------
# dummy variables
#-----------------------
library(caret)

xDummy <- dummyVars(formula = ~ .,data = care_df_model,sep = "_", 
                    levelsOnly = FALSE, fullRank = TRUE)

care_dummy_var_df <- data.frame(predict(xDummy,care_df_model))
care_dummy_var_df$readmit_yes <- factor(care_dummy_var_df$readmit_yes)

#--------------------------------------------------------
# Splitting the dataset in the train, validation and test
#--------------------------------------------------------
#
# Split the dataset into train and test dataset
# tr

set.seed(100)
indices <- sample(1:nrow(care_dummy_var_df), 0.6*nrow(care_dummy_var_df))

train_df <- care_dummy_var_df[indices,] # training Dataset
validation_df <- care_dummy_var_df[-indices,]

#indices_test <- sample(1:nrow(validation_df),0.5*nrow(validation_df))
#validation_df <- care_dummy_var_df[indices_test,] # test_dataset
#test_df <- care_dummy_var_df[-indices_test,] # training_dataset

# renaming the class label
names(train_df)[length(train_df)-3] <- "readmit"
names(validation_df)[length(validation_df)-3] <- "readmit"
#names(test_df)[length(test_df)-3] <- "readmit"


#--------------------------
# Model building 
#-------------------------

# using logsitic regression
init_model <- glm(formula = readmit ~ ., data = train_df,family = "binomial")

# step AIC of the initial model, 
registerDoParallel(cl = 4,cores = 4)
init_model_step_AIC <- MASS::stepAIC(object = init_model,direction = "both")

summary(init_model_step_AIC)
vif(init_model_step_AIC) 
# COmments: most of the values aren't not large enough suggesting absence of multicollienarity

# significant variable insulin_Up 
model_01 <- glm(formula = readmit ~ race_Asian + race_Hispanic + race_Other + 
                  gender_Male + age_.30.40. + age_.40.50. + age_.50.60. + age_.60.70. + 
                  age_.70.80. + age_.80.90. + admission_type_id + time_in_hospital + 
                  num_lab_procedures + num_procedures + number_outpatient + 
                  number_emergency + number_inpatient + number_diagnoses + 
                  max_glu_serum_.300 + max_glu_serum_None + A1Cresult_Norm + 
                  metformin_Steady + metformin_Up + insulin_Steady + 
                  change_No + diabetesMed_Yes + comorbidity_1 + comorbidity_2 + 
                  comorbidity_3, family = "binomial", data = train_df)

summary(model_01)

# non-sigificant variable, age_.30.40.
model_02 <- glm(formula = readmit ~ race_Asian + race_Hispanic + race_Other + 
                  gender_Male +  age_.40.50. + age_.50.60. + age_.60.70. + 
                  age_.70.80. + age_.80.90. + admission_type_id + time_in_hospital + 
                  num_lab_procedures + num_procedures + number_outpatient + 
                  number_emergency + number_inpatient + number_diagnoses + 
                  max_glu_serum_.300 + max_glu_serum_None + A1Cresult_Norm + 
                  metformin_Steady + metformin_Up + insulin_Steady + change_No + 
                  diabetesMed_Yes + comorbidity_1 + comorbidity_2 + comorbidity_3, 
                family = "binomial", data = train_df)

summary(model_02)

logsitic_final <- model_02

#-----------------
# Model prediction - logisitic
#-----------------
# getting the validation predictor variables
validation_predictors <- names(train_df)[names(train_df) != "readmit"]

# predicting
predict_glm <- stats::predict(object = logsitic_final,
                              validation_df[,validation_predictors],
                              type = "response")


predict_glm_yes_no <-  as.factor(ifelse(predict_glm > 0.55, 1,0))


# confusionMatrix
confusionMatrix(data = predict_glm_yes_no,
                reference = factor(validation_df$readmit),
                positive = "1")
# thus the accuracy of the dataset is extremely, 
# calculating the cut-off probaility
perform_fn <- function(cutoff) 
{
  attri_pred <- as.factor(ifelse(predict_glm >= cutoff, 1,0))
  conf <- confusionMatrix(attri_pred, factor(validation_df$readmit), positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  #colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(predict_glm)
# Sequence and Empty Matrix to Store thte Probabilities, Sensitivity, Specificity and Accouracy
s = seq(0.15,0.96,length = 100)
output_matrix = matrix(0,100,3)

# Iterate th for loop to compute Sens, Spec, and Accuracy ---
for (i in 1:100) {
  output_matrix[i,] = perform_fn(s[i])
} 
# create an output matrix data.frame

colnames(output_matrix) <- c("sensitivity", "specificity", "accuracy")
head(output_matrix)

output_matrix <- data.frame(output_matrix)
head(output_matrix)
# Computing the Cutoff Matrix and CUTOFF VALUE accoracy and specificiy ----
cutoff_max <- s[which.max(abs(output_matrix$sensitivity + output_matrix$specificity + output_matrix$accuracy))]

cut_off <- s[which.min(abs(output_matrix$sensitivity - output_matrix$specificity))] # CUTOFF VALUE ----
cut_off

predict_glm_yes_no_cutoff <- factor(ifelse(predict_glm > cut_off,1,0))

confusionMatrix(predict_glm_yes_no_cutoff,factor(validation_df$readmit))

# thus the balanced accuracy with is 0.6133 is a nominal estimate


# Using K-cross validation

train_df$readmit <- factor(train_df$readmit)
summary(model_02)

trn_cntrol <- trainControl(method = "cv",
             number = 5,
             allowParallel = TRUE
             )

logistic_model_cv <- caret::train(readmit ~ race_Asian + race_Hispanic + race_Other + 
                                    gender_Male + age_.40.50. + age_.50.60. + age_.60.70. + age_.70.80. + 
                                    age_.80.90. + admission_type_id + time_in_hospital + num_lab_procedures + 
                                    num_procedures + number_outpatient + number_emergency + number_inpatient + 
                                    number_diagnoses + max_glu_serum_.300 + max_glu_serum_None + 
                                    A1Cresult_Norm + metformin_Steady + metformin_Up + insulin_Steady + 
                                    change_No + diabetesMed_Yes + comorbidity_1 + comorbidity_2 + 
                                    comorbidity_3,
                               data = train_df,
                               method ="glm",
                               family = "binomial",
                               trControl = trn_cntrol)

predict_glm_cv <- stats::predict(logistic_model_cv,validation_df[,validation_predictors])

#predict_glm_cv_1 <- as.matrix(predict_glm_cv["1"])
factor(as.vector(predict_glm_cv_1))
confusionMatrix(factor(predict_glm_cv) ,factor(validation_df$readmit))

# K-cross validaton too hasn't significantly improved the balanced accuracy with logistic regression


#-------------modelling using random forest---------

registerDoParallel(cl = 4,cores = 4)
# train control using k-cross validation of 10 fold.
trn_cntrol_rf <- caret::trainControl(method = "repeatedcv",
                                     number = 10,
                                     repeats = 3,
                                     allowParallel = TRUE)

tunegrid_rf <- expand.grid(.mtry = round(sqrt(ncol(train_df))), ntree = seq(100,500,100))
#tunegrid_rf <-  expand.grid(interaction.depth = 1:3,shrinkage = .1,n.trees = c(10, 50, 100),n.minobsinnode = 10)

# creating the treebag boosting model for the insurance dataframe
library(randomForest)
model_rf <- randomForest(readmit ~ race_Asian + race_Hispanic + race_Other + 
                               gender_Male + age_.40.50. + age_.50.60. + age_.60.70. + age_.70.80. + 
                               age_.80.90. + admission_type_id + time_in_hospital + num_lab_procedures + 
                               num_procedures + number_outpatient + number_emergency + number_inpatient + 
                               number_diagnoses + max_glu_serum_.300 + max_glu_serum_None + 
                               A1Cresult_Norm + metformin_Steady + metformin_Up + insulin_Steady + 
                               change_No + diabetesMed_Yes + comorbidity_1 + comorbidity_2 + 
                               comorbidity_3,
                             data = train_df,
                             #method = "rf",
                             trControl = trn_cntrol_rf,
                             tuneGrid = tunegrid_rf,
                             metric = "auc")

#model_rf <- ins_model_rf
attributes(model_rf)
summary(model_rf)

# predicting using the random forest
predict_rf <-  stats::predict(model_rf,validation_df[,validation_predictors],type = "response")
head(predict_rf)

confusionMatrix(data = predict_rf,reference = factor(validation_df$readmit),positive = "1")


# Accuracy : 0.7618          
# 95% CI : (0.7564, 0.7672)
# No Information Rate : 0.533           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5153          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.6397          
# Specificity : 0.8689          
# Pos Pred Value : 0.8104          
# Neg Pred Value : 0.7335          
# Prevalence : 0.4670          
# Detection Rate : 0.2987          
# Detection Prevalence : 0.3686          
# Balanced Accuracy : 0.7543          

# Thus balanced accuracy, sensitivity,sensivity, detection rate accuracy have singificantly improved using random forests

#----------------------------------
# stratification of customer
#----------------------------------

# using the probabilities generated from the random forest
predict_prob_rf <-  stats::predict(model_rf,validation_df[,validation_predictors],type = "prob")

rf_prob <- as.matrix(predict_prob_rf)
as.data.frame(rf_prob)[2] %>% head()
rf_prob <- as.data.frame(rf_prob)[2]


validation_df$stratificaiton <-  ifelse(test = as.data.frame(rf_prob) > 0.7,
                                yes = "High risk", 
                                no = ifelse(test = rf_prob < 0.3,
                                            yes = "Low Risk",
                                            no = "Medium Risk"))








# 
# 
# # ------------model building using decision trees ------------
# # set the number of folds in cross test to 5
# tree.control = trainControl(method = "cv", number = 5)
# 
# # set the search space for CP
# tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))
# 
# # train model
# tree.model <- train(readmit ~ train_df,
#                     data = train_df,
#                     method = "rpart",
#                     metric = "auc",
#                     trControl = tree.control,
#                     tuneGrid = tree.grid,
#                     control = rpart.control(minsplit = 50,
#                                             minbucket = 20))
# 
# 
# # Thus using the values from the 
# 
# 
# #-------------modelling using Extreme Gradient Boosting---------
# 
# registerDoParallel(cl = 4,cores = 4)
# trn_cntrol_xgb <- caret::trainControl(method = "repeatedcv",
#                                       number = 3,
#                                       repeats = 3,
#                                       #summaryFunction = twoClassSummary,	# Use AUC to pick the best model
#                                       allowParallel = TRUE)
# 
# # and tuning parapmeters for the xgb tree
# xgb.grid <- expand.grid(eta = c(0.05,0.3, 0.075), # 3 
#                         nrounds = c(50, 75, 100),  # 3
#                         max_depth = 3:6,  # 4
#                         min_child_weight = c(2.0, 2.25), #2 
#                         colsample_bytree = c(0.3, 0.4, 0.5), # 3
#                         gamma = 0, #1
#                         subsample = 1)  # 1
# 
# # Modelling the dataset using the Extreme Gradient Boosting algorithm
# model_xgb <- caret::train(readmit ~ race_Asian + race_Hispanic + race_Other + 
#                             gender_Male + age_.40.50. + age_.50.60. + age_.60.70. + age_.70.80. + 
#                             age_.80.90. + admission_type_id + time_in_hospital + num_lab_procedures + 
#                             num_procedures + number_outpatient + number_emergency + number_inpatient + 
#                             number_diagnoses + max_glu_serum_.300 + max_glu_serum_None + 
#                             A1Cresult_Norm + metformin_Steady + metformin_Up + insulin_Steady + 
#                             change_No + diabetesMed_Yes + comorbidity_1 + comorbidity_2 + 
#                             comorbidity_3, 
#                           data = train_df,
#                           method = "xgbTree",
#                           trControl = trn_cntrol_xgb,
#                           tuneGrid = xgb.grid)
# 
# model_xgb








