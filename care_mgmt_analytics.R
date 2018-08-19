#Objective of the Case Study is to:
# - identify among its diabetic patients, 
# - the ones that are at high risk of getting re-admitted to the hospital

library(dplyr)
library(tidyr)

setwd("~/OneDrive/OneDrive - Atimi Software Inc/Upgrad/case study/care_management_analytics/")
care_mangment_df <- read.csv("~/OneDrive/OneDrive - Atimi Software Inc/Upgrad/case study/care_management_analytics/diabetic_data_original.csv",stringsAsFactors = TRUE)

head(care_mangment_df)

# check the structure of data
str(care_mangment_df)

#----Data preparation
#----Feature Engineering -------------
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

names(care_df)[length(care_df)-1] <- "readmit"

# diabetic patients are who had frequently visited:
care_df %>% 
  grouped_df(c('patient_nbr')) %>% 
  summarise( cnt = n()) %>% 
  filter(cnt > 10) %>%  # frequent visitors
  arrange(desc(cnt)) 


####################################
# Exploratory Data analysis
#####################################

library(ggplot2)

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

sapply(care_df, summary)

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
        legend.background = element_blank()) + 
  
#  



# Scale numeric attributes and create dummy variables for categorical ones.
# Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".
# Create the derived metric 'comorbidity', according to the following scheme -