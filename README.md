# Care Management Analytics

### Problem Statement

A care management organisation wants to identify among its diabetic patients, the ones that are at high risk of getting re-admitted to the hospital. They wish to intervene by providing some incentive to these patients that will help them improve their health. 

### Objective: 
- To identify high-risk diabetic patients through risk stratification. 
- This will help the payer to decide what are the right intervention programs for these patients.

### Data preparation
- Removed redundant variables.
- Checked for missing values and treat them accordingly.
- Scaled numeric attributes and create dummy variables for categorical ones.
- Changed the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".
- Created the derived metric 'comorbidity'

![alt text](https://cdn.upgrad.com/UpGrad/temp/91fb95f6-f205-451a-8669-003f9c7168d0/1+-+Copy+(6).png)


### Data Exploration
- Performed basic data exploration for some categorical attributes
- Performed basic data exploration for some numerical attributes

### Model Building
- Divided data into training and testing dataset
- Compared the performance of at least two algorithms and decided which one to use for predicting risk of readmission for the patient
- Stratified population into 3 risk buckets:
  - High risk (Probability of readmission >0.7)
  - Medium risk (0.3 < Probability of readmission < 0.7)
  - Low risk (Probability of readmission < 0.3)

### Approach
- Tried the modelling techniques Logistic regression, Random Forest 
- Using Logistic reggression, identified the variables that
