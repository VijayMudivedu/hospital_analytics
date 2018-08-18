# Care Management Analytics

### Problem Statement

A care management organisation wants to identify among its diabetic patients, the ones that are at high risk of getting re-admitted to the hospital. They wish to intervene by providing some incentive to these patients that will help them improve their health. 

### Objective: 
- To identify high-risk diabetic patients through risk stratification. 
- This will help the payer to decide what are the right intervention programs for these patients.

### Data preparation

- Remove redundant variables.
- Check for missing values and treat them accordingly.
- Scale numeric attributes and create dummy variables for categorical ones.
- Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".
- Create the derived metric 'comorbidity'

![alt text](https://cdn.upgrad.com/UpGrad/temp/91fb95f6-f205-451a-8669-003f9c7168d0/1+-+Copy+(6).png)


### Data Exploration

- Perform basic data exploration for some categorical attributes
- Perform basic data exploration for some numerical attributes

### Model Building
- Divide your data into training and testing dataset
- Compare the performance of at least two algorithms and decide which one to use for predicting risk of readmission for the patient
- Stratify your population into 3 risk buckets:
- High risk (Probability of readmission >0.7)
- Medium risk (0.3 < Probability of readmission < 0.7)
- Low risk (Probability of readmission < 0.3)
