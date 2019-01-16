# -*- coding: utf-8 -*-
"""
Created on Tue Dec 25 10:00:08 2018

@author: Chintan
"""


# Importing Libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from fancyimpute import KNN 
from scipy import stats

# =============================================================================
# Read data
# =============================================================================

df = pd.read_excel("Absenteeism_at_work_Project.xls")

df.info()

# =============================================================================
# Variable's data type
# =============================================================================
df.dtypes

# =============================================================================
# Distributing Categorical and Continous variable
# =============================================================================

continuous_vars = ['Distance from Residence to Work', 'Service time', 'Age', 'Work load Average/day ', 'Transportation expense',
       'Hit target', 'Weight', 'Height', 'Body mass index', 'Absenteeism time in hours']

categorical_vars = ['ID','Reason for absence','Month of absence','Day of the week',
                     'Seasons','Disciplinary failure', 'Education', 'Social drinker',
                     'Social smoker', 'Pet', 'Son']


# =============================================================================
# Missing value analysis and treatment
# =============================================================================

#Creating dataframe with missing values present in each variable
missing_val = pd.DataFrame(df.isnull().sum()).reset_index()
missing_val

#Rename variable for missing value of dataframe
missing_val = missing_val.rename(columns = {'index': 'Variables', 0: 'Missing_percentage'})

#Percentage finding
missing_val['Missing_percentage'] = (missing_val['Missing_percentage']/len(df))*100

#Sort missing values
missing_val = missing_val.sort_values('Missing_percentage', ascending = False).reset_index(drop = True)

missing_val
# =============================================================================
# Imputing the missing value
# =============================================================================
# =============================================================================
# Dropping the observation, which have missing values in target variable
# =============================================================================
df = df.drop(df[df['Absenteeism time in hours'].isnull()].index, axis=0)
print(df['Absenteeism time in hours'].isnull().sum())
print(df.shape)


#Imputing with KNN imputation algorithm
df = pd.DataFrame(KNN(k = 3).fit_transform(df), columns = df.columns)
#Check, if any variable has missing values
df.isnull().sum().sum()


# =============================================================================
# Outlier analysis for all features/variable
# Ploting BoxPlot of continuous variables
# =============================================================================
plt.boxplot(df['Transportation expense'])
plt.xlabel("'Transportation expense'")
plt.title("BoxPlot of Variable 'Transportation expense '")
plt.ylabel('Values')

plt.boxplot([df['Height']])
plt.xlabel("'Height'")
plt.title("BoxPlot of Variable of 'Height'")
plt.ylabel('Values')


plt.boxplot(df['Work load Average/day '])
plt.xlabel("Work load Average/day ")
plt.title("BoxPlot of Variable 'Work load Average/day '")
plt.ylabel('Values')

plt.boxplot([ df['Distance from Residence to Work'], df['Service time'], df['Age'], df['Hit target'], df['Weight'], df['Body mass index']])
plt.xlabel(['1. Distance from Residence to Work', '2. Service time', '3. Age', '4. Hit target', '5. Weight', '6. Body mass index'])
plt.title("BoxPlot of rest of the Variables")
plt.ylabel('Values')


# list of variables which doesn't have outlier
neglect = ['Distance from Residence to Work', 'Weight', 'Body mass index']

# Looping over all continuou variables to detect and remove Outliers
for i in continuous_vars:
    # Avoiding the variables which doesn't have outlier
    if i in neglect:
        continue
    # Getting 75 and 25 percentile of variable "i"
    q75, q25 = np.percentile(df[i], [75,25])
    # Calculating Interquartile range
    iqr = q75 - q25
    
    # Calculating upper extream and lower extream
    minimum = q25 - (iqr*1.5)
    maximum = q75 + (iqr*1.5)
    
# Replacing all the outliers value to NA
df.loc[df[i]< minimum,i] = np.nan
df.loc[df[i]> maximum,i] = np.nan


# Imputing missing values with KNN
df = pd.DataFrame(KNN(k = 3).fit_transform(df), columns = df.columns)
# Checking if there is any missing value
df.isnull().sum().sum()

# =============================================================================
# Correlation analysis for continuous variables
# =============================================================================
df.info()


df_corr = df.loc[:,continuous_vars]

#Set the width and hieght of the plot
f, ax = plt.subplots(figsize=(10, 10))

#Generate correlation matrix
corr = df_corr.corr()

#Plot using seaborn library
sns.heatmap(corr, mask=np.zeros_like(corr, dtype=np.bool), 
            cmap=sns.diverging_palette(220, 50, as_cmap=True),
            square=True, ax=ax, annot = True)
plt.plot()

# =============================================================================
# loop for ANOVA test Since the target variable is continuous
# =============================================================================
for i in categorical_vars:
    f, p = stats.f_oneway(df[i], df["Absenteeism time in hours"])
    print("P value for variable "+str(i)+" is "+str(p))

# Droping the variables which has redundant information
to_drop = ['Weight','Age','Service time','ID']
df = df.drop(to_drop, axis = 1)

# Updating the Continuous Variables and Categorical Variables after droping some variables
continuous_vars = [i for i in continuous_vars if i not in to_drop]
categorical_vars = [i for i in categorical_vars if i not in to_drop]

final_data = df.copy()

figure(num=None, figsize=(8, 6), dpi=80, facecolor='w', edgecolor='k')

def check_relation(x,y):
    df = final_data.groupby(x)[y].sum()
    df = df.reset_index()
    df[y] = (df[y]*100)/sum(final_data[y])
    df = df.sort_values(by=['Absenteeism time in hours'])
    print(df)
    return df.plot.barh(x=x,y=y, figsize = (12,10))

# =============================================================================
# Checking the relationship between categorical independent variable and target variable
# =============================================================================

#Checking relationship between reason for absence vs Absenteeism time in hours
check_relation('Reason for absence','Absenteeism time in hours')

#Checking relationship between Month of absence vs Absenteeism time in hours
check_relation('Month of absence','Absenteeism time in hours')

#Checking relationship between Day of the week vs Absenteeism time in hours
check_relation('Day of the week','Absenteeism time in hours')

#Checking relationship between Education vs Absenteeism time in hours
check_relation('Education','Absenteeism time in hours')

#Checking relationship between Son vs Absenteeism time in hours
check_relation('Son','Absenteeism time in hours')



#Forecasting for 2011
# =============================================================================
#  As per data, there is no seperation for year wise. However, as per primary
# investigation, months are repated three times. Therefore, we can assume that given
# data is for three years
# =============================================================================
#Aggreate month of absence and depdnent variable
absence_record_monthly= final_data.groupby('Month of absence')['Absenteeism time in hours'].sum()
absence_record_monthly

absence_record_monthly=absence_record_monthly.reset_index()
absence_record_monthly

#Dividing dataset by 3
absence_record_monthly['Absenteeism hours per month'] = absence_record_monthly['Absenteeism time in hours']/3

absence_record_monthly['Month of absence'] = absence_record_monthly['Month of absence'].astype('int')
absence_record_monthly

#calling final data
final_data

#dropping categorical data from
final_data=final_data.drop(categorical_vars,axis=1)
# Importing the dataset
X = final_data.iloc[:, :-1].values
y = final_data.iloc[:, -1].values


#==============================================================================
# Feature ranking with recursive feature elimination
#==============================================================================
from sklearn.feature_selection import RFE
from sklearn.svm import SVR
# feature extraction
estimator = SVR(kernel="linear")
selector = RFE(estimator, 2, step=1)
selector = selector.fit(X, y)
selector.support_
selector.ranking_

X=X[:,[4,5]]



# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state = 0)

# =============================================================================
# Feature Scaling
# =============================================================================
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

# =============================================================================
# Fitting Random Forest Regressor & SVR to X_train and y_train
# =============================================================================
from sklearn.ensemble import RandomForestRegressor
regressor_RandomForestRegressor = RandomForestRegressor(n_estimators = 10, random_state = 0)
regressor_RandomForestRegressor.fit(X_train, y_train)

#SVR
from sklearn.svm import SVR
regressor_SVR = SVR(kernel = 'rbf')
regressor_SVR.fit(X_train, y_train)

#Linear Regression
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 3)

X_poly = poly_reg.fit_transform(X_train)
regressor_L_R = LinearRegression()
regressor_L_R.fit(X_poly, y_train)

# =============================================================================
# preidicting test set results For Random Forest and SVR
# =============================================================================
y_pred=regressor_RandomForestRegressor.predict(X_test)
y_pred=y_pred.astype(int)


y_pred_SVR=regressor_SVR.predict(X_test)
y_pred_SVR=y_pred_SVR.astype(int)


y_pred_pl_r = regressor_L_R.predict(poly_reg.fit_transform(X_test))
y_pred_pl_r = y_pred_pl_r.astype(int)
# =============================================================================
# Evaluating the model using R2 Score
# =============================================================================

from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error

R2_RF=r2_score(y_test, y_pred)
print(R2_RF)
RMSE_RF=mean_squared_error(y_test, y_pred)
print(RMSE_RF)


R2_SVR=r2_score(y_test, y_pred_SVR)
print(R2_SVR)
RMSE_SVR=mean_squared_error(y_test, y_pred_SVR)
print(RMSE_SVR)

R2_LR=r2_score(y_test, y_pred_pl_r)
print(R2_LR)
RMSE_LR=mean_squared_error(y_test, y_pred_pl_r)
print(RMSE_LR)

# =============================================================================
# Fitting 10 fold cross validation for Random Forest & SVR
# =============================================================================
from sklearn.model_selection import cross_val_score
accuracies_L = cross_val_score(estimator = regressor_RandomForestRegressor, 
                               X = X_train, y = y_train, cv = 10)
accuracies_L.mean()
accuracies_L.std()

accuracies_SVR = cross_val_score(estimator = regressor_SVR, X = X_train, y = y_train, cv = 10)
accuracies_SVR.mean()
accuracies_SVR.std()

accuracies_LR = cross_val_score(estimator = regressor_L_R, X = X_train, y = y_train, cv = 10)
accuracies_LR.mean()
accuracies_LR.std()



#