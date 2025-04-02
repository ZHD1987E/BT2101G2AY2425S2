import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import NearestNeighbors
from statsmodels.api import OLS

data = pd.read_excel('BT2101_Survey_(Responses).xlsx')
data = data.drop([12, 40]) 

# Step 1: Preprocessing
# Normalize continuous variables
scaler = StandardScaler()
data[['A_level_UAS', 'Poly_GPA', 'IB_score', 'Y1_GPA', 'Current_GPA']] = scaler.fit_transform(
    data[['A_level_UAS', 'Poly_GPA', 'IB_score', 'Y1_GPA', 'Current_GPA']]
)

# Step 2: Estimate Propensity Scores
covariates = ['Age', 'Gender', 'Study_Hours', 'Non_Educational_Hours', 
              'Cognitive_Ability', 'Work_Ethic', 'Procrastination']
X = data[covariates]
y = data['AI_Usage_Frequency']

# Logistic Regression for Propensity Scores
log_reg = LogisticRegression()
data['Propensity_Score'] = log_reg.fit(X, y).predict_proba(X)[:, 1]

# Step 3: Matching
# Nearest Neighbor Matching based on Propensity Scores
nn = NearestNeighbors(n_neighbors=1)
nn.fit(data[['Propensity_Score']])
distances, indices = nn.kneighbors(data[['Propensity_Score']])

data['Matched_ID'] = indices.flatten()

# Step 4: Estimate Causal Effect
matched_data = data.loc[data['Matched_ID'].dropna().index]
IV = matched_data['AI_Usage_Frequency']
DV = matched_data['Current_GPA']

# Regression Model to Estimate Causal Effect
model = OLS(DV, IV).fit()
print(model.summary())