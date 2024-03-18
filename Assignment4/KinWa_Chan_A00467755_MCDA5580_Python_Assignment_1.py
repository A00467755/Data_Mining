#!/usr/bin/env python
# coding: utf-8

#Install packages
#pip install fredapi
#pip install yfinance

#Chan Kin Wa A00467755

# In[1]:


#Problem (Assignment 1)
"""
The US stock market fluctuated one of the major reason was Federal Reserve is continuing increasing the Federal Funds Effective Rate. 
Beside, the stock market is also reflecting the economy situation. This study is going to find the relationship of the stock market performance  
(use Dow Jones Index as an indicator) to Federal Funds Effective Rate and unemployment rate
"""

#Analysis
"""
# Assumption: 
# Covid period (2020 - 2022) is exlcuded Covid is a special factor distorted normal market behaviour
# Data from 2010 -2019 are used to reflect the patterns of these figures in recent years


Dow Jones Index vs Federal Funds Effective Rate
------------------------------------------------
1.
We can observe the r-square in the linear regression analysis is high (~0.76), it seems there is high relationship between Dow Jones Index
and Federal Funds Effective Rate. 

However, we can observe many "outliners" between Federal Funds Effective Rate from 0 to 0.5.
Further quantitative anaylsis can be done discover more features of these "outliners"  or qualitative anaylsis may be 
required to explore the reasons behind.


2.
From the line chart we can observe the trend of the set of data are similar.


3.
For conclude, there are hints that Federal Funds Effective Rate and Dow Jones Index are generally following a proportional relationship. However, there 
are many exception cases, further quantitative or qualitative anaylsis may required


Dow Jones Index vs unemployment rate
------------------------------------------------

1.
We can observe the r-square in the linear regression analysis is very high (~0.87), it seems there is high relationship between Dow Jones Index
and Federal Funds Effective Rate.


2.
From the line chart we can observe the two sets of data are inversely proportioned with the trend of Dow Jones Index is more fluctuated compared to 
that for unemployment rate is more smooth.

3.
For conclude, there are hints that Federal Funds Effective Rate and Dow Jones Index are generally following an inversely proportional relationship. 
However, there are many exception cases (in linear regession analysis), further quantitative or qualitative anaylsis may required

"""


import matplotlib.pyplot as plt
import pandas as pd
from fredapi import Fred
import yfinance as yf
from scipy.stats import linregress


#Prepare Data
#####################################
# download FRED fund rate data
fred = Fred(api_key='cd0d6eacc4d7e8f1ed780b9ef80f6191') # replace with your FRED API key
start_date = "2010-01-01"
end_date = "2019-12-31"
fund_rate_df = pd.DataFrame(fred.get_series('FEDFUNDS', start_date, end_date)).rename(columns={0: 'FRED Fund Rate'})
unemployment = pd.DataFrame(fred.get_series('UNRATE', start_date, end_date)).rename(columns={0: 'Unemployment Rate'})

# download Dow Jones data
dow_df = yf.download('^DJI', start=start_date, end=end_date, interval='1mo')['Adj Close'].to_frame().rename(columns={'Adj Close': 'Dow Jones Index'})



#Dow Jones Index vs Federal Funds Effective Rate
###########################################################################

# combine the data into a single dataframe
combined_df = pd.concat([fund_rate_df, dow_df], axis=1)

# Perform linear regression analysis
#####################################
slope, intercept, r_value, p_value, std_err = linregress(combined_df['FRED Fund Rate'], combined_df['Dow Jones Index'])

# create a scatter plot of the data
plt.scatter(combined_df['FRED Fund Rate'], combined_df['Dow Jones Index'])

# add a regression line to the plot
plt.plot(combined_df['FRED Fund Rate'], intercept + slope * combined_df['FRED Fund Rate'], 'r', label='Regression Line')

# add labels and legend
plt.xlabel('FRED Fund Rate')
plt.ylabel('Dow Jones Index')
plt.title('FRED Fund Rate vs. Dow Jones Index')
plt.legend()

# print the regression statistics
print(f"Slope: {slope}")
print(f"Intercept: {intercept}")
print(f"R-squared: {r_value**2}")
print(f"P-value: {p_value}")
print(f"Standard Error: {std_err}")

# show the plot
plt.show()

# Show Line Chart to show the trend
#####################################
# create a figure and axis objects
fig, ax1 = plt.subplots()

# create a secondary axis object
ax2 = ax1.twinx()

# plot the data on the respective axis objects
ax1.plot(combined_df.index, combined_df['FRED Fund Rate'], 'r--', label='FRED Fund Rate')
ax2.plot(combined_df.index, combined_df['Dow Jones Index'], 'g--', label='Dow Jones Index')

# add labels and legend
ax1.set_xlabel('Year')
ax1.set_ylabel('FRED Fund Rate (Index)', color='red')
ax2.set_ylabel('Dow Jones Index (Index)', color='green')
plt.title('FRED Fund Rate and Dow Jones Index Data')
plt.legend()

# show the plot
plt.show()



#Dow Jones Index vs Federal Unemployment Rate
###########################################################################

combined_df = pd.concat([unemployment, dow_df], axis=1)

# Perform linear regression analysis
#####################################
# perform linear regression analysis
slope, intercept, r_value, p_value, std_err = linregress(combined_df['Unemployment Rate'], combined_df['Dow Jones Index'])

# create a scatter plot of the data
plt.scatter(combined_df['Unemployment Rate'], combined_df['Dow Jones Index'])

# add a regression line to the plot
plt.plot(combined_df['Unemployment Rate'], intercept + slope * combined_df['Unemployment Rate'], 'r', label='Regression Line')

# add labels and legend
plt.xlabel('Unemployment Rate')
plt.ylabel('Dow Jones Index')
plt.title('Unemployment Rate vs. Dow Jones Index')
plt.legend()

# print the regression statistics
print(f"Slope: {slope}")
print(f"Intercept: {intercept}")
print(f"R-squared: {r_value**2}")
print(f"P-value: {p_value}")
print(f"Standard Error: {std_err}")

# show the plot
plt.show()



# Show Line Chart to show the trend
#####################################
# Plotting the data
fig, ax1 = plt.subplots()

ax1.plot(unemployment, color='red')
ax1.set_xlabel('Year')
ax1.set_ylabel('Unemployment Rate', color='red')
ax1.tick_params(axis='y', labelcolor='red')
ax1.set_ylim([3, 15])

ax2 = ax1.twinx()
ax2.plot(dow_df, color='blue')
ax2.set_ylabel('Dow Jones Industrial Average', color='blue')
ax2.tick_params(axis='y', labelcolor='blue')

plt.title('Unemployment Rate vs Dow Jones Industrial Average')
plt.show()