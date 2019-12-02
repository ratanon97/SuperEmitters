#-------------------------------------------------------------------------------
#Techno Economic Assessment of Options to Mitigate Super-Emitters in the Natural Gas Supply Chain
#MASTER SCRIPT FOR TECHNO-ECONOMIC ASSESSMENT OF OPTIONS TO MITIGATE SUPER EMITTERS IN THE NATURAL GAS SUPPLY CHAIN
#Developer: Ratanon Suksumrun CID: 01541619
#Course: Advanced Chemical Engineering with Process Systems Engineering
#Academic Year: 2018-2019
#Email: rs2318@ic.ac.uk
#Supervisor: Dr. Adam Hawkes
#Co-Supervisors: Dr.Paul Balcombe and Dr.Jasmin Cooper
#Python Version 
#Original Code Written in R, Python version is developed for learning purposes
#-------------------------------------------------------------------------------
#Import basic libraries
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import seaborn as sns
import statistics as stats
#-------------------------------------------------------------------------------
#Characterisation of EUR Data Script
#Source: Balcombe et al (2015)
#Script Source: Alberta EUR Data
#Note: EUR Data is US-Based. Proxy dataset for Canadian Data. Year collected: 2016
#Set working directory for EUR Data 
#Import EUR Data
print(os.getcwd())
os.chdir("C:\\Users\\KOMSUN\\Documents\\Files from Imperial Laptop\\Modules\\Research Project\\Data Collection\\CSV Inputs")
EUR_Data = pd.read_csv("US_EUR_Data.csv")
#------------------------------------------------------------------------------
#Data Frame Preparation
import re #Regular Expresions  
EUR_Data.iloc[14,1] = "US"
EUR_Data.iloc[24,1] = "US" #Change it into regular expressions later
EUR_Data["EUR_m3"] = EUR_Data["Value"] * 1000000
EUR_Data["LN_EUR_m3"] = np.log(EUR_Data["EUR_m3"])
Mean_EUR = round(stats.mean(EUR_Data["Value"]))
SD_EUR = stats.stdev(EUR_Data["Value"])
Median_EUR = round(stats.median(EUR_Data["LN_EUR_m3"]))
Mean_LN_EUR_m3 = round(stats.mean(EUR_Data["LN_EUR_m3"]))
SD_LN_EUR_m3 = stats.stdev(EUR_Data["LN_EUR_m3"])
Median_LN_EUR_m3 = round(stats.median(EUR_Data["LN_EUR_m3"]))
Mean_LN_EUR = round(stats.mean(EUR_Data.iloc[:,6]))
SD_LN_EUR = stats.stdev(EUR_Data.iloc[:,6])
Median_LN_EUR = round(stats.median(EUR_Data.iloc[:,6]))
#--------------------------------------------------------
#Probability Distribution Data Fitting 
import scipy.stats as s #Use scipy.stats, numpy and matplotlin for the distribution fitting
#Weibull Distribution
(exp1, k1, loc1, lam1) = s.exponweib.fit(EUR_Data["Value"], floc=0, f0=1) #Shape Parameter is f0, Location Parameter is 0
sns.set_style("whitegrid")
fig, ax = plt.subplots() #Put the whole graph in a "subplot" but will still give a normal graph #HACK
fig.set_size_inches(11.7,8.27)
plt.plot(EUR_Data["Value"],s.exponweib.pdf(EUR_Data["Value"],exp1, k1, loc1, lam1))
plt.hist(EUR_Data["Value"], bins=np.linspace(0, 16, 33), alpha=0.5)
plt.title('Histogram and Distribution Curves of US')
plt.xlabel('EUR (Mm3)')
plt.ylabel('Density')
plt.show()
