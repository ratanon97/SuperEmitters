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
test = EUR_Data["Value"] * 2
#--------------------------------------------------------
#Probability Distribution Data Fitting 
import scipy.stats as s #Use scipy.stats, numpy and matplotlib for the distribution fitting
fwEURshape,fwEURlocation,fwEURscale = s.invweibull.fit(EUR_Data["Value"] ) #Weibull Distribution
fgEURshape,fgEURlocation,fgEURscale = s.gamma.fit(EUR_Data["Value"]) #Gamma Distribution
flnEURshape,flnEURlocation,flnEURscale = s.lognorm.fit(EUR_Data["Value"]) #Log-Normal Distribution
fllEURshape,fllEURlocation,fllEURscale = s.fisk.fit(EUR_Data["Value"]) #Log-Logistic Distribution (Fisk in scipy)
#Estimate parameters for the plot using the MLE estimate
sns.set_style("darkgrid")
fig, ax = plt.subplots() #Put the whole graph in a "subplot" but will still give a normal graph #HACK
fig.set_size_inches(11.7,8.27)
plt.plot(EUR_Data["Value"],s.invweibull.pdf(EUR_Data["Value"],fwEURshape,fwEURlocation,fwEURscale),label="Weibull")
plt.plot(EUR_Data["Value"],s.gamma.pdf(EUR_Data["Value"],fgEURshape,fgEURlocation,fgEURscale),label="Gamma")
plt.plot(EUR_Data["Value"],s.lognorm.pdf(EUR_Data["Value"],flnEURshape,flnEURlocation,flnEURscale),label="Log-Normal")
plt.plot(EUR_Data["Value"],s.fisk.pdf(EUR_Data["Value"],fllEURshape,fllEURlocation,fllEURscale),label="Log-Logistic")
#plt.hist(EUR_Data["Value"], bins=np.linspace(0, 16, 33), alpha=0.5)
plt.title('Probability Distribution Curves of US EUR Data', fontsize = 20)
plt.xlabel('EUR (Mm3)',fontsize = 14)
plt.ylabel('Density',fontsize = 14)
plt.legend(frameon=True,fancybox=True,
           shadow=True,framealpha=1,prop={"size":14})
plt.show()
#Implement AIC, BIC Criterion and MLE best fit
def AIC(length,log_lik):
    return 2*length - 2*(log_lik)
#Cumulative Distribution Data Fitting
sns.set_style("darkgrid")
fig, ax = plt.subplots() #Put the whole graph in a "subplot" but will still give a normal graph #HACK
fig.set_size_inches(11.7,8.27)
plt.plot(EUR_Data["Value"],s.invweibull.cdf(EUR_Data["Value"],fwEURshape,fwEURlocation,fwEURscale),label="Weibull")
plt.plot(EUR_Data["Value"],s.gamma.cdf(EUR_Data["Value"],fgEURshape,fgEURlocation,fgEURscale),label="Gamma")
plt.plot(EUR_Data["Value"],s.lognorm.cdf(EUR_Data["Value"],flnEURshape,flnEURlocation,flnEURscale),label="Log-Normal")
plt.plot(EUR_Data["Value"],s.fisk.cdf(EUR_Data["Value"],fllEURshape,fllEURlocation,fllEURscale),label="Log-Logistic")
#plt.hist(EUR_Data["Value"], bins=np.linspace(0, 16, 33), alpha=0.5)
plt.title('Cumulative Probability Distribution Curves of US EUR Data', fontsize = 20)
plt.xlabel('EUR (Mm3)',fontsize = 14)
plt.ylabel('Cumulative Density',fontsize = 14)
plt.legend(frameon=True,fancybox=True,
           shadow=True,framealpha=1,prop={"size":14})
plt.show()