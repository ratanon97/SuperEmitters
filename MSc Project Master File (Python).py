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
#import plotnine
import math
#-------------------------------------------------------------------------------
#Characterisation of EUR Data Script
#Source: Balcombe et al (2015)
#Script Source: Alberta EUR Data
#Note: EUR Data is US-Based. Proxy dataset for Canadian Data. Year collected: 2016
#Set working directory for EUR Data 
#Import EUR Data
print(os.getcwd())
os.chdir("/Users/ratanonsuksumrun/Documents/GitHub/SuperEmitters/Spreadsheets")
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
#3 tuples are produced based on the fit function: Index 0 = Shape, Index 1 =Location Index 2 = Scale
fwEUR = s.invweibull.fit(EUR_Data["Value"] ) #Weibull Distribution
fgEUR = s.gamma.fit(EUR_Data["Value"]) #Gamma Distribution
flnEUR = s.lognorm.fit(EUR_Data["Value"]) #Log-Normal Distribution
fllEUR = s.fisk.fit(EUR_Data["Value"]) #Log-Logistic Distribution (Fisk in scipy)
#Estimate parameters for the plot using the MLE estimate
sns.set_style("darkgrid")
fig, ax = plt.subplots() #Put the whole graph in a "subplot" but will still give a normal graph #HACK
fig.set_size_inches(11.7,8.27)
plt.plot(EUR_Data["Value"],s.invweibull.pdf(EUR_Data["Value"],fwEUR[0],fwEUR[1],fwEUR[2]),label="Weibull")
plt.plot(EUR_Data["Value"],s.gamma.pdf(EUR_Data["Value"],fgEUR[0],fgEUR[1],fgEUR[2]),label="Gamma")
plt.plot(EUR_Data["Value"],s.lognorm.pdf(EUR_Data["Value"],flnEUR[0],flnEUR[1],flnEUR[2]),label="Log-Normal")
plt.plot(EUR_Data["Value"],s.fisk.pdf(EUR_Data["Value"],fllEUR[0],fllEUR[1],fllEUR[2]),label="Log-Logistic")
#plt.hist(EUR_Data["Value"], bins=np.linspace(0, 16, 33), alpha=0.5)
plt.title('Probability Distribution Curves of US EUR Data', fontsize = 20)
plt.xlabel('EUR (Mm3)',fontsize = 14)
plt.ylabel('Density',fontsize = 14)
plt.legend(frameon=True,fancybox=True,
           shadow=True,framealpha=1,prop={"size":14})
plt.show()
#Cumulative Distribution Data Fitting
sns.set_style("darkgrid")
fig, ax = plt.subplots() #Put the whole graph in a "subplot" but will still give a normal graph #HACK
fig.set_size_inches(11.7,8.27)
plt.plot(EUR_Data["Value"],s.invweibull.cdf(EUR_Data["Value"],fwEUR[0],fwEUR[1],fwEUR[2]),label="Weibull")
plt.plot(EUR_Data["Value"],s.gamma.cdf(EUR_Data["Value"],fgEUR[0],fgEUR[1],fgEUR[2]),label="Gamma")
plt.plot(EUR_Data["Value"],s.lognorm.cdf(EUR_Data["Value"],flnEUR[0],flnEUR[1],flnEUR[2]),label="Log-Normal")
plt.plot(EUR_Data["Value"],s.fisk.cdf(EUR_Data["Value"],fllEUR[0],fllEUR[1],fllEUR[2]),label="Log-Logistic")
#plt.hist(EUR_Data["Value"], bins=np.linspace(0, 16, 33), alpha=0.5)
plt.title('Cumulative Probability Distribution Curves of US EUR Data', fontsize = 20)
plt.xlabel('EUR (Mm3)',fontsize = 14)
plt.ylabel('Cumulative Density',fontsize = 14)
plt.legend(frameon=True,fancybox=True,
           shadow=True,framealpha=1,prop={"size":14})
plt.show()
#--------------------------------------------------------
#Statistical Tests Calculations
#Goodness-of-Fit Criterion Tests
#OOP Approach
class log_likelihoodPDF:
    
    #Attributes
    def __init__(self,data,fitted_params):
        self.data = data #Data to be used
        self.fitted_params = fitted_params #Fitted statistical distribution parameters
        
    #Methods - 4 Statistical Distributions
    def Weibull(self):
        return np.sum(s.invweibull.logpdf(self.data,self.fitted_params[0],self.fitted_params[1],self.fitted_params[2]))
    def Gamma(self):
        return np.sum(s.gamma.logpdf(self.data,self.fitted_params[0],self.fitted_params[1],self.fitted_params[2]))
    def LN(self):
        return np.sum(s.lognorm.logpdf(self.data,self.fitted_params[0],self.fitted_params[1],self.fitted_params[2]))
    def LL(self):
        return np.sum(s.fisk.logpdf(self.data,self.fitted_params[0],self.fitted_params[1],self.fitted_params[2]))
            
class StatisticalTests:
    
    #Attributes
    def __init__(self,length,log_lik,data):
        self.length = length #Length of the statistical distribution fitted parameters
        self.log_lik = log_lik #MLE values from the log_likelihoodPDF class
        self.data = data #Data to be used
    
    #Methods - AIC and BIC
    def AIC(self):
        return 2*(self.length) - 2*(self.log_lik)
    def BIC(self):
        return -2*(self.log_lik) + np.log(len(self.data))*(self.length)
    
#Maximum Log-Likelihood Objects
fwLogLikEUR = log_likelihoodPDF(EUR_Data["Value"],fwEUR)
fgLogLikEUR = log_likelihoodPDF(EUR_Data["Value"],fgEUR)
flnLogLikEUR = log_likelihoodPDF(EUR_Data["Value"],flnEUR)
fllLogLikEUR = log_likelihoodPDF(EUR_Data["Value"],fllEUR)
#Instance of the Log Likelihood Objects 
MLE_Weibull_EUR = fwLogLikEUR.Weibull()
MLE_Gamma_EUR = fwLogLikEUR.Gamma()
MLE_LN_EUR = fwLogLikEUR.LN()
MLE_LL_EUR = fwLogLikEUR.LL()
#AIC/BIC Criterion Tests Objects
fwST = StatisticalTests(len(fwEUR),MLE_Weibull_EUR,EUR_Data["Value"])
fgST = StatisticalTests(len(fgEUR),MLE_Gamma_EUR,EUR_Data["Value"])
flnST = StatisticalTests(len(flnEUR),MLE_LN_EUR,EUR_Data["Value"])
fllST = StatisticalTests(len(fllEUR),MLE_LN_EUR,EUR_Data["Value"])
#AIC Tests
AIC_Weibull_EUR = fwST.AIC()
AIC_Gamma_EUR = fgST.AIC()
AIC_LN_EUR = flnST.AIC()
AIC_LL_EUR = fllST.AIC()
#BIC Tests
BIC_Weibull_EUR = fwST.BIC()
BIC_Gamma_EUR = fgST.BIC()
BIC_LN_EUR = flnST.BIC()
BIC_LL_EUR = fllST.BIC()
#Goodness-of-Fit Statistics
#KS Test
fwKSEUR = s.kstest(fwEUR,"norm")
fgKSEUR = s.kstest(fgEUR,"norm")
flnKSEUR = s.kstest(flnEUR,"norm")
fllKSEUR = s.kstest(fllEUR,"norm")
#AD Test
fwADEUR = s.anderson(fwEUR,"norm")
fgADEUR = s.anderson(fgEUR,"norm")
flnADEUR = s.anderson(flnEUR,"norm")
fllADEUR = s.anderson(fllEUR,"norm")
#------------------------------------------------------------------------------
#Alberta GreenPath Fugitive Data Script
#This script aims to evaluate the emission sources provided by GreenPath Energy
#This script aims to find the best fitting probability distribution for the methane emissions data
#Lorenz curves were applied to reporting segment and emission sources
#Super-emitters can be identified through Lorenz curves using the 80/20 rule as a benchmark
#If the remaining 20% of total population emits 80% of total emissions, then this is an indication of a super-emitter
#Not all segments and sources were modelled due to the small amount of population it was collected
#Top 5% emitters were evaluated to see the skewness contributions by the emitters to the Lorenz curves
#Majority of the emissions are leaked emissions
#Source script: Alberta GreenPath Fugitive Data
#Report Source: GreenPath 2016 Alberta Fugitive and Vented Emissions Inventory Study
#Import Emissions Data
print(os.getcwd())
os.chdir("/Users/ratanonsuksumrun/Documents/GitHub/SuperEmitters/Spreadsheets")
Fug_GreenPath = pd.read_csv("Alberta GreenPath Fugitive Data.csv")
#Extra Notes: Methane emissions were collected using bottom-up methods
#GWP Used: GWP100 of 25 for Methane and GWP100 of 298 for NO2
#Data was collected in the year of 2016
#----------------------------------------------------------
#Data Frame Preparation
#Remove irrelevant columns 
Fug_GreenPath = Fug_GreenPath.drop("Province",1)
Fug_GreenPath = Fug_GreenPath.drop("Loc1",1)
Fug_GreenPath = Fug_GreenPath.drop("Loc2",1)
Fug_GreenPath.columns = ["Service_Area",
                            "Unique_Location",
                            "Facility_Type",
                            "Facility_Commodity",
                            "Facility_StartUp",
                            "Year",
                            "Process_Block",
                            "Component_Main_Type",
                            "Component_Sub_Type",
                            "GPE_Tag",
                            "GPE_Media",
                            "Emission_Description",
                            "Emission_Type",
                            "Camera_Distance_ft",
                            "Emission_Status",
                            "Created_At",
                            "Last_Inspected_At",
                            "HSE_Issue",
                            "Service_Type_Composition",
                            "Service_Type",
                            "Measurement_Method",
                            "Emission_Rate_cfm",
                            "Leak_Percentage",
                            "Background_Percentage",
                            "Flow",
                            "12_Month_Production",
                            "Extra_Notes"]
#Data Frame Manipulation
Fug_GreenPath["Emission_Rate_cmm"] = Fug_GreenPath["Emission_Rate_cfm"] * 0.035315 #To convert cubic feet to cubic metre, multiply cubic feet with 0.035315
Fug_GreenPath["Emission_Rate_cmh"] = Fug_GreenPath["Emission_Rate_cmm"] * 60 #Cubic meter per hour
Fug_GreenPath["Emission_Rate_cmd"] = Fug_GreenPath["Emission_Rate_cmh"] * 1440 #Cubic meter per day
#Replace the values in the Facility Type
Fug_GreenPath = Fug_GreenPath.replace(to_replace =["Gas Multiwell Group Battery", 
                                                   "Gas Multiwell Proration Outside Se Alberta Battery",
                                                   "Gas Single-well Battery"], 
                                      value ="Gas Battery") 
Fug_GreenPath["Facility_Type"] = pd.Categorical(Fug_GreenPath["Facility_Type"])
#Construction of the Lorenz curves for visualisation in ggplot and the base plot for emission source population
#Process/Block Population
#Wellhead
PBWH_Filter = Fug_GreenPath["Process_Block"] == "Wellhead"
Fug_GreenPath_WH = Fug_GreenPath[PBWH_Filter]
Fug_GreenPath_WH = Fug_GreenPath.sort_values(by=["Emission_Rate_cmd"]) #Order the values
#Implement Lorenz Curve function or cumulative sum!
FGWH_Lorenz = Fug_GreenPath_WH["Emission_Rate_cmd"].cumsum() / Fug_GreenPath_WH["Emission_Rate_cmd"].sum()
#Skewness Analysis
In_Depth_Fug_GreenPath_WH = Fug_GreenPath.loc[:,["Facility_Type",
                                                 "Emission_Type",
                                                 "Process_Block",
                                                 "Service_Type",
                                                 "Component_Main_Type",
                                                 "Emission_Rate_cmd"]]
IDPBWH_Filter = In_Depth_Fug_GreenPath_WH["Process_Block"] == "Wellhead"
In_Depth_Fug_GreenPath_WH = In_Depth_Fug_GreenPath_WH[IDPBWH_Filter]
In_Depth_Fug_GreenPath_WH["Rank"] = In_Depth_Fug_GreenPath_WH["Emission_Rate_cmd"].rank() #Find the ranks for the top 5% emitters
In_Depth_Fug_GreenPath_WH = In_Depth_Fug_GreenPath_WH.sort_values(by=["Emission_Rate_cmd"])
In_Depth_Fug_GreenPath_WH = In_Depth_Fug_GreenPath_WH.reset_index()
In_Depth_Fug_GreenPath_WH = In_Depth_Fug_GreenPath_WH.drop("level_0",1)
In_Depth_Fug_GreenPath_WH = In_Depth_Fug_GreenPath_WH.drop("index",1)

for h in range(0,len(In_Depth_Fug_GreenPath_WH)):
    if h == round(0.05 *len(In_Depth_Fug_GreenPath_WH)):
        Test_Filter = In_Depth_Fug_GreenPath_WH["Rank"] == len(In_Depth_Fug_GreenPath_WH)-h
        In_Depth_Fug_GreenPath_WH_Test = In_Depth_Fug_GreenPath_WH[Test_Filter] #Fix this

In_Depth_Fug_GreenPath_WH = pd.DataFrame(In_Depth_Fug_GreenPath_WH_Test)

#Process/Block Population
#Wellhead
PBWH_Filter = Fug_GreenPath["Process_Block"] == "Wellhead"
Fug_GreenPath_WH = Fug_GreenPath[PBWH_Filter]
Fug_GreenPath_WH = Fug_GreenPath.sort_values(by=["Emission_Rate_cmd"]) #Order the values
#Implement Lorenz Curve function or cumulative sum!
FGWH_Lorenz = Fug_GreenPath_WH["Emission_Rate_cmd"].cumsum() / Fug_GreenPath_WH["Emission_Rate_cmd"].sum()


