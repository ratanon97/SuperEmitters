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
EUR_Data[]