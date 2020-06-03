#---------------------------------------------------------
#MASTER SCRIPT FOR TECHNO-ECONOMIC ASSESSMENT OF OPTIONS TO MITIGATE SUPER EMITTERS IN THE NATURAL GAS SUPPLY CHAIN
#Developer: Ratanon Suksumrun CID: 01541619
#Course: Advanced Chemical Engineering with Process Systems Engineering
#Academic Year: 2018-2019
#Imperial Email: rs2318@ic.ac.uk or ratanon.suksumrun18@imperial.ac.uk
#Imperial email is active until 30th July 2020
#Personal Email: ratanon97@hotmail.com
#Linkedin: https://www.linkedin.com/in/ratanon-suksumrun/
#Supervisor: Dr. Adam Hawkes
#Co-Supervisor 1: Dr.Paul Balcombe Email: p.balcombe@imperial.ac.uk
#Co-Supervisor 2: and Dr.Jasmin Cooper Email: jasmin.cooper@imperial.ac.uk
#Research Group: Sustainable Gas Institute (Part of Imperial College London)
#--------------------------------------------------------
#Characterisation of EUR Data Script
#Purpose of this script is to fit a statistical distribution for the EUR data
#This is later used for the Monte Carlo simulation as the random number generation
#must be generated from the best fitted distribution within this script
#Source: METHANE AND CO2EMISSIONS FROM THE NATURAL GAS SUPPLY CHAINAN EVIDENCE ASSESSMENT 
#Author: Balcombe et al (2015)
#Script Source: Alberta EUR Data
#Note: EUR Data is US-Based. Proxy dataset for Canadian Data. Year collected: 2016
#--------------------------------------------------------
#Set working directory for EUR Data 
getwd() #Find out the working directory from your associated computer/laptop
setwd("/Users/ratanonsuksumrun/Documents/GitHub/SuperEmitters/Spreadsheets") #Set your working directory accordingly to the location of your file input
EUR_Data <-read.csv("US_EUR_Data.csv") #Import the file input, File type must be .csv only 
#--------------------------------------------------------
#Data Frame Preparation
#EUR_Data data frame is adjusted accordingly
library(tidyverse) 
#Import tidyverse, dplyr and ggplot2 packages are part of this library and can be imported separately
# %>% and mutate are part of the dplyr library
EUR_Data <- EUR_Data %>% 
  mutate(Region=gsub("Marcellus","US",Region)) %>% 
  mutate(Region=gsub("Barnett","US",Region)) %>% #Change Marcellus and Barnett factors into US factors
  mutate(EUR_m3=Value*1000000) %>% #Expand EUR into m3 from Mm3
  mutate(LN_EUR_m3=log(EUR_m3)) #Log the EUR (m3) column
colnames(EUR_Data)[7] <- c("LN") #Change the name of the 7th column to LN
Mean_EUR <- mean(EUR_Data$Value)
SD_EUR <- sd(EUR_Data$Value) 
Median_EUR <- median(EUR_Data$Value) #Calculate mean, median and standard deviation of the EUR (Mm3)
Mean_LN_EUR <- mean(EUR_Data$LN)
SD_LN_EUR <- sd(EUR_Data$LN)
Median_LN_EUR <- median(EUR_Data$LN) #Calculate mean, median and standard deviation of the natural log of EUR (Mm3)
Mean_LN_EUR_m3 <- mean(EUR_Data$LN_EUR_m3)
SD_LN_EUR_m3 <- sd(EUR_Data$LN_EUR_m3)
Median_LN_EUR_m3 <- median(EUR_Data$LN_EUR_m3) #Calculate mean, median and standard deviation of the natural log of EUR (m3)
colnames(EUR_Data)[which(names(EUR_Data)=="Value")] <- "EUR"
#--------------------------------------------------------
#Probability Distribution Data Fitting 
library(fitdistrplus) #Load this library to import Weibull, Gamma and Log-Normal functions  to plot the probability distribution functions
library(actuar) #Load this library to import log-logistic functions to create log-logistic distributions
#Acronym List
#Object name goes in order: f, w/g/ln/ll, data you want to fit
#f = fit, #w,g,ln,ll = Weibull, Gamma, Log Normal, Log Logistic 
#Fit your associated data with the four distributions
fwEUR <- fitdist(EUR_Data$EUR,"weibull") #Weibull Distribution
fgEUR <- fitdist(EUR_Data$EUR,"gamma") #Gamma Distribution
flnEUR <- fitdist(EUR_Data$EUR,"lnorm")# Log-Normal Distribution
fllEUR <- fitdist(EUR_Data$EUR,"llogis") #Log-Logistic Distribution 
#Acronym List
#C = Cumulative
#Prob = Probability
#Dist = Distribution
#Object name goes in order: C, Prob, Dist, data you want to import
#Fit a probability density function to the four fitted distributions
#Plot style is "ggplot" but can be changed into "graphics", which is R's base plotting system
ProbDistEUR <- denscomp(list(fwEUR,fgEUR,flnEUR,fllEUR),
                        xlab="EUR (Mm3)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
#Fit a cumulative probability density function to the four fitted distributions
CProbDistEUR <- cdfcomp(list(fwEUR,fgEUR,flnEUR,fllEUR),
                        xlab="EUR (Mm3)",
                        ylab="Cumulative Distribution Function",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
#Fit the probability density function object to the relevant ggplot functions
ProbDistEUR+
  ggtitle("Histogram and Distribution Curves of EUR Data (US)")+
  #scale_x_continuous(trans="log")+ 
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Fit the cumulative probability density function object to the relevant ggplot functions
CProbDistEUR+
  ggtitle("Cumulative Distribution Curves of EUR Data (US)")+
  #scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#--------------------------------------------------------
#Statistics List from fitting the distributions
#Acronym list:
#GOF = Goodness of Fit, LogLik = Log Likelihood
#Object assigning order: GOF/LogLik/Summary/Stats, data you want to fit
GOFEUR<-gofstat(list(fwEUR,
                     fgEUR,
                     flnEUR,
                     fllEUR)) #Form a list and input it to the gofstat function (Requires a list)
LogLikEUR<-list(logW=logLik(fwEUR),
                logG=logLik(fgEUR),
                logln=logLik(flnEUR),
                logll=logLik(fllEUR)) #Form a list of the Log Likelihood values
SummaryEUR <- list(SummaryW=summary(fwEUR),
                   SummaryG=summary(fgEUR),
                   Summaryln=summary(flnEUR),
                   Summaryll=summary(fllEUR)) #Form a list of the summary functions' results
StatsEUR <- list(GOF=GOFEUR,
                    LogLikelihood=LogLikEUR,
                    Summary=SummaryEUR) #Form a list of the calculated statistics for ease of access of all statistical results
#Log-Logistic is best at fitting but we will use LOG NORMAL for the Monte Carlo Simulation
#---------------------------------------------------------
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
#---------------------------------------------------------
#Set Working Directory for GreenPath Fugitive Data
getwd() #See the working directory of the associated computer to import the data
setwd("/Users/ratanonsuksumrun/Documents/GitHub/SuperEmitters/Spreadsheets") #Set your working directory accordingly to the location of your file input
Fug_GreenPath <- read.csv("Alberta GreenPath Fugitive Data.csv") 
#---------------------------------------------------------
#Extra Notes: Methane emissions were collected using bottom-up methods
#GWP Used: GWP100 of 25 for Methane and GWP100 of 298 for NO2
#Data was collected in the year of 2016
#----------------------------------------------------------
#Data Frame Preparation
#Remove irrelevant columns 
Fug_GreenPath$Province<- NULL
Fug_GreenPath$Loc1 <- NULL
Fug_GreenPath$Loc2 <- NULL
#Rename all column names to an appropriate one
colnames(Fug_GreenPath)<- c("Service_Area",
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
                            "Extra_Notes")
#dplyr data frame manipulation
library(dplyr) #Part of the tidyverse library, can just import tidyverse library
Fug_GreenPath <- Fug_GreenPath %>% 
  mutate(Emission_Rate_cmm=Emission_Rate_cfm*0.035315) %>% #To convert cubic feet to cubic metre, multiply cubic feet with 0.035315
  mutate(Emission_Rate_cmh=Emission_Rate_cmm*60) %>% 
  mutate(Emission_Rate_cmd=Emission_Rate_cmm*1440) #Convert emission rates into appropriate units
Fug_GreenPath$Facility_Type <- gsub("Gas Multiwell Group Battery","Gas Battery",Fug_GreenPath$Facility_Type)
Fug_GreenPath$Facility_Type <- gsub("Gas Multiwell Proration Outside Se Alberta Battery","Gas Battery",Fug_GreenPath$Facility_Type)
Fug_GreenPath$Facility_Type <- gsub("Gas Single-well Battery","Gas Battery",Fug_GreenPath$Facility_Type)
Fug_GreenPath$Facility_Type <- factor(Fug_GreenPath$Facility_Type) 
#Convert Multiwell and Single well to just gas batteries
#Note: Using the gsub function will defactorise the factors into normal strings, use the factor function for this
#---------------------------------------------------------------------
#Construction of the Lorenz curves for visualisation in ggplot and the base plot for segment population
#Segment Population
#Gas Wells
Fug_GreenPath_GW <- Fug_GreenPath %>% 
  filter(Facility_Type=="Gas Well") %>% #Filter data for gas wells
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd))) #Calculate cumulative function for the emission rate of gas wells
Fug_GreenPath_GW$Facility_Type <- as.numeric(Fug_GreenPath_GW$Facility_Type) #To model the cumulative population of the filtered dataset, turn the reporting segment factors into a number
Fug_GreenPath_GW <- Fug_GreenPath_GW %>% 
  mutate(Number_Of_Sites_CS=cumsum(Facility_Type)/max(sum(Facility_Type))) #Calculate cumulative function of the number of population
write.csv(Fug_GreenPath_GW,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_GW.csv") #Export the data frame file into a csv file for reporting purposes
#Skewness Analysis
#Evaluation of the top 5% emitters preparation using the dplyr library
In_Depth_Fug_GreenPath_GW<- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% #Filter the appropriate columns for the top 5% emitters
  filter(Facility_Type=="Gas Well") %>% #Filter the data to gas wells only
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% #Rank the emission rate with the largest emission value to be ranked the last
  filter(Rank==14) #Filter the top 5% emitter
#Gas Batteries
Fug_GreenPath_GB <- Fug_GreenPath %>% 
  filter(Facility_Type=="Gas Battery") #Filter data for gas batteries
Fug_GreenPath_GB<-Fug_GreenPath_GB[order(Fug_GreenPath_GB$Emission_Rate_cmd),] #Order the emission volume from minimum to maximum
#Note: Ordering does not affect the values of the cumulative frequencies
Fug_GreenPath_GB <- Fug_GreenPath_GB %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd))) #Calculate cumulative function for the emission rate of gas batteries
Fug_GreenPath_GB$Facility_Type <- as.numeric(Fug_GreenPath_GB$Facility_Type) #To model the cumulative population of the filtered dataset, turn the reporting segment factors into a number
Fug_GreenPath_GB <- Fug_GreenPath_GB %>% 
  mutate(Number_Of_Sites_CS=cumsum(Facility_Type)/max(sum(Facility_Type))) #Calculate cumulative function of the number of population
write.csv(Fug_GreenPath_GB,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_GB.csv") #Export the data frame file into a csv file for reporting purposes
#Skewness Analysis
In_Depth_Fug_GreenPath_GB<- Fug_GreenPath %>% #NoteL dplyr::select is preferrable than just writing select as select function is part of another package
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% #Filter appropriate columns
  filter(Facility_Type=="Gas Battery") %>% #Filter gas batteries data
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% #Rank the emission rate with the largest emission value to be ranked the last
  filter(Rank==10) #Filter the top 5% emitter
#Segment Top 5 GreenPath
Segment_Top_5_GreenPath <- rbind.data.frame(In_Depth_Fug_GreenPath_GB,
                                            In_Depth_Fug_GreenPath_GW) #Combine rows of the top 5% emitter from reporting segment data frame 
colnames(Segment_Top_5_GreenPath) <- c("Petrinex_Facility_Subtype",
                                       "Emission_Type",
                                       "Major_Equipment",
                                       "Component_Service_Type",
                                       "Component_Type",
                                       "CH4_Leak_Rate_m3_per_day_per_source",
                                       "Rank") #Rename the column names appropriately
#---------------------------------------------------------------
#Construction of the Lorenz curves for visualisation in ggplot and the base plot for emission source population
#Process/Block Population
#Wellhead
Fug_GreenPath_WH <- Fug_GreenPath %>% 
  filter(Process_Block=="Wellhead") #Filter the wellhead datas with piping and dplyr library
Fug_GreenPath_WH<-Fug_GreenPath_WH[order(Fug_GreenPath_WH$Emission_Rate_cmd),] #Order the emission rate m3/day from minimum to maximum
Fug_GreenPath_WH <- Fug_GreenPath_WH %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd))) #Calculate the cumulative function of the emission rate m3/day
Fug_GreenPath_WH$Process_Block <- as.numeric(Fug_GreenPath_WH$Process_Block) #Convert the wellhead factor to a number
Fug_GreenPath_WH <- Fug_GreenPath_WH %>% 
  mutate(Number_Of_Sites_CS=cumsum(Process_Block)/max(sum(Process_Block))) #Calculate cumulative function of the number of population
write.csv(Fug_GreenPath_WH,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_WH.csv") #Export the data frame to a csv file type
#Skewness Analysis
In_Depth_Fug_GreenPath_WH<- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% #Filter the appropriate columns
  filter(Process_Block=="Wellhead") %>% #Filter the wellhead data
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% #Rank the emission volume with the largest volume to be ranked last
  filter(Rank==12) #Rank the top 5% emitter
#Filter/Separation
Fug_GreenPath_FS <- Fug_GreenPath %>% 
  filter(Process_Block=="Filter/Separation") #Filter the Filter/Separation emission source data
Fug_GreenPath_FS<-Fug_GreenPath_FS[order(Fug_GreenPath_FS$Emission_Rate_cmd),] #Order the emission volume data from minimum to maximum
Fug_GreenPath_FS <- Fug_GreenPath_FS %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd))) # Calculate the cumulative function of the emission volume
Fug_GreenPath_FS$Process_Block <- as.numeric(Fug_GreenPath_FS$Process_Block) #Convert the Filter/Separation to a number
Fug_GreenPath_FS <- Fug_GreenPath_FS %>% 
  mutate(Number_Of_Sites_CS=cumsum(Process_Block)/max(sum(Process_Block))) #Calculate the cumulative function of the population of the filter/separation
write.csv(Fug_GreenPath_FS,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_FS.csv") #Export data frame to csv file
#Skewness Analysis
In_Depth_Fug_GreenPath_FS<- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% #Filter approproiate columns
  filter(Process_Block=="Filter/Separation") %>% #Filter Filter/Separatiion data
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% #Rank emission volume with the largest volume as the last
  filter(Rank==9) #Filter the top 5% emitter 
#Equipment Top 5 GreenPath
Equipment_Top_5_GreenPath <- rbind.data.frame(In_Depth_Fug_GreenPath_WH,
                                              In_Depth_Fug_GreenPath_FS) # Combine the top 5% emitter emission source data frames by rows
colnames(Equipment_Top_5_GreenPath) <- c("Petrinex_Facility_Subtype",
                                         "Emission_Type",
                                         "Major_Equipment",
                                         "Component_Service_Type",
                                         "Component_Type",
                                         "CH4_Leak_Rate_m3_per_day_per_source",
                                         "Rank") #Change column names to appropriate names
#---------------------------------------------------------------
library(ggplot2) #Import ggplot2 library for ggplot visualisation
library(ineq) #Import ineq library for Lorenz curve visualisation
library(gglorenz) #Import gglorenz for Lorenz curve visualisation with ggplot functions
#Visualise the histogram for the emission volumes of GreenPath using ggplot
Fug_GreenPath_Histo <- ggplot(data=Fug_GreenPath,
                              aes(x=Emission_Rate_cmh))
Fug_GreenPath_Histo + geom_histogram(aes(y=stat(density)),
                                     fill="LightBlue",
                                     colour="Black",
                                     size=0.2)+
  #geom_density(fill="Purple",
  #alpha = 0.3,
  #size = 1)+
  scale_x_continuous(trans="log")+
  # facet_grid(.~Service_Type,
  #            scales="free",
  #            space="free")+
  xlab("Methane Emissions")+
  ylab("Density")+
  ggtitle("Alberta Fugitive Pneumatic Device Data 2016")+ #Making the graph look niceer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20),
        strip.text=element_text(size=10))
#---------------------------------------------------------
#Lorenz Curve fitting using the ggplot2 and gglorenz libraries
#Object name order: F, GW/GB/WH/FS/GW, Lorenz
#F = Fugitive, GW = Gas Wells, GB = Gas Batteries, FS = Filter/Separation 
#Assigning a ggplot object to the desired reporting segment and emission source
FGW_Lorenz <- ggplot(data=Fug_GreenPath_GW,aes(x=Number_Of_Sites_CS))
FGB_Lorenz <- ggplot(data=Fug_GreenPath_GB,aes(x=Number_Of_Sites_CS))
FWH_Lorenz <- ggplot(data=Fug_GreenPath_WH,aes(x=Number_Of_Sites_CS))
FFS_Lorenz <- ggplot(data=Fug_GreenPath_FS,aes(x=Number_Of_Sites_CS)) 
#Use the ggplot object with the gglorenz function "stat_lorenz"
#The function calculates the cumulative proportions for both emission and number of populations
FGW_Lorenz + stat_lorenz(aes(Emission_Rate_cmd),
                         size=3,
                         colour="magenta4")+
  xlab("Percent Of Gas Wells Population")+
  ylab("Percent of Emissions")+
  ggtitle("Gas Wells (Pneumatics) vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
FGB_Lorenz + stat_lorenz(aes(Emission_Rate_cmd),
                         size=3,
                         colour="magenta4")+
  xlab("Percent Of Gas Batteries Population")+
  ylab("Percent of Emissions")+
  ggtitle("Gas Batteries (Pneumatics) vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
FWH_Lorenz + stat_lorenz(aes(Emission_Rate_cmd),
                         size=3,
                         colour="magenta4")+
  xlab("Percent Of Wellheads Population")+
  ylab("Percent of Emissions")+
  ggtitle("Wellheads (Pneumatics) vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
FFS_Lorenz + stat_lorenz(aes(Emission_Rate_cmd),
                         size=3,
                         colour="magenta4")+
  xlab("Percent Of Filter/Separation Population")+
  ylab("Percent of Emissions")+
  ggtitle("Filter/Separation (Pneumatics) vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#-------------------------------------------------------
#Plotting multiple lorenz curves using the the Lc function from ineq package and the R base plot
#Segment Population
#Plot the Lorenz curve using any emission from the reporting segment population
plot(Lc(Fug_GreenPath_GW$Emission_Rate_cmh),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Segment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
#Plot the Lorenz curves from the remaining reporting segment population
lines(Lc(Fug_GreenPath_GB$Emission_Rate_cmh),
      col="blue",
      lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Gas Wells","Gas Batteries"),
       col=c("red","blue"),pch=15:18,horiz=F)
grid(col="lightgray")   
#Equipment Population
#Plot the Lorenz curve using any emission from the emission source population
plot(Lc(Fug_GreenPath_WH$Emission_Rate_cmh),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Equipment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
#Plot the Lorenz curves from the remaining emission source population
lines(Lc(Fug_GreenPath_FS$Emission_Rate_cmh),
      col="blue",
      lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Wellhead","Filter/Separation"),
       col=c("red","blue"),pch=15:18,horiz=F)
grid(col="lightgray")                         
#-------------------------------------------------------
#Probability Distribution Data Fitting 
library(fitdistrplus) #Load this library to import Weibull, Gamma and Log-Normal functions  to plot the probability distribution functions
library(actuar) #Load this library to import log-logistic functions to create log-logistic distributions
#Acronym List
#Object name goes in order: f, w/g/ln/ll, data you want to fit
#f = fit, #w,g,ln,ll = Weibull, Gamma, Log Normal, Log Logistic 
#Fit your associated data with the four distributions
fwFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"weibull") #Weibull Distribution
fgFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"gamma") #Gamma Distribution
flnFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"lnorm")# Log-Normal Distribution
fllFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"llogis") #Log-Logistic Distribution 
#Assign a probability density function to the four distributions using the ggplot plotstyle
ProbDistFugPneu <- denscomp(list(fwFugPneumatic,
                                 fgFugPneumatic,
                                 flnFugPneumatic,
                                 fllFugPneumatic),
                            xlab="Methane Emissions (% of Production)",
                            plotstyle="ggplot",
                            legendtext=c("Weibull",
                                         "Gamma",
                                         "Log-Normal",
                                         "Log-Logistic"))
#Assign a cumulative probability density function to the four distributions using the ggplot plotstyle
CProbDistFugPneu <- cdfcomp(list(fwFugPneumatic,
                                 fgFugPneumatic,
                                 flnFugPneumatic,
                                 fllFugPneumatic),
                            xlab="ln(Methane Emissions (m3/day))",
                            ylab="Cumulative Distribution Function",
                            plotstyle="ggplot",
                            legendtext=c("Weibull",
                                         "Gamma",
                                         "Log-Normal",
                                         "Log-Logistic"))
#Visualise the probability densisty function using the relevant ggplot functions
ProbDistFugPneu+
  ggtitle("Histogram and Distribution Curves of Fugitive Pneumatic Devices Data 2016")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Visualise the cumulative probability densisty function using the relevant ggplot functions
CProbDistFugPneu+
  ggtitle("Cumulative Distribution Curves of Fugitive Pneumatic Devices Data 2016")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#----------------------------------------------
#Statistics List
#Acronym list
#GOF: Goodness of Fit, LogLik= Log Likelihood
GOFFugPneu<-gofstat(list(fwFugPneumatic,
                         fgFugPneumatic,
                         flnFugPneumatic,
                         fllFugPneumatic)) #Form a list of the four distributions and input it to the gofstat function
LogLikFugPneu<-list(logW=logLik(fwFugPneumatic),
                    logG=logLik(fgFugPneumatic),
                    logln=logLik(flnFugPneumatic),
                    logll=logLik(fllFugPneumatic)) #Form a list of the log likelihood values of the four distributions
SummaryFugPneu<-list(SummaryW=summary(fwFugPneumatic),
                     SummaryG=summary(fgFugPneumatic),
                     Summaryln=summary(flnFugPneumatic),
                     Summaryll=summary(fllFugPneumatic)) #Form a list of the summary statistics of the four distributions
StatsFugPneu <- list(GOF=GOFFugPneu,
                     LogLikelihood=LogLikFugPneu,
                     Summary=SummaryFugPneu) #Form a list of all the calculated statistics for ease of access
#Log-Logistic Distribution is good at fitting but Log-normal is used instead as the nature of the log-normal is more appropriate to model the continuous nature of the emissions
#----------------------------------------------
#Monte Carlo Simulation
#Acronym list:
#MC = Monte Carlo, FGP = Fugitive Green Path, Fug = Fugitive
set.seed(123) #This makes sure the results from the random number generation stays the same everytime the simulation is ran
#Create an MC Function for the GreenPath Script based on the EUR
MC_Function_FGP <- function(df,mean,sd){
  FGP_Methane_Throughput <- (df$Emission_Rate_cmd/rlnorm(nrow(df),mean,sd))*100
  return(FGP_Methane_Throughput)
} #Input the desired data frame along with the mean and standard deviation from the best fitted distribution of the EUR
#Amount of random numbers are generated based on the number of rows from the inputted data frame
Fug_GreenPath_Sim <- replicate(10000,MC_Function_FGP(Fug_GreenPath,Mean_LN_EUR,SD_LN_EUR)) #The calculation is repeated 10,000 times using the replicate function
Fug_GreenPath_Sim <- data.frame(Fug_GreenPath_Sim) #Replicate function returns a matrix object, so the matrix object is converted into a data frame type structure
#Calculations from Simulation are applied to see the mean, median, standard deviation and the percentiles
#Sapply function is used to calculate the mean, median and quantiles of the whole dataframe
Mean_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,mean)
Median_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,median)
Q5_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.05))
Q25_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.25))
Q75_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.75))
Q95_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.95))
#Filter the values that exceeded 100% as emissions exceeding 100% does not exist in reality
FilteredMean_Fug_GreenPath_Sim<-Mean_Fug_GreenPath_Sim[Mean_Fug_GreenPath_Sim <= 100]
FilteredQ75_Fug_GreenPath_Sim <- Q75_Fug_GreenPath_Sim[Q75_Fug_GreenPath_Sim<=100]
FilteredQ95_Fug_GreenPath_Sim <- Q95_Fug_GreenPath_Sim[Q95_Fug_GreenPath_Sim<=100]
#Find the minimum and maximum values of the mean, median and the percentiles
MinFilteredMeanFGP<-min(FilteredMean_Fug_GreenPath_Sim)
MaxFilteredMeanFGP<-max(FilteredMean_Fug_GreenPath_Sim)
MinFilteredMedianFGP<-min(Median_Fug_GreenPath_Sim)
MaxFilteredMedianFGP<-max(Median_Fug_GreenPath_Sim)
MinFilteredQ5FGP<-min(Q5_Fug_GreenPath_Sim)
MaxFilteredQ5FGP<-max(Q5_Fug_GreenPath_Sim)
MinFilteredQ25FGP<-min(Q25_Fug_GreenPath_Sim)
MaxFilteredQ25FGP<-max(Q25_Fug_GreenPath_Sim)
MinFilteredQ75FGP<-min(FilteredQ75_Fug_GreenPath_Sim)
MaxFilteredQ75FGP<-max(FilteredQ75_Fug_GreenPath_Sim)
MinFilteredQ95FGP<-min(FilteredQ95_Fug_GreenPath_Sim)
MaxFilteredQ95FGP<-max(FilteredQ95_Fug_GreenPath_Sim)
#---------------------------------------------
#Visualtion of the Monte Carlo graphs using the empricial cumulative distribution function with the base plot
#Plot the first ecdf from the simulation
#color coding: alpha is used to make the curves more transparent
plot(ecdf(Fug_GreenPath_Sim[,1]),
     xlab="Methane Emissions (% of EUR)",
     ylab="Cumulative Distribution",
     main="Monte Carlo Simulation of GreenPath Fugitive Equipment Leak Data (2016)",
     xlim=c(0,100),
     cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     col=alpha("deepskyblue2",0.3)) 
grid(col="lightgray")
for(j in 2:1000){
  lines(ecdf(Fug_GreenPath_Sim[,j]),
        col=alpha("deepskyblue2",0.3))
} #Loop the curves from column 2 to 1000 with the deep sky blue 2 color
for(h in 1001:2000){
  lines(ecdf(Fug_GreenPath_Sim[,h]),
        col=alpha("cadetblue2",0.3))
} #Loop the curves from column 1001 to 2000 with the cadetblue2 color
for(l in 2001:3000){
  lines(ecdf(Fug_GreenPath_Sim[,l]),
        col=alpha("darkorchid2",0.3))
} #Loop the curves from column 2001 to 3000 with the darkorchid2 color
for(p in 3001:4000){
  lines(ecdf(Fug_GreenPath_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
} #Loop the curves from column 3001 to 4000 with the lightskyblue2 color
for(o in 4001:5000){
  lines(ecdf(Fug_GreenPath_Sim[,o]),
        col=alpha("blueviolet",0.3))
} #Loop the curves from column 4001 to 5000 with the blueviolet color
for(k in 5001:6000){
  lines(ecdf(Fug_GreenPath_Sim[,k]),
        col=alpha("darkslateblue",0.3))
} #Loop the curves from column 5001 to 6000 with the darkslateblue color
for(s in 6001:7000){
  lines(ecdf(Fug_GreenPath_Sim[,s]),
        col=alpha("darkmagenta",0.3))
} #Loop the curves from column 6001 to 7000 with the lightskyblue2 color
for(u in 7001:8000){
  lines(ecdf(Fug_GreenPath_Sim[,u]),
        col=alpha("deeppink1",0.3))
} #Loop the curves from column 7001 to 8000 with the deeppink1 color
for(x in 8001:9000){
  lines(ecdf(Fug_GreenPath_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
} #Loop the curves from column 8001 to 9000 with the cornflowerblue color
for(f in 9001:10000){
  lines(ecdf(Fug_GreenPath_Sim[,f]),
        col=alpha("darkorchid",0.3))
} #Loop the curves from column 9001 to 10000 with the darkorchid color
#----------------------------------------------
#Visualising the Monte Carlo histograms
#Not an essential part of the project but can be used to view the heavy-tail
MC_Fug_GreenPath_Histo <- ggplot(data=Fug_GreenPath_Sim)
MC_Fug_GreenPath + geom_histogram(aes(x=X990,y=stat(density)),
                                  fill="LightBlue",
                                  colour="Black",
                                  binwidth = 0.03,
                                  size=1)+
  ggtitle("Monte Carlo Simulations of GreenPath Fugitive Data 2016")+
  xlab("Methane Emissions (% of Production)")+
  ylab("Density")+
  xlim(0,1)+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#-------------------------------------------------------
#Alberta Clearstone Fugitive Data Script
#This script aims to evaluate the emission sources provided by Clearstone Engineering
#This script aims to find the best fitting probability distribution for the methane emissions data
#Lorenz curves were applied to reporting segment and emission sources
#Super-emitters can be identified through Lorenz curves using the 80/20 rule as a benchmark
#If the remaining 20% of total population emits 80% of total emissions, then this is an indication of a super-emitter
#Not all segments and sources were modelled due to the small amount of population it was collected
#Top 5% emitters were evaluated to see the skewness contributions by the emitters to the Lorenz curves
#Majority of the emissions are leaked emissions
#Source script: Alberta Fugitive Equipment Leak Data
#Report Source: Update of Equipment,Component and Fugitive Emission Factors for Alberta Upstream Oil and Gas.
getwd() #See the working directory of the associated computer to import the data
setwd("/Users/ratanonsuksumrun/Documents/GitHub/SuperEmitters/Spreadsheets") #Set the desired working directory
ClearStone <- read.csv("Alberta Fugitive Equipment Leak Data.csv") #Read the csv file input for the Clearstone script
#---------------------------------------------------------
#Notes: This is bottom up method
#GWP Used: GWP100 of 25 for Methane and GWP100 of 298 for NO2
#Emissions data were collected in 2017
#----------------------------------------------------------
#Data Frame Preparation
#Rename the column names accordingly
colnames(ClearStone) <- c("Site_ID",
                          "AER_Field_Centre",
                          "Petrinex_Facility_Subtype",
                          "Well_Status",
                          "Emission_Type",
                          "Major_Equipment",
                          "Component_Service_Type",
                          "Component_Type",
                          "Measurement_Device",
                          "Leak_Tag",
                          "THC_Leak_Rate_m3_per_hour_per_source")
CH4_vol_percent <- 0.9287 #percentage of volume for methane in natural gas
#Import dplyr library, part of the tidyverse package for convenient data manipulation
library(dplyr)
ClearStone <- ClearStone %>% 
  mutate(CH4_Leak_Rate_m3_per_hour_per_source=THC_Leak_Rate_m3_per_hour_per_source * CH4_vol_percent) %>% 
  mutate(CH4_Leak_Rate_m3_per_day_per_source=CH4_Leak_Rate_m3_per_hour_per_source*24) %>% 
  mutate(CH4_Leak_Rate_kg_per_day_per_source=CH4_Leak_Rate_m3_per_day_per_source*0.8) %>% 
  mutate(CH4_Leak_Rate_Mm3_per_day_per_source=CH4_Leak_Rate_m3_per_day_per_source/1000000)
#Convert the THC leak rate into CH4 Leak Rate with appropriate units
Avg_Abs_Emission_Rate <- 21.51347 #Important note: Combined with the Fugitive emissions from GreenPath
#-------------------------------------------------------------------------
#Construction of the Lorenz curves for visualisation in ggplot and the base plot for emission source population
#Reciprocating Compressor
ClearStone_Compressor <- ClearStone %>% 
  filter(Major_Equipment=="Reciprocating Compressor") %>% #Filter to Reciprocating compressor population
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emissions
ClearStone_Compressor$Major_Equipment <- as.numeric(ClearStone_Compressor$Major_Equipment)#Convert factor name to a number for calculating the cumulative population
ClearStone_Compressor <- ClearStone_Compressor %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative population
write.csv(ClearStone_Compressor,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Compressor.csv") #Export compressor dataframe into a csv file
#Skewness Analysis
In_Depth_ClearStone_Compressor <- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select appropriate columns
  filter(Major_Equipment=="Reciprocating Compressor") %>% #Filter to Reciprocating compressor population
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank emissions with starting from the lowest to highest emissions
  filter(Rank >= 49) #Filter the top 5% emitters
#Separators
ClearStone_Separator <- ClearStone %>% 
  filter(Major_Equipment=="Separator") %>% #Filter the Separator data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate cumulative emission 
ClearStone_Separator$Major_Equipment <- as.numeric(ClearStone_Separator$Major_Equipment) #Convert the factor into a number for cumulative calculation of population
ClearStone_Separator <- ClearStone_Separator %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative proportion
write.csv(ClearStone_Separator,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Separator.csv") #Export the dataframe to a csv file type
#Skewness Analysis
In_Depth_ClearStone_Separator <- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select appropriate columns
  filter(Major_Equipment=="Separator") %>% #Filter the separator data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission data from lowest to highest
  filter(Rank >= 35) #Filter the top 5% emitters
#Wellheads
ClearStone_Wellhead <- ClearStone %>% 
  filter(Major_Equipment=="Wellhead") %>% #Filter the wellhead data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))#Calculate the cumulative emission
ClearStone_Wellhead$Major_Equipment <- as.numeric(ClearStone_Wellhead$Major_Equipment) #Convert the factor into the number for cumulative population calculation
ClearStone_Wellhead <- ClearStone_Wellhead %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative proportion
write.csv(ClearStone_Wellhead,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Wellhead.csv") #Export the dataframe of the wellheads to a csv file type
#Skewness Analysis
In_Depth_ClearStone_Wellhead <- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate columns
  filter(Major_Equipment=="Wellhead") %>% #Filter the Wellhead data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission data from lowest to highest
  filter(Rank >= 44) #Filter the top 5% emitters
#Dehydrator Glycol
ClearStone_DG <- ClearStone %>% 
  filter(Major_Equipment=="Dehydrator - Glycol") %>% #Filter the dehydrator glycol data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission
ClearStone_DG$Major_Equipment <- as.numeric(ClearStone_DG$Major_Equipment) #Convert the factor into the number for cumulative population calculation
ClearStone_DG <- ClearStone_DG %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative proportion
write.csv(ClearStone_DG,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_DG.csv") #Export the dataframe of the dehydrator glycol to a csv file type
#Skewness Analysis
In_Depth_ClearStone_DG<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate columns
  filter(Major_Equipment=="Dehydrator - Glycol") %>% #Filter the Dehydrator Glycol data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission data from lowest to highest
  filter(Rank == 8) #Filter the top 5% emitters
#Flare Knockout Drum
ClearStone_FKD <- ClearStone %>% 
  filter(Major_Equipment=="Flare KnockOut Drum") %>% #Filter the flare knockout drum data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission
ClearStone_FKD$Major_Equipment <- as.numeric(ClearStone_FKD$Major_Equipment) #Convert the factor into the number for cumulative population calculation
ClearStone_FKD <- ClearStone_FKD %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment)))#Calculate the cumulative proportion
write.csv(ClearStone_FKD,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_FKD.csv") #Export the dataframe of the flare knockout drum to a csv file type
#Skewness Analysis
In_Depth_ClearStone_FKD<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate columns
  filter(Major_Equipment=="Flare KnockOut Drum") %>% #Filter the Flare Knockout Drum data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission data from lowest to highest
  filter(Rank == 8) #Filter the top 5% emitter
#Production Tank (fixed roof)
ClearStone_PTFR <- ClearStone %>% 
  filter(Major_Equipment=="Production Tank (fixed roof)") %>% #Filter the Production Tank (Fixed Roof) data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission
ClearStone_PTFR$Major_Equipment <- as.numeric(ClearStone_PTFR$Major_Equipment) #Convert the factor into the number for cumulative population calculation
ClearStone_PTFR <- ClearStone_PTFR %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative proportion
write.csv(ClearStone_PTFR,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_PTFR.csv") #Export the dataframe of the production tank fixed roof to a csv file type
#Skewness Analysis
In_Depth_ClearStone_PTFR<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate columns
  filter(Major_Equipment=="Production Tank (fixed roof)") %>% #Filter the production tank fixed roof data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission data from lowest to highest
  filter(Rank == 13) # Filter the top 5% emitter
#Gas Pipeline Header
ClearStone_GPH <- ClearStone %>% 
  filter(Major_Equipment=="Gas Pipeline Header") %>% #Filter the Gas Pipeline Header data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission
ClearStone_GPH$Major_Equipment <- as.numeric(ClearStone_GPH$Major_Equipment) #Convert the factor into the number for cumulative population calculation
ClearStone_GPH <- ClearStone_GPH %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative proportion
write.csv(ClearStone_GPH,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_GPH.csv") #Export the dataframe of the gas pipeline header to a csv file type
#Skewness Analysis
In_Depth_ClearStone_GPH<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate columns
  filter(Major_Equipment=="Gas Pipeline Header") %>% #Filter the gas pipeline header data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission data from lowest to highest
  filter(Rank == 7) #Filter the top 5% emitter
#Screw Compressor
ClearStone_SC<- ClearStone %>% 
  filter(Major_Equipment=="Screw Compressor") %>% #Filter the Screw compressor data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission 
ClearStone_SC$Major_Equipment <- as.numeric(ClearStone_SC$Major_Equipment) #Convert the factor into the number for cumulative population calculation
ClearStone_SC <- ClearStone_SC %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative proportion
write.csv(ClearStone_SC,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_SC.csv") #Export the dataframe of the screw compressor to a csv file type
#Skewness Analysis
In_Depth_ClearStone_SC<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate column
  filter(Major_Equipment=="Screw Compressor") %>% #Filter the screw compressor data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission from lowest to highest
  filter(Rank == 8) #Filter the top 5% emitter
#Electric Driver Compressors
#Indicate some possible use of electric drivers in the report
ClearStone_ED<- ClearStone %>% 
  filter(Major_Equipment=="Screw Compressor - Electric Driver" | Major_Equipment=="Reciprocating Compressor - Electric Driver") %>% #Filter the electric driver compressors type
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission
ClearStone_ED$Major_Equipment <- as.numeric(ClearStone_ED$Major_Equipment) #Convert the factor into the number for cumulative population calculation
ClearStone_ED <- ClearStone_ED %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) #Calculate the cumulative proportion
write.csv(ClearStone_ED,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_ED.csv") #Export the dataframe of the electric driver compressors to a csv file type
#Skewness Analysis
In_Depth_ClearStone_ED<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate column
  filter(Major_Equipment=="Screw Compressor - Electric Driver" | Major_Equipment=="Reciprocating Compressor - Electric Driver") %>% #Filter the electric driver compressors type
  mutate(Major_Equipment=gsub("Screw Compressor - Electric Driver","Electric Driver Compressor",Major_Equipment)) %>%  #Change Screw Electric driver compressors' name to just electric driver compressors
  mutate(Major_Equipment=gsub("Reciprocating Compressor - Electric Driver","Electric Driver Compressor",Major_Equipment)) %>% #Change Reciprocating Electric driver compressors' name to just electric driver compressors
  mutate(Major_Equipment=factor(Major_Equipment)) %>% #Convert type to factor
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission from lowest to highest
  filter(Rank == 6) #Filter the top 5% emitter
#--------------------------------------------------------------
#Top 5 Analysis
#Equipment List. The purpose is to see the magnitude of the top 5% emitters and how they compare with the average absolute emission rate
Equipment_List <- list(ClearStone_Compressor,
                       ClearStone_Separator,
                       ClearStone_Wellhead,
                       ClearStone_DG,
                       ClearStone_FKD,
                       ClearStone_PTFR,
                       ClearStone_GPH,
                       ClearStone_SC,
                       ClearStone_ED) #Form a list for the Clearstone equipment objects
SK_Equipment_List <- list(ClearStone_Compressor$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_Separator$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_Wellhead$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_DG$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_FKD$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_PTFR$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_GPH$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_SC$CH4_Leak_Rate_m3_per_day_per_source,
                          ClearStone_ED$CH4_Leak_Rate_m3_per_day_per_source,
                          Fug_GreenPath_FS$Emission_Rate_cmd,
                          Fug_GreenPath_WH$Emission_Rate_cmd) #Combine the emission m3/day list for both Clearstone and GreenPath's equipment sources
Top_5_Function <- function(lst){ceiling(0.05*nrow(lst))} #Create a function to see the top 5% emitter of the total combined Clearstone top 5% emitter dataframe
Top_5_Percent_List <- lapply(Equipment_List,Top_5_Function) #Apply the function to the Equipment_List and create a list out of it
Skewness_Equipment_List <- lapply(SK_Equipment_List,skewness) #Calculate the skewness value for each of the emission volume dataframe in a list form
Kurtosis_Equipment_List <- lapply(SK_Equipment_List,kurtosis) #Calculate the kurtosis value for each of the emission volume dataframe in a list form
Skewness_Equipment_Vector <- sapply(SK_Equipment_List,skewness) #Calculate the skewness value for each of the emission volume dataframe in a vector form
Kurtosis_Equipment_Vector <- sapply(SK_Equipment_List,kurtosis)#Calculate the kurtosis value for each of the emission volume dataframe in a vector form
names(Top_5_Percent_List) <- c("ClearStone_Compressor",
                               "ClearStone_Separator",
                               "ClearStone_Wellhead",
                               "ClearStone_DG",
                               "ClearStone_FKD",
                               "ClearStone_PTFR",
                               "ClearStone_GPH",
                               "ClearStone_SC",
                               "ClearStone_ED") #name the columns in the Top_5_Percent_List
names(SK_Equipment_List) <- c("ClearStone_Compressor",
                              "ClearStone_Separator",
                              "ClearStone_Wellhead",
                              "ClearStone_DG",
                              "ClearStone_FKD",
                              "ClearStone_PTFR",
                              "ClearStone_GPH",
                              "ClearStone_SC",
                              "ClearStone_ED",
                              "Fug_GreenPath_FS",
                              "Fug_GreenPath_WH") #name the columns in the SK_Equipment_List
Equipment_Top_5 <- rbind.data.frame(In_Depth_ClearStone_Compressor,
                                    In_Depth_ClearStone_Separator,
                                    In_Depth_ClearStone_Wellhead,
                                    In_Depth_ClearStone_DG,
                                    In_Depth_ClearStone_FKD,
                                    In_Depth_ClearStone_PTFR,
                                    In_Depth_ClearStone_GPH,
                                    In_Depth_ClearStone_SC,
                                    In_Depth_ClearStone_ED) #Combine Clearstone's equipment source top 5% emitter data Frame by Row
Finalised_Equipment_Top_5 <- rbind.data.frame(Equipment_Top_5,
                                              Equipment_Top_5_GreenPath) #Combine GreenPath's equipment source top 5% emitter data Frame by Row
Finalised_Equipment_Top_5 <- Finalised_Equipment_Top_5 %>% 
  mutate(Major_Equipment=gsub("Filter/Separation","Separator",Major_Equipment)) #Change the name of the filter/separation to separator
write.csv(Finalised_Equipment_Top_5,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Top 5 Equipment Scatter Analysis.csv") #Export the dataframe of the top 5% emitters in the equipment population for both Clearstone and GreenPath to a csv file type
#-------------------------------------------------------------------------------
#Segment Population
#Gas Batteries
ClearStone_Battery <- ClearStone %>% 
  filter(Petrinex_Facility_Subtype!="Compressor Station"&Petrinex_Facility_Subtype!="Gas Gathering System") #Filter gas battery populations by filtering out gas gathering systems and compressor stations
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Multiwell effluent","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype) #Change Gas Multiwell Effluent to gas battery
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Multiwell Group","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype) #Change Gas Multiwell Group to gas battery
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Multiwell proration outside SE AB","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype) #Change Gas Multiwell proration outside SE AB to gas battery
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Single","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype) #Change Gas Single to gas battery
ClearStone_Battery$Petrinex_Facility_Subtype <- factor(ClearStone_Battery$Petrinex_Facility_Subtype) #Convert gsub results into factor
ClearStone_Battery <- ClearStone_Battery %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate cumulative emission
ClearStone_Battery$Petrinex_Facility_Subtype <- as.numeric(ClearStone_Battery$Petrinex_Facility_Subtype) #Convert the factor into a number for cumulative proportion calculation
ClearStone_Battery <- ClearStone_Battery %>%
  mutate(Number_Of_Sites_CS=cumsum(Petrinex_Facility_Subtype)/max(sum(Petrinex_Facility_Subtype))) #Calculate the cumulative proportion
write.csv(ClearStone_Battery,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Battery.csv") #Export the dataframe of the gas battery population to a csv file type
#Skewness Analysis
In_Depth_ClearStone_Battery<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate columns
  filter(Petrinex_Facility_Subtype!="Compressor Station"&Petrinex_Facility_Subtype!="Gas Gathering System") %>% #Filter gas battery populations by filtering out gas gathering systems and compressor stations
  mutate(Petrinex_Facility_Subtype=gsub("Gas Multiwell effluent","Gas Battery",Petrinex_Facility_Subtype)) %>% #Change Gas Multiwell Effluent to gas battery
  mutate(Petrinex_Facility_Subtype=gsub("Gas Multiwell Group","Gas Battery",Petrinex_Facility_Subtype)) %>% #Change Gas Multiwell Group to gas battery
  mutate(Petrinex_Facility_Subtype=gsub("Gas Multiwell proration outside SE AB","Gas Battery",Petrinex_Facility_Subtype)) %>% #Change Gas Multiwell proration outside SE AB to gas battery
  mutate(Petrinex_Facility_Subtype=gsub("Gas Single","Gas Battery",Petrinex_Facility_Subtype)) %>% #Change Gas Single to gas battery
  mutate(Petrinex_Facility_Subtype=factor(Petrinex_Facility_Subtype)) %>% #Convert gsub results into factor
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank emission from lowest to highest
  filter(Rank >= 107) #Filter the top 5% emitters
#Compressor Stations
ClearStone_Compressor_Station <- ClearStone %>% 
  filter(Petrinex_Facility_Subtype=="Compressor Station") %>% #Filter the compressor station data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission
ClearStone_Compressor_Station$Petrinex_Facility_Subtype <- as.numeric(ClearStone_Compressor_Station$Petrinex_Facility_Subtype) #Convert the factor into a number for cumulative proportion calculation
ClearStone_Compressor_Station <- ClearStone_Compressor_Station %>%
  mutate(Number_Of_Sites_CS=cumsum(Petrinex_Facility_Subtype)/max(sum(Petrinex_Facility_Subtype))) #Calculate cumulative proportion
write.csv(ClearStone_Compressor_Station,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_CS.csv") #Export the dataframe of the compressor station population to a csv file type
#Skewness Analysis
In_Depth_ClearStone_Compressor_Station<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select appropriate columns
  filter(Petrinex_Facility_Subtype=="Compressor Station") %>% #Filter the compressor station data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission from lowest to highest
  filter(Rank == 19) #Filter the top 5% emitter
#Gas Gathering Systems
ClearStone_Gathering <- ClearStone %>% 
  filter(Petrinex_Facility_Subtype=="Gas Gathering System") %>% #Filter the gas gathering system data
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source))) #Calculate the cumulative emission
ClearStone_Gathering$Petrinex_Facility_Subtype <- as.numeric(ClearStone_Gathering$Petrinex_Facility_Subtype) #Convert the factor into a number for cumulative proportion calculation
ClearStone_Gathering <- ClearStone_Gathering %>%
  mutate(Number_Of_Sites_CS=cumsum(Petrinex_Facility_Subtype)/max(sum(Petrinex_Facility_Subtype))) #Calculate cumulative proportion
write.csv(ClearStone_Gathering,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Gathering.csv") #Export the dataframe of the gas gathering system population to a csv file type
#Skewness Analysis
In_Depth_ClearStone_Gathering<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% #Select the appropriate columns
  filter(Petrinex_Facility_Subtype=="Gas Gathering System") %>%  #Filter the gas gathering system data
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% #Rank the emission from lowest to highest
  filter(Rank >= 61)#Filter the top 5% emitter
Segment_Top_5 <- rbind.data.frame(In_Depth_ClearStone_Battery,
                                  In_Depth_ClearStone_Compressor_Station,
                                  In_Depth_ClearStone_Gathering) #Combine top 5% emitter of segment population data frame by row
Finalised_Segment_Top_5 <- rbind.data.frame(Segment_Top_5,
                                            Segment_Top_5_GreenPath) #Combine top 5% emitters from Clearstone and GreenPath's segment population dataframe by row
write.csv(Finalised_Segment_Top_5,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Top 5 Segment Scatter Analysis.csv") #Export the dataframe of the top 5% emitters in the segment population for both Clearstone and GreenPath to a csv file type
#----------------------------------------------------------
#Data Visualisation of Histograms 
library(ggplot2) #Import ggplot2 library for ggplot visualisation
library(ineq) #Import ineq library for Lorenz curve visualisation
library(gglorenz) #Import gglorenz for Lorenz curve visualisation with ggplot functions
#Visualise the histogram for the emission volumes of Clearstone using ggplot
ClearStone_Histo <- ggplot(data=ClearStone,aes(x=CH4_Leak_Rate_m3_per_day_per_source))
ClearStone_Histo + geom_histogram(aes(y=stat(density)),
                                  fill="LightBlue",
                                  colour="Black",
                                  size=1)+
  scale_x_continuous(trans="log")+ #Transform into log base, optional line. 
  # facet_grid(Emission_Type~.,
  #           scales="free",
  #            space="free")+
  xlab("Methane Emissions (m3/day)")+
  ylab("Density")+
  ggtitle("Alberta Fugitive Equipment Leak Data 2017")+ #Making the graph look nicer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20),
        strip.text=element_text(size=10))
#Visualise the probability density of the separator equipment population from Clearstone
SPRT_Plot <- ggplot(data=ClearStone_Separator,aes(x=CH4_Leak_Rate_m3_per_day_per_source))
SPRT_Plot + geom_density(aes(y=stat(density)),
                         fill="LightBlue",
                         colour="black",
                         size=1)+
  xlab("Methane Leak Rate (m3/day)")+
  ylab("Density")+
  ggtitle("Density Curve of Separator Equipment")+
  #scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#----------------------------------------------------------------------------------
#Lorenz Curves
#Cumulative Curve (Lorenz Curve) (% of Sites)
#Object Order: CC/SPRT/WH/DG/FKD/PTFR/BTRY/CCS/GS/GPH/SC/ED, Lorenz
#Acronym list
#CC: Reciprocating Compressor, SPRT: Separator, WH = Wellhead, DG = Dehydrator-Glycol, FKD= FLare Knockout Drum
#PTFR = Production Tank Fixed Roof, #BTRY = Battery, #CCS = Compressor Station, GS = Gas Gathering System
#GPH = Gas Pipeline Header, SC = Screw Compressor ED = Electric Driver Compressor
#Assign all Clearstone equipment and segment populations to a ggplot object for Lorenz curve plot
CC_Lorenz <- ggplot(data=ClearStone_Compressor,aes(x=Number_Of_Sites_CS))
SPRT_Lorenz <- ggplot(data=ClearStone_Separator,aes(x=Number_Of_Sites_CS))
WH_Lorenz <- ggplot(data=ClearStone_Wellhead,aes(x=Number_Of_Sites_CS)) 
DG_Lorenz <- ggplot(data=ClearStone_DG,aes(x=Number_Of_Sites_CS))
FKD_Lorenz <- ggplot(data=ClearStone_FKD,aes(x=Number_Of_Sites_CS))
PTFR_Lorenz <- ggplot(data=ClearStone_PTFR,aes(x=Number_Of_Sites_CS))
BTRY_Lorenz <- ggplot(data=ClearStone_Battery,aes(x=Number_Of_Sites_CS))
CCS_Lorenz <- ggplot(data=ClearStone_Compressor_Station,aes(x=Number_Of_Sites_CS))
GS_Lorenz <- ggplot(data=ClearStone_Gathering,aes(x=Number_Of_Sites_CS))
GPH_Lorenz <- ggplot(data=ClearStone_GPH,aes(x=Number_Of_Sites_CS))
SC_Lorenz <- ggplot(data=ClearStone_SC,aes(x=Number_Of_Sites_CS))
ED_Lorenz <- ggplot(data=ClearStone_ED,aes(x=Number_Of_Sites_CS))
#Use the ggplot object with the gglorenz function "stat_lorenz"
#The function calculates the cumulative proportions for both emission and number of populations
CC_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                        size=3,
                        colour="magenta4")+
  xlab("Percent Of Compressor Equipment Population")+
  ylab("Percent of Emissions")+
  ggtitle("Reciprocating Compressor vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
SPRT_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                          size=3,
                          colour="magenta4")+
  xlab("Percent Of Separator Equipment Population")+
  ylab("Percent of Emissions")+
  ggtitle("Separator vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
WH_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                        size=3,
                        colour="magenta4")+
  xlab("Percent Of Wellhead Equipment Population")+
  ylab("Percent of Emissions")+
  ggtitle("Wellhead vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
DG_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                        size=3,
                        colour="magenta4")+
  xlab("Percent Of Dehydrator-Glycol Equipment Population")+
  ylab("Percent of Emissions")+
  ggtitle("Dehydrator-Glycol vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
FKD_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                         size=3,
                         colour="magenta4")+
  xlab("Percent Of Flare Knockout Drum Equipment Population")+
  ylab("Percent of Emissions")+
  ggtitle("Flare Knockout Drum vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
PTFR_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                          size=3,
                          colour="magenta4")+
  xlab("Percent Of Production Tank (Fixed roof) Equipment Population")+
  ylab("Percent of Emissions")+
  ggtitle("Production Tank (Fixed Roof) vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
BTRY_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                          size=3,
                          colour="magenta4")+
  xlab("Percent Of Gas Batteries Population")+
  ylab("Percent of Emissions")+
  ggtitle("Gas Batteries vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
CCS_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                         size=3,
                         colour="magenta4")+
  xlab("Percent Of Compressor Stations Population")+
  ylab("Percent of Emissions")+
  ggtitle("Compressor Stations vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
GS_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                        size=3,
                        colour="magenta4")+
  xlab("Percent Of Gathering Systems Population")+
  ylab("Percent of Emissions")+
  ggtitle("Gas Gathering Systems vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
GPH_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                         size=3,
                         colour="magenta4")+
  xlab("Percent Of Gas Pipeline Headers Population")+
  ylab("Percent of Emissions")+
  ggtitle("Gas Pipeline Headers vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
SC_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                        size=3,
                        colour="magenta4")+
  xlab("Percent Of Screw Compressors Population")+
  ylab("Percent of Emissions")+
  ggtitle("Screw Compressors vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
ED_Lorenz + stat_lorenz(aes(CH4_Leak_Rate_m3_per_day_per_source),
                        size=3,
                        colour="magenta4")+
  xlab("Percent Of Electric Driver Compressors Population")+
  ylab("Percent of Emissions")+
  ggtitle("Electric Driver Compressors vs Total Emissions")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#----------------------------------------------------------------------------------
#Plotting Multiple Lorenz Curves
#Plot the Lorenz curve using the emission m3/day from the emission source population
#Equipment Population
plot(Lc(ClearStone_Compressor$CH4_Leak_Rate_m3_per_day_per_source),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Equipment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
#Plot the Lorenz curves from the remaining emission source population
lines(Lc(ClearStone_Separator$CH4_Leak_Rate_m3_per_day_per_source),
      col="blue",
      lwd=4)
lines(Lc(ClearStone_Wellhead$CH4_Leak_Rate_m3_per_day_per_source),
      col="orange",
      lwd=4)
lines(Lc(ClearStone_DG$CH4_Leak_Rate_m3_per_day_per_source),
      col="magenta",
      lwd=4)
lines(Lc(ClearStone_FKD$CH4_Leak_Rate_m3_per_day_per_source),
      col="slateblue3",
      lwd=4)
lines(Lc(ClearStone_PTFR$CH4_Leak_Rate_m3_per_day_per_source),
      col="black",
      lwd=4)
lines(Lc(ClearStone_GPH$CH4_Leak_Rate_m3_per_day_per_source),
      col="dodgerblue3",
      lwd=4)
lines(Lc(ClearStone_SC$CH4_Leak_Rate_m3_per_day_per_source),
      col="thistle3",
      lwd=4)
lines(Lc(ClearStone_ED$CH4_Leak_Rate_m3_per_day_per_source),
      col="springgreen2",
      lwd=4)
legend("topleft",inset= 0.01,legend=c("Reciprocating Compressor",
                                      "Separator",
                                      "Wellhead",
                                      "Dehydrator-Glycol",
                                      "Flare Knockout Drum",
                                      "Production Tank (Fixed Roof)",
                                      "Gas Pipeline Header",
                                      "Screw Compressors",
                                      "Electric Driver Compressors"),
       col=c("red",
             "blue",
             "orange",
             "magenta",
             "slateblue3",
             "black",
             "dodgerblue3",
             "thistle3",
             "springgreen2"),pch=15:18,
       horiz=F)
grid(col="lightgray")
#Segment Population
#Plot the Lorenz curve using the emission m3/day from the reporting segment population
plot(Lc(ClearStone_Battery$CH4_Leak_Rate_m3_per_day_per_source),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Segment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
#Plot the Lorenz curves for the remaining reporting segment population
lines(Lc(ClearStone_Compressor_Station$CH4_Leak_Rate_m3_per_day_per_source),
      col="blue",
      lwd=4)
lines(Lc(ClearStone_Gathering$CH4_Leak_Rate_m3_per_day_per_source),
      col="orange",
      lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Battery",
                "Compressor Station",
                "Gathering System"),
       col=c("red","blue","orange"),
       pch=15:18,
       horiz=F,
       border = "white")
grid(col="lightgray")
#-------------------------------------------------------------
#Top 5% Emitters Analysis
#Plot scatter graphs for the top 5% emitters' equipment and reporting segment populations against the average absolute emission rate
#Equipment Population Version
Equipment_Top_5_Scatter <- ggplot(data=Finalised_Equipment_Top_5) #Assign a ggplot object for the top 5% emitter equipment population
Equipment_Top_5_Plot<- Equipment_Top_5_Scatter + geom_point(aes(x=ceiling(Avg_Abs_Emission_Rate),
                                                                y=CH4_Leak_Rate_m3_per_day_per_source,
                                                                colour=Major_Equipment,
                                                                size=CH4_Leak_Rate_m3_per_day_per_source))+ #Map the columns to its appropriate mapping functions based on the characteristics of the column
  xlim(21.975,22.025)+ #Set an x axis limit between 21.975 and 22.025 m3/day
  xlab("Average Absolute Emission Rate (m3/day)")+
  ylab("Methane Leak Rate (m3/day)")+
  scale_y_continuous(breaks = ceiling(seq(min(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          max(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          by = 50)))+ #Set the y axis intervals between the minimum and the maximum value of the emission volume by 50m3/day
  ggtitle("Equipment Methane Emissions against Average Absolute Emission Rate")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
Equipment_Top_5_Plot$labels$size = "Methane Emissions (m3/day)" #Change the label name of the size legend of the scatter graph
Equipment_Top_5_Plot$labels$colour = "Equipment" #Change the label name of the colour legend of the scatter graph
#Zoomed in version between 0 - 105 m3/day, as the there are more emission being emitted around this range
Equipment_Top_5_Plot + coord_cartesian(ylim=c(0,105))+ #Constrain the y axises to between 0 and 105m3/day
  scale_y_continuous(breaks = ceiling(seq(min(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          max(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          by = 5))) #Set the y axis intervals between the minimum and the maximum value of the emission volume by 5m3/day
#Segment Population Version
Segment_Top_5_Scatter <- ggplot(data=Finalised_Segment_Top_5) #Assign a ggplot object for the top 5% emitter equipment population
Segment_Top_5_Plot<- Segment_Top_5_Scatter + geom_point(aes(x=ceiling(Avg_Abs_Emission_Rate),
                                                            y=CH4_Leak_Rate_m3_per_day_per_source,
                                                            colour=Petrinex_Facility_Subtype,
                                                            size=CH4_Leak_Rate_m3_per_day_per_source))+ #Map the columns to its appropriate mapping functions based on the characteristics of the column
  xlim(21.975,22.025)+ #Set an x axis limit between 21.975 and 22.025 m3/day
  xlab("Average Absolute Emission Rate (m3/day)")+
  ylab("Methane Leak Rate (m3/day)")+
  scale_y_continuous(breaks = ceiling(seq(min(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          max(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source), 
                                          by = 50)))+ #Set the y axis intervals between the minimum and the maximum value of the emission volume by 50m3/day
  ggtitle("Segment Methane Emissions against Average Absolute Emission Rate")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
Segment_Top_5_Plot$labels$size = "Methane Emissions (m3/day)" #Change the label name of the size legend of the scatter graph
Segment_Top_5_Plot$labels$colour = "Segment" #Change the label name of the colour legend of the scatter graph
#Zoomed in version between 25 - 100 m3/day, as the there are more emission being emitted around this range
Segment_Top_5_Plot + coord_cartesian(ylim=c(25,100))+
  scale_y_continuous(breaks = ceiling(seq(min(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          max(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          by = 5))) #Set the y axis intervals between the minimum and the maximum value of the emission volume by 5m3/day
#-------------------------------------------------------------
#Probability Distribution Fitting
library(fitdistrplus) #Load this library to import Weibull, Gamma and Log-Normal functions  to plot the probability distribution functions
library(actuar) #Load this library to import log-logistic functions to create log-logistic distributions
#Acronym List
#Object name goes in order: f, w/g/ln/ll, data you want to fit
#f = fit, #w,g,ln,ll = Weibull, Gamma, Log Normal, Log Logistic 
#Fit your associated data with the four distributions
fwFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"weibull") #Weibull Distribution
fgFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"gamma") #Gamma Distribution
flnFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"lnorm")# Log-Normal Distribution
fllFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"llogis") #Log-Logistic Distribution 
#Assign a probability density function to the four distributions using the ggplot plotstyle
ProbDistFug <- denscomp(list(fwFug,fgFug,flnFug,fllFug),
                        xlab="Methane Emissions (m3/day)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
#Assign a cumulative probability density function to the four distributions using the ggplot plotstyle
CProbDistFug <- cdfcomp(list(fwFug,fgFug,flnFug,fllFug),
                        xlab="ln(Methane Emissions (m3/day))",
                        ylab="Cumulative Distribution Function",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
#Visualise the probability densisty function using the relevant ggplot functions
ProbDistFug+
  ggtitle("Histogram and Distribution Curves of Fugitive Equipment 2017")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Visualise the cumulative probability densisty function using the relevant ggplot functions
CProbDistFug+
  ggtitle("Cumulative Distribution Curves of Fugitive Equipment Data 2017")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#----------------------------------------------
#Statistics List
#Acronym list
#GOF: Goodness of Fit, LogLik= Log Likelihood
GOFFug<-gofstat(list(fwFug,
                     fgFug,
                     flnFug,
                     fllFug)) #Form a list of the four distributions and input it to the gofstat function
LoglikFug<-list(logW=logLik(fwFug),
                logG=logLik(fgFug),
                logln=logLik(flnFug),
                logll=logLik(fllFug)) #Form a list of the log likelihood values of the four distributions
SummaryFug<-list(SummaryW=summary(fwFug),
                 SummaryG=summary(fgFug),
                 Summaryln=summary(flnFug),
                 Summaryll=summary(fllFug)) #Form a list of the summary statistics of the four distributions
StatsFug <- list(GOF=GOFFug,
                 Logliklihood=LoglikFug,
                 Summary=SummaryFug)#Form a list of all the calculated statistics for ease of access
#Log-Logistic Distribution is good at fitting but Log-normal is used instead as the nature of the log-normal is more appropriate to model the continuous nature of the emissions
#----------------------------------------------
#Monte Carlo Simulation
#Acronym list:
#MC = Monte Carlo, CS = Clearstone
set.seed(123) #This makes sure the results from the random number generation stays the same everytime the simulation is ran
#Create an MC function for the Clearstone script based on the EUR
MC_Function_CS <- function(df,mean,sd){
  CS_Methane_Throughput <- (df$CH4_Leak_Rate_m3_per_day_per_source/rlnorm(nrow(df),mean,sd))*100
  return(CS_Methane_Throughput)
}#Input the desired data frame along with the mean and standard deviation from the best fitted distribution for the EUR
#Amount of random numbers are generated based on the number of rows from the inputted data frame
ClearStone_Sim <- replicate(10000,MC_Function_CS(ClearStone,Mean_LN_EUR,SD_LN_EUR)) #The calculation is repeated 10,000 times using the replicate function
ClearStone_Sim <- data.frame(ClearStone_Sim) #Replicate function returns a matrix object, so the matrix object is converted into a data frame type structure
#Calculations from Simulation are applied to see the mean, median, standard deviation and the percentiles
#Sapply function is used to calculate the mean, median and quantiles of the whole dataframe
Mean_ClearStone_Sim <- sapply(ClearStone_Sim,mean)
Median_ClearStone_Sim <- sapply(ClearStone_Sim,median)
Q5_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.05))
Q25_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.25))
Q75_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.75))
Q95_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.95))
#Filter the values that exceeded 100% as emissions exceeding 100% does not exist in reality
FilteredMean_ClearStone_Sim<-Mean_ClearStone_Sim[Mean_ClearStone_Sim <= 100]
FilteredQ95_ClearStone_Sim <- Q95_ClearStone_Sim[Q95_ClearStone_Sim<=100]
#Find the minimum and maximum values of the mean, median and the percentiles
MinFilteredMeanCSS<-min(FilteredMean_ClearStone_Sim)
MaxFilteredMeanCSS<-max(FilteredMean_ClearStone_Sim)
MinFilteredMedianCSS<-min(Median_ClearStone_Sim)
MaxFilteredMedianCSS<-max(Median_ClearStone_Sim)
MinFilteredQ5CSS<-min(Q5_ClearStone_Sim)
MaxFilteredQ5CSS<-max(Q5_ClearStone_Sim)
MinFilteredQ25CSS<-min(Q25_ClearStone_Sim)
MaxFilteredQ25CSS<-max(Q25_ClearStone_Sim)
MinFilteredQ75CSS<-min(Q75_ClearStone_Sim)
MaxFilteredQ75CSS<-max(Q75_ClearStone_Sim)
MinFilteredQ95CSS<-min(FilteredQ95_ClearStone_Sim)
MaxFilteredQ95CSS<-max(FilteredQ95_ClearStone_Sim)
#---------------------------------------------
#Visualtion of the Monte Carlo graphs using the empricial cumulative distribution function with the base plot
#Plot the first ecdf from the simulation
#color coding: alpha is used to make the curves more transparent
plot(ecdf(ClearStone_Sim[,1]),
     xlab="Methane Emissions (% of EUR)",
     ylab="Cumulative Distribution",
     main="Monte Carlo Simulation of Alberta Fugitive Equipment Data 2017",
     xlim=c(0,100),
     cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     col=alpha("deepskyblue2",0.3))
grid(col="lightgray")
for(j in 2:1000){
  lines(ecdf(ClearStone_Sim[,j]),
        col=alpha("deepskyblue2",0.3))
} #Loop the curves from column 2 to 1000 with the deep sky blue 2 color
for(h in 1001:2000){
  lines(ecdf(ClearStone_Sim[,h]),
        col=alpha("cadetblue2",0.3))
} #Loop the curves from column 1001 to 2000 with the cadet blue 2 color
for(l in 2001:3000){
  lines(ecdf(ClearStone_Sim[,l]),
        col=alpha("darkorchid2",0.3))
} #Loop the curves from column 2001 to 3000 with the dark orchid 2 color
for(p in 3001:4000){
  lines(ecdf(ClearStone_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
} #Loop the curves from column 3001 to 4000 with the light sky blue 2 color
for(o in 4001:5000){
  lines(ecdf(ClearStone_Sim[,o]),
        col=alpha("blueviolet",0.3))
} #Loop the curves from column 4001 to 5000 with the blue violet color
for(k in 5001:6000){
  lines(ecdf(ClearStone_Sim[,k]),
        col=alpha("darkslateblue",0.3))
} #Loop the curves from column 5001 to 6000 with the dark slate blue color
for(s in 6001:7000){
  lines(ecdf(ClearStone_Sim[,s]),
        col=alpha("darkmagenta",0.3))
} #Loop the curves from column 6001 to 7000 with the dark magenta color
for(u in 7001:8000){
  lines(ecdf(ClearStone_Sim[,u]),
        col=alpha("deeppink1",0.3))
} #Loop the curves from column 7001 to 8000 with the deep pink 1 color
for(x in 8001:9000){
  lines(ecdf(ClearStone_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
} #Loop the curves from column 8001 to 9000 with the corn flower blue color
for(f in 9001:10000){
  lines(ecdf(ClearStone_Sim[,f]),
        col=alpha("darkorchid",0.3))
} #Loop the curves from column 9001 to 10000 with the dark orchid color
#----------------------------------------------
#Visualising the Monte Carlo histograms
#Not an essential part of the project but can be used to view the heavy-tail
MC_ClearStone_Histo <- ggplot(data=ClearStone_Sim)
MC_ClearStone_Histo + geom_histogram(aes(x=X10,y=stat(density)),
                                     fill="LightBlue",
                                     colour="Black",
                                     binwidth = 0.001,
                                     size=1)+
  scale_x_continuous(trans="log")+
  ggtitle("Monte Carlo Simulations of Alberta Fugitive Equipment Data 2017")+
  xlab("Methane Emissions (% of Production)")+
  ylab("Density")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#-----------------------------------------------------
#Source: Alberta NG Production Data for Red Deer
#This script aims to evaluate the emission sources provided by Zavala-Araiza et al (2018)
#This script aims to find the best fitting probability distribution for the methane emissions data
#Lorenz curves were applied to the methane emissions, irrespective of its measurement methods
#Super-emitters can be identified through Lorenz curves using the 80/20 rule as a benchmark
#If the remaining 20% of total population emits 80% of total emissions, then this is an indication of a super-emitter
#Top 5% emitters were evaluated to see the skewness contributions by the emitters to the Lorenz curves
#Majority of the emissions are facility emissions from production facilities
#Source script: Alberta Production Data for Red Deer
#Report Source: Methane emissions from oil and gas production sites in Alberta, Canada
#--------------------------------------------
#Set working directory for Production Data
getwd()
setwd("/Users/ratanonsuksumrun/Documents/GitHub/SuperEmitters/Spreadsheets")#Assign working directory
NGProduction <-read.csv("Alberta Production Data for Red Deer.csv",row.names=1)#The imported data had a Site ID column dedicated to each row. This column was the first column in the data and the row.names = 1 was used to turn that column into a row column in RStudio
#Note: the production data in this script are from the Red Deer region
#The data was collected on the year 2016
#This is a top down method, aerial measurements
#--------------------------------------------
#Adjustments to the Production Dataframe 
NGProduction$Oil_production_bbld <- NULL #Take out the Oil Production Data
NGProduction$Gas_production_cmd <- NGProduction$Gas_production_mcfd*35.315 #Conversion source: Google. This is an approximate conversion. 1mcf = 35.315 cubic meter
NGProduction$Gas_production_cms <- NGProduction$Gas_production_cmd/86400 #Convert production per day to per second
NGProduction$CH4_emission_rate_kgs <- NGProduction$CH4_emission_rate_kgh/3600 #Convert methane emissions rate into seconds
NGProduction$CH4_emission_m3 <- NGProduction$CH4_emission_rate_kgs/0.8 #Convert the emissions rate into m3. This is for calculating the % of methane emitted from the total gas production
NGProduction$log_CH4_emission_m3 <- log(NGProduction$CH4_emission_m3)
NGProduction$log_gas_production_cms <- log(NGProduction$Gas_production_cms)
NGProduction$CH4_percentage <- NGProduction$CH4_emission_m3 / NGProduction$Gas_production_cms#Of Throughput data = Methane emissions over instantaneous gas production rate
colnames(NGProduction) #Check column names of the NGProduction Data Frame
NGProduction$CH4_emission_m3_per_day <- NGProduction$CH4_emission_m3*86400
AbsRedDeerEmission <- mean(NGProduction$CH4_emission_m3_per_day)#Absolute Emission Rate in Red Deer Script
#--------------------------------------------------- 
#Filter out the inappropriate % methane data
infinityfilt <-NGProduction$CH4_percentage != Inf & NGProduction$CH4_percentage < 100 #Filter the values that does not exceed 100%
NGProduction<-NGProduction[infinityfilt,] #Apply the infinity filter
#Descriptive Statistics 
RedDeerMean <- mean(NGProduction$Gas_production_cms)
RedDeerSD <- sd(NGProduction$Gas_production_cms)
Log_RDMean <- mean(NGProduction$log_gas_production_cms)
Log_RDSD <- sd(NGProduction$log_gas_production_cms)
#----------------------------------------------------
#Lorenz curve modelling for both top down methods
#Cumulative Of throughput was also used to in the Lorenz curve instead of the cumulative population
library(dplyr)
#Tracer Method
NGProduction_Tracer <- NGProduction %>% 
  dplyr::select(Method,CH4_emission_m3,Gas_production_cms,CH4_percentage,both_methods) %>% #Select appropriate columns
  filter(Method=="tracer flux") %>% #Filter tracer flux data
  filter(both_methods!="a"& both_methods!="b"&both_methods!="c"&both_methods!="d") %>% #Some measurements were measured with gaussian dispersion so this was filtered out
  mutate(Of_Throughput=CH4_percentage*100)#Convert to percentage
Tracer_Desc_OTP <- order(NGProduction_Tracer$Of_Throughput) #Order the Of Throughput values from lowest to highest
OTP_Desc_Tracer <- NGProduction_Tracer[Tracer_Desc_OTP,] #Apply the order filter
OTP_Desc_Tracer <- OTP_Desc_Tracer %>% 
  mutate(Of_Throughput_CS=cumsum(Of_Throughput)/sum(Of_Throughput)) #Calculate the cumulative of throughput 
Tracer_Desc_Order_CH4 <- order(NGProduction_Tracer$CH4_emission_m3) #ORder the emission volume from the lowest to highest
Tracer_CH4_desc <- NGProduction_Tracer[Tracer_Desc_Order_CH4,] #apply the order filter
Tracer_CH4_desc <- Tracer_CH4_desc %>% 
  mutate(CH4_emission_m3_CS=cumsum(CH4_emission_m3)/sum(CH4_emission_m3)) #Calculate the cumulative emission
#Gaussian dispersion
NGProduction_GD <- NGProduction %>% 
  dplyr::select(Method,CH4_emission_m3,Gas_production_cms,CH4_percentage,both_methods) %>% #Select appropriate columns
  filter(Method=="Gaussian dispersion") %>% #Filter Gaussian disperson data
  filter(both_methods!="a"& both_methods!="b"&both_methods!="c"&both_methods!="d") %>% #Some measurements were measured with tracer flux so this was filtered out
  mutate(Of_Throughput_CS=cumsum(CH4_percentage)/max(sum(CH4_percentage))) %>% #Calculate the cumulative of throughput 
  mutate(CH4_emission_m3_CS=cumsum(CH4_emission_m3*CH4_percentage)/max(sum(CH4_emission_m3*CH4_percentage)))#Calculate the cumulative emission
#Both Methods
NGProduction_Both_Methods <- NGProduction %>% 
  dplyr::select(Method,CH4_emission_m3,Gas_production_cms,CH4_percentage,both_methods) %>% #Select appropriate columns
  filter(both_methods == "a"|both_methods == "b"| both_methods == "c" | both_methods == "d") %>% #Filter both methods data
  mutate(Of_Throughput=CH4_percentage*100) 
Desc_Order_OTP <- order(NGProduction_Both_Methods$Of_Throughput) #Order the Of Throughput values from lowest to highest
OTP_Desc <- NGProduction_Both_Methods[Desc_Order_OTP,] #apply the order filter
OTP_Desc <- OTP_Desc %>% 
  mutate(Of_Throughput_CS=cumsum(Of_Throughput)) #Calculate the cumulative of throughput 
Desc_Order_CH4 <- order(NGProduction_Both_Methods$CH4_emission_m3) #Order the emission values from lowest to highest
CH4_Desc <- NGProduction_Both_Methods[Desc_Order_CH4,] #apply the order filter
CH4_Desc <- CH4_Desc %>% 
  mutate(CH4_emission_m3_CS=cumsum(CH4_emission_m3))#Calculate the cumulative emission
#----------------------------------------------------------------------------------
#Plotting Multiple Lorenz Curves
#Plot the Lorenz curve using the emission m3/s with the x axis as a percentage of total population
library(ineq)
plot(Lc(NGProduction$CH4_emission_m3),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Top-Down Method Population Analysis (Red Deer)",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
lines(Lc(NGProduction_Both_Methods$CH4_emission_m3),
      col="blue",
      lwd=4)
lines(Lc(NGProduction_GD$CH4_emission_m3),
      col="orange",
      lwd=4)
#Plot the Lorenz curve using the emission m3/s with the x axis as a percentage of total of throughput
plot(Lc(NGProduction_Both_Methods$CH4_percentage),
     xlab="Percent of Of Throughput Population",
     ylab="Percent of Total Emissions",
     main="Of Throughput for Top-Down Method Population Analysis (Red Deer)",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
#Skewness Analysis
#Filter the Top 5% emitter of the Red Deer Script, this will be used to combine with the top 5% emitters from the Medicine Hat script later
#This will be used to plot a scatter graph of the emissions by the top 5% emitter against the average absolute emission rate in the top down script
Skewness_RedDeer <- NGProduction %>% 
  dplyr::select(Gas_production_cmd,CH4_emission_m3_per_day,CH4_percentage) %>% #Select appropriate columns under dplyr library
  mutate(Rank=rank(CH4_percentage,ties.method="max")) %>% #Rank the of throughput from lowest to highest
  filter(Rank >= 52) #Filter the top 5% emitter
Skewness_PE_RedDeer <- NGProduction %>% 
  dplyr::select(Gas_production_cmd,CH4_emission_m3_per_day,CH4_percentage) %>% #Select appropriate columns under dplyr library
  mutate(Rank=rank(CH4_emission_m3_per_day,ties.method="max")) %>% #Rank the emission from lowest to highest
  filter(Rank >= 52) %>% #Filter the top 5% emitter
  mutate(Region=factor("Red Deer")) #Insert a column called Region embedded with the Red Deer region factor
Agg_Skewness_RedDeer <- Skewness_PE_RedDeer %>% 
  dplyr::select(CH4_emission_m3_per_day,Region) #Select appropriate columns for combining
#----------------------------------------------------
#Data Visualisation
#Plot a histogram of the of throughput data 
library(ggplot2)
NGProductionMethane <- ggplot(data=NGProduction,
                              aes(x=CH4_percentage)) 
NGProductionMethane + 
  geom_histogram(fill="LightBlue",
                 colour="Black",
                 size=1)+
  scale_x_continuous(trans="log")+
  xlab("Methane Emissions (% of Production)")+
  ylab("Density")+
  ggtitle("Alberta Methane Emissions in Natural Gas Production")+ 
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#Plotting a Lorenz curve manually using just ggplot
#Can be adjusted using the gglorenz library but this is not necessary
#This would only be used for enhancing the Lorenz curve visualisation
ggplot() + geom_line(aes(x=OTP_Desc$Of_Throughput_CS,
                         y=CH4_Desc$CH4_emission_m3_CS))
ggplot() + geom_line(aes(x=OTP_Desc_Tracer$Of_Throughput_CS,
                         y=Tracer_CH4_desc$CH4_emission_m3_CS)) 
ggplot(data=NGProduction_GD)+ geom_line(aes(x=Of_Throughput_CS,
                                            y=CH4_emission_m3_CS),
                                        size=3,
                                        colour="dodgerblue3")
ggplot(data=NGProduction,aes(x=CH4_percentage))+ 
  stat_lorenz(aes(CH4_emission_m3),
              desc=FALSE,
              size=3,
              fill="dodgerblue3")
#----------------------------------------------------
#Probability Distribution of Data Fitting
library(fitdistrplus) #Load all the fitdistr to plot the probability distribution functions (Weibull, Gamma and Log Normal)
library(actuar) #Load the Log-Logistic Distribution function
fwRedDeer <- fitdist(NGProduction$CH4_percentage,"weibull") #Weibull Distribution
fgRedDeer <- fitdist(NGProduction$CH4_percentage,"gamma") #Gamma Distribution
flnRedDeer <- fitdist(NGProduction$CH4_percentage,"lnorm")# Log-Normal Distribution
fllRedDeer <- fitdist(NGProduction$CH4_percentage,"llogis") #Log-Logistic Distribution 
#Assign a probability density function to the four distributions using the ggplot plotstyle
ProbDistRedDeer<-denscomp(list(fwRedDeer,
                               fgRedDeer,
                               flnRedDeer,
                               fllRedDeer), 
                          xlab="Methane Emissions (% of Production)",
                          plotstyle="ggplot",
                          legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                          line01lty="solid")
#Assign a cumulative probability density function to the four distributions using the ggplot plotstyle
CProbDistRedDeer<-cdfcomp(list(fwRedDeer,
                               fgRedDeer,
                               flnRedDeer,
                               fllRedDeer),
                          xlab="ln(Methane Emissions (m3/day))",
                          ylab="Cumulative Distribution Function",
                          plotstyle="ggplot",
                          legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                          line01lty="solid")
#Visualise the probability densisty function using the relevant ggplot functions
ProbDistRedDeer+
  ggtitle("Histogram and Distribution Curves of Production Data (Red Deer 2016)")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Visualise the cumulative probability densisty function using the relevant ggplot functions
CProbDistRedDeer+
  ggtitle("Cumulative Distribution Curves of Production Data in Red Deer 2016")+ 
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Summary Statistics
GOFProRedDeer<-gofstat(list(fwRedDeer,
                            fgRedDeer,
                            flnRedDeer,
                            fllRedDeer)) #Form a list of the four distributions and input it to the gofstat function
LogLikProRedDeer<-list(logW=logLik(fwRedDeer),
                       logG=logLik(fgRedDeer),
                       logln=logLik(flnRedDeer),
                       logll=logLik(fllRedDeer)) #Form a list of the log likelihood values of the four distributions
SummaryProRedDeer <- list(SummaryW=summary(fwRedDeer),
                          SummaryG=summary(fgRedDeer),
                          Summaryln=summary(flnRedDeer),
                          Summaryll=summary(fllRedDeer)) #Form a list of the summary statistics of the four distributions
StatsProRedDeer <- list(GOF=GOFProRedDeer,
                        LogLikelihood=LogLikProRedDeer,
                        Summary=SummaryProRedDeer) #Form a list of all the calculated statistics for ease of access
#Distribution fitting:
#Log-Logistic is the best at fittings
#However, Logistic is used for discrete variables. Not as appropriate. Log Normal is used instead
#----------------------------------------------------
#Monte Carlo Simulation
#Acronym list:
#MC = Monte Carlo, RD = Red Deer
set.seed(123) #This makes sure the results from the random number generation stays the same everytime the simulation is ran
#Create an MC function for the Red Deer script based on the EUR
MC_Function <- function(df,mean,sd){
  RD_Methane_Throughput_MC <- (df$CH4_emission_m3_per_day/rlnorm(nrow(df),mean,sd))*100
  return(RD_Methane_Throughput_MC)
}#Input the desired data frame along with the mean and standard deviation from the best fitted distribution for the EUR
#Amount of random numbers are generated based on the number of rows from the inputted data frame
RedDeer_Sim <- replicate(10000,MC_Function(NGProduction,Mean_LN_EUR,SD_LN_EUR)) #The calculation is repeated 10,000 times using the replicate function
RedDeer_Sim <- data.frame(RedDeer_Sim)#Replicate function returns a matrix object, so the matrix object is converted into a data frame type structure
#Calculations from Simulation are applied to see the mean, median, standard deviation and the percentiles
#Sapply function is used to calculate the mean, median and quantiles of the whole dataframe
Mean_RedDeer_Sim <- sapply(RedDeer_Sim,mean)
Median_RedDeer_Sim <- sapply(RedDeer_Sim,median)
Q5_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.05))
Q25_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.25))
Q75_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.75))
Q95_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.95))
#Filter the values that exceeded 100% as emissions exceeding 100% does not exist in reality
FilteredMean_RedDeer_Sim<-Mean_RedDeer_Sim[Mean_RedDeer_Sim <= 100]
FilteredMedian_RedDeer_Sim<-Median_RedDeer_Sim[Median_RedDeer_Sim <= 100]
FilteredQ75_RedDeer_Sim <- Q75_RedDeer_Sim[Q75_RedDeer_Sim<=100]
FilteredQ95_RedDeer_Sim <- Q95_RedDeer_Sim[Q95_RedDeer_Sim<=100]
#Find the minimum and maximum values of the mean, median and the percentiles
MinFilteredMeanRD<-min(FilteredMean_RedDeer_Sim)
MaxFilteredMeanRD<-max(FilteredMean_RedDeer_Sim)
MinFilteredMedianRD<-min(FilteredMedian_RedDeer_Sim)
MaxFilteredMedianRD<-max(FilteredMedian_RedDeer_Sim)
MinFilteredQ5RD<-min(Q5_RedDeer_Sim)
MaxFilteredQ5RD<-max(Q5_RedDeer_Sim)
MinFilteredQ25RD<-min(Q25_RedDeer_Sim)
MaxFilteredQ25RD<-max(Q25_RedDeer_Sim)
#--------------------------------------
#Visualtion of the Monte Carlo graphs using the empricial cumulative distribution function with the base plot
#Plot the first ecdf from the simulation
#color coding: alpha is used to make the curves more transparent
plot(ecdf(RedDeer_Sim[,1]),
     xlab="Methane Emissions (% of EUR)",
     ylab="Cumulative Distribution",
     main="Monte Carlo Simulation of Production Data (Red Deer)",
     xlim=c(0,100),
     cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     col=alpha("deepskyblue2",0.3))
grid(col="lightgray")
for(j in 2:1000){
  lines(ecdf(RedDeer_Sim[,j]),
        col=alpha("deepskyblue2",0.3))
} #Loop the curves from column 2 to 1000 with the deep sky blue 2 color
for(h in 1001:2000){
  lines(ecdf(RedDeer_Sim[,h]),
        col=alpha("cadetblue2",0.3))
} #Loop the curves from column 1001 to 2000 with the cadet blue 2 color
for(l in 2001:3000){
  lines(ecdf(RedDeer_Sim[,l]),
        col=alpha("darkorchid2",0.3))
} #Loop the curves from column 2001 to 3000 with the dark orchid 2 color
for(p in 3001:4000){
  lines(ecdf(RedDeer_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
} #Loop the curves from column 3001 to 4000 with the light sky blue 2 color
for(o in 4001:5000){
  lines(ecdf(RedDeer_Sim[,o]),
        col=alpha("blueviolet",0.3))
} #Loop the curves from column 4001 to 5000 with the blue violet color
for(k in 5001:6000){
  lines(ecdf(RedDeer_Sim[,k]),
        col=alpha("darkslateblue",0.3))
} #Loop the curves from column 5001 to 6000 with the dark slate blue color
for(s in 6001:7000){
  lines(ecdf(RedDeer_Sim[,s]),
        col=alpha("darkmagenta",0.3))
} #Loop the curves from column 6001 to 7000 with the dark magenta color
for(u in 7001:8000){
  lines(ecdf(RedDeer_Sim[,u]),
        col=alpha("deeppink1",0.3))
} #Loop the curves from column 7001 to 8000 with the deep pink 1 color
for(x in 8001:9000){
  lines(ecdf(RedDeer_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
} #Loop the curves from column 8001 to 9000 with the corn flower blue color
for(f in 9001:10000){
  lines(ecdf(RedDeer_Sim[,f]),
        col=alpha("darkorchid",0.3))
} #Loop the curves from column 9001 to 10000 with the dark orchid color
#--------------------------------------
#Visualising the Monte Carlo histograms
#Not an essential part of the project but can be used to view the heavy-tail
MC_RedDeer_Histo <- ggplot(data=RedDeer_Sim)
MC_RedDeer_Histo + geom_histogram(aes(x=X10,y=stat(density)),
                                  fill="LightBlue",
                                  colour="Black",
                                  binwidth = 0.2,
                                  size=1)+
  scale_x_continuous(trans="log")+
  ggtitle("Monte Carlo Simulations of Alberta Production Data (Red Deer) 2016")+
  xlab("Methane Emissions (% of Production)")+
  ylab("Density")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#----------------------------------------------------------
#Source: Alberta NG Production Data for Medicine Hat
#This script aims to evaluate the emission sources provided by O'Connell et al (2019)
#This script aims to find the best fitting probability distribution for the methane emissions data
#Lorenz curves were applied to the methane emissions, irrespective of its measurement methods
#Super-emitters can be identified through Lorenz curves using the 80/20 rule as a benchmark
#If the remaining 20% of total population emits 80% of total emissions, then this is an indication of a super-emitter
#Top 5% emitters were evaluated to see the skewness contributions by the emitters to the Lorenz curves
#Majority of the emissions are facility emissions from production facilities
#Source script: Alberta NG Production Data Medicine Hat
#Report Source: Methane emissions from contrasting production regions within Alberta, Canada: Implications under incoming federal methane regulations
#--------------------------------------------
#Set working directory for Production Data (Medicine Hat)
getwd()
setwd("/Users/ratanonsuksumrun/Documents/GitHub/SuperEmitters/Spreadsheets") #Assign working directory
NGProductionMedicine <- read.csv("Alberta Production Data for Medicine Hat.csv",row.names=1) #The imported data had a Site ID column dedicated to each row. This column was the first column in the data and the row.names = 1 was used to turn that column into a row column in RStudio
#Note: the production data in this script are from the Medicine Hat region
#Gas source in this region study is CONVENTIONAL GAS
#This data set was collected in Autumn 2016
#This is a top down method, aerial measurements
#--------------------------------------------
#Data Frame Preparation
#Rename the column names accordingly
colnames(NGProductionMedicine)[1:7] <- c("Mean_CH4_m3day",
                                         "Median_CH4_m3day",
                                         "Mdl_CH4_m3day",
                                         "SD_CH4_m3day",
                                         "SE_CH4_m3day",
                                         "Min_CH4_m3day",
                                         "Max_CH4_m3day")
#Using the dplyr libary
library(dplyr)
#Add a logarithmic column, not necessarily important
NGProductionMedicine <- NGProductionMedicine %>% 
  mutate(LN_Mean_CH4_m3day=log(Mean_CH4_m3day))
#Skewness Analysis
#Create the top 5% emitter data frame for the medicine hat script
Skewness_MedicineHat <- NGProductionMedicine %>% 
  dplyr::select(Mean_CH4_m3day,Median_CH4_m3day) %>% #Select appropriate columns
  mutate(Rank=rank(Mean_CH4_m3day,ties.method="max")) %>% #Rank the mean emission volume from lowest to highest
  filter(Rank >= 89) %>% #Filter the top 5% emitters
  mutate(Region=factor("Medicine Hat")) #Add a Region column embedded with Medicine Hat factor
Agg_Skewness_MedicineHat <- Skewness_MedicineHat %>% #Assign a new object
  dplyr::select(Mean_CH4_m3day,Region) #Select the appropriate columns
colnames(Agg_Skewness_MedicineHat)[1] <- c("CH4_emission_m3_per_day") #Change column name
TopDown_5_Percent <- rbind.data.frame(Agg_Skewness_RedDeer,
                                      Agg_Skewness_MedicineHat) #Combine both top 5% emitters dataframe by row from Red Deer and Medicine Hat scripts 
Avg_Mean_Emission_MedHat <- mean(NGProductionMedicine$Mean_CH4_m3day) #Average absolute emission rate for Medicine Hat
Avg_Mean_TopDown_Emission <- mean(c(Avg_Mean_Emission_MedHat,AbsRedDeerEmission)) #Average absolute emission rate for top down
#----------------------------------------------
#Data Visualisation
#Plot a histogram of the emissions data data 
library(ggplot2)
library(gglorenz)
MedHatProduction <- ggplot(data=NGProductionMedicine,
                         aes(x=Mean_CH4_m3day))
#Normal Version
MedHatProduction + 
  geom_histogram(aes(y=stat(density)),
                 fill="LightBlue",
                 colour="Black",
                 size=1)+
  xlab("Methane Emissions (m3/day")+
  ylab("Density")+
  ggtitle("Alberta (Medicine Hat) Methane Emissions in Natural Gas Production")+ 
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#Plotting Multiple Lorenz Curves
#Plot the Lorenz curve using the emission m3/day for the Medicine Hat Script
plot(Lc(NGProductionMedicine$Median_CH4_m3day),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Top-Down Method Population Analysis (Medicine Hat)",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
lines(Lc(NGProductionMedicine$Mean_CH4_m3day),
      col="blue",
      lwd=4)
lines(Lc(NGProductionMedicine$Min_CH4_m3day),
      col="orange",
      lwd=4)
lines(Lc(NGProductionMedicine$Max_CH4_m3day),
      col="green",
      lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Median","Mean","Min","Max"),
       col=c("red","blue","orange","green"),pch=15:18,horiz=F)
grid(col="lightgray")   
#Skewness Plot
#Plot the scatter graph of the top 5% emitters of the top down scripts against the average absolute emission rate
TopDown_Top_5_Scatter <- ggplot(data=TopDown_5_Percent)
TopDown_Top_5_Plot<- TopDown_Top_5_Scatter + geom_point(aes(x=ceiling(Avg_Mean_TopDown_Emission),
                                                            y=CH4_emission_m3_per_day,
                                                            colour=Region,
                                                            size=CH4_emission_m3_per_day))+
  xlab("Average Absolute Emission Rate (m3/day)")+
  ylab("Methane Leak Rate (m3/day)")+
  scale_y_continuous(limits = c(0, 2500), 
                     breaks = seq(0, 2500, by = 100))+
  ggtitle("Top-Down Methane Emissions against Average Absolute Emission Rate")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
#----------------------------------------------------
#Probability Distribution Data Fitting 
library(fitdistrplus) #Load all the fitdistr to plot the probability distribution functions (Weibull, Gamma and Log Normal)
library(actuar) #Load the Log-Logistic Distribution function
fwProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"weibull") #Weibull Distribution
fgProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"gamma") #Gamma Distribution
flnProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"lnorm")# Log-Normal Distribution
fllProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"llogis") #Log-Logistic Distribution 
#Assign a probability density function to the four distributions using the ggplot plotstyle
ProbDistMed <- denscomp(list(fwProMedicine,fgProMedicine,flnProMedicine,fllProMedicine),
                        xlab="Methane Emissions (Mean m3/day)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
#Assign a cumulative probability density function to the four distributions using the ggplot plotstyle
CProbDistMed <- cdfcomp(list(fwProMedicine,fgProMedicine,flnProMedicine,fllProMedicine),
                        xlab="ln(Methane Emissions (m3/day))",
                        ylab="Cumulative Distribution Function",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
#Visualise the probability densisty function using the relevant ggplot functions
ProbDistMed+
  ggtitle("Histogram and Distribution Curves of Production Data (Medicine Hat)")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Visualise the cumulative probability densisty function using the relevant ggplot functions
CProbDistMed+
  ggtitle("Cumulative Distribution Curves of Production Data in Medicine Hat 2016")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Summary Statistics
GOFProMed<-gofstat(list(fwProMedicine,
                        fgProMedicine,
                        flnProMedicine,
                        fllProMedicine))#Form a list of the four distributions and input it to the gofstat function
LogLikProMed<-list(logW=logLik(fwProMedicine),
                   logG=logLik(fgProMedicine),
                   logln=logLik(flnProMedicine),
                   logll=logLik(fllProMedicine)) #Form a list of the log likelihood values of the four distributions
SummaryProMed <- list(SummaryW=summary(fwProMedicine),
                      SummaryG=summary(fgProMedicine),
                      Summaryln=summary(flnProMedicine),
                      Summaryll=summary(fllProMedicine)) #Form a list of the summary statistics of the four distributions
StatsProMed <- list(GOF=GOFProMed,
                    LogLikelihood=LogLikProMed,
                    Summary=SummaryProMed) #Form a list of all the calculated statistics for ease of access
#Distribution fitting:
#Log-Logistic is the best at fittings
#However, Logistic is used for discrete variables. Not as appropriate. Log Normal is used instead
#----------------------------------------------------
#Monte Carlo Simulation
#Acronym list:
#MC = Monte Carlo, PD = Production, MH = Medicine Hat
set.seed(123) #This makes sure the results from the random number generation stays the same everytime the simulation is ran
#Create an MC function for the Red Deer script based on the EUR
MC_Function_PDMH <- function(df,mean,sd){
  MH_Methane_Throughput_MC <- (df$Mean_CH4_m3day/rlnorm(nrow(df),mean,sd))*100
  return(MH_Methane_Throughput_MC)
}#Input the desired data frame along with the mean and standard deviation from the best fitted distribution for the EUR
#Amount of random numbers are generated based on the number of rows from the inputted data frame
Medicine_Sim <- replicate(10000,MC_Function_PDMH(NGProductionMedicine,Mean_LN_EUR,SD_LN_EUR))#The calculation is repeated 10,000 times using the replicate function
Medicine_Sim <- data.frame(Medicine_Sim)#Replicate function returns a matrix object, so the matrix object is converted into a data frame type structure
#Calculations from Simulation are applied to see the mean, median, standard deviation and the percentiles
#Sapply function is used to calculate the mean, median and quantiles of the whole dataframe
Mean_Medicine_Sim <- sapply(Medicine_Sim,mean)
Median_Medicine_Sim <- sapply(Medicine_Sim,median)
Q5_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.05))
Q25_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.25))
Q75_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.75))
Q95_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.95))
#Filter the values that exceeded 100% as emissions exceeding 100% does not exist in reality
FilteredMean_Medicine_Sim<-Mean_Medicine_Sim[Mean_Medicine_Sim <= 100]
FilteredQ75_Medicine_Sim <- Q75_Medicine_Sim[Q75_Medicine_Sim<=100]
FilteredQ95_Medicine_Sim <- Q95_Medicine_Sim[Q95_Medicine_Sim<=100]
#Find the minimum and maximum values of the mean, median and the percentiles
MinFilteredMeanMH<-min(FilteredMean_Medicine_Sim)
MaxFilteredMeanMH<-max(FilteredMean_Medicine_Sim)
MinFilteredMedianMH<-min(Median_Medicine_Sim)
MaxFilteredMedianMH<-max(Median_Medicine_Sim)
MinFilteredQ5MH<-min(Q5_Medicine_Sim)
MaxFilteredQ5MH<-max(Q5_Medicine_Sim)
MinFilteredQ25MH<-min(Q25_Medicine_Sim)
MaxFilteredQ25MH<-max(Q25_Medicine_Sim)
MinFilteredQ75MH<-min(FilteredQ75_Medicine_Sim)
MaxFilteredQ75MH<-max(FilteredQ75_Medicine_Sim)
MinFilteredQ95MH<-min(FilteredQ95_Medicine_Sim)
MaxFilteredQ95MH<-max(FilteredQ95_Medicine_Sim)
#---------------------------------------------
#Visualtion of the Monte Carlo graphs using the empricial cumulative distribution function with the base plot
#Plot the first ecdf from the simulation
#color coding: alpha is used to make the curves more transparent
plot(ecdf(Medicine_Sim[,1]),
     xlab="Methane Emissions (% of EUR)",
     ylab="Cumulative Distribution",
     main="Monte Carlo Simulation of Production Data (Medicine Hat)",
     cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     xlim=c(0,100),
     col=alpha("deepskyblue2",0.3))
grid(col="lightgray")
for(j in 2:1000){
  lines(ecdf(Medicine_Sim[,j]),
        col=alpha("deepskyblue2",0.3))
} #Loop the curves from column 2 to 1000 with the deep sky blue 2 color
for(h in 1001:2000){
  lines(ecdf(Medicine_Sim[,h]),
        col=alpha("cadetblue2",0.3))
} #Loop the curves from column 1001 to 2000 with the cadet blue 2 color
for(l in 2001:3000){
  lines(ecdf(Medicine_Sim[,l]),
        col=alpha("darkorchid2",0.3))
} #Loop the curves from column 2001 to 3000 with the dark orchid 2 color
for(p in 3001:4000){
  lines(ecdf(Medicine_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
} #Loop the curves from column 3001 to 4000 with the light sky blue 2 color
for(o in 4001:5000){
  lines(ecdf(Medicine_Sim[,o]),
        col=alpha("blueviolet",0.3))
} #Loop the curves from column 4001 to 5000 with the blue violet color
for(k in 5001:6000){
  lines(ecdf(Medicine_Sim[,k]),
        col=alpha("darkslateblue",0.3))
} #Loop the curves from column 5001 to 6000 with the deep slate blue color
for(s in 6001:7000){
  lines(ecdf(Medicine_Sim[,s]),
        col=alpha("darkmagenta",0.3))
} #Loop the curves from column 6001 to 7000 with the dark magenta color
for(u in 7001:8000){
  lines(ecdf(Medicine_Sim[,u]),
        col=alpha("deeppink1",0.3))
} #Loop the curves from column 7001 to 8000 with the deep pink 1 color
for(x in 8001:9000){
  lines(ecdf(Medicine_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
} #Loop the curves from column 8001 to 9000 with the corn flower blue color
for(f in 9001:10000){
  lines(ecdf(Medicine_Sim[,f]),
        col=alpha("darkorchid",0.3))
} #Loop the curves from column 9001 to 10000 with the dark orchid color
#--------------------------------------
#Visualising the Monte Carlo histograms
#Not an essential part of the project but can be used to view the heavy-tail
MC_Medicine_Histo <- ggplot(data=Medicine_Sim)
MC_Medicine_Histo + geom_histogram(aes(x=X990,y=stat(density)),
                                   fill="LightBlue",
                                   colour="Black",
                                   binwidth = 0.03,
                                   size=1)+
  ggtitle("Monte Carlo Simulations of Alberta Production Data (Medicine Hat) 2016")+
  xlab("Methane Emissions (% of Production)")+
  ylab("Density")+
  xlim(0,1)+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#----------------------------------------------------
#Economic Asessement
#Source: Alberta Economic Assessment
#Economically assess a variety of mitigation technologies from the ARPA-E's MONITOR program
#This assessment is assessed in a 45% reduced emissions scenario
#Considered technologies in this script are IBM and Aeris' technologies
#Not all technologies from the program were considered due to confidentiality purposes
#Results from this assessment include the mitigation cost of the technologies applied to the bottom up scripts' emission sources population
#Mitigation costs were also optimised to calculate the optimised cost of the technologies in a 45% reduction scenario
#Sensitivtiy analysis of the mitigation cost was conducted to model the cost fluctuations in 10 years time
#----------------------------------------------------
#Basic Economic Data for the calculations
AECO_C_NGPrice <- 1.48 #CAD/GJ #Price Year 2018, the AECO-C Natural Gas Price
#Mitigation cost of the technologies are dependent on the AECO_C Natural Gas Price and Natural Gas Energy Content
AECO_C_BaseNGPrice_Year2018_2028 <- c(1.48,1.75,2.19,2.55,2.62,2.76,3.01,3.15,3.35,3.44,3.52) #Prices of the AECO-C natural gas feom 2018-2028 for the sensitivty analysis 
Mean_AECO <- mean(AECO_C_BaseNGPrice_Year2018_2028) #Calculate the mean of the AECO_C natural gas price from 2018-2028
SD_AECO <- sd(AECO_C_BaseNGPrice_Year2018_2028) #Calculate the standard deviation of the AECO_C natural gas price from 2018-2028
Min_AECO <- min(AECO_C_BaseNGPrice_Year2018_2028) #Calculate the minimum AECO_C natural gas price from 2018-2028
Max_AECO <- max(AECO_C_BaseNGPrice_Year2018_2028) #Calculate the maximum of the AECO_C natural gas price from 2018-2028
NG_Energy_Content <- 53.20 #GJ/Tonne of NG
NGPrice <- AECO_C_NGPrice*NG_Energy_Content #Natural gas price for the year 2018
NGPrice2018_2028 <- AECO_C_BaseNGPrice_Year2018_2028*NG_Energy_Content #Vector of natural gas prices for the year between 2018-2028
LB_GWP100 <- 25 #Lower bound of the Global Warming Potential in 100 years timeframe
UB_GWP100 <- 36 #Upper bound of the Global Warming Potential in 100 years timeframe
#----------------------------------------------------
#Data Frame Preparation for Economic Datasets
library(tidyverse) #Import tidyverse packages to use the piping and dplyr packages
#Convert units of the emission rates into appropropriate units and create columns containing 45% of the original emission values
Economic_ClearStone <- ClearStone %>% 
  mutate(CH4_Leak_Rate_tonne_per_day_per_source=CH4_Leak_Rate_kg_per_day_per_source*1000) %>%
  mutate(CO2eq_Leak_Rate_tonne_per_day_per_source=CH4_Leak_Rate_tonne_per_day_per_source*LB_GWP100) %>% #Use the lower bound of the GWP100 as it is more commonly used
  mutate(CH4_Leak_Rate_tonne_per_year_per_source=CH4_Leak_Rate_tonne_per_day_per_source*365) %>% 
  mutate(CO2eq_Leak_Rate_tonne_per_year_per_source=CO2eq_Leak_Rate_tonne_per_day_per_source*365) %>% 
  mutate(Reduced_CH4_Leak_Rate_tonne_per_year_per_source=CH4_Leak_Rate_tonne_per_year_per_source *0.45) %>% #Reduce all the emission volumes into 45% of its original value
  mutate(Reduced_CH4_Leak_Rate_tonne_per_day_per_source=Reduced_CH4_Leak_Rate_tonne_per_year_per_source/365) %>%
  mutate(Reduced_CO2eq_Leak_Rate_tonne_per_year_per_source=Reduced_CH4_Leak_Rate_tonne_per_year_per_source*LB_GWP100) %>% 
  mutate(Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source=Reduced_CH4_Leak_Rate_tonne_per_year_per_source/365) 
#----------------------------------------------------
#Preparation of the total costs for the technologies
IBM_OPEX_Per_Year <- 1526 #USD Currency, OPEX for IBM's technology
AERIS_Cost_Per_Year <- 1000 #USD CUrrency, OPEX for Aeris's technology
CAD_FOREX <- function(USD){USD*1.31} #Currency Exchange was averaged for 5 years
CAD_IBM_OPEX_Per_Year <- ceiling(CAD_FOREX(IBM_OPEX_Per_Year))
CAD_IBM_OPEX_Per_Day <- ceiling(CAD_IBM_OPEX_Per_Year/365)
CAD_AERIS_Cost_Per_Year <- ceiling(CAD_FOREX(AERIS_Cost_Per_Year))
CAD_AERIS_Cost_Per_Day <- ceiling(CAD_AERIS_Cost_Per_Year/365)
Total_CO2eqEmission_tpd_ClearStone <- sum(Economic_ClearStone$CO2eq_Leak_Rate_tonne_per_day_per_source)
Total_CH4Emission_tpd_ClearStone <- sum(Economic_ClearStone$CH4_Leak_Rate_tonne_per_day_per_source)
Reduced_Total_CO2eqEmission_tpd_ClearStone <- 0.45*Total_CO2eqEmission_tpd_ClearStone
Reduced_Total_CH4Emission_tpd_ClearStone <- 0.45*Total_CH4Emission_tpd_ClearStone
#--------------------------------------------
#Reciprocating Compressor (Policy Reducation Scenario)
Economic_CSRC <- Economic_ClearStone %>%
  filter(Major_Equipment=="Reciprocating Compressor") 
Total_CO2eq_Emission_CSRC <- sum(Economic_CSRC$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSRC <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSRC)
TotalCost_IBM_Per_Year_CSRC <- TotalCost_IBM_Per_Day_CSRC*365
TotalCost_AERIS_Per_Day_CSRC <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSRC)
TotalCost_AERIS_Per_Year_CSRC <- TotalCost_AERIS_Per_Day_CSRC*365
Reduced_Total_CH4Emission_tpd_CSRC <- sum(Economic_CSRC$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSRC <- (TotalCost_IBM_Per_Day_CSRC - (Reduced_Total_CH4Emission_tpd_CSRC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSRC*UB_GWP100)
UB_IBM_MiCo_CSRC <- (TotalCost_IBM_Per_Day_CSRC - (Reduced_Total_CH4Emission_tpd_CSRC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSRC*LB_GWP100)
Avg_IBM_MiCo_CSRC <- mean(c(LB_IBM_MiCo_CSRC,UB_IBM_MiCo_CSRC))
LB_AERIS_MiCo_CSRC <- (TotalCost_AERIS_Per_Day_CSRC - (Reduced_Total_CH4Emission_tpd_CSRC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSRC*UB_GWP100)
UB_AERIS_MiCo_CSRC <- (TotalCost_AERIS_Per_Day_CSRC - (Reduced_Total_CH4Emission_tpd_CSRC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSRC*LB_GWP100)
Avg_AERIS_MiCo_CSRC <- mean(c(LB_AERIS_MiCo_CSRC,UB_AERIS_MiCo_CSRC))
#Optimisation Model For Reciprocating Compressor
MC_CERI_CSRC <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSRC$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSRC <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSRC
}
Optim_MC_CSRC<-optim(Avg_IBM_MiCo_CSRC,
                     MC_CERI_CSRC,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSRC$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSRC<-optim(Total_CO2eq_Emission_CSRC,
                     EM_CERI_CSRC,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSRC$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Separator (Policy Reducation Scenario)
Economic_CSSPRT <- Economic_ClearStone %>%
  filter(Major_Equipment=="Separator") 
Total_CO2eq_Emission_CSSPRT <- sum(Economic_CSSPRT$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSSPRT <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSSPRT)
TotalCost_IBM_Per_Year_CSSPRT <- TotalCost_IBM_Per_Day_CSSPRT*365
TotalCost_AERIS_Per_Day_CSSPRT <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSSPRT)
TotalCost_AERIS_Per_Year_CSSPRT <- TotalCost_AERIS_Per_Day_CSSPRT*365
Reduced_Total_CH4Emission_tpd_CSSPRT <- sum(Economic_CSSPRT$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSSPRT <- (TotalCost_IBM_Per_Day_CSSPRT - (Reduced_Total_CH4Emission_tpd_CSSPRT*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSPRT*UB_GWP100)
UB_IBM_MiCo_CSSPRT <- (TotalCost_IBM_Per_Day_CSSPRT - (Reduced_Total_CH4Emission_tpd_CSSPRT*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSPRT*LB_GWP100)
Avg_IBM_MiCo_CSSPRT <- mean(c(LB_IBM_MiCo_CSSPRT,UB_IBM_MiCo_CSSPRT))
LB_AERIS_MiCo_CSSPRT <- (TotalCost_AERIS_Per_Day_CSSPRT - (Reduced_Total_CH4Emission_tpd_CSSPRT*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSPRT*UB_GWP100)
UB_AERIS_MiCo_CSSPRT <- (TotalCost_AERIS_Per_Day_CSSPRT - (Reduced_Total_CH4Emission_tpd_CSSPRT*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSPRT*LB_GWP100)
Avg_AERIS_MiCo_CSSPRT <- mean(c(LB_AERIS_MiCo_CSSPRT,UB_AERIS_MiCo_CSSPRT))
#Optimisation Model For Separators
MC_CERI_CSSPRT <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSSPRT$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSSPRT <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSSPRT
}
Optim_MC_CSSPRT<-optim(Avg_IBM_MiCo_CSSPRT,
                       MC_CERI_CSSPRT,
                       method="Brent",
                       lower=0,
                       upper=max(Economic_CSSPRT$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSSPRT<-optim(Total_CO2eq_Emission_CSSPRT,
                       EM_CERI_CSSPRT,
                       method="Brent",
                       lower=0,
                       upper=max(Economic_CSSPRT$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Wellheads (Policy Reducation Scenario)
Economic_CSWH <- Economic_ClearStone %>%
  filter(Major_Equipment=="Wellhead") 
Total_CO2eq_Emission_CSWH <- sum(Economic_CSWH$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSWH <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSWH)
TotalCost_IBM_Per_Year_CSWH <- TotalCost_IBM_Per_Day_CSWH*365
TotalCost_AERIS_Per_Day_CSWH <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSWH)
TotalCost_AERIS_Per_Year_CSWH <- TotalCost_AERIS_Per_Day_CSWH*365
Reduced_Total_CH4Emission_tpd_CSWH <- sum(Economic_CSWH$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSWH <- (TotalCost_IBM_Per_Day_CSWH - (Reduced_Total_CH4Emission_tpd_CSWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSWH*UB_GWP100)
UB_IBM_MiCo_CSWH <- (TotalCost_IBM_Per_Day_CSWH - (Reduced_Total_CH4Emission_tpd_CSWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSWH*LB_GWP100)
Avg_IBM_MiCo_CSWH <- mean(c(LB_IBM_MiCo_CSWH,UB_IBM_MiCo_CSWH))
LB_AERIS_MiCo_CSWH <- (TotalCost_AERIS_Per_Day_CSWH - (Reduced_Total_CH4Emission_tpd_CSWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSWH*UB_GWP100)
UB_AERIS_MiCo_CSWH <- (TotalCost_AERIS_Per_Day_CSWH - (Reduced_Total_CH4Emission_tpd_CSWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSWH*LB_GWP100)
Avg_AERIS_MiCo_CSWH <- mean(c(LB_AERIS_MiCo_CSWH,UB_AERIS_MiCo_CSWH))
#Optimisation Model For Wellheads
MC_CERI_CSWH <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSWH$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSWH <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSWH
}
Optim_MC_CSWH<-optim(Avg_IBM_MiCo_CSWH,
                     MC_CERI_CSWH,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSWH$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSWH<-optim(Total_CO2eq_Emission_CSWH,
                     EM_CERI_CSWH,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSWH$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Dehydrator Glycol (Policy Reducation Scenario)
Economic_CSDG <- Economic_ClearStone %>%
  filter(Major_Equipment=="Dehydrator - Glycol")
Total_CO2eq_Emission_CSDG <- sum(Economic_CSDG$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSDG <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSDG)
TotalCost_IBM_Per_Year_CSDG <- TotalCost_IBM_Per_Day_CSDG*365
TotalCost_AERIS_Per_Day_CSDG <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSDG)
TotalCost_AERIS_Per_Year_CSDG <- TotalCost_AERIS_Per_Day_CSDG*365
Reduced_Total_CH4Emission_tpd_CSDG <- sum(Economic_CSDG$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSDG <- (TotalCost_IBM_Per_Day_CSDG - (Reduced_Total_CH4Emission_tpd_CSDG*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSDG*UB_GWP100)
UB_IBM_MiCo_CSDG <- (TotalCost_IBM_Per_Day_CSDG - (Reduced_Total_CH4Emission_tpd_CSDG*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSDG*LB_GWP100)
Avg_IBM_MiCo_CSDG <- mean(c(LB_IBM_MiCo_CSDG,UB_IBM_MiCo_CSDG))
LB_AERIS_MiCo_CSDG <- (TotalCost_AERIS_Per_Day_CSDG - (Reduced_Total_CH4Emission_tpd_CSDG*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSDG*UB_GWP100)
UB_AERIS_MiCo_CSDG <- (TotalCost_AERIS_Per_Day_CSDG - (Reduced_Total_CH4Emission_tpd_CSDG*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSDG*LB_GWP100)
Avg_AERIS_MiCo_CSDG <- mean(c(LB_AERIS_MiCo_CSDG,UB_AERIS_MiCo_CSDG))
#Optimisation Model For Dehydrator Glycol
MC_CERI_CSDG <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSDG$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSDG <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSDG
}
Optim_MC_CSDG<-optim(Avg_IBM_MiCo_CSDG,
                     MC_CERI_CSDG,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSDG$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSDG<-optim(Total_CO2eq_Emission_CSDG,
                     EM_CERI_CSDG,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSDG$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Flare Knockout Drum (Policy Reducation Scenario)
Economic_CSFKD <- Economic_ClearStone %>%
  filter(Major_Equipment=="Flare KnockOut Drum")
Total_CO2eq_Emission_CSFKD  <- sum(Economic_CSFKD$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSFKD <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSFKD)
TotalCost_IBM_Per_Year_CSFKD <- TotalCost_IBM_Per_Day_CSFKD*365
TotalCost_AERIS_Per_Day_CSFKD <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSFKD)
TotalCost_AERIS_Per_Year_CSFKD <- TotalCost_AERIS_Per_Day_CSFKD*365
Reduced_Total_CH4Emission_tpd_CSFKD <- sum(Economic_CSFKD$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSFKD <- (TotalCost_IBM_Per_Day_CSFKD - (Reduced_Total_CH4Emission_tpd_CSFKD*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSFKD*UB_GWP100)
UB_IBM_MiCo_CSFKD <- (TotalCost_IBM_Per_Day_CSFKD - (Reduced_Total_CH4Emission_tpd_CSFKD*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSFKD*LB_GWP100)
Avg_IBM_MiCo_CSFKD <- mean(c(LB_IBM_MiCo_CSFKD,UB_IBM_MiCo_CSFKD))
LB_AERIS_MiCo_CSFKD <- (TotalCost_AERIS_Per_Day_CSFKD - (Reduced_Total_CH4Emission_tpd_CSFKD*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSFKD*UB_GWP100)
UB_AERIS_MiCo_CSFKD <- (TotalCost_AERIS_Per_Day_CSFKD - (Reduced_Total_CH4Emission_tpd_CSFKD*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSFKD*LB_GWP100)
Avg_AERIS_MiCo_CSFKD <- mean(c(LB_AERIS_MiCo_CSFKD,UB_AERIS_MiCo_CSFKD))
#Optimisation Model For Flare Knockout Drum
MC_CERI_CSFKD <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSFKD$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSFKD <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSFKD
}
Optim_MC_CSFKD<-optim(Avg_IBM_MiCo_CSFKD,
                      MC_CERI_CSFKD,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_CSFKD$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSFKD<-optim(Total_CO2eq_Emission_CSFKD,
                      EM_CERI_CSFKD,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_CSFKD$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Gas Pipeline Header (Policy Reducation Scenario)
Economic_CSGPH <- Economic_ClearStone %>%
  filter(Major_Equipment=="Gas Pipeline Header") 
Total_CO2eq_Emission_CSGPH  <- sum(Economic_CSGPH$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSGPH <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSGPH)
TotalCost_IBM_Per_Year_CSGPH <- TotalCost_IBM_Per_Day_CSGPH*365
TotalCost_AERIS_Per_Day_CSGPH <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSGPH)
TotalCost_AERIS_Per_Year_CSGPH <- TotalCost_AERIS_Per_Day_CSGPH*365
Reduced_Total_CH4Emission_tpd_CSGPH <- sum(Economic_CSGPH$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSGPH <- (TotalCost_IBM_Per_Day_CSGPH - (Reduced_Total_CH4Emission_tpd_CSGPH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSGPH*UB_GWP100)
UB_IBM_MiCo_CSGPH <- (TotalCost_IBM_Per_Day_CSGPH - (Reduced_Total_CH4Emission_tpd_CSGPH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSGPH*LB_GWP100)
Avg_IBM_MiCo_CSGPH <- mean(c(LB_IBM_MiCo_CSGPH,UB_IBM_MiCo_CSGPH))
LB_AERIS_MiCo_CSGPH <- (TotalCost_AERIS_Per_Day_CSGPH - (Reduced_Total_CH4Emission_tpd_CSGPH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSGPH*UB_GWP100)
UB_AERIS_MiCo_CSGPH <- (TotalCost_AERIS_Per_Day_CSGPH - (Reduced_Total_CH4Emission_tpd_CSGPH*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSGPH*LB_GWP100)
Avg_AERIS_MiCo_CSGPH <- mean(c(LB_AERIS_MiCo_CSGPH,UB_AERIS_MiCo_CSGPH))
#Optimisation Model For Flare Knockout Drum
MC_CERI_CSGPH <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSGPH$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSGPH <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSGPH
}
Optim_MC_CSGPH<-optim(Avg_IBM_MiCo_CSGPH,
                      MC_CERI_CSGPH,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_CSGPH$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSGPH<-optim(Total_CO2eq_Emission_CSGPH,
                      EM_CERI_CSGPH,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_CSGPH$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Production Tank (fixed roof) (Policy Reducation Scenario)
Economic_CSPTFR <- Economic_ClearStone %>%
  filter(Major_Equipment=="Production Tank (fixed roof)")
Total_CO2eq_Emission_CSPTFR  <- sum(Economic_CSPTFR$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSPTFR <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSPTFR)
TotalCost_IBM_Per_Year_CSPTFR <- TotalCost_IBM_Per_Day_CSPTFR*365
TotalCost_AERIS_Per_Day_CSPTFR <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSPTFR)
TotalCost_AERIS_Per_Year_CSPTFR <- TotalCost_AERIS_Per_Day_CSPTFR*365
Reduced_Total_CH4Emission_tpd_CSPTFR <- sum(Economic_CSPTFR$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSPTFR <- (TotalCost_IBM_Per_Day_CSPTFR - (Reduced_Total_CH4Emission_tpd_CSPTFR*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSPTFR*UB_GWP100)
UB_IBM_MiCo_CSPTFR <- (TotalCost_IBM_Per_Day_CSPTFR - (Reduced_Total_CH4Emission_tpd_CSPTFR*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSPTFR*LB_GWP100)
Avg_IBM_MiCo_CSPTFR <- mean(c(LB_IBM_MiCo_CSPTFR,UB_IBM_MiCo_CSPTFR))
LB_AERIS_MiCo_CSPTFR <- (TotalCost_AERIS_Per_Day_CSPTFR - (Reduced_Total_CH4Emission_tpd_CSPTFR*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSPTFR*UB_GWP100)
UB_AERIS_MiCo_CSPTFR <- (TotalCost_AERIS_Per_Day_CSPTFR - (Reduced_Total_CH4Emission_tpd_CSPTFR*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSPTFR*LB_GWP100)
Avg_AERIS_MiCo_CSPTFR <- mean(c(LB_AERIS_MiCo_CSPTFR,UB_AERIS_MiCo_CSPTFR))
#Optimisation Model For Flare Knockout Drum
MC_CERI_CSPTFR <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSPTFR$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSPTFR <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSPTFR
}
Optim_MC_CSPTFR<-optim(Avg_IBM_MiCo_CSPTFR,
                       MC_CERI_CSPTFR,
                       method="Brent",
                       lower=0,
                       upper=max(Economic_CSPTFR$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSPTFR<-optim(Total_CO2eq_Emission_CSPTFR,
                       EM_CERI_CSPTFR,
                       method="Brent",
                       lower=0,
                       upper=max(Economic_CSPTFR$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Screw Compressor (Policy Reducation Scenario)
Economic_CSSC <- Economic_ClearStone %>%
  filter(Major_Equipment=="Screw Compressor")
Total_CO2eq_Emission_CSSC  <- sum(Economic_CSSC$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSSC <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSSC)
TotalCost_IBM_Per_Year_CSSC <- TotalCost_IBM_Per_Day_CSSC*365
TotalCost_AERIS_Per_Day_CSSC <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSSC)
TotalCost_AERIS_Per_Year_CSSC <- TotalCost_AERIS_Per_Day_CSSC*365
Reduced_Total_CH4Emission_tpd_CSSC <- sum(Economic_CSSC$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSSC <- (TotalCost_IBM_Per_Day_CSSC - (Reduced_Total_CH4Emission_tpd_CSSC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSC*UB_GWP100)
UB_IBM_MiCo_CSSC <- (TotalCost_IBM_Per_Day_CSSC - (Reduced_Total_CH4Emission_tpd_CSSC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSC*LB_GWP100)
Avg_IBM_MiCo_CSSC <- mean(c(LB_IBM_MiCo_CSSC,UB_IBM_MiCo_CSSC))
LB_AERIS_MiCo_CSSC <- (TotalCost_AERIS_Per_Day_CSSC - (Reduced_Total_CH4Emission_tpd_CSSC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSC*UB_GWP100)
UB_AERIS_MiCo_CSSC <- (TotalCost_AERIS_Per_Day_CSSC - (Reduced_Total_CH4Emission_tpd_CSSC*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSSC*LB_GWP100)
Avg_AERIS_MiCo_CSSC <- mean(c(LB_AERIS_MiCo_CSSC,UB_AERIS_MiCo_CSSC))
#Optimisation Model For Flare Knockout Drum
MC_CERI_CSSC <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSSC$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSSC <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSSC
}
Optim_MC_CSSC<-optim(Avg_IBM_MiCo_CSSC,
                     MC_CERI_CSSC,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSSC$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSSC<-optim(Total_CO2eq_Emission_CSSC,
                     EM_CERI_CSSC,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSSC$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#-------------------------------------------------------------------
#Electric Driver Compressor (Policy Reducation Scenario)
Economic_CSED <- Economic_ClearStone %>%
  filter(Major_Equipment=="Screw Compressor - Electric Driver" | Major_Equipment=="Reciprocating Compressor - Electric Driver") 
Total_CO2eq_Emission_CSED  <- sum(Economic_CSED$CO2eq_Leak_Rate_tonne_per_day_per_source)
TotalCost_IBM_Per_Day_CSED <- CAD_IBM_OPEX_Per_Day * nrow(Economic_CSED)
TotalCost_IBM_Per_Year_CSED <- TotalCost_IBM_Per_Day_CSED*365
TotalCost_AERIS_Per_Day_CSED <- CAD_AERIS_Cost_Per_Day * nrow(Economic_CSED)
TotalCost_AERIS_Per_Year_CSED <- TotalCost_AERIS_Per_Day_CSED*365
Reduced_Total_CH4Emission_tpd_CSED <- sum(Economic_CSED$Reduced_CH4_Leak_Rate_tonne_per_day_per_source)
LB_IBM_MiCo_CSED <- (TotalCost_IBM_Per_Day_CSED - (Reduced_Total_CH4Emission_tpd_CSED*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSED*UB_GWP100)
UB_IBM_MiCo_CSED <- (TotalCost_IBM_Per_Day_CSED - (Reduced_Total_CH4Emission_tpd_CSED*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSED*LB_GWP100)
Avg_IBM_MiCo_CSED <- mean(c(LB_IBM_MiCo_CSED,UB_IBM_MiCo_CSED))
LB_AERIS_MiCo_CSED <- (TotalCost_AERIS_Per_Day_CSED - (Reduced_Total_CH4Emission_tpd_CSED*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSED*UB_GWP100)
UB_AERIS_MiCo_CSED <- (TotalCost_AERIS_Per_Day_CSED - (Reduced_Total_CH4Emission_tpd_CSED*NGPrice))/(Reduced_Total_CH4Emission_tpd_CSED*LB_GWP100)
Avg_AERIS_MiCo_CSED <- mean(c(LB_AERIS_MiCo_CSED,UB_AERIS_MiCo_CSED))
#Optimisation Model For Flare Knockout Drum
MC_CERI_CSED <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_CSED$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source)*mc
}
EM_CERI_CSED <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_CSED
}
Optim_MC_CSED<-optim(Avg_IBM_MiCo_CSED,
                     MC_CERI_CSED,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSED$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
Optim_EM_CSED<-optim(Total_CO2eq_Emission_CSED,
                     EM_CERI_CSED,
                     method="Brent",
                     lower=0,
                     upper=max(Economic_CSED$Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source))
#----------------------------------------------------------------------
#GreenPath Dataset
CH4_Density <- 0.6709793
Economic_Fug_GreenPath <- Fug_GreenPath %>% 
  mutate(Emission_Rate_kgd=Emission_Rate_cmd*CH4_Density) %>%
  mutate(Emission_Rate_tonne_per_day=Emission_Rate_kgd*1000) %>% 
  mutate(Emission_Rate_tonne_per_year=Emission_Rate_tonne_per_day*365) %>% 
  mutate(CO2eq_Emission_Rate_tonne_per_day=Emission_Rate_tonne_per_day*25) %>% 
  mutate(CO2eq_Emission_Rate_tonne_per_year=Emission_Rate_tonne_per_year*25) %>% 
  mutate(Reduced_Emission_Rate_tonne_per_year=Emission_Rate_tonne_per_year *0.45) %>% 
  mutate(Reduced_Emission_Rate_tonne_per_day=Reduced_Emission_Rate_tonne_per_year/365) %>% 
  mutate(Reduced_CO2eq_Emission_Rate_tonne_per_year=CO2eq_Emission_Rate_tonne_per_year*25) %>% 
  mutate(Reduced_CO2eq_Emission_Rate_tonne_per_day=Reduced_CO2eq_Emission_Rate_tonne_per_year/365)
Total_CO2eqEmission_tpd_GreenPath <- sum(Economic_Fug_GreenPath$CO2eq_Emission_Rate_tonne_per_day)
Total_CH4Emission_tpd_GreenPath <- sum(Economic_Fug_GreenPath$Emission_Rate_tonne_per_day)
Reduced_Total_CO2eqEmission_tpd_GreenPath <- 0.45*Total_CO2eqEmission_tpd_GreenPath
Reduced_Total_CH4Emission_tpd_GreenPath <- 0.45*Total_CH4Emission_tpd_GreenPath
#-------------------------------------------------------------------
#Wellheads (GreenPath) (Policy Reducation Scenario)
Economic_FGPWH <- Economic_Fug_GreenPath %>%
  filter(Process_Block=="Wellhead") 
Total_CO2eq_Emission_FGPWH  <- sum(Economic_FGPWH$CO2eq_Emission_Rate_tonne_per_day)
TotalCost_IBM_Per_Day_FGPWH <- CAD_IBM_OPEX_Per_Day * nrow(Economic_FGPWH)
TotalCost_IBM_Per_Year_FGPWH <- TotalCost_IBM_Per_Day_FGPWH*365
TotalCost_AERIS_Per_Day_FGPWH <- CAD_AERIS_Cost_Per_Day * nrow(Economic_FGPWH)
TotalCost_AERIS_Per_Year_FGPWH <- TotalCost_AERIS_Per_Day_FGPWH*365
Reduced_Total_CH4Emission_tpd_FGPWH <- sum(Economic_FGPWH$Reduced_Emission_Rate_tonne_per_day)
LB_IBM_MiCo_FGPWH <- (TotalCost_IBM_Per_Day_FGPWH - (Reduced_Total_CH4Emission_tpd_FGPWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPWH*UB_GWP100)
UB_IBM_MiCo_FGPWH <- (TotalCost_IBM_Per_Day_FGPWH - (Reduced_Total_CH4Emission_tpd_FGPWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPWH*LB_GWP100)
Avg_IBM_MiCo_FGPWH <- mean(c(LB_IBM_MiCo_FGPWH,UB_IBM_MiCo_FGPWH))
LB_AERIS_MiCo_FGPWH <- (TotalCost_AERIS_Per_Day_FGPWH - (Reduced_Total_CH4Emission_tpd_FGPWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPWH*UB_GWP100)
UB_AERIS_MiCo_FGPWH <- (TotalCost_AERIS_Per_Day_FGPWH - (Reduced_Total_CH4Emission_tpd_FGPWH*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPWH*LB_GWP100)
Avg_AERIS_MiCo_FGPWH <- mean(c(LB_AERIS_MiCo_FGPWH,UB_AERIS_MiCo_FGPWH))
#Optimisation Model For Wellheads (GreenPath)
MC_CERI_FGPWH <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_FGPWH$CO2eq_Emission_Rate_tonne_per_day)*mc
}
EM_CERI_FGPWH <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_FGPWH
}
Optim_MC_FGPWH<-optim(Avg_IBM_MiCo_FGPWH,
                      MC_CERI_FGPWH,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_FGPWH$Reduced_CO2eq_Emission_Rate_tonne_per_day))
Optim_EM_FGPWH<-optim(Total_CO2eq_Emission_FGPWH,
                      EM_CERI_FGPWH,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_FGPWH$Reduced_CO2eq_Emission_Rate_tonne_per_day))
#-------------------------------------------------------------------
#Filter/Separation (GreenPath) (Policy Reducation Scenario)
Economic_FGPFS <- Economic_Fug_GreenPath %>%
  filter(Process_Block=="Filter/Separation")
Total_CO2eq_Emission_FGPFS  <- sum(Economic_FGPFS$CO2eq_Emission_Rate_tonne_per_day)
TotalCost_IBM_Per_Day_FGPFS <- CAD_IBM_OPEX_Per_Day * nrow(Economic_FGPFS)
TotalCost_IBM_Per_Year_FGPFS <- TotalCost_IBM_Per_Day_FGPFS*365
TotalCost_AERIS_Per_Day_FGPFS <- CAD_AERIS_Cost_Per_Day * nrow(Economic_FGPFS)
TotalCost_AERIS_Per_Year_FGPFS <- TotalCost_AERIS_Per_Day_FGPFS*365
Reduced_Total_CH4Emission_tpd_FGPFS <- sum(Economic_FGPFS$Reduced_Emission_Rate_tonne_per_day)
LB_IBM_MiCo_FGPFS <- (TotalCost_IBM_Per_Day_FGPFS - (Reduced_Total_CH4Emission_tpd_FGPFS*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPFS*UB_GWP100)
UB_IBM_MiCo_FGPFS <- (TotalCost_IBM_Per_Day_FGPFS - (Reduced_Total_CH4Emission_tpd_FGPFS*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPFS*LB_GWP100)
Avg_IBM_MiCo_FGPFS <- mean(c(LB_IBM_MiCo_FGPFS,UB_IBM_MiCo_FGPFS))
LB_AERIS_MiCo_FGPFS <- (TotalCost_AERIS_Per_Day_FGPFS - (Reduced_Total_CH4Emission_tpd_FGPFS*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPFS*UB_GWP100)
UB_AERIS_MiCo_FGPFS <- (TotalCost_AERIS_Per_Day_FGPFS - (Reduced_Total_CH4Emission_tpd_FGPFS*NGPrice))/(Reduced_Total_CH4Emission_tpd_FGPFS*LB_GWP100)
Avg_AERIS_MiCo_FGPFS <- mean(c(LB_AERIS_MiCo_FGPFS,UB_AERIS_MiCo_FGPFS))
#Optimisation Model For Filter/Separation (GreenPath)
MC_CERI_FGPFS <- function(mitigatingcost){
  mc <- mitigatingcost[1]
  sum(Economic_FGPFS$CO2eq_Emission_Rate_tonne_per_day)*mc
}
EM_CERI_FGPFS <- function(emission){
  em <- emission[1]
  0.45*em*Avg_IBM_MiCo_FGPFS
}
Optim_MC_FGPFS<-optim(Avg_IBM_MiCo_FGPFS,
                      MC_CERI_FGPFS,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_FGPFS$Reduced_CO2eq_Emission_Rate_tonne_per_day))
Optim_EM_FGPFS<-optim(Total_CO2eq_Emission_FGPFS,
                      EM_CERI_FGPFS,
                      method="Brent",
                      lower=0,
                      upper=max(Economic_FGPFS$Reduced_CO2eq_Emission_Rate_tonne_per_day))
#--------------------------------------------------
#Sensitivity Analysis For Natural Gas Price
library(sensitivity)
library(ggplot2)
library(ggthemes)
set.seed(123)
NGPrice_Sens <- data.frame(rnorm(10000,Mean_AECO,SD_AECO))
colnames(NGPrice_Sens) <- c("Natural_Gas_Price")
NGPrice_Sens_Histo <- ggplot(data=NGPrice_Sens,aes(x=Natural_Gas_Price))
NGPrice_Sens_Histo + 
  geom_histogram(aes(y=stat(density)),
                 color="black",
                 fill="dodgerblue3",
                 binwidth=0.05)+
  theme_set(theme_bw())+
  xlab("Natural Gas ($CAD/GJ)")+
  ylab("Density")+
  ggtitle("Uncertainty Analysis of AECO-C Natural Gas Price")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#Sensitivity Analysis for Mitigation Cost 
set.seed(123)
Min_Value_AECO <- min(NGPrice_Sens)
Max_Value_AECO <- max(NGPrice_Sens)
LB_NGPrice <- Min_Value_AECO*NG_Energy_Content
UB_NGPrice <- Max_Value_AECO*NG_Energy_Content
CS_IBM_Total_Cost <- CAD_IBM_OPEX_Per_Day * nrow(Economic_ClearStone)
CS_AERIS_Total_Cost <- CAD_AERIS_Cost_Per_Day * nrow(Economic_ClearStone)
LB_Sapply_IBMMC_Cost <- function(Price){
  (CS_IBM_Total_Cost - (Reduced_Total_CH4Emission_tpd_ClearStone*Price))/(Reduced_Total_CH4Emission_tpd_ClearStone*UB_GWP100)
}
UB_Sapply_IBMMC_Cost <- function(Price){
  (CS_IBM_Total_Cost - (Reduced_Total_CH4Emission_tpd_ClearStone*Price))/(Reduced_Total_CH4Emission_tpd_ClearStone*LB_GWP100)
}
LB_Sapply_AERISMC_Cost <- function(Price){
  (CS_AERIS_Total_Cost - (Reduced_Total_CH4Emission_tpd_ClearStone*Price))/(Reduced_Total_CH4Emission_tpd_ClearStone*UB_GWP100)
}
UB_Sapply_AERISMC_Cost <- function(Price){
  (CS_AERIS_Total_Cost - (Reduced_Total_CH4Emission_tpd_ClearStone*Price))/(Reduced_Total_CH4Emission_tpd_ClearStone*LB_GWP100)
}
LB_CS_IBM_MC_2019_2028 <- sapply(NGPrice2018_2028,LB_Sapply_IBMMC_Cost)
UB_CS_IBM_MC_2019_2028 <- sapply(NGPrice2018_2028,UB_Sapply_IBMMC_Cost)
LB_CS_AERIS_MC_2019_2028 <- sapply(NGPrice2018_2028,LB_Sapply_AERISMC_Cost)
UB_CS_AERIS_MC_2019_2028 <- sapply(NGPrice2018_2028,UB_Sapply_AERISMC_Cost)
Mean_IBM_MC_2019_2028 <- mean(c(LB_CS_IBM_MC_2019_2028,UB_CS_IBM_MC_2019_2028))
SD_IBM_MC_2019_2028 <- sd(c(LB_CS_IBM_MC_2019_2028,UB_CS_IBM_MC_2019_2028))
Mean_AERIS_MC_2019_2028 <- mean(c(LB_CS_AERIS_MC_2019_2028,UB_CS_AERIS_MC_2019_2028))
SD_AERIS_MC_2019_2028 <- sd(c(LB_CS_AERIS_MC_2019_2028,UB_CS_AERIS_MC_2019_2028))
IBM_MC_Sens <- data.frame(rnorm(10000,Mean_IBM_MC_2019_2028,SD_IBM_MC_2019_2028))
AERIS_MC_Sens <- data.frame(rnorm(10000,Mean_AERIS_MC_2019_2028,SD_AERIS_MC_2019_2028))
colnames(IBM_MC_Sens) <- c("IBM_Mitigating_Cost")
colnames(AERIS_MC_Sens) <- c("AERIS_Mitigating_Cost")
IBM_MC_Sens_Histo <- ggplot(data=IBM_MC_Sens,aes(x=IBM_Mitigating_Cost))
IBM_MC_Sens_Histo + 
  geom_histogram(aes(y=stat(density)),
                 color="black",
                 fill="dodgerblue3",
                 binwidth=0.1)+
  theme_set(theme_bw())+
  xlab("IBM Mitigating Cost ($CAD/tonne of CO2eq))")+
  ylab("Density")+
  ggtitle("Uncertainty Analysis of IBM Mitigation Cost")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
AERIS_MC_Sens_Histo <- ggplot(data=AERIS_MC_Sens,aes(x=AERIS_Mitigating_Cost))
AERIS_MC_Sens_Histo + 
  geom_histogram(aes(y=stat(density)),
                 color="black",
                 fill="dodgerblue3",
                 binwidth=0.1)+
  theme_set(theme_bw())+
  xlab("AERIS Mitigating Cost ($CAD/tonne of CO2eq))")+
  ylab("Density")+
  ggtitle("Uncertainty Analysis of AERIS Mitigation Cost")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))