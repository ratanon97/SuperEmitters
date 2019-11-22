#--------------------------------------------
#Set working directory for Government Production Data
getwd()
setwd("C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files")
NGPreProduction <-read.csv("New Alberta Pre-Production Data.csv")
#----------------------------------------------
#Basic Data Frame Adjustments
colnames(NGPreProduction)<-c("Well_Fluid_Type",
                             "PSAC_Area",
                             "Drill_Type",
                             "First_Date",
                             "Act_On_Production",
                             "On_Production_Years",
                             "Activity_Years",
                             "Production_Mm3_per_year",
                             "Well_Count",
                             "Well_Drain_Count")
NGPreProduction$PSAC_Area <- factor(NGPreProduction$PSAC_Area)
NGPreProduction$First_Date <- factor(NGPreProduction$First_Date)
NGPreProduction$Act_On_Production <- factor(NGPreProduction$Act_On_Production)
NGPreProduction$On_Production_Years <- factor(NGPreProduction$On_Production_Years)
NGPreProduction$Activity_Years <- factor(NGPreProduction$Activity_Years)
NGPreProduction$Production_Mm3_per_day <- NGPreProduction$Production_Mm3_per_year/365
NGPreProduction$Production_Mm3_per_second <- NGPreProduction$Production_Mm3_per_day/86400
#2013-2017 Filter. 2018 isn't included due to not being the most updated one
Year_Filter <- (NGPreProduction$Activity_Years == "2013") | 
               (NGPreProduction$Activity_Years == "2014") | 
               (NGPreProduction$Activity_Years == "2015") | 
               (NGPreProduction$Activity_Years == "2016") | 
               (NGPreProduction$Activity_Years == "2017") 
#Assigning the filter to a new data frame
NGPreProductionFiltered <- NGPreProduction[Year_Filter,]
#Using the apply functions to calculate mean and median for all years under specific categories
#List of factors to be inputted as an index in the tapply/aggregate.data.frame function
AYWFT <- list(Activity_Years=NGPreProductionFiltered$Activity_Years,
              Well_Fluid_Type=NGPreProductionFiltered$Well_Fluid_Type,
              Drill_Type=NGPreProductionFiltered$Drill_Type) 
WFT <- list(Well_Fluid_Type=NGPreProductionFiltered$Well_Fluid_Type,
            Drill_Type=NGPreProductionFiltered$Drill_Type)
EUR_list <- list(Well_Fluid_Type=On_Prod$Well_Fluid_Type,
                      Drill_Type=On_Prod$Drill_Type) 
#Calculate the mean EUR in cmd for the filtered years
mean_production_Mm3d<-aggregate.data.frame(NGPreProductionFiltered$Production_Mm3_per_day,
                                  AYWFT,
                                  mean,na.rm=TRUE) #Returns a dataframe
#Calculate the median EUR in cmd for the filtered years
median_production_Mm3d<-aggregate.data.frame(NGPreProductionFiltered$Production_Mm3_per_day,
                                     AYWFT,
                                     median,na.rm=TRUE) #Returns a dataframe
sd_production_Mm3d <- aggregate.data.frame(NGPreProductionFiltered$Production_Mm3_per_day,
                                    AYWFT,
                                    sd,na.rm=TRUE)
max_production_Mm3d <- aggregate.data.frame(NGPreProductionFiltered$Production_Mm3_per_day,
                                   AYWFT,
                                   max,na.rm=TRUE)
min_production_Mm3d <- aggregate.data.frame(NGPreProductionFiltered$Production_Mm3_per_day,
                                    AYWFT,
                                    min,na.rm=TRUE)
#Calculate the mean of the EUR for all the years and not by in each year
#This is used for the Monte Carlo testing in the T&D Bottom Up
mean_production_Mm3d_all_years <- aggregate.data.frame(NGPreProductionFiltered$Production_Mm3_per_day,
                                     WFT,
                                     mean,na.rm=TRUE)
sd_production_Mm3d_all_years <- aggregate.data.frame(NGPreProductionFiltered$Production_Mm3_per_day,
                                               WFT,
                                               sd,na.rm=TRUE)
#Changing the names of the column
colnames(mean_production_Mm3d)[4] <- c("Mean_Production_Mm3_per_day")
colnames(mean_production_Mm3d_all_years)[3] <- c("Mean_Production_Mm3_per_day")
colnames(median_production_Mm3d)[4] <- c("Median_Production_Mm3_per_day")
colnames(sd_production_Mm3d)[4] <- c("Standard_Deviation_Production_Mm3_per_day")
colnames(sd_production_Mm3d_all_years)[3] <- c("Standard_Deviation_Production_Mm3_per_day")
colnames(max_production_Mm3d)[4] <- c("Max_Production_Mm3_per_day")
colnames(min_production_Mm3d)[4] <- c("Min_Production_Mm3_per_day")
#----------------------------------------
#DPLYR LIBRARY DATAFRAME ADJUSTMENTS
library(dplyr)
#Filter the Vertical/Deviated and Gas Type but with the DYPLR library
#Median Version
Production_Gas_Vertical_Median <- median_production_Mm3d %>% 
  filter(Activity_Years == "2013" | 
           Activity_Years == "2014" | 
           Activity_Years == "2015" | 
           Activity_Years == "2016" | 
           Activity_Years == "2017" ) %>% 
  filter(Well_Fluid_Type == "Gas" & Drill_Type=="Vertical/Deviated")
#Filter based on individual activity years
#This is used for the IF ELSE condition in the Alberta T&D Bottom Up Script
Production_Gas_Vertical_Median_2017 <- Production_Gas_Vertical_Median %>% 
  filter(Activity_Years=="2017")
Production_Gas_Vertical_Median_2016 <- Production_Gas_Vertical_Median %>% 
  filter(Activity_Years=="2016")
Production_Gas_Vertical_Median_2015 <- Production_Gas_Vertical_Median %>% 
  filter(Activity_Years=="2015")
Production_Gas_Vertical_Median_2014 <- Production_Gas_Vertical_Median %>% 
  filter(Activity_Years=="2014")
Production_Gas_Vertical_Median_2013 <- Production_Gas_Vertical_Median %>% 
  filter(Activity_Years=="2013")
#All of these are still data frames and not vectors or matrix
#-------------------------------------------------------------------
#Mean Version
#This is used for the IF ELSE condition in the Alberta T&D Bottom Up Script
Production_Gas_Vertical_Mean <- mean_production_Mm3d %>% 
  filter(Activity_Years == "2013" | 
           Activity_Years == "2014" | 
           Activity_Years == "2015" | 
           Activity_Years == "2016" | 
           Activity_Years == "2017" ) %>% 
  filter(Well_Fluid_Type == "Gas" & Drill_Type=="Vertical/Deviated")
#Filter by Individual Years
#Possible use in the rnorm distribution function
Production_Gas_Vertical_Mean_2017 <- Production_Gas_Vertical_Mean %>% 
  filter(Activity_Years=="2017")
Production_Gas_Vertical_Mean_2016 <- Production_Gas_Vertical_Mean %>% 
  filter(Activity_Years=="2016")
Production_Gas_Vertical_Mean_2015 <- Production_Gas_Vertical_Mean %>% 
  filter(Activity_Years=="2015")
Production_Gas_Vertical_Mean_2014 <- Production_Gas_Vertical_Mean %>% 
  filter(Activity_Years=="2014")
Production_Gas_Vertical_Mean_2013 <- Production_Gas_Vertical_Mean %>% 
  filter(Activity_Years=="2013")
#Mean for TD Data in Monte Carlo for all years
mean_production_Mm3d_all_years_Gas_Vertical <- mean_production_Mm3d_all_years %>% 
  filter(Well_Fluid_Type=="Gas" & Drill_Type=="Vertical/Deviated")
#---------------------------------------------------------------
#Standard Deviation Version
Production_Gas_Vertical_SD <- sd_production_Mm3d %>% 
  filter(Activity_Years == "2013" | 
           Activity_Years == "2014" | 
           Activity_Years == "2015" | 
           Activity_Years == "2016" | 
           Activity_Years == "2017" ) %>% 
  filter(Well_Fluid_Type == "Gas" & Drill_Type=="Vertical/Deviated")
#Filter based on individual activity years
#Possibly will be used in rnorm distribituion functions
Production_Gas_Vertical_SD_2017 <- Production_Gas_Vertical_SD %>% 
  filter(Activity_Years=="2017")
Production_Gas_Vertical_SD_2016 <- Production_Gas_Vertical_SD %>% 
  filter(Activity_Years=="2016")
Production_Gas_Vertical_SD_2015 <- Production_Gas_Vertical_SD %>% 
  filter(Activity_Years=="2015")
Production_Gas_Vertical_SD_2014 <- Production_Gas_Vertical_SD %>% 
  filter(Activity_Years=="2014")
Production_Gas_Vertical_SD_2013 <- Production_Gas_Vertical_SD %>% 
  filter(Activity_Years=="2013")
#SD for TD Data in Monte Carlo for all years
sd_production_Mm3d_all_years_Gas_Vertical <- sd_production_Mm3d_all_years %>% 
  filter(Well_Fluid_Type=="Gas" & Drill_Type=="Vertical/Deviated")
#-------------------------------------------------------
#Max Version
Production_Gas_Vertical_Max <- max_production_Mm3d %>% 
  filter(Activity_Years == "2013" | 
           Activity_Years == "2014" | 
           Activity_Years == "2015" | 
           Activity_Years == "2016" | 
           Activity_Years == "2017" ) %>% 
  filter(Well_Fluid_Type == "Gas" & Drill_Type=="Vertical/Deviated")
#Filter based on individual activity years
#Possibly to be used in the runif distribution function
Production_Gas_Vertical_Max_2017 <- Production_Gas_Vertical_Max %>% 
  filter(Activity_Years=="2017")
Production_Gas_Vertical_Max_2016 <- Production_Gas_Vertical_Max %>% 
  filter(Activity_Years=="2016")
Production_Gas_Vertical_Max_2015 <- Production_Gas_Vertical_Max %>% 
  filter(Activity_Years=="2015")
Production_Gas_Vertical_Max_2014 <- Production_Gas_Vertical_Max %>% 
  filter(Activity_Years=="2014")
Production_Gas_Vertical_Max_2013 <- Production_Gas_Vertical_Max %>% 
  filter(Activity_Years=="2013")
#------------------------------------------------------------
#Min Version
Production_Gas_Vertical_Min <- min_production_Mm3d %>% 
  filter(Activity_Years == "2013" | 
           Activity_Years == "2014" | 
           Activity_Years == "2015" | 
           Activity_Years == "2016" | 
           Activity_Years == "2017" ) %>% 
  filter(Well_Fluid_Type == "Gas" & Drill_Type=="Vertical/Deviated")
#Filter based on individual activity years
#Possibly to be used in the runif distribution function
Production_Gas_Vertical_Min_2017 <- Production_Gas_Vertical_Min %>% 
  filter(Activity_Years=="2017")
Production_Gas_Vertical_Min_2016 <- Production_Gas_Vertical_Min %>% 
  filter(Activity_Years=="2016")
Production_Gas_Vertical_Min_2015 <- Production_Gas_Vertical_Min %>% 
  filter(Activity_Years=="2015")
Production_Gas_Vertical_Min_2014 <- Production_Gas_Vertical_Min %>% 
  filter(Activity_Years=="2014")
Production_Gas_Vertical_Min_2013 <- Production_Gas_Vertical_Min %>% 
  filter(Activity_Years=="2013")
#----------------------------------------------------------------------------
#Apply the Median/Mean values for the Year 2016
#used for the Medicine Hat and GreenPath scripts
MGMedian<-Production_Gas_Vertical_Median_2016[,"Median_Production_Mm3_per_day"]
MGMean<-Production_Gas_Vertical_Mean_2016[,"Mean_Production_Mm3_per_day"]
MGSD <- Production_Gas_Vertical_SD_2016[,"Standard_Deviation_Production_Mm3_per_day"]
#2017 Data. Used for Fugitive 
FugMean <- Production_Gas_Vertical_Mean_2017[,"Mean_Production_Mm3_per_day"]
FugMedian <- Production_Gas_Vertical_Median_2017[,"Median_Production_Mm3_per_day"]
FugSD <- Production_Gas_Vertical_SD_2017[,"Standard_Deviation_Production_Mm3_per_day"]
FugMax <- Production_Gas_Vertical_Max_2017[,"Max_Production_Mm3_per_day"]
FugMin <- Production_Gas_Vertical_Min_2017[,"Min_Production_Mm3_per_day"]
#TD Data for the Monte Carlo Simulation
TDMean <- mean_production_Mm3d_all_years_Gas_Vertical[,"Mean_Production_Mm3_per_day"]
TDSD <- sd_production_Mm3d_all_years_Gas_Vertical[,"Standard_Deviation_Production_Mm3_per_day"]
#--------------------------------------------------------------                                  
#Calculation Check:
median_production_gv_cmd <- round(median(NGPreProductionFiltered[NGPreProductionFiltered$Well_Fluid_Type=="Gas" & 
                                                          NGPreProductionFiltered$Drill_Type=="Vertical/Deviated" &
                                                          NGPreProductionFiltered$Activity_Years=="2016",
                                                          "Production_Mm3_per_day"],
                                  na.rm = TRUE),
                           3)
mean_production_gv_cmd <- round(mean(NGPreProductionFiltered[NGPreProductionFiltered$Well_Fluid_Type=="Gas" & 
                                                NGPreProductionFiltered$Drill_Type=="Vertical/Deviated" &
                                                NGPreProductionFiltered$Activity_Years=="2016",
                                                "Production_Mm3_per_day"],
                              na.rm = TRUE),
                         3)
mean_production_gv_cmd
#---------------------------------------------------------
#Production Visualisation
#Year Filter Version
library(ggplot2)
Production_Filtered_Histo<- ggplot(data=NGPreProductionFiltered,
                          aes(x=Production_Mm3_per_day))
Production_Filtered_Histo + 
  geom_histogram(aes(y=stat(density),
                     fill=Well_Fluid_Type),
                 colour="Black",
                 binwidth = 0.4)+
  scale_x_continuous(trans="log")+
  #facet_grid(Well_Fluid_Type~Activity_Years,scales="free",space="free")+ #To create facets on Natural Gas Distribution and Pipeline Transportation
  xlab("Production in Mm3/day")+
  ylab("Density")+
  labs(fill="Well Fluid Type")+
  ggtitle("Alberta Production Data 2013-2017")+ #Making the graph look niceer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=c(0,1),
        legend.justification=c(0,1),
        legend.key = element_rect(colour = "transparent",fill="white"),
        plot.title=element_text(size=20),
        strip.text=element_text(size=15))
#---------------------------------------------------------
#Probability Distribution Fitting
library(fitdistrplus)
library(actuar) #Import Log-Logistic Distribution
fwProduction <- fitdist(NGPreProductionFiltered$Production_Mm3_per_day,"weibull") #Weibull Distribution
flnProduction <- fitdist(NGPreProductionFiltered$Production_Mm3_per_day,"lnorm")# Log-Normal Distribution
fllProduction <- fitdist(NGPreProductionFiltered$Production_Mm3_per_day,"llogis") #Log-Logistic Distribution 

ProbDistProduction <- denscomp(list(fwProduction,
                                 flnProduction,
                                 fllProduction),
                        xlab="Production in Mm3/day",
                        plotstyle="ggplot",
                        legendtext=c("Weibull","Log-Normal","Log-Logistic"))


CProbDistProduction <- cdfcomp(list(fwProduction,
                             flnProduction,
                             fllProduction),
                         xlab="Production in Mm3/day",
                         plotstyle="ggplot",
                         legendtext=c("Weibull","Log-Normal","Log-Logistic"))

ProbDistProduction+
  ggtitle("Histogram and Distribution Curves of Production Data(2013-2017)")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))

CProbDistProduction+
  ggtitle("Cumulative Distribution Curves of Production Data(2013-2017)")+
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
GOF_Production<-gofstat(list(fwProduction,
                      flnProduction,
                      fllProduction))
LogLikProduction<- list(logW=logLik(fwProduction),
                 logln=logLik(flnProduction),
                 logll=logLik(fllProduction))
SummaryProduction <- list(SummaryW=summary(fwProduction),
                          Summaryln=summary(flnProduction),
                          Summaryll=summary(fllProduction))
StatsProduction<-list(GOF=GOF_Production,
               LogLiklihood=LogLikProduction,
               Summary=SummaryProduction)
#Distribution Summary
#Production is best fitted to be log-normal distributed
#------------------------------------------------------------------
#EUR Calculation Test 
#Exponential Decline Function
#Using the Production Volume (not rate)
#Qi is the initial production volume at Year 1
#R is the decline factor <- yet to find
#t is the well lifetime, assumed to be 30 years as this is the typical average well lifetime
#---------------------------------------------------------------------
#Filter based on the On_Production_Years
#Easiest to start
On_Prod <- NGPreProductionFiltered %>% 
  filter(On_Production_Years== "2013"|
         On_Production_Years== "2014"|
         On_Production_Years== "2015"|
         On_Production_Years== "2016"|
         On_Production_Years== "2017") 
On_Prod_Gas_VD <- On_Prod %>%  
  filter(Well_Fluid_Type=="Gas"&Drill_Type=="Vertical/Deviated")
On_Prod_Shale_VD <- On_Prod %>% 
  filter(Well_Fluid_Type=="Shale"&Drill_Type=="Vertical/Deviated")
On_Prod_CBM_VD <- On_Prod %>% 
  filter(Well_Fluid_Type=="Coalbed Methane"&Drill_Type=="Vertical/Deviated")
On_Prod_Gas_HD <- On_Prod %>% 
  filter(Well_Fluid_Type=="Gas"&Drill_Type=="Horizontal")
On_Prod_Shale_HD <- On_Prod %>% 
  filter(Well_Fluid_Type=="Shale"&Drill_Type=="Horizontal")
On_Prod_CBM_HD <- On_Prod %>% 
  filter(Well_Fluid_Type=="Coalbed Methane"&Drill_Type=="Horizontal")
#-------------------------------------------------------------
#Creating the function and initialising the time
#For Visualisation purposes and create rows in the empty matrix
exp_prod_vol <- function(qi,r,t){
  qi*((1-r)^(t-1))
}
Time <- 30 #In Years
Time_vector <- c(1:30)
Time_matrix <- matrix(Time_vector)
Time_data_frame <- data.frame(Time_matrix)
colnames(Time_data_frame) <- "Time_In_Year"
#----------------------------------------------------------------
#EUR Gas/VD Version
Gas_VD_matrix <- matrix(NA,
                     nrow=Time,
                     ncol=nrow(On_Prod_Gas_VD))
  for (i in 1:ncol(Gas_VD_matrix)){
  Gas_VD_matrix[,i] <- exp_prod_vol(On_Prod_Gas_VD$Production_Mm3_per_year[i],
                                 0.55,
                                 1)}
  for (j in 2:Time){
  Gas_VD_matrix[j,] <- exp_prod_vol(Gas_VD_matrix[j-1,],
                                                     0.55,
                                                     j)
  }
Gas_VD_data_frame <- data.frame(Gas_VD_matrix)
Cumsum_Gas_VD_matrix <- apply(Gas_VD_matrix,2,cumsum)
Cumsum_Gas_VD_data_frame <- data.frame(Cumsum_Gas_VD_matrix)
EUR_Gas_VD <- data.frame(t(Cumsum_Gas_VD_data_frame[30,]))
Mean_EUR_Gas_VD <- round(mean(EUR_Gas_VD$X30),3)
Median_EUR_Gas_VD <- round(median(EUR_Gas_VD$X30),3)
SD_EUR_Gas_VD <- round(sd(EUR_Gas_VD$X30),3)
Log_EUR_Gas_VD <- log(EUR_Gas_VD$X30)
Log_EUR_Gas_VD <- data.frame(Log_EUR_Gas_VD)
colnames(Log_EUR_Gas_VD) <- "ln_EUR_Mm3"
Mean_Log_EUR_Gas_VD <- mean(Log_EUR_Gas_VD$ln_EUR_Mm3)
SD_Log_EUR_Gas_VD <- sd(Log_EUR_Gas_VD$ln_EUR_Mm3)
quantile(EUR_Gas_VD$X30,c(.05,.25,.50,.75,.95))
#EUR Shale/VD Version
Shale_VD_matrix <- matrix(NA,
                         nrow=Time,
                         ncol=nrow(On_Prod_Shale_VD))
for (i in 1:ncol(Shale_VD_matrix)){
  Shale_VD_matrix[,i] <- exp_prod_vol(On_Prod_Shale_VD$Production_Mm3_per_year[i],
                                  0.55,
                                  1)
}
for (j in 2:Time){
  Shale_VD_matrix[j,] <- exp_prod_vol(Shale_VD_matrix[j-1,],
                                    0.55,
                                    j)
}
Shale_VD_data_frame <- data.frame(Shale_VD_matrix)
Cumsum_Shale_VD_matrix <- apply(Shale_VD_matrix,2,cumsum)
Cumsum_Shale_VD_data_frame <- data.frame(Cumsum_Shale_VD_matrix)
EUR_Shale_VD <- data.frame(t(Cumsum_Shale_VD_data_frame[30,]))
Mean_EUR_Shale_VD <- round(mean(EUR_Shale_VD$X30),3)
Median_EUR_Shale_VD <- round(median(EUR_Shale_VD$X30),3)
SD_EUR_Shale_VD <- round(sd(EUR_Shale_VD$X30),3)
Log_EUR_Shale_VD <- log(EUR_Shale_VD$X30)
Log_EUR_Shale_VD <- data.frame(Log_EUR_Shale_VD)
colnames(Log_EUR_Shale_VD) <- "ln_EUR_Mm3"
Mean_Log_EUR_Shale_VD <- mean(Log_EUR_Shale_VD$ln_EUR_Mm3)
SD_Log_EUR_Shale_VD <- sd(Log_EUR_Shale_VD$ln_EUR_Mm3)
#EUR CBM/VD Version
CBM_VD_matrix <- matrix(NA,
                          nrow=Time,
                          ncol=nrow(On_Prod_CBM_VD))
for (i in 1:ncol(CBM_VD_matrix)){
  CBM_VD_matrix[,i] <- exp_prod_vol(On_Prod_CBM_VD$Production_Mm3_per_year[i],
                                      0.55,
                                      1)
}
for (j in 2:Time){
  CBM_VD_matrix[j,] <- exp_prod_vol(CBM_VD_matrix[j-1,],
                                      0.55,
                                      j)
}
CBM_VD_data_frame <- data.frame(CBM_VD_matrix)
Cumsum_CBM_VD_matrix <- apply(CBM_VD_matrix,2,cumsum)
Cumsum_CBM_VD_data_frame <- data.frame(Cumsum_CBM_VD_matrix)
EUR_CBM_VD <- data.frame(t(Cumsum_CBM_VD_data_frame[30,]))
Mean_EUR_CBM_VD <- round(mean(EUR_CBM_VD$X30),3)
Median_EUR_CBM_VD <- round(median(EUR_CBM_VD$X30),3)
SD_EUR_CBM_VD <- round(sd(EUR_CBM_VD$X30),3)
Log_EUR_CBM_VD <- log(EUR_CBM_VD$X30)
Log_EUR_CBM_VD <- data.frame(Log_EUR_CBM_VD)
colnames(Log_EUR_CBM_VD) <- "ln_EUR_Mm3"
Mean_Log_EUR_CBM_VD <- mean(Log_EUR_CBM_VD$ln_EUR_Mm3)
SD_Log_EUR_CBM_VD <- sd(Log_EUR_CBM_VD$ln_EUR_Mm3)
#EUR Gas/HD Version
Gas_HD_matrix <- matrix(NA,
                      nrow=Time,
                      ncol=nrow(On_Prod_Gas_HD))
for (i in 1:ncol(Gas_HD_matrix)){
  Gas_HD_matrix[,i] <- exp_prod_vol(On_Prod_Gas_HD$Production_Mm3_per_year[i],
                                  0.55,
                                  1)
}
for (j in 2:Time){
  Gas_HD_matrix[j,] <- exp_prod_vol(Gas_HD_matrix[j-1,],
                                    0.55,
                                    j)
}
Gas_HD_data_frame <- data.frame(Gas_HD_matrix)
Cumsum_Gas_HD_matrix <- apply(Gas_HD_matrix,2,cumsum)
Cumsum_Gas_HD_data_frame <- data.frame(Cumsum_Gas_HD_matrix)
EUR_Gas_HD <- data.frame(t(Cumsum_Gas_HD_data_frame[30,]))
Mean_EUR_Gas_HD <- round(mean(EUR_Gas_HD$X30),3)
Median_EUR_Gas_HD <- round(median(EUR_Gas_HD$X30),3)
SD_EUR_Gas_HD <- round(sd(EUR_Gas_HD$X30),3)
Log_EUR_Gas_HD <- log(EUR_Gas_HD$X30)
Log_EUR_Gas_HD <- data.frame(Log_EUR_Gas_HD)
colnames(Log_EUR_Gas_HD) <- "ln_EUR_Mm3"
Mean_Log_EUR_Gas_HD <- mean(Log_EUR_Gas_HD$ln_EUR_Mm3)
SD_Log_EUR_Gas_HD <- sd(Log_EUR_Gas_HD$ln_EUR_Mm3)
#EUR Shale/HD Version
Shale_HD_matrix <- matrix(NA,
                          nrow=Time,
                          ncol=nrow(On_Prod_Shale_HD))
for (i in 1:ncol(Shale_HD_matrix)){
  Shale_HD_matrix[,i] <- exp_prod_vol(On_Prod_Shale_HD$Production_Mm3_per_year[i],
                                      0.55,
                                      1)
}
for (j in 2:Time){
  Shale_HD_matrix[j,] <- exp_prod_vol(Shale_HD_matrix[j-1,],
                                    0.55,
                                    j)
}
Shale_HD_data_frame <- data.frame(Shale_HD_matrix)
Cumsum_Shale_HD_matrix <- apply(Shale_HD_matrix,2,cumsum)
Cumsum_Shale_HD_data_frame <- data.frame(Cumsum_Shale_HD_matrix)
EUR_Shale_HD <- data.frame(t(Cumsum_Shale_HD_data_frame[30,]))
Mean_EUR_Shale_HD <- round(mean(EUR_Shale_HD$X30),3)
Median_EUR_Shale_HD <- round(median(EUR_Shale_HD$X30),3)
SD_EUR_Shale_HD <- round(sd(EUR_Shale_HD$X30),3)
Log_EUR_Shale_HD <- log(EUR_Shale_HD$X30)
Log_EUR_Shale_HD <- data.frame(Log_EUR_Shale_HD)
colnames(Log_EUR_Shale_HD) <- "ln_EUR_Mm3"
Mean_Log_EUR_Shale_HD <- mean(Log_EUR_Shale_HD$ln_EUR_Mm3)
SD_Log_EUR_Shale_HD <- sd(Log_EUR_Shale_HD$ln_EUR_Mm3)
#EUR CBM/HD Version
CBM_HD_matrix <- matrix(NA,
                        nrow=Time,
                        ncol=nrow(On_Prod_CBM_HD))
for (i in 1:ncol(CBM_HD_matrix)){
  CBM_HD_matrix[,i] <- exp_prod_vol(On_Prod_CBM_HD$Production_Mm3_per_year[i],
                                    0.55,
                                    1:30)
}
for (j in 2:Time){
  CBM_HD_matrix[j,] <- exp_prod_vol(CBM_HD_matrix[j-1,],
                                      0.55,
                                      j)
}
CBM_HD_data_frame <- data.frame(CBM_HD_matrix)
Cumsum_CBM_HD_matrix <- apply(CBM_HD_matrix,2,cumsum)
Cumsum_CBM_HD_data_frame <- data.frame(Cumsum_CBM_HD_matrix)
EUR_CBM_HD <- data.frame(t(Cumsum_CBM_HD_data_frame[30,]))
Mean_EUR_CBM_HD <- round(mean(EUR_CBM_HD$X30),3)
Median_EUR_CBM_HD <- round(median(EUR_CBM_HD$X30),3)
SD_EUR_CBM_HD <- round(sd(EUR_CBM_HD$X30),3)
Log_EUR_CBM_HD <- log(EUR_CBM_HD$X30)
Log_EUR_CBM_HD <- data.frame(Log_EUR_CBM_HD)
colnames(Log_EUR_CBM_HD) <- "ln_EUR_Mm3"
Mean_Log_EUR_CBM_HD <- mean(Log_EUR_CBM_HD$ln_EUR_Mm3)
SD_Log_EUR_CBM_HD <- sd(Log_EUR_CBM_HD$ln_EUR_Mm3)
#------------------------------------------------------------------------
#Exponential Decline Visualisation
library(ggplot2)
ggplot()+
  geom_line(aes(x=Time_data_frame$Time_In_Year,
                y=Gas_VD_data_frame$X1),
            colour="Red",
            size=2)+
  xlab("Time/year")+
  ylab("Production (Mm3)")+
  ggtitle("Exponential Decline Function of Production Data 2013 (Gas/Vertical)")+ 
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#Cumulative Production 
ggplot()+
  geom_line(aes(x=Time_data_frame$Time_In_Year,
                y=Cumsum_Gas_VD_data_frame$X1),
            colour="Red",
            size=2)+
  xlab("Time/year")+
  ylab("Production (Mm3)")+
  ggtitle("Cumulative Production Data 2013 (Gas/Vertical)")+ 
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#EUR Histogram (Log Scale)
EUR_Histo_Gas_VD <- ggplot(data=Log_EUR_Gas_VD)
EUR_Histo_Gas_HD <- ggplot(data=Log_EUR_Gas_HD)
EUR_Histo_Shale_VD <- ggplot(data=Log_EUR_Shale_VD)
EUR_Histo_Shale_HD<-ggplot(data=Log_EUR_Shale_HD)
EUR_Histo_CBM_VD <- ggplot(data=Log_EUR_CBM_VD)
EUR_Histo_CBM_HD <- ggplot(data=Log_EUR_CBM_HD)
EUR_Plot_Function <- function(Histo_Object,title){
Histo_Object + geom_histogram(aes(x=ln_EUR_Mm3,
                               y=stat(density)),
                           fill="LightBlue",
                           colour="Black",
                           binwidth = 0.2,
                           size=1)+
  xlab("ln(EUR(Mm3))")+
  ylab("Density")+
  ggtitle(title)+ 
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
}
EUR_Plot_Function(EUR_Histo_Gas_VD,"Alberta EUR Data for Gas/Vertical 2013-2017")
EUR_Plot_Function(EUR_Histo_Shale_VD,"Alberta EUR Data for Shale/Vertical 2013-2017")
EUR_Plot_Function(EUR_Histo_CBM_VD,"Alberta EUR Data for CBM/Vertical 2013-2017")
EUR_Plot_Function(EUR_Histo_Gas_HD,"Alberta EUR Data for Gas/Horizontal 2013-2017")
EUR_Plot_Function(EUR_Histo_Shale_HD,"Alberta EUR Data for Shale/Horizontal 2013-2017")
EUR_Plot_Function(EUR_Histo_CBM_HD,"Alberta EUR Data for CBM/Horizontal 2013-2017")
#------------------------------------------------------------------------
#Probability Distribution Fitting
library(fitdistrplus)
library(actuar)
#Fit Distributions
flnEUR_Gas_VD <- fitdist(EUR_Gas_VD$X30,"lnorm")
fllEUR_Gas_VD <- fitdist(EUR_Gas_VD$X30,"llogis")
flnEUR_Shale_VD <- fitdist(EUR_Shale_VD$X30,"lnorm")
fllEUR_Shale_VD <- fitdist(EUR_Shale_VD$X30,"llogis")
flnEUR_CBM_VD <- fitdist(EUR_CBM_VD$X30,"lnorm")
fllEUR_CBM_VD <- fitdist(EUR_CBM_VD$X30,"llogis")
flnEUR_Gas_HD <- fitdist(EUR_Gas_HD$X30,"lnorm")
fllEUR_Gas_HD <- fitdist(EUR_Gas_HD$X30,"llogis")
flnEUR_Shale_HD <- fitdist(EUR_Shale_HD$X30,"lnorm")
fllEUR_Shale_HD <- fitdist(EUR_Shale_HD$X30,"llogis")
flnEUR_CBM_HD <- fitdist(EUR_CBM_HD$X30,"lnorm")
fllEUR_CBM_HD <- fitdist(EUR_CBM_HD$X30,"llogis")
#List Form
fEUR_Gas_VD <- list(flnEUR_Gas_VD,fllEUR_Gas_VD)
fEUR_Shale_VD <- list(flnEUR_Shale_VD,fllEUR_Shale_VD)
fEUR_CBM_VD <- list(flnEUR_CBM_VD,fllEUR_CBM_VD)
fEUR_Gas_HD <- list(flnEUR_Gas_HD,fllEUR_Gas_HD)
fEUR_Shale_HD <- list(flnEUR_Shale_HD,fllEUR_Shale_HD)
fEUR_CBM_HD <- list(flnEUR_CBM_HD,fllEUR_CBM_HD)
#Assigning as an object for Dens Comp and CDF
ProbDistEUR <- function(EUR_list){denscomp(EUR_list,
                               xlab="EUR Mm3",
                               plotstyle="ggplot",
                               legendtext=c("Log-Normal","Log-Logistic"))}
ProbDistEUR_Gas_VD <- ProbDistEUR(fEUR_Gas_VD)
ProbDistEUR_Shale_VD <- ProbDistEUR(fEUR_Shale_VD)
ProbDistEUR_CBM_VD <- ProbDistEUR(fEUR_CBM_VD)
ProbDistEUR_Gas_HD <- ProbDistEUR(fEUR_Gas_HD)
ProbDistEUR_Shale_HD <- ProbDistEUR(fEUR_Shale_HD)
ProbDistEUR_CBM_HD <- ProbDistEUR(fEUR_CBM_HD)
CProbDistEUR <- function(EUR_list){cdfcomp(EUR_list,
                        xlab="EUR Mm3",
                        plotstyle="ggplot",
                        legendtext=c("Log-Normal","Log-Logistic"))}
CProbDistEUR_Gas_VD <- CProbDistEUR(fEUR_Gas_VD)
CProbDistEUR_Shale_VD <- CProbDistEUR(fEUR_Shale_VD)
CProbDistEUR_CBM_VD <- CProbDistEUR(fEUR_CBM_VD)
CProbDistEUR_Gas_HD <- CProbDistEUR(fEUR_Gas_HD)
CProbDistEUR_Shale_HD <- CProbDistEUR(fEUR_Shale_HD)
CProbDistEUR_CBM_HD <- CProbDistEUR(fEUR_CBM_HD)
#Prob Dist and CDF Plotting function
EUR_Plot<-function(info,title){info+
  ggtitle(title)+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))}
ProbDistPlot_EUR_Gas_VD <- EUR_Plot(ProbDistEUR_Gas_VD,
                                       "Histogram and Distribution Curves of Alberta EUR Data (Gas/Vertical) 2013-2017")
ProbDistPlot_EUR_Shale_VD <- EUR_Plot(ProbDistEUR_Shale_VD,
                                       "Histogram and Distribution Curves of Alberta EUR Data (Shale/Vertical) 2013-2017")
ProbDistPlot_EUR_CBM_VD <- EUR_Plot(ProbDistEUR_CBM_VD,
                                       "Histogram and Distribution Curves of Alberta EUR Data (CBM/Vertical) 2013-2017")
ProbDistPlot_EUR_Gas_HD <- EUR_Plot(ProbDistEUR_Gas_HD,
                                       "Histogram and Distribution Curves of Alberta EUR Data (Gas/Horizontal) 2013-2017")
ProbDistPlot_EUR_Shale_HD <- EUR_Plot(ProbDistEUR_Shale_HD,
                                       "Histogram and Distribution Curves of Alberta EUR Data (Shale/Horizontal) 2013-2017")
ProbDistPlot_EUR_CBM_HD <- EUR_Plot(ProbDistEUR_CBM_HD,
                                       "Histogram and Distribution Curves of Alberta EUR Data (CBM/Horizontal) 2013-2017")
CProbDistPlot_EUR_Gas_VD <- EUR_Plot(CProbDistEUR_Gas_VD,
                                    "Cumulative Distribution Curves of Alberta EUR Data (Gas/Vertical) 2013-2017")
CProbDistPlot_EUR_Shale_VD <- EUR_Plot(CProbDistEUR_Shale_VD,
                                      "Cumulative Distribution Curves of Alberta EUR Data (Shale/Vertical) 2013-2017")
CProbDistPlot_EUR_CBM_VD <- EUR_Plot(CProbDistEUR_CBM_VD,
                                    "Cumulative Distribution Curves of Alberta EUR Data (CBM/Vertical) 2013-2017")
CProbDistPlot_EUR_Gas_HD <- EUR_Plot(CProbDistEUR_Gas_HD,
                                    "Cumulative Distribution Curves of Alberta EUR Data (Gas/Horizontal) 2013-2017")
CProbDistPlot_EUR_Shale_HD <- EUR_Plot(CProbDistEUR_Shale_HD,
                                      "Cumulative Distribution Curves of Alberta EUR Data (Shale/Horizontal) 2013-2017")
CProbDistPlot_EUR_CBM_HD <- EUR_Plot(CProbDistEUR_CBM_HD,
                                    "Cumulative Distribution Curves of Alberta EUR Data (CBM/Horizontal) 2013-2017")
EUR_Statistics <- function(EUR_list,ln_EUR,ll_EUR){
GOF<-gofstat(EUR_list)
LogLik<-list(logln=logLik(ln_EUR),
                 logll=logLik(ll_EUR))
Summary<-list(Summaryln=summary(ln_EUR),
                  Summaryll=summary(ll_EUR))
Stats<- list(GOF=GOF,
             LogLikelihood=LogLik,
             Summary=Summary)
}
EUR_Gas_VD_Statistics <- EUR_Statistics(fEUR_Gas_VD,
                                        flnEUR_Gas_VD,
                                        fllEUR_Gas_VD)
EUR_Shale_VD_Statistics <- EUR_Statistics(fEUR_Shale_VD,
                                        flnEUR_Shale_VD,
                                        fllEUR_Shale_VD)
EUR_CBM_VD_Statistics <- EUR_Statistics(fEUR_CBM_VD,
                                        flnEUR_CBM_VD,
                                        fllEUR_CBM_VD)
EUR_Gas_HD_Statistics <- EUR_Statistics(fEUR_Gas_HD,
                                        flnEUR_Gas_HD,
                                        fllEUR_Gas_HD)
EUR_Shale_HD_Statistics <- EUR_Statistics(fEUR_Shale_HD,
                                        flnEUR_Shale_HD,
                                        fllEUR_Shale_HD)
EUR_CBM_HD_Statistics <- EUR_Statistics(fEUR_CBM_HD,
                                        flnEUR_CBM_HD,
                                        fllEUR_CBM_HD)
#--------------------------------------------------------
#Appendices
# #Monte Carlo Library Testing
# library(MonteCarlo)
# set.seed(123)
# 
# Methane_Test <- function(N,ME,SD){
#   Mat <- matrix(NA, 
#                 nrow=nrow(GreenPath),
#                 ncol=N)
#   EUR <-rnorm(N,ME,SD) #Generate Sample for EUR
#   for(i in 1:ncol(Mat)){
#   Mat[,i]<- GreenPath$Emission_Factor_m3_per_day/EUR[i]
#   }
#   names(Mat) <- "Mat"
#   return(list("Mat"=unname(Mat)))
# }
# 
# n_grid <- 10000
# loc_grid <- FugMean
# scale_grid <- FugSD
# 
# 
# Param<-list(N=n_grid,ME=loc_grid,SD=scale_grid)
# 
# Result <- MonteCarlo(func=Methane_Test,nrep=1000,param_list=Param)
# 
# for (i in )
#  for (j in 1:30){
#    ttest_df[,i]<-aggregate.data.frame(On_Prod$Production_Mm3_per_year[i],
#                        EUR_list,
#                        exp_prod_vol,r=0.5,t=j)
#  }
#HYPERBOLIC EQUATION CURVE
# Gas_VD_matrix <- matrix(NA,
#                         nrow=30,
#                         ncol=nrow(On_Prod_Gas_VD))
# for (i in 1:ncol(Gas_VD_matrix)){
#   Gas_VD_matrix[,i] <- exp_prod_vol(On_Prod_Gas_VD$Production_Mm3_per_year[i],
#                                     0.55,
#                                     1)}
# for (j in 2){
#   Gas_VD_matrix[j,] <- exp_prod_vol(Gas_VD_matrix[1,],
#                                     0.55,
#                                     j)
# }
# for (k in 3:6){
#   Gas_VD_matrix[k,] <- exp_prod_vol(Gas_VD_matrix[1,],
#                                     0.26,
#                                     k)
# }
# for (l in 7:14){
#   Gas_VD_matrix[l,] <- exp_prod_vol(Gas_VD_matrix[1,],
#                                     0.16,
#                                     l)
# }
# for (m in 15:30){
#   Gas_VD_matrix[m,] <- exp_prod_vol(Gas_VD_matrix[1,],
#                                     0.13,
#                                     m)
# }