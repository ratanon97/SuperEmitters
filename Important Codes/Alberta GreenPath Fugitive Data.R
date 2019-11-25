#Set Working Directory for GreenPath Fugitive Data
getwd() #See the working directory of the associated computer to import the data
setwd("C:\\Users\\KOMSUN\\Documents\\Files from Imperial Laptop\\Modules\\Research Project\\Data Collection\\CSV Inputs")
Fug_GreenPath <- read.csv("Alberta GreenPath Fugitive Data.csv") 
#---------------------------------------------------------
#Notes: This is bottom up method
#GWP Used: GWP100 of 25 for Methane and GWP100 of 298 for NO2
#Year 2016 for this script
#----------------------------------------------------------
#Data Frame Preparation
Fug_GreenPath$Province<- NULL
Fug_GreenPath$Loc1 <- NULL
Fug_GreenPath$Loc2 <- NULL
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
#DPLYR LIBRARY DATAFRAME ADJUSTMENTS
library(dplyr)
Fug_GreenPath <- Fug_GreenPath %>% 
  mutate(Emission_Rate_cmm=Emission_Rate_cfm*0.035315) %>% 
  mutate(Emission_Rate_cmh=Emission_Rate_cmm*60) %>% 
  mutate(Emission_Rate_cmd=Emission_Rate_cmm*1440)
write.csv(Fug_GreenPath,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath.csv")
Fug_GreenPath$Facility_Type <- gsub("Gas Multiwell Group Battery","Gas Battery",Fug_GreenPath$Facility_Type)
Fug_GreenPath$Facility_Type <- gsub("Gas Multiwell Proration Outside Se Alberta Battery","Gas Battery",Fug_GreenPath$Facility_Type)
Fug_GreenPath$Facility_Type <- gsub("Gas Single-well Battery","Gas Battery",Fug_GreenPath$Facility_Type)
Fug_GreenPath$Facility_Type <- factor(Fug_GreenPath$Facility_Type)
#------------------------------------------------------------------
#Total Population
Top_5_Fug_GreenPath <- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% 
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% 
  filter(Rank>=24)
#---------------------------------------------------------------------
#Segment Population
#Gas Wells
Fug_GreenPath_GW <- Fug_GreenPath %>% 
  filter(Facility_Type=="Gas Well") %>% 
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd)))
Fug_GreenPath_GW$Facility_Type <- as.numeric(Fug_GreenPath_GW$Facility_Type)
Fug_GreenPath_GW <- Fug_GreenPath_GW %>% 
  mutate(Number_Of_Sites_CS=cumsum(Facility_Type)/max(sum(Facility_Type)))
write.csv(Fug_GreenPath_GW,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_GW.csv")
#Skewness Analysis
In_Depth_Fug_GreenPath_GW<- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% 
  filter(Facility_Type=="Gas Well") %>% 
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% 
  filter(Rank==14)
#Gas Batteries
Fug_GreenPath_GB <- Fug_GreenPath %>% 
  filter(Facility_Type=="Gas Battery") 
Fug_GreenPath_GB<-Fug_GreenPath_GB[order(Fug_GreenPath_GB$Emission_Rate_cmd),] 
Fug_GreenPath_GB <- Fug_GreenPath_GB %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd)))
Fug_GreenPath_GB$Facility_Type <- as.numeric(Fug_GreenPath_GB$Facility_Type)
Fug_GreenPath_GB <- Fug_GreenPath_GB %>% 
  mutate(Number_Of_Sites_CS=cumsum(Facility_Type)/max(sum(Facility_Type)))
write.csv(Fug_GreenPath_GB,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_GB.csv")
#Skewness Analysis
In_Depth_Fug_GreenPath_GB<- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% 
  filter(Facility_Type=="Gas Battery") %>% 
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% 
  filter(Rank==10)
#Segment Top 5 GreenPath
Segment_Top_5_GreenPath <- rbind.data.frame(In_Depth_Fug_GreenPath_GB,
                                              In_Depth_Fug_GreenPath_GW)
colnames(Segment_Top_5_GreenPath) <- c("Petrinex_Facility_Subtype",
                                         "Emission_Type",
                                         "Major_Equipment",
                                         "Component_Service_Type",
                                         "Component_Type",
                                         "CH4_Leak_Rate_m3_per_day_per_source",
                                         "Rank")
#---------------------------------------------------------------
#Process/Block Population
#Wellhead
Fug_GreenPath_WH <- Fug_GreenPath %>% 
  filter(Process_Block=="Wellhead") 
Fug_GreenPath_WH<-Fug_GreenPath_WH[order(Fug_GreenPath_WH$Emission_Rate_cmd),] 
Fug_GreenPath_WH <- Fug_GreenPath_WH %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd)))
Fug_GreenPath_WH$Process_Block <- as.numeric(Fug_GreenPath_WH$Process_Block)
Fug_GreenPath_WH <- Fug_GreenPath_WH %>% 
  mutate(Number_Of_Sites_CS=cumsum(Process_Block)/max(sum(Process_Block)))
write.csv(Fug_GreenPath_WH,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_WH.csv")
#Skewness Analysis
In_Depth_Fug_GreenPath_WH<- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% 
  filter(Process_Block=="Wellhead") %>% 
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% 
  filter(Rank==12)
#Filter/Separation
Fug_GreenPath_FS <- Fug_GreenPath %>% 
  filter(Process_Block=="Filter/Separation") 
Fug_GreenPath_FS<-Fug_GreenPath_FS[order(Fug_GreenPath_FS$Emission_Rate_cmd),] 
Fug_GreenPath_FS <- Fug_GreenPath_FS %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd)))
Fug_GreenPath_FS$Process_Block <- as.numeric(Fug_GreenPath_FS$Process_Block)
Fug_GreenPath_FS <- Fug_GreenPath_FS %>% 
  mutate(Number_Of_Sites_CS=cumsum(Process_Block)/max(sum(Process_Block)))
write.csv(Fug_GreenPath_FS,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Fug_GreenPath_FS.csv")
#Skewness Analysis
In_Depth_Fug_GreenPath_FS<- Fug_GreenPath %>% 
  dplyr::select(Facility_Type,
                Emission_Type,
                Process_Block,
                Service_Type,
                Component_Main_Type,
                Emission_Rate_cmd) %>% 
  filter(Process_Block=="Filter/Separation") %>% 
  mutate(Rank=rank(Emission_Rate_cmd,ties.method="max")) %>% 
  filter(Rank==9)
#Equipment Top 5 GreenPath
Equipment_Top_5_GreenPath <- rbind.data.frame(In_Depth_Fug_GreenPath_WH,
                                            In_Depth_Fug_GreenPath_FS)
colnames(Equipment_Top_5_GreenPath) <- c("Petrinex_Facility_Subtype",
                                         "Emission_Type",
                                         "Major_Equipment",
                                         "Component_Service_Type",
                                         "Component_Type",
                                         "CH4_Leak_Rate_m3_per_day_per_source",
                                         "Rank")
#---------------------------------------------------------------
#Component Main Type
#Connector
Fug_GreenPath_CNT <- Fug_GreenPath %>% 
  filter(Component_Main_Type=="Connector") 
Fug_GreenPath_CNT<-Fug_GreenPath_CNT[order(Fug_GreenPath_CNT$Emission_Rate_cmd),] 
Fug_GreenPath_CNT <- Fug_GreenPath_CNT %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd)))
Fug_GreenPath_CNT$Component_Main_Type <- as.numeric(Fug_GreenPath_CNT$Component_Main_Type)
Fug_GreenPath_CNT <- Fug_GreenPath_CNT %>% 
  mutate(Number_Of_Sites_CS=cumsum(Component_Main_Type)/max(sum(Component_Main_Type)))
#Valve
Fug_GreenPath_Valve <- Fug_GreenPath %>% 
  filter(Component_Main_Type=="Valve") 
Fug_GreenPath_Valve<-Fug_GreenPath_Valve[order(Fug_GreenPath_Valve$Emission_Rate_cmd),] 
Fug_GreenPath_Valve <- Fug_GreenPath_Valve %>%
  mutate(Emission_CS=cumsum(Emission_Rate_cmd)/max(sum(Emission_Rate_cmd)))
Fug_GreenPath_Valve$Component_Main_Type <- as.numeric(Fug_GreenPath_Valve$Component_Main_Type)
Fug_GreenPath_Valve <- Fug_GreenPath_Valve %>% 
  mutate(Number_Of_Sites_CS=cumsum(Component_Main_Type)/max(sum(Component_Main_Type)))
#---------------------------------------------------------------
library(ggplot2)
library(ineq)
library(gglorenz)
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
#Lorenz Curve fitting
FGW_Lorenz <- ggplot(data=Fug_GreenPath_GW,aes(x=Number_Of_Sites_CS))
FGB_Lorenz <- ggplot(data=Fug_GreenPath_GB,aes(x=Number_Of_Sites_CS))
FWH_Lorenz <- ggplot(data=Fug_GreenPath_WH,aes(x=Number_Of_Sites_CS))
FFS_Lorenz <- ggplot(data=Fug_GreenPath_FS,aes(x=Number_Of_Sites_CS))
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
#Plottin multiple lorenz curves
plot(Lc(Fug_GreenPath$Emission_Rate_cmh),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Segment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
#Segment Population
plot(Lc(Fug_GreenPath_GW$Emission_Rate_cmh),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Segment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
lines(Lc(Fug_GreenPath_GB$Emission_Rate_cmh),
      col="blue",
      lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Gas Wells","Gas Batteries"),
       col=c("red","blue"),pch=15:18,horiz=F)
grid(col="lightgray")   
#Equipment Population
plot(Lc(Fug_GreenPath_WH$Emission_Rate_cmh),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Equipment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
lines(Lc(Fug_GreenPath_FS$Emission_Rate_cmh),
      col="blue",
      lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Wellhead","Filter/Separation"),
       col=c("red","blue"),pch=15:18,horiz=F)
grid(col="lightgray")                         
#Component Main Type
plot(Lc(Fug_GreenPath_CNT$Emission_Rate_cmd),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Component Main Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
lines(Lc(Fug_GreenPath_Valve$Emission_Rate_cmd),
      col="blue",
      lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Connector","Valve"),
       col=c("red","blue"),pch=15:18,horiz=F)
grid(col="lightgray")
#----------------------------------------------
#Probability Distribution Data Fitting 
library(fitdistrplus) #Load all the fitdistr to plot the probability distribution functions
library(actuar) #Load the Log-Logistic Distribution function
fwFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"weibull") #Weibull Distribution
fgFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"gamma") #Gamma Distribution
flnFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"lnorm")# Log-Normal Distribution
fllFugPneumatic <- fitdist(Fug_GreenPath$Emission_Rate_cmh,"llogis") #Log-Logistic Distribution 
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
#Log-Logistic Distribution is good at fitting
#----------------------------------------------
#Statistics List
GOFFugPneu<-gofstat(list(fwFugPneumatic,
                      fgFugPneumatic,
                      flnFugPneumatic,
                      fllFugPneumatic))
LogLikFugPneu<-list(logW=logLik(fwFugPneumatic),
                 logG=logLik(fgFugPneumatic),
                 logln=logLik(flnFugPneumatic),
                 logll=logLik(fllFugPneumatic))
SummaryFugPneu<-list(SummaryW=summary(fwFugPneumatic),
                  SummaryG=summary(fgFugPneumatic),
                  Summaryln=summary(flnFugPneumatic),
                  Summaryll=summary(fllFugPneumatic))
StatsFugPneu <- list(GOF=GOFFugPneu,
                  LogLikelihood=LogLikFugPneu,
                  Summary=SummaryFugPneu)
#----------------------------------------------
#Monte Carlo Testing
set.seed(123)
MC_Function_FGP <- function(df,mean,sd){
  FGP_Methane_Throughput <- (df$Emission_Rate_cmd/rlnorm(nrow(df),mean,sd))*100
  return(FGP_Methane_Throughput)
}
Fug_GreenPath_Sim <- replicate(10000,MC_Function_FGP(Fug_GreenPath,Mean_LN_EUR,SD_LN_EUR))
Fug_GreenPath_Sim <- data.frame(Fug_GreenPath_Sim)
#Calculations from Simulation
Mean_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,mean)
Median_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,median)
Q5_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.05))
Q25_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.25))
Q75_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.75))
Q95_Fug_GreenPath_Sim <- sapply(Fug_GreenPath_Sim,quantile,c(.95))
FilteredMean_Fug_GreenPath_Sim<-Mean_Fug_GreenPath_Sim[Mean_Fug_GreenPath_Sim <= 100]
FilteredQ75_Fug_GreenPath_Sim <- Q75_Fug_GreenPath_Sim[Q75_Fug_GreenPath_Sim<=100]
FilteredQ95_Fug_GreenPath_Sim <- Q95_Fug_GreenPath_Sim[Q95_Fug_GreenPath_Sim<=100]
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
#Multiple Plots
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
}
for(h in 1001:2000){
  lines(ecdf(Fug_GreenPath_Sim[,h]),
        col=alpha("cadetblue2",0.3))
}
for(l in 2001:3000){
  lines(ecdf(Fug_GreenPath_Sim[,l]),
        col=alpha("darkorchid2",0.3))
}
for(p in 3001:4000){
  lines(ecdf(Fug_GreenPath_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
}
for(o in 4001:5000){
  lines(ecdf(Fug_GreenPath_Sim[,o]),
        col=alpha("blueviolet",0.3))
}
for(k in 5001:6000){
  lines(ecdf(Fug_GreenPath_Sim[,k]),
        col=alpha("darkslateblue",0.3))
}
for(s in 6001:7000){
  lines(ecdf(Fug_GreenPath_Sim[,s]),
        col=alpha("darkmagenta",0.3))
}
for(u in 7001:8000){
  lines(ecdf(Fug_GreenPath_Sim[,u]),
        col=alpha("deeppink1",0.3))
}
for(x in 8001:9000){
  lines(ecdf(Fug_GreenPath_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
}
for(f in 9001:10000){
  lines(ecdf(Fug_GreenPath_Sim[,f]),
        col=alpha("darkorchid",0.3))
}
#----------------------------------------------
#Visualisation Testing
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

strings = c("Important text,      !Comment that could be removed", "Other String")
gsub("(,[ ]*!.*)$", "", strings)