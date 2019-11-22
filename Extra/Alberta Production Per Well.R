#--------------------------------------------
#Set working directory for Production Per Well Data
getwd()
setwd("C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files")
ProductionPerWell <-read.csv("New Alberta Pre-Production Data.csv")
#----------------------------------------------
#Basic Data Frame Adjustments
colnames(ProductionPerWell)<-c("Well_Fluid_Type",
                             "PSAC_Area",
                             "Drill_Type",
                             "First_Date",
                             "Act_On_Production",
                             "On_Production_Years",
                             "Activity_Years",
                             "Production_Mm3_per_year",
                             "Well_Count",
                             "Well_Drain_Count")
ProductionPerWell$PSAC_Area <- factor(ProductionPerWell$PSAC_Area)
ProductionPerWell$First_Date <- factor(ProductionPerWell$First_Date)
ProductionPerWell$Act_On_Production <- factor(ProductionPerWell$Act_On_Production)
#Dplyr library function
library(dplyr)
ProductionPerWell <- ProductionPerWell %>%
  mutate(Prod_Per_Well=Production_Mm3_per_year/Well_Count) %>% 
  filter(Act_On_Production!="" & Act_On_Production!="<1962") 
#Convert factor into character and into a number
ProductionPerWell$Act_On_Production <- as.numeric(as.character(ProductionPerWell$Act_On_Production))
ProductionPerWell <- ProductionPerWell %>% 
  mutate(Well_Age=Activity_Years-Act_On_Production) %>%
  mutate(Prod_Per_Well_Per_Month=Prod_Per_Well/12) %>% 
  mutate(Prod_Per_Well_Per_Day=Prod_Per_Well_Per_Month/30) %>% 
  filter(Well_Age>=0 & Well_Age<=30)
#2017 for Gas/Shale/CBM with V/H drills
Filter_PPW <- function(WFT,DT,OPY){
  ProductionPerWell %>%
  filter(Well_Fluid_Type==WFT &Drill_Type==DT) %>% 
  filter(Activity_Years==OPY)
}
Gas_V_PPW_2017 <- Filter_PPW("Gas","Vertical/Deviated","2017")
Gas_H_PPW_2017 <- Filter_PPW("Gas","Horizontal","2017")
Shale_V_PPW_2017 <- Filter_PPW("Shale","Vertical/Deviated","2017")
Shale_H_PPW_2017 <- Filter_PPW("Shale","Horizontal","2017")
CBM_V_PPW_2017 <- Filter_PPW("Coalbed Methane","Vertical/Deviated","2017")
CBM_H_PPW_2017 <- Filter_PPW("Coalbed Methane","Horizontal","2017")
List_2017_Data <- list(Gas_VD=Gas_V_PPW_2017$Prod_Per_Well_Per_Day,
                       Gas_HD=Gas_H_PPW_2017$Prod_Per_Well_Per_Day,
                       Shale_VD=Shale_V_PPW_2017$Prod_Per_Well_Per_Day,
                       Shale_HD=Shale_H_PPW_2017$Prod_Per_Well_Per_Day,
                       CBM_VD=CBM_V_PPW_2017$Prod_Per_Well_Per_Day,
                       CBM_HD=CBM_H_PPW_2017$Prod_Per_Well_Per_Day)
List_2017_mean <- lapply(List_2017_Data,mean)
List_2017_sd <- lapply(List_2017_Data,sd)
List_2017_var <- lapply(List_2017_Data,var)
Log_List_2017_mean <- lapply(List_2017_mean,log)
Log_List_2017_sd <- lapply(List_2017_sd,log)
Log_List_2017_var <- lapply(List_2017_var,log)
#2016 for Gas/Shale/CBM with V/H drills
Gas_V_PPW_2016 <- Filter_PPW("Gas","Vertical/Deviated","2016")
Gas_H_PPW_2016 <- Filter_PPW("Gas","Horizontal","2016")
Shale_V_PPW_2016 <- Filter_PPW("Shale","Vertical/Deviated","2016")
Shale_H_PPW_2016 <- Filter_PPW("Shale","Horizontal","2016")
CBM_V_PPW_2016 <- Filter_PPW("Coalbed Methane","Vertical/Deviated","2016")
CBM_H_PPW_2016 <- Filter_PPW("Coalbed Methane","Horizontal","2016")
#Filter based on Quartile Ranges
#This function is based on the DPLYR library
Quantile_Function <- function(PPW_Object,QValue){PPW_Object %>% 
  filter(Prod_Per_Well_Per_Day <= quantile(PPW_Object$Prod_Per_Well_Per_Day,c(QValue)))}
#Gas Vertical/Drill 2017
GVD_5th_2017 <- Quantile_Function(Gas_V_PPW_2017,.05)
GVD_25th_2017 <- Quantile_Function(Gas_V_PPW_2017,.25)
GVD_50th_2017 <- Quantile_Function(Gas_V_PPW_2017,.50)
GVD_75th_2017 <- Quantile_Function(Gas_V_PPW_2017,.75)
GVD_95th_2017 <- Quantile_Function(Gas_V_PPW_2017,.95)
GVD_Quartile <- list(Fifth=GVD_5th_2017$Prod_Per_Well_Per_Day,
                     TF=GVD_25th_2017$Prod_Per_Well_Per_Day,
                     FT=GVD_50th_2017$Prod_Per_Well_Per_Day,
                     STF=GVD_75th_2017$Prod_Per_Well_Per_Day,
                     NTF=GVD_95th_2017$Prod_Per_Well_Per_Day)
mean_GVD_2017<-lapply(GVD_Quartile,mean)
var_GVD_2017 <- lapply(GVD_Quartile,var)
sd_GVD_2017 <-lapply(GVD_Quartile,sd)
#2016
GVD_5th_2016 <- Quantile_Function(Gas_V_PPW_2016,.05)
GVD_25th_2016 <- Quantile_Function(Gas_V_PPW_2016,.25)
GVD_50th_2016 <- Quantile_Function(Gas_V_PPW_2016,.50)
GVD_75th_2016 <- Quantile_Function(Gas_V_PPW_2016,.75)
GVD_95th_2016 <- Quantile_Function(Gas_V_PPW_2016,.95)
GVD_Quartile <- list(Fifth=GVD_5th_2016$Prod_Per_Well_Per_Day,
                     TF=GVD_25th_2016$Prod_Per_Well_Per_Day,
                     FT=GVD_50th_2016$Prod_Per_Well_Per_Day,
                     STF=GVD_75th_2016$Prod_Per_Well_Per_Day,
                     NTF=GVD_95th_2016$Prod_Per_Well_Per_Day)
mean_GVD_2016<-lapply(GVD_Quartile,mean)
var_GVD_2016 <- lapply(GVD_Quartile,var)
sd_GVD_2016 <-lapply(GVD_Quartile,sd)
#Shale Vertical/Drill
SVD_5th_2017 <- Quantile_Function(Shale_V_PPW_2017,.05)
SVD_25th_2017 <- Quantile_Function(Shale_V_PPW_2017,.25)
SVD_50th_2017 <- Quantile_Function(Shale_V_PPW_2017,.50)
SVD_75th_2017 <- Quantile_Function(Shale_V_PPW_2017,.75)
SVD_95th_2017 <- Quantile_Function(Shale_V_PPW_2017,.95)
SVD_Quartile <- list(Fifth=SVD_5th_2017$Prod_Per_Well_Per_Day,
                     TF=SVD_25th_2017$Prod_Per_Well_Per_Day,
                     FT=SVD_50th_2017$Prod_Per_Well_Per_Day,
                     STF=SVD_75th_2017$Prod_Per_Well_Per_Day,
                     NTF=SVD_95th_2017$Prod_Per_Well_Per_Day)
mean_SVD_2017<-lapply(SVD_Quartile,mean)
var_SVD_2017 <- lapply(SVD_Quartile,var)
sd_SVD_2017 <-lapply(SVD_Quartile,sd)
#CBM Vertical/Drill
CBMVD_5th_2017 <- Quantile_Function(CBM_V_PPW_2017,.05)
CBMVD_25th_2017 <- Quantile_Function(CBM_V_PPW_2017,.25)
CBMVD_50th_2017 <- Quantile_Function(CBM_V_PPW_2017,.50)
CBMVD_75th_2017 <- Quantile_Function(CBM_V_PPW_2017,.75)
CBMVD_95th_2017 <- Quantile_Function(CBM_V_PPW_2017,.95)
CBMVD_Quartile <- list(Fifth=CBMVD_5th_2017$Prod_Per_Well_Per_Day,
                     TF=CBMVD_25th_2017$Prod_Per_Well_Per_Day,
                     FT=CBMVD_50th_2017$Prod_Per_Well_Per_Day,
                     STF=CBMVD_75th_2017$Prod_Per_Well_Per_Day,
                     NTF=CBMVD_95th_2017$Prod_Per_Well_Per_Day)
mean_CBMVD_2017<-lapply(CBMVD_Quartile,mean)
var_CBMVD_2017 <- lapply(CBMVD_Quartile,var)
sd_CBMVD_2017 <-lapply(CBMVD_Quartile,sd)
#Gas Horizontal Drill
GHD_5th_2017 <- Quantile_Function(Gas_H_PPW_2017,.05)
GHD_25th_2017 <- Quantile_Function(Gas_H_PPW_2017,.25)
GHD_50th_2017 <- Quantile_Function(Gas_H_PPW_2017,.50)
GHD_75th_2017 <- Quantile_Function(Gas_H_PPW_2017,.75)
GHD_95th_2017 <- Quantile_Function(Gas_H_PPW_2017,.95)
GHD_Quartile <- list(Fifth=GHD_5th_2017$Prod_Per_Well_Per_Day,
                     TF=GHD_25th_2017$Prod_Per_Well_Per_Day,
                     FT=GHD_50th_2017$Prod_Per_Well_Per_Day,
                     STF=GHD_75th_2017$Prod_Per_Well_Per_Day,
                     NTF=GHD_95th_2017$Prod_Per_Well_Per_Day)
mean_GHD_2017<-lapply(GHD_Quartile,mean)
var_GHD_2017 <- lapply(GHD_Quartile,var)
sd_GHD_2017 <-lapply(GHD_Quartile,sd)
#Shale Horizontal Drill
SHD_5th_2017 <- Quantile_Function(Shale_H_PPW_2017,.05)
SHD_25th_2017 <- Quantile_Function(Shale_H_PPW_2017,.25)
SHD_50th_2017 <- Quantile_Function(Shale_H_PPW_2017,.50)
SHD_75th_2017 <- Quantile_Function(Shale_H_PPW_2017,.75)
SHD_95th_2017 <- Quantile_Function(Shale_H_PPW_2017,.95)
SHD_Quartile <- list(Fifth=SHD_5th_2017$Prod_Per_Well_Per_Day,
                     TF=SHD_25th_2017$Prod_Per_Well_Per_Day,
                     FT=SHD_50th_2017$Prod_Per_Well_Per_Day,
                     STF=SHD_75th_2017$Prod_Per_Well_Per_Day,
                     NTF=SHD_95th_2017$Prod_Per_Well_Per_Day)
mean_SHD_2017<-lapply(SHD_Quartile,mean)
var_SHD_2017 <- lapply(SHD_Quartile,var)
sd_SHD_2017 <-lapply(SHD_Quartile,sd)
#CBM Horizontal Drill 
CBMHD_5th_2017 <- Quantile_Function(CBM_H_PPW_2017,.05)
CBMHD_25th_2017 <- Quantile_Function(CBM_H_PPW_2017,.25)
CBMHD_50th_2017 <- Quantile_Function(CBM_H_PPW_2017,.50)
CBMHD_75th_2017 <- Quantile_Function(CBM_H_PPW_2017,.75)
CBMHD_95th_2017 <- Quantile_Function(CBM_H_PPW_2017,.95)
CBMHD_Quartile <- list(Fifth=CBMHD_5th_2017$Prod_Per_Well_Per_Day,
                       TF=CBMHD_25th_2017$Prod_Per_Well_Per_Day,
                       FT=CBMHD_50th_2017$Prod_Per_Well_Per_Day,
                       STF=CBMHD_75th_2017$Prod_Per_Well_Per_Day,
                       NTF=CBMHD_95th_2017$Prod_Per_Well_Per_Day)
mean_CBMHD_2017<-lapply(CBMHD_Quartile,mean)
var_CBMHD_2017 <- lapply(CBMHD_Quartile,var)
sd_CBMHD_2017 <-lapply(CBMHD_Quartile,sd)
qqnorm(Gas_V_PPW_2017$Prod_Per_Well)
qqline(Gas_V_PPW_2017$Prod_Per_Well)
#--------------------------------------------------------------------------------------
#Data Visualisation
library(ggplot2)
library(gghighlight)
library(MESS)
#2017 Data Object Assigning
Gas_VD_2017_Plot <- ggplot(data=Gas_V_PPW_2017)
Shale_VD_2017_Plot <- ggplot(data=Shale_V_PPW_2017)
CBM_VD_2017_Plot <- ggplot(data=CBM_V_PPW_2017)
Gas_HD_2017_Plot <- ggplot(data=Gas_H_PPW_2017)
Shale_HD_2017_Plot <- ggplot(data=Shale_H_PPW_2017)
CBM_HD_2017_Plot <- ggplot(data=CBM_H_PPW_2017)
#2016 Data Object Assigning
Gas_VD_2016_Plot <- ggplot(data=Gas_V_PPW_2016)
Shale_VD_2016_Plot <- ggplot(data=Shale_V_PPW_2016)
CBM_VD_2016_Plot <- ggplot(data=CBM_V_PPW_2016)
Gas_HD_2016_Plot <- ggplot(data=Gas_H_PPW_2016)
Shale_HD_2016_Plot <- ggplot(data=Shale_H_PPW_2016)
CBM_HD_2016_Plot <- ggplot(data=CBM_H_PPW_2016)
#Quantile Plot  Highlight
SHD_5th_2017_Plot <- ggplot(data=SHD_5th_2017)
#Histogram PLotting
HistoPlot_Function <- function(Histo_Object,title){
  Histo_Object + geom_histogram(aes(x=Prod_Per_Well_Per_Day,
                                  y=stat(density)),
                                fill="dodgerblue3",
                                colour="Black",
                                binwidth = 0.002,
                                size=1)+
    xlab("Production Per Well (Mm3)")+
    ylab("Density")+
    ggtitle(title)+ 
    theme(axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          axis.text.x=element_text(size=15,colour="black"),
          axis.text.y=element_text(size=15,colour="black"),
          plot.title=element_text(size=20))
}
DensityPlot_Function <- function(Density_Object,title){
  Density_Object + geom_density(aes(x=Prod_Per_Well_Per_Day,
                                    y=stat(density)),
                                fill="dodgerblue3",
                                colour="Black",
                                binwidth = 0.002,
                                size=1)+
    xlab("Production Per Well (Mm3)")+
    ylab("Density")+
    ggtitle(title)+ 
    theme(axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          axis.text.x=element_text(size=15,colour="black"),
          axis.text.y=element_text(size=15,colour="black"),
          plot.title=element_text(size=20))
}
#2017 Version
Gas_VD_2017_Histo <- HistoPlot_Function(Gas_VD_2017_Plot,"Alberta Gas/Vertical Production Per Well (2017)")
Shale_VD_2017_Histo <- HistoPlot_Function(Shale_VD_2017_Plot,"Alberta Shale/Vertical Production Per Well (2017)")
CBM_VD_2017_Histo <- HistoPlot_Function(CBM_VD_2017_Plot,"Alberta CBM/Vertical Production Per Well (2017)")
Gas_HD_2017_Histo <- HistoPlot_Function(Gas_HD_2017_Plot,"Alberta Gas/Horizontal Production Per Well (2017)")
Shale_HD_2017_Histo <- HistoPlot_Function(Shale_HD_2017_Plot,"Alberta Shale/Horizontal Production Per Well (2017)")
CBM_HD_2017_Histo <- HistoPlot_Function(CBM_HD_2017_Plot,"Alberta CBM/Horizontal Production Per Well (2017)")
#2016 Version
Gas_VD_2016_Histo <- HistoPlot_Function(Gas_VD_2016_Plot,"Alberta Gas/Vertical Production Per Well (2016)")
Shale_VD_2016_Histo <- HistoPlot_Function(Shale_VD_2016_Plot,"Alberta Shale/Vertical Production Per Well (2016)")
CBM_VD_2016_Histo <- HistoPlot_Function(CBM_VD_2016_Plot,"Alberta CBM/Vertical Production Per Well (2016)")
Gas_HD_2016_Histo <- HistoPlot_Function(Gas_HD_2016_Plot,"Alberta Gas/Horizontal Production Per Well (2016)")
Shale_HD_2016_Histo <- HistoPlot_Function(Shale_HD_2016_Plot,"Alberta Shale/Horizontal Production Per Well (2016)")
CBM_HD_2016_Histo <- HistoPlot_Function(CBM_HD_2016_Plot,"Alberta CBM/Horizontal Production Per Well (2016)")
#Quartile Plots
SHD_5th_2017_Dens <- DensityPlot_Function(SHD_5th_2017_Plot,"a")
#Scatter Plot
Gas_VD_2017_Plot + 
  geom_point(aes(x=Well_Age,y=Prod_Per_Well_Per_Day))+
  xlab("Well Age (Year)")+
  ylab("Production Per Well")+
  ggtitle("Alberta Gas/Vertical Production Per Well (2016)")+ #Making the graph look niceer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#Area Calculation for Total Production Estimates
Area_Gas_VD_2017<- auc(x=Gas_V_PPW_2017$Prod_Per_Well_Per_Day,
                       y=Gas_V_PPW_2017$Well_Age)

#----------------------------------------------------------------------------------
#Probability Distribution Fitting
library(fitdistrplus)
library(actuar)
FitDist_Plot<-function(info,title){info+
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
#2017 Data Fit Dist
flnProd_Gas_VD_2017 <- fitdist(Gas_V_PPW_2017$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Gas_VD_2017 <- fitdist(Gas_V_PPW_2017$Prod_Per_Well_Per_Day,"llogis",start=list(shape=median(Gas_V_PPW_2017$Prod_Per_Well_Per_Day)))
flnProd_Shale_VD_2017 <- fitdist(Shale_V_PPW_2017$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Shale_VD_2017 <- fitdist(Shale_V_PPW_2017$Prod_Per_Well_Per_Day,"llogis",
                                 start=list(shape=1))
flnProd_CBM_VD_2017 <- fitdist(CBM_V_PPW_2017$Prod_Per_Well_Per_Day,"lnorm")
fllProd_CBM_VD_2017 <- fitdist(CBM_V_PPW_2017$Prod_Per_Well_Per_Day,"llogis",
                               start=list(shape=1))
flnProd_Gas_HD_2017 <- fitdist(Gas_H_PPW_2017$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Gas_HD_2017 <- fitdist(Gas_H_PPW_2017$Prod_Per_Well_Per_Day,"llogis",
                               start=list(shape=median(Gas_H_PPW_2017$Prod_Per_Well_Per_Day)))
flnProd_Shale_HD_2017 <- fitdist(Shale_H_PPW_2017$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Shale_HD_2017 <- fitdist(Shale_H_PPW_2017$Prod_Per_Well_Per_Day,"llogis",
                                 start=list(shape=median(Shale_H_PPW_2017$Prod_Per_Well_Per_Day)))
flnProd_CBM_HD_2017 <- fitdist(CBM_H_PPW_2017$Prod_Per_Well_Per_Day,"lnorm")
fllProd_CBM_HD_2017 <- fitdist(CBM_H_PPW_2017$Prod_Per_Well_Per_Day,"llogis",
                               start=list(shape=median(CBM_H_PPW_2017$Prod_Per_Well_Per_Day)))
#2016 Data
flnProd_Gas_VD_2016 <- fitdist(Gas_V_PPW_2016$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Gas_VD_2016 <- fitdist(Gas_V_PPW_2016$Prod_Per_Well_Per_Day,"llogis",
                               start=list(shape=median(Gas_V_PPW_2016$Prod_Per_Well_Per_Day)))
flnProd_Shale_VD_2016 <- fitdist(Shale_V_PPW_2016$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Shale_VD_2016 <- fitdist(Shale_V_PPW_2016$Prod_Per_Well_Per_Day,"llogis",
                                 start=list(shape=1))
flnProd_CBM_VD_2016 <- fitdist(CBM_V_PPW_2016$Prod_Per_Well_Per_Day,"lnorm")
fllProd_CBM_VD_2016 <- fitdist(CBM_V_PPW_2016$Prod_Per_Well_Per_Day,"llogis",
                               start=list(shape=1))
flnProd_Gas_HD_2016 <- fitdist(Gas_H_PPW_2016$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Gas_HD_2016 <- fitdist(Gas_H_PPW_2016$Prod_Per_Well_Per_Day,"llogis",
                               start=list(shape=median(Gas_H_PPW_2016$Prod_Per_Well_Per_Day)))
flnProd_Shale_HD_2016 <- fitdist(Shale_H_PPW_2016$Prod_Per_Well_Per_Day,"lnorm")
fllProd_Shale_HD_2016 <- fitdist(Shale_H_PPW_2016$Prod_Per_Well_Per_Day,"llogis",
                                 start=list(shape=median(Shale_H_PPW_2016$Prod_Per_Well_Per_Day)))
flnProd_CBM_HD_2016 <- fitdist(CBM_H_PPW_2016$Prod_Per_Well_Per_Day,"lnorm")
fllProd_CBM_HD_2016 <- fitdist(CBM_H_PPW_2016$Prod_Per_Well_Per_Day,"llogis",
                               start=list(shape=median(CBM_H_PPW_2016$Prod_Per_Well_Per_Day)))
#List Form 2017 Version
fProd_Gas_VD_2017 <- list(flnProd_Gas_VD_2017,fllProd_Gas_VD_2017)
fProd_Shale_VD_2017 <- list(flnProd_Shale_VD_2017,fllProd_Shale_VD_2017)
fProd_CBM_VD_2017 <- list(flnProd_CBM_VD_2017,fllProd_CBM_VD_2017)
fProd_Gas_HD_2017 <- list(flnProd_Gas_HD_2017,fllProd_Gas_HD_2017)
fProd_Shale_HD_2017 <- list(flnProd_Shale_HD_2017,fllProd_Shale_HD_2017)
fProd_CBM_HD_2017 <- list(flnProd_CBM_HD_2017,fllProd_CBM_HD_2017)
#List Form 2016 Version
fProd_Gas_VD_2016 <- list(flnProd_Gas_VD_2016,fllProd_Gas_VD_2016)
fProd_Shale_VD_2016 <- list(flnProd_Shale_VD_2016,fllProd_Shale_VD_2016)
fProd_CBM_VD_2016 <- list(flnProd_CBM_VD_2016,fllProd_CBM_VD_2016)
fProd_Gas_HD_2016 <- list(flnProd_Gas_HD_2016,fllProd_Gas_HD_2016)
fProd_Shale_HD_2016 <- list(flnProd_Shale_HD_2016,fllProd_Shale_HD_2016)
fProd_CBM_HD_2016 <- list(flnProd_CBM_HD_2016,fllProd_CBM_HD_2016)
#Assigning as an object for Dens Comp and CDF
#Probability Distribution Fitting
ProbDistPPW <- function(PPW_list){denscomp(PPW_list,
                                           xlab="Production Per Well (Mm3)",
                                           plotstyle="ggplot",
                                           legendtext=c("Log-Normal","Log-Logistic"))}
#2017 Data
ProbDistPPW_Gas_VD_2017 <- ProbDistPPW(fProd_Gas_VD_2017)
ProbDistPPW_Shale_VD_2017 <- ProbDistPPW(fProd_Shale_VD_2017)
ProbDistPPW_CBM_VD_2017 <- ProbDistPPW(fProd_CBM_VD_2017)
ProbDistPPW_Gas_HD_2017 <- ProbDistPPW(fProd_Gas_HD_2017)
ProbDistPPW_Shale_HD_2017 <- ProbDistPPW(fProd_Shale_HD_2017)
ProbDistPPW_CBM_HD_2017 <- ProbDistPPW(fProd_CBM_HD_2017)
#2016 Data
ProbDistPPW_Gas_VD_2016 <- ProbDistPPW(fProd_Gas_VD_2016)
ProbDistPPW_Shale_VD_2016 <- ProbDistPPW(fProd_Shale_VD_2016)
ProbDistPPW_CBM_VD_2016 <- ProbDistPPW(fProd_CBM_VD_2016)
ProbDistPPW_Gas_HD_2016 <- ProbDistPPW(fProd_Gas_HD_2016)
ProbDistPPW_Shale_HD_2016 <- ProbDistPPW(fProd_Shale_HD_2016)
ProbDistPPW_CBM_HD_2016 <- ProbDistPPW(fProd_CBM_HD_2016)
#Cumulative Distribution Fitting
CProbDistPPW <- function(PPW_list){cdfcomp(PPW_list,
                                           xlab="Production Per Well (Mm3)",
                                           plotstyle="ggplot",
                                           legendtext=c("Log-Normal","Log-Logistic"))}
#2017 Data
CProbDistPPW_Gas_VD_2017 <- CProbDistPPW(fProd_Gas_VD_2017)
CProbDistPPW_Shale_VD_2017 <- CProbDistPPW(fProd_Shale_VD_2017)
CProbDistPPW_CBM_VD_2017 <- CProbDistPPW(fProd_CBM_VD_2017)
CProbDistPPW_Gas_HD_2017 <- CProbDistPPW(fProd_Gas_HD_2017)
CProbDistPPW_Shale_HD_2017 <- CProbDistPPW(fProd_Shale_HD_2017)
CProbDistPPW_CBM_HD_2017 <- CProbDistPPW(fProd_CBM_HD_2017)
#2016 Data
CProbDistPPW_Gas_VD_2016 <- CProbDistPPW(fProd_Gas_VD_2016)
CProbDistPPW_Shale_VD_2016 <- CProbDistPPW(fProd_Shale_VD_2016)
CProbDistPPW_CBM_VD_2016 <- CProbDistPPW(fProd_CBM_VD_2016)
CProbDistPPW_Gas_HD_2016 <- CProbDistPPW(fProd_Gas_HD_2016)
CProbDistPPW_Shale_HD_2016 <- CProbDistPPW(fProd_Shale_HD_2016)
CProbDistPPW_CBM_HD_2016 <- CProbDistPPW(fProd_CBM_HD_2016)
#Prob Dist and CDF Plotting function
PPW_Plot<-function(info,title){info+
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
#2017 Data
ProbDistPlot_PPW_Gas_VD_2017 <- PPW_Plot(ProbDistPPW_Gas_VD_2017,
                                    "Histogram and Distribution Curves of Alberta Production Per Well Data (Gas/Vertical) 2017")
ProbDistPlot_PPW_Shale_VD_2017 <- PPW_Plot(ProbDistPPW_Shale_VD_2017,
                                      "Histogram and Distribution Curves of Alberta Production Per Well Data (Shale/Vertical) 2017")
ProbDistPlot_PPW_CBM_VD_2017 <- PPW_Plot(ProbDistPPW_CBM_VD_2017,
                                    "Histogram and Distribution Curves of Alberta Production Per Well Data (CBM/Vertical) 2017")
ProbDistPlot_PPW_Gas_HD_2017 <- PPW_Plot(ProbDistPPW_Gas_HD_2017,
                                    "Histogram and Distribution Curves of Alberta Production Per Well Data (Gas/Horizontal) 2017")
ProbDistPlot_PPW_Shale_HD_2017 <- PPW_Plot(ProbDistPPW_Shale_HD_2017,
                                      "Histogram and Distribution Curves of Alberta Production Per Well Data (Shale/Horizontal) 2017")
ProbDistPlot_PPW_CBM_HD_2017 <- PPW_Plot(ProbDistPPW_CBM_HD_2017,
                                    "Histogram and Distribution Curves of Alberta Production Per Well Data (CBM/Horizontal) 2017")
CProbDistPlot_PPW_Gas_VD_2017 <- PPW_Plot(CProbDistPPW_Gas_VD_2017,
                                     "Cumulative Distribution Curves of Alberta Production Per Well Data (Gas/Vertical) 2017")
CProbDistPlot_PPW_Shale_VD_2017 <- PPW_Plot(CProbDistPPW_Shale_VD_2017,
                                       "Cumulative Distribution Curves of Alberta Production Per Well Data (Shale/Vertical) 2017")
CProbDistPlot_PPW_CBM_VD_2017 <- PPW_Plot(CProbDistPPW_CBM_VD_2017,
                                     "Cumulative Distribution Curves of Alberta Production Per Well Data (CBM/Vertical)2017")
CProbDistPlot_PPW_Gas_HD_2017 <- PPW_Plot(CProbDistPPW_Gas_HD_2017,
                                     "Cumulative Distribution Curves of Alberta Production Per Well Data (Gas/Horizontal) 2017")
CProbDistPlot_PPW_Shale_HD_2017 <- PPW_Plot(CProbDistPPW_Shale_HD_2017,
                                       "Cumulative Distribution Curves of Alberta Production Per Well Data (Shale/Horizontal) 2017")
CProbDistPlot_PPW_CBM_HD_2017 <- PPW_Plot(CProbDistPPW_CBM_HD_2017,
                                     "Cumulative Distribution Curves of Alberta Production Per Well Data (CBM/Horizontal) 2017")
#2016 Data
ProbDistPlot_PPW_Gas_VD_2016 <- PPW_Plot(ProbDistPPW_Gas_VD_2016,
                                         "Histogram and Distribution Curves of Alberta Production Per Well Data (Gas/Vertical) 2016")
ProbDistPlot_PPW_Shale_VD_2016 <- PPW_Plot(ProbDistPPW_Shale_VD_2016,
                                           "Histogram and Distribution Curves of Alberta Production Per Well Data (Shale/Vertical) 2016")
ProbDistPlot_PPW_CBM_VD_2016 <- PPW_Plot(ProbDistPPW_CBM_VD_2016,
                                         "Histogram and Distribution Curves of Alberta Production Per Well Data (CBM/Vertical) 2016")
ProbDistPlot_PPW_Gas_HD_2016 <- PPW_Plot(ProbDistPPW_Gas_HD_2016,
                                         "Histogram and Distribution Curves of Alberta Production Per Well Data (Gas/Horizontal) 2016")
ProbDistPlot_PPW_Shale_HD_2016 <- PPW_Plot(ProbDistPPW_Shale_HD_2016,
                                           "Histogram and Distribution Curves of Alberta Production Per Well Data (Shale/Horizontal) 2016")
ProbDistPlot_PPW_CBM_HD_2016 <- PPW_Plot(ProbDistPPW_CBM_HD_2016,
                                         "Histogram and Distribution Curves of Alberta Production Per Well Data (CBM/Horizontal) 2016")
CProbDistPlot_PPW_Gas_VD_2016 <- PPW_Plot(CProbDistPPW_Gas_VD_2016,
                                          "Cumulative Distribution Curves of Alberta Production Per Well Data (Gas/Vertical) 2016")
CProbDistPlot_PPW_Shale_VD_2016 <- PPW_Plot(CProbDistPPW_Shale_VD_2016,
                                            "Cumulative Distribution Curves of Alberta Production Per Well Data (Shale/Vertical) 2016")
CProbDistPlot_PPW_CBM_VD_2016 <- PPW_Plot(CProbDistPPW_CBM_VD_2016,
                                          "Cumulative Distribution Curves of Alberta Production Per Well Data (CBM/Vertical) 2016")
CProbDistPlot_PPW_Gas_HD_2016 <- PPW_Plot(CProbDistPPW_Gas_HD_2016,
                                          "Cumulative Distribution Curves of Alberta Production Per Well Data (Gas/Horizontal) 2016")
CProbDistPlot_PPW_Shale_HD_2016 <- PPW_Plot(CProbDistPPW_Shale_HD_2016,
                                            "Cumulative Distribution Curves of Alberta Production Per Well Data (Shale/Horizontal) 2016")
CProbDistPlot_PPW_CBM_HD_2016 <- PPW_Plot(CProbDistPPW_CBM_HD_2016,
                                          "Cumulative Distribution Curves of Alberta Production Per Well Data (CBM/Horizontal) 2016")
#Statistics Fitting for 2016-2017 Data
PPW_Statistics <- function(PPW_list,ln_PPW,ll_PPW){
  GOF<-gofstat(PPW_list)
  LogLik<-list(logln=logLik(ln_PPW),
               logll=logLik(ll_PPW))
  Summary<-list(Summaryln=summary(ln_PPW),
                Summaryll=summary(ll_PPW))
  Stats<- list(GOF=GOF,
               LogLikelihood=LogLik,
               Summary=Summary)
}
#2017 Data
PPW_Gas_VD_Statistics_2017 <- PPW_Statistics(fProd_Gas_VD_2017,
                                        flnProd_Gas_VD_2017,
                                        fllProd_Gas_VD_2017)
PPW_Shale_VD_Statistics_2017 <- PPW_Statistics(fProd_Shale_VD_2017,
                                             flnProd_Shale_VD_2017,
                                             fllProd_Shale_VD_2017)
PPW_CBM_VD_Statistics_2017 <- PPW_Statistics(fProd_CBM_VD_2017,
                                             flnProd_CBM_VD_2017,
                                             fllProd_CBM_VD_2017)
PPW_Gas_HD_Statistics_2017 <- PPW_Statistics(fProd_Gas_HD_2017,
                                             flnProd_Gas_HD_2017,
                                             fllProd_Gas_HD_2017)
PPW_Shale_HD_Statistics_2017 <- PPW_Statistics(fProd_Shale_HD_2017,
                                             flnProd_Shale_HD_2017,
                                             fllProd_Shale_HD_2017)
PPW_CBM_HD_Statistics_2017 <- PPW_Statistics(fProd_CBM_HD_2017,
                                             flnProd_CBM_HD_2017,
                                             fllProd_CBM_HD_2017)
#2016 Data
PPW_Gas_VD_Statistics_2016 <- PPW_Statistics(fProd_Gas_VD_2016,
                                             flnProd_Gas_VD_2016,
                                             fllProd_Gas_VD_2016)
PPW_Shale_VD_Statistics_2016 <- PPW_Statistics(fProd_Shale_VD_2016,
                                               flnProd_Shale_VD_2016,
                                               fllProd_Shale_VD_2016)
PPW_CBM_VD_Statistics_2016 <- PPW_Statistics(fProd_CBM_VD_2016,
                                             flnProd_CBM_VD_2016,
                                             fllProd_CBM_VD_2016)
PPW_Gas_HD_Statistics_2016 <- PPW_Statistics(fProd_Gas_HD_2016,
                                             flnProd_Gas_HD_2016,
                                             fllProd_Gas_HD_2016)
PPW_Shale_HD_Statistics_2016 <- PPW_Statistics(fProd_Shale_HD_2016,
                                               flnProd_Shale_HD_2016,
                                               fllProd_Shale_HD_2016)
PPW_CBM_HD_Statistics_2016 <- PPW_Statistics(fProd_CBM_HD_2016,
                                             flnProd_CBM_HD_2016,
                                             fllProd_CBM_HD_2016)
