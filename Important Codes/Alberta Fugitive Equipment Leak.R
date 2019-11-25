#Set Working Directory for ClearStone Data
getwd() #See the working directory of the associated computer to import the data
setwd("C:\\Users\\KOMSUN\\Documents\\Files from Imperial Laptop\\Modules\\Research Project\\Data Collection\\CSV Inputs")
ClearStone <- read.csv("Alberta Fugitive Equipment Leak Data.csv") 
#---------------------------------------------------------
#Notes: This is bottom up method
#GWP Used: GWP100 of 25 for Methane and GWP100 of 298 for NO2
#Year 2017 for this script
#----------------------------------------------------------
#Data Frame Preparation
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
CH4_vol_percent <- 0.9287
#DPLYR Library Calcultion
library(dplyr)
library(tidyr)
ClearStone <- ClearStone %>% 
  mutate(CH4_Leak_Rate_m3_per_hour_per_source=THC_Leak_Rate_m3_per_hour_per_source * CH4_vol_percent) %>% 
  mutate(CH4_Leak_Rate_m3_per_day_per_source=CH4_Leak_Rate_m3_per_hour_per_source*24) %>% 
  mutate(CH4_Leak_Rate_kg_per_day_per_source=CH4_Leak_Rate_m3_per_day_per_source*0.8) %>% 
  mutate(CH4_Leak_Rate_Mm3_per_day_per_source=CH4_Leak_Rate_m3_per_day_per_source/1000000)
Avg_Abs_Emission_Rate <- 21.51347 #Important note: Combined with the Fugitive emissions from GreenPath
write.csv(ClearStone,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Clearstone.csv")
#-------------------------------------------------------------------------
#Reciprocating Compressor
ClearStone_Compressor <- ClearStone %>% 
  filter(Major_Equipment=="Reciprocating Compressor") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_Compressor$Major_Equipment <- as.numeric(ClearStone_Compressor$Major_Equipment)
ClearStone_Compressor <- ClearStone_Compressor %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment)))
write.csv(ClearStone_Compressor,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Compressor.csv")
#Skewness Analysis
In_Depth_ClearStone_Compressor <- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Reciprocating Compressor") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank >= 49)
#Separators
ClearStone_Separator <- ClearStone %>% 
  filter(Major_Equipment=="Separator") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_Separator$Major_Equipment <- as.numeric(ClearStone_Separator$Major_Equipment)
ClearStone_Separator <- ClearStone_Separator %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) 
write.csv(ClearStone_Separator,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Separator.csv")
#Skewness Analysis
In_Depth_ClearStone_Separator <- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Separator") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank >= 35)
#Wellheads
ClearStone_Wellhead <- ClearStone %>% 
  filter(Major_Equipment=="Wellhead") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_Wellhead$Major_Equipment <- as.numeric(ClearStone_Wellhead$Major_Equipment)
ClearStone_Wellhead <- ClearStone_Wellhead %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) 
write.csv(ClearStone_Wellhead,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Wellhead.csv")
#Skewness Analysis
In_Depth_ClearStone_Wellhead <- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Wellhead") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank >= 44)
#Dehydrator Glycol
ClearStone_DG <- ClearStone %>% 
  filter(Major_Equipment=="Dehydrator - Glycol") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_DG$Major_Equipment <- as.numeric(ClearStone_DG$Major_Equipment)
ClearStone_DG <- ClearStone_DG %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment)))
write.csv(ClearStone_DG,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_DG.csv")
#Skewness Analysis
In_Depth_ClearStone_DG<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Dehydrator - Glycol") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank == 8)
#Flare Knockout Drum
ClearStone_FKD <- ClearStone %>% 
  filter(Major_Equipment=="Flare KnockOut Drum") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_FKD$Major_Equipment <- as.numeric(ClearStone_FKD$Major_Equipment)
ClearStone_FKD <- ClearStone_FKD %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max"))
write.csv(ClearStone_FKD,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_FKD.csv")
#Skewness Analysis
In_Depth_ClearStone_FKD<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Flare KnockOut Drum") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank == 8)
#Production Tank (fixed roof)
ClearStone_PTFR <- ClearStone %>% 
  filter(Major_Equipment=="Production Tank (fixed roof)") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_PTFR$Major_Equipment <- as.numeric(ClearStone_PTFR$Major_Equipment)
ClearStone_PTFR <- ClearStone_PTFR %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) 
write.csv(ClearStone_PTFR,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_PTFR.csv")
#Skewness Analysis
In_Depth_ClearStone_PTFR<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Production Tank (fixed roof)") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank == 13)
#Gas Pipeline Header
ClearStone_GPH <- ClearStone %>% 
  filter(Major_Equipment=="Gas Pipeline Header") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_GPH$Major_Equipment <- as.numeric(ClearStone_GPH$Major_Equipment)
ClearStone_GPH <- ClearStone_GPH %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment)))
write.csv(ClearStone_GPH,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_GPH.csv")
#Skewness Analysis
In_Depth_ClearStone_GPH<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Gas Pipeline Header") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank == 7)
#Line Heater
#Too little data to do a super emitter analysis
ClearStone_LH <- ClearStone %>% 
  filter(Major_Equipment=="Line Heater") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_LH$Major_Equipment <- as.numeric(ClearStone_LH$Major_Equipment)
ClearStone_LH <- ClearStone_LH %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment)))
#Liquid Pump
#Too little data to do a super emitter analysis
ClearStone_LP <- ClearStone %>% 
  filter(Major_Equipment=="Liquid Pump") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_LP$Major_Equipment <- as.numeric(ClearStone_LP$Major_Equipment)
ClearStone_LP <- ClearStone_LP %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) 
#Screw Compressor
ClearStone_SC<- ClearStone %>% 
filter(Major_Equipment=="Screw Compressor") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_SC$Major_Equipment <- as.numeric(ClearStone_SC$Major_Equipment)
ClearStone_SC <- ClearStone_SC %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) 
write.csv(ClearStone_SC,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_SC.csv")
#Skewness Analysis
In_Depth_ClearStone_SC<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Screw Compressor") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank == 8)
#Electric Driver Compressors
#Indicate some possible use of electric drivers in the report
ClearStone_ED<- ClearStone %>% 
  filter(Major_Equipment=="Screw Compressor - Electric Driver" | Major_Equipment=="Reciprocating Compressor - Electric Driver") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_ED$Major_Equipment <- as.numeric(ClearStone_ED$Major_Equipment)
ClearStone_ED <- ClearStone_ED %>% 
  mutate(Number_Of_Sites_CS=cumsum(Major_Equipment)/max(sum(Major_Equipment))) 
write.csv(ClearStone_ED,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_ED.csv")
#Skewness Analysis
In_Depth_ClearStone_ED<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Major_Equipment=="Screw Compressor - Electric Driver" | Major_Equipment=="Reciprocating Compressor - Electric Driver") %>% 
  mutate(Major_Equipment=gsub("Screw Compressor - Electric Driver","Electric Driver Compressor",Major_Equipment)) %>% 
  mutate(Major_Equipment=gsub("Reciprocating Compressor - Electric Driver","Electric Driver Compressor",Major_Equipment)) %>% 
  mutate(Major_Equipment=factor(Major_Equipment)) %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank == 6)
#--------------------------------------------------------------
#Top 5 Analysis
#Equipment List. The purpose is to see the 5% amount for each dataset
Equipment_List <- list(ClearStone_Compressor,
                       ClearStone_Separator,
                       ClearStone_Wellhead,
                       ClearStone_DG,
                       ClearStone_FKD,
                       ClearStone_PTFR,
                       ClearStone_GPH,
                       ClearStone_SC,
                       ClearStone_ED)
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
                       Fug_GreenPath_WH$Emission_Rate_cmd)
Top_5_Function <- function(lst){ceiling(0.05*nrow(lst))}
Top_5_Percent_List <- lapply(Equipment_List,Top_5_Function)
Skewness_Equipment_List <- lapply(SK_Equipment_List,skewness)
Kurtosis_Equipment_List <- lapply(SK_Equipment_List,kurtosis)
Skewness_Equipment_Vector <- sapply(SK_Equipment_List,skewness)
Kurtosis_Equipment_Vector <- sapply(SK_Equipment_List,kurtosis)
names(Top_5_Percent_List) <- c("ClearStone_Compressor",
                               "ClearStone_Separator",
                               "ClearStone_Wellhead",
                               "ClearStone_DG",
                               "ClearStone_FKD",
                               "ClearStone_PTFR",
                               "ClearStone_GPH",
                               "ClearStone_SC",
                               "ClearStone_ED")
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
                              "Fug_GreenPath_WH")
#Combine Data Frame by Row
Equipment_Top_5 <- rbind.data.frame(In_Depth_ClearStone_Compressor,
                                    In_Depth_ClearStone_Separator,
                                    In_Depth_ClearStone_Wellhead,
                                    In_Depth_ClearStone_DG,
                                    In_Depth_ClearStone_FKD,
                                    In_Depth_ClearStone_PTFR,
                                    In_Depth_ClearStone_GPH,
                                    In_Depth_ClearStone_SC,
                                    In_Depth_ClearStone_ED)
Finalised_Equipment_Top_5 <- rbind.data.frame(Equipment_Top_5,
                                              Equipment_Top_5_GreenPath)
Finalised_Equipment_Top_5 <- Finalised_Equipment_Top_5 %>% 
  mutate(Major_Equipment=gsub("Filter/Separation","Separator",Major_Equipment))
write.csv(Finalised_Equipment_Top_5,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Top 5 Equipment Scatter Analysis.csv")
#-------------------------------------------------------------------------------
#Segment Population
##Gas Batteries
ClearStone_Battery <- ClearStone %>% 
  filter(Petrinex_Facility_Subtype!="Compressor Station"&Petrinex_Facility_Subtype!="Gas Gathering System")
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Multiwell effluent","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype)
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Multiwell Group","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype)
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Multiwell proration outside SE AB","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype)
ClearStone_Battery$Petrinex_Facility_Subtype <- gsub("Gas Single","Gas Battery",ClearStone_Battery$Petrinex_Facility_Subtype)
ClearStone_Battery$Petrinex_Facility_Subtype <- factor(ClearStone_Battery$Petrinex_Facility_Subtype)
ClearStone_Battery <- ClearStone_Battery %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_Battery$Petrinex_Facility_Subtype <- as.numeric(ClearStone_Battery$Petrinex_Facility_Subtype)
ClearStone_Battery <- ClearStone_Battery %>%
  mutate(Number_Of_Sites_CS=cumsum(Petrinex_Facility_Subtype)/max(sum(Petrinex_Facility_Subtype)))
write.csv(ClearStone_Battery,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Battery.csv")
#Skewness Analysis
In_Depth_ClearStone_Battery<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Petrinex_Facility_Subtype!="Compressor Station"&Petrinex_Facility_Subtype!="Gas Gathering System") %>% 
  mutate(Petrinex_Facility_Subtype=gsub("Gas Multiwell effluent","Gas Battery",Petrinex_Facility_Subtype)) %>% 
  mutate(Petrinex_Facility_Subtype=gsub("Gas Multiwell Group","Gas Battery",Petrinex_Facility_Subtype)) %>% 
  mutate(Petrinex_Facility_Subtype=gsub("Gas Multiwell proration outside SE AB","Gas Battery",Petrinex_Facility_Subtype)) %>% 
  mutate(Petrinex_Facility_Subtype=gsub("Gas Single","Gas Battery",Petrinex_Facility_Subtype)) %>% 
  mutate(Petrinex_Facility_Subtype=factor(Petrinex_Facility_Subtype)) %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank >= 107)
#Compressor Stations
ClearStone_Compressor_Station <- ClearStone %>% 
  filter(Petrinex_Facility_Subtype=="Compressor Station") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_Compressor_Station$Petrinex_Facility_Subtype <- as.numeric(ClearStone_Compressor_Station$Petrinex_Facility_Subtype)
ClearStone_Compressor_Station <- ClearStone_Compressor_Station %>%
  mutate(Number_Of_Sites_CS=cumsum(Petrinex_Facility_Subtype)/max(sum(Petrinex_Facility_Subtype)))
write.csv(ClearStone_Compressor_Station,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_CS.csv")
#Skewness Analysis
In_Depth_ClearStone_Compressor_Station<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Petrinex_Facility_Subtype=="Compressor Station") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank == 19)
#Gas Gathering Systems
ClearStone_Gathering <- ClearStone %>% 
  filter(Petrinex_Facility_Subtype=="Gas Gathering System") %>% 
  mutate(Emission_CS=cumsum(CH4_Leak_Rate_m3_per_day_per_source)/max(sum(CH4_Leak_Rate_m3_per_day_per_source)))
ClearStone_Gathering$Petrinex_Facility_Subtype <- as.numeric(ClearStone_Gathering$Petrinex_Facility_Subtype)
ClearStone_Gathering <- ClearStone_Gathering %>%
  mutate(Number_Of_Sites_CS=cumsum(Petrinex_Facility_Subtype)/max(sum(Petrinex_Facility_Subtype)))
write.csv(ClearStone_Gathering,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\ClearStone_Gathering.csv")
#Skewness Analysis
In_Depth_ClearStone_Gathering<- ClearStone %>% 
  dplyr::select(Petrinex_Facility_Subtype,Emission_Type,Major_Equipment,Component_Service_Type,Component_Type,CH4_Leak_Rate_m3_per_day_per_source) %>% 
  filter(Petrinex_Facility_Subtype=="Gas Gathering System") %>% 
  mutate(Rank=rank(CH4_Leak_Rate_m3_per_day_per_source,ties.method="max")) %>% 
  filter(Rank >= 61)
Segment_Top_5 <- rbind.data.frame(In_Depth_ClearStone_Battery,
                                  In_Depth_ClearStone_Compressor_Station,
                                  In_Depth_ClearStone_Gathering)
Finalised_Segment_Top_5 <- rbind.data.frame(Segment_Top_5,
                                            Segment_Top_5_GreenPath)
write.csv(Finalised_Segment_Top_5,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Top 5 Segment Scatter Analysis.csv")
#----------------------------------------------------------
#Data Visualisation
library(ggplot2)
library(gglorenz)
library(ineq)
ClearStone_Histo <- ggplot(data=ClearStone,aes(x=CH4_Leak_Rate_m3_per_day_per_source))
ClearStone_Histo + geom_histogram(aes(y=stat(density)),
                                  fill="LightBlue",
                           colour="Black",
                           size=1)+
  scale_x_continuous(trans="log")+
    # facet_grid(Emission_Type~.,
    #           scales="free",
    #            space="free")+
  xlab("Methane Emissions (m3/day)")+
  ylab("Density")+
  ggtitle("Alberta Fugitive Equipment Leak Data 2017")+ #Making the graph look niceer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20),
        strip.text=element_text(size=10))
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
#Plotting Multiple Lorenz Curves
#Equipment Population
plot(Lc(ClearStone_Compressor$CH4_Leak_Rate_m3_per_day_per_source),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Equipment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
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
#-----------------------------------------------------------
#Segment Population
plot(Lc(ClearStone_Battery$CH4_Leak_Rate_m3_per_day_per_source),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Segment Population Analysis",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
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
#EQuipment Population Version
Equipment_Top_5_Scatter <- ggplot(data=Finalised_Equipment_Top_5)
Equipment_Top_5_Plot<- Equipment_Top_5_Scatter + geom_point(aes(x=ceiling(Avg_Abs_Emission_Rate),
                                         y=CH4_Leak_Rate_m3_per_day_per_source,
                                         colour=Major_Equipment,
                                         size=CH4_Leak_Rate_m3_per_day_per_source))+
  xlim(21.975,22.025)+
  xlab("Average Absolute Emission Rate (m3/day)")+
  ylab("Methane Leak Rate (m3/day)")+
  scale_y_continuous(breaks = ceiling(seq(min(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          max(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          by = 50)))+
  ggtitle("Equipment Methane Emissions against Average Absolute Emission Rate")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
Equipment_Top_5_Plot$labels$size = "Methane Emissions (m3/day)"
Equipment_Top_5_Plot$labels$colour = "Equipment"
Equipment_Top_5_Plot + coord_cartesian(ylim=c(0,105))+
  scale_y_continuous(breaks = ceiling(seq(min(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          max(Finalised_Equipment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          by = 5)))
#Segment Population Version
Segment_Top_5_Scatter <- ggplot(data=Finalised_Segment_Top_5)
Segment_Top_5_Plot<- Segment_Top_5_Scatter + geom_point(aes(x=ceiling(Avg_Abs_Emission_Rate),
                                                                y=CH4_Leak_Rate_m3_per_day_per_source,
                                                                colour=Petrinex_Facility_Subtype,
                                                                size=CH4_Leak_Rate_m3_per_day_per_source))+
  xlim(21.975,22.025)+
  xlab("Average Absolute Emission Rate (m3/day)")+
  ylab("Methane Leak Rate (m3/day)")+
  scale_y_continuous(breaks = ceiling(seq(min(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                          max(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source), 
                                          by = 50)))+
  ggtitle("Segment Methane Emissions against Average Absolute Emission Rate")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
Segment_Top_5_Plot$labels$size = "Methane Emissions (m3/day)"
Segment_Top_5_Plot$labels$colour = "Segment"
Segment_Top_5_Plot + coord_cartesian(ylim=c(25,100))+
scale_y_continuous(breaks = ceiling(seq(min(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                        max(Finalised_Segment_Top_5$CH4_Leak_Rate_m3_per_day_per_source),
                                        by = 5)))
#-------------------------------------------------------------
#Probability Distribution Fitting
library(fitdistrplus) #Load all the fitdistr to plot the probability distribution functions
library(actuar) #Load the Log-Logistic Distribution function
library(EnvStats)
fwFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"weibull") #Weibull Distribution
fgFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"gamma") #Gamma Distribution
flnFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"lnorm")# Log-Normal Distribution
fllFug <- fitdist(ClearStone$CH4_Leak_Rate_kg_per_day_per_source,"llogis") #Log-Logistic Distribution 
ProbDistFug <- denscomp(list(fwFug,fgFug,flnFug,fllFug),
                        xlab="Methane Emissions (m3/day)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))

CProbDistFug <- cdfcomp(list(fwFug,fgFug,flnFug,fllFug),
                        xlab="ln(Methane Emissions (m3/day))",
                        ylab="Cumulative Distribution Function",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
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
#Summary Statistics
GOFFug<-gofstat(list(fwFug,
                       fgFug,
                       flnFug,
                       fllFug))
LoglikFug<-list(logW=logLik(fwFug),
                  logG=logLik(fgFug),
                  logln=logLik(flnFug),
                  logll=logLik(fllFug))
SummaryFug<-list(SummaryW=summary(fwFug),
                  SummaryG=summary(fgFug),
                  Summaryln=summary(flnFug),
                  Summaryll=summary(fllFug))
StatsFug <- list(GOF=GOFFug,
                 Logliklihood=LoglikFug,
                 Summary=SummaryFug)
#Log-Normal is best at fitting these distributions
#----------------------------------------------
#Monte Carlo Testing
set.seed(123)
MC_Function_CS <- function(df,mean,sd){
  CS_Methane_Throughput <- (df$CH4_Leak_Rate_m3_per_day_per_source/rlnorm(nrow(df),mean,sd))*100
  return(CS_Methane_Throughput)
}
ClearStone_Sim <- replicate(10000,MC_Function_CS(ClearStone,Mean_LN_EUR,SD_LN_EUR))
ClearStone_Sim <- data.frame(ClearStone_Sim)
#----------------------------------------------
#Calculations from Simulation
Mean_ClearStone_Sim <- sapply(ClearStone_Sim,mean)
Median_ClearStone_Sim <- sapply(ClearStone_Sim,median)
Q5_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.05))
Q25_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.25))
Q75_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.75))
Q95_ClearStone_Sim <- sapply(ClearStone_Sim,quantile,c(.95))
FilteredMean_ClearStone_Sim<-Mean_ClearStone_Sim[Mean_ClearStone_Sim <= 100]
FilteredQ95_ClearStone_Sim <- Q95_ClearStone_Sim[Q95_ClearStone_Sim<=100]
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
#Multiple Plots
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
}
for(h in 1001:2000){
  lines(ecdf(ClearStone_Sim[,h]),
        col=alpha("cadetblue2",0.3))
}
for(l in 2001:3000){
  lines(ecdf(ClearStone_Sim[,l]),
        col=alpha("darkorchid2",0.3))
}
for(p in 3001:4000){
  lines(ecdf(ClearStone_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
}
for(o in 4001:5000){
  lines(ecdf(ClearStone_Sim[,o]),
        col=alpha("blueviolet",0.3))
}
for(k in 5001:6000){
  lines(ecdf(ClearStone_Sim[,k]),
        col=alpha("darkslateblue",0.3))
}
for(s in 6001:7000){
  lines(ecdf(ClearStone_Sim[,s]),
        col=alpha("darkmagenta",0.3))
}
for(u in 7001:8000){
  lines(ecdf(ClearStone_Sim[,u]),
        col=alpha("deeppink1",0.3))
}
for(x in 8001:9000){
  lines(ecdf(ClearStone_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
}
for(f in 9001:10000){
  lines(ecdf(ClearStone_Sim[,f]),
        col=alpha("darkorchid",0.3))
}
#----------------------------------------------
#Visualisation Testing
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