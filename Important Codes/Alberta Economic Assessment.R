#Economic Assessment Script
#Implement the Optimal Emission Reduction Model from CERI
AECO_C_NGPrice <- 1.48 #CAD/GJ #Price Year 2018
AECO_C_BaseNGPrice_Year2018_2028 <- c(1.48,1.75,2.19,2.55,2.62,2.76,3.01,3.15,3.35,3.44,3.52)
Mean_AECO <- mean(AECO_C_BaseNGPrice_Year2018_2028)
SD_AECO <- sd(AECO_C_BaseNGPrice_Year2018_2028)
Min_AECO <- min(AECO_C_BaseNGPrice_Year2018_2028)
Max_AECO <- max(AECO_C_BaseNGPrice_Year2018_2028)
NG_Energy_Content <- 53.20 #GJ/Tonne of NG
NGPrice <- AECO_C_NGPrice*NG_Energy_Content
NGPrice2018_2028 <- AECO_C_BaseNGPrice_Year2018_2028*NG_Energy_Content
LB_GWP100 <- 25
UB_GWP100 <- 36
#Data Preparation for Economic Datasets
library(tidyverse)
Economic_ClearStone <- ClearStone %>% 
  mutate(CH4_Leak_Rate_tonne_per_day_per_source=CH4_Leak_Rate_kg_per_day_per_source*1000) %>%
  mutate(CO2eq_Leak_Rate_tonne_per_day_per_source=CH4_Leak_Rate_tonne_per_day_per_source*LB_GWP100) %>% 
  mutate(CH4_Leak_Rate_tonne_per_year_per_source=CH4_Leak_Rate_tonne_per_day_per_source*365) %>% 
  mutate(CO2eq_Leak_Rate_tonne_per_year_per_source=CO2eq_Leak_Rate_tonne_per_day_per_source*365) %>% 
  mutate(Reduced_CH4_Leak_Rate_tonne_per_year_per_source=CH4_Leak_Rate_tonne_per_year_per_source *0.45) %>%
  mutate(Reduced_CH4_Leak_Rate_tonne_per_day_per_source=Reduced_CH4_Leak_Rate_tonne_per_year_per_source/365) %>%
  mutate(Reduced_CO2eq_Leak_Rate_tonne_per_year_per_source=Reduced_CH4_Leak_Rate_tonne_per_year_per_source*LB_GWP100) %>% 
  mutate(Reduced_CO2eq_Leak_Rate_tonne_per_day_per_source=Reduced_CH4_Leak_Rate_tonne_per_year_per_source/365) #GWP100 of 25
#IBM AIMS Technology
IBM_OPEX_Per_Year <- 1526 #USD Currency
AERIS_Cost_Per_Year <- 1000 #USD CUrrency
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
  xlab("Aeris Mitigating Cost ($CAD/tonne of CO2eq))")+
  ylab("Density")+
  ggtitle("Uncertainty Analysis of Aeris Mitigation Cost")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))