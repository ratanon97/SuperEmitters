#--------------------------------------------
#Set working directory for Production Data (Medicine Hat)
getwd()
setwd("C:\\Users\\KOMSUN\\Documents\\Files from Imperial Laptop\\Modules\\Research Project\\Data Collection\\CSV Inputs")
NGProductionMedicine <- read.csv("Alberta Production Data for Medicine Hat.csv",row.names=1)
#Note: the production data in this script are from the Medicine Hat region
#Gas source in this region study is CONVENTIONAL GAS
#This data set was collected in Autumn 2016
#--------------------------------------------
#Adjustments to the Data Frame
colnames(NGProductionMedicine)[1:7] <- c("Mean_CH4_m3day",
                                         "Median_CH4_m3day",
                                         "Mdl_CH4_m3day",
                                         "SD_CH4_m3day",
                                         "SE_CH4_m3day",
                                         "Min_CH4_m3day",
                                         "Max_CH4_m3day")
library(dplyr)
NGProductionMedicine <- NGProductionMedicine %>% 
  mutate(LN_Mean_CH4_m3day=log(Mean_CH4_m3day))
#Skewness Analysis
Skewness_MedicineHat <- NGProductionMedicine %>% 
  dplyr::select(Mean_CH4_m3day,Median_CH4_m3day) %>% 
  mutate(Rank=rank(Mean_CH4_m3day,ties.method="max")) %>% 
  filter(Rank >= 89) %>% 
  mutate(Region=factor("Medicine Hat"))
Agg_Skewness_MedicineHat <- Skewness_MedicineHat %>% 
  dplyr::select(Mean_CH4_m3day,Region)
colnames(Agg_Skewness_MedicineHat)[1] <- c("CH4_emission_m3_per_day")
TopDown_5_Percent <- rbind.data.frame(Agg_Skewness_RedDeer,
                                      Agg_Skewness_MedicineHat)
Avg_Mean_Emission_MedHat <- mean(NGProductionMedicine$Mean_CH4_m3day)
Avg_Mean_TopDown_Emission <- mean(c(Avg_Mean_Emission_MedHat,AbsRedDeerEmission))
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
#Lorenz Curve
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
#Mean/Day version
library(fitdistrplus) #Load all the fitdistr to plot the probability distribution functions
library(actuar) #Load the Log-Logistic Distribution function
fwProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"weibull") #Weibull Distribution
fgProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"gamma") #Gamma Distribution
flnProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"lnorm")# Log-Normal Distribution
fllProMedicine <- fitdist(NGProductionMedicine$Mean_CH4_m3day,"llogis") #Log-Logistic Distribution 
ProbDistMed <- denscomp(list(fwProMedicine,fgProMedicine,flnProMedicine,fllProMedicine),
                        xlab="Methane Emissions (Mean m3/day)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
CProbDistMed <- cdfcomp(list(fwProMedicine,fgProMedicine,flnProMedicine,fllProMedicine),
               xlab="ln(Methane Emissions (m3/day))",
               ylab="Cumulative Distribution Function",
               plotstyle="ggplot",
               legendtext=c("Weibull",
                            "Gamma",
                            "Log-Normal",
                            "Log-Logistic"))
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
GOFProMed<-gofstat(list(fwProMedicine,
                          fgProMedicine,
                          flnProMedicine,
                          fllProMedicine))
LogLikProMed<-list(logW=logLik(fwProMedicine),
                  logG=logLik(fgProMedicine),
                  logln=logLik(flnProMedicine),
                  logll=logLik(fllProMedicine))
SummaryProMed <- list(SummaryW=summary(fwProMedicine),
                          SummaryG=summary(fgProMedicine),
                          Summaryln=summary(flnProMedicine),
                          Summaryll=summary(fllProMedicine))
StatsProMed <- list(GOF=GOFProMed,
                    LogLikelihood=LogLikProMed,
                    Summary=SummaryProMed)
#----------------------------------------------
#Monte Carlo Testing
set.seed(123)
MC_Function_PDMH <- function(df,mean,sd){
  MH_Methane_Throughput_MC <- (df$Mean_CH4_m3day/rlnorm(nrow(df),mean,sd))*100
  return(MH_Methane_Throughput_MC)
}
Medicine_Sim <- replicate(10000,MC_Function_PDMH(NGProductionMedicine,Mean_LN_EUR,SD_LN_EUR))
Medicine_Sim <- data.frame(Medicine_Sim)
#Calculations from Simulation
Mean_Medicine_Sim <- sapply(Medicine_Sim,mean)
Median_Medicine_Sim <- sapply(Medicine_Sim,median)
Q5_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.05))
Q25_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.25))
Q75_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.75))
Q95_Medicine_Sim <- sapply(Medicine_Sim,quantile,c(.95))
FilteredMean_Medicine_Sim<-Mean_Medicine_Sim[Mean_Medicine_Sim <= 100]
FilteredQ75_Medicine_Sim <- Q75_Medicine_Sim[Q75_Medicine_Sim<=100]
FilteredQ95_Medicine_Sim <- Q95_Medicine_Sim[Q95_Medicine_Sim<=100]
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
#Visualisation Testing
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
#---------------------------------------------
#Multiple Plots
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
}
for(h in 1001:2000){
  lines(ecdf(Medicine_Sim[,h]),
        col=alpha("cadetblue2",0.3))
}
for(l in 2001:3000){
  lines(ecdf(Medicine_Sim[,l]),
        col=alpha("darkorchid2",0.3))
}
for(p in 3001:4000){
  lines(ecdf(Medicine_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
}
for(o in 4001:5000){
  lines(ecdf(Medicine_Sim[,o]),
        col=alpha("blueviolet",0.3))
}
for(k in 5001:6000){
  lines(ecdf(Medicine_Sim[,k]),
        col=alpha("darkslateblue",0.3))
}
for(s in 6001:7000){
  lines(ecdf(Medicine_Sim[,s]),
        col=alpha("darkmagenta",0.3))
}
for(u in 7001:8000){
  lines(ecdf(Medicine_Sim[,u]),
        col=alpha("deeppink1",0.3))
}
for(x in 8001:9000){
  lines(ecdf(Medicine_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
}
for(f in 9001:10000){
  lines(ecdf(Medicine_Sim[,f]),
        col=alpha("darkorchid",0.3))
}