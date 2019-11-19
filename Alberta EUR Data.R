#--------------------------------------------
#Set working directory for EUR Data 
#Note: EUR Data is US-Based. Proxy dataset for Canadian Data. Year collected: 2016
getwd()
setwd("C:\\Users\\KOMSUN\\Documents\\Files from Imperial Laptop\\Modules\\Research Project\\Data Collection\\CSV Inputs")
EUR_Data <-read.csv("US_EUR_Data.csv")
#--------------------------------------------
#Data Frame Preparation
library(tidyverse)
EUR_Data <- EUR_Data %>% 
  mutate(Region=gsub("Marcellus","US",Region)) %>% 
  mutate(Region=gsub("Barnett","US",Region)) %>% 
  mutate(EUR_m3=Value*1000000) %>% 
  mutate(LN_EUR_m3=log(EUR_m3))
colnames(EUR_Data)[7] <- c("LN")
Mean_EUR <- mean(EUR_Data$Value)
SD_EUR <- sd(EUR_Data$Value)
Median_EUR <- median(EUR_Data$Value)
Mean_LN_EUR <- mean(EUR_Data$LN)
Mean_LN_EUR_m3 <- mean(EUR_Data$LN_EUR_m3)
SD_LN_EUR <- sd(EUR_Data$LN)
SD_LN_EUR_m3 <- sd(EUR_Data$LN_EUR_m3)
Median_LN_EUR <- median(EUR_Data$LN)
Median_LN_EUR_m3 <- median(EUR_Data$LN_EUR_m3)
#--------------------------------------------
#Probability Distribution Data Fitting 
library(fitdistrplus) #Load all the fitdistr to plot the probability distribution functions
library(actuar) #Load the Log-Logistic Distribution function
fwEUR <- fitdist(EUR_Data$Value,"weibull") #Weibull Distribution
fgEUR <- fitdist(EUR_Data$Value,"gamma") #Gamma Distribution
flnEUR <- fitdist(EUR_Data$Value,"lnorm")# Log-Normal Distribution
fllEUR <- fitdist(EUR_Data$Value,"llogis") #Log-Logistic Distribution 
ProbDistEUR <- denscomp(list(fwEUR,fgEUR,flnEUR,fllEUR),
                        xlab="EUR (Mm3)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
CProbDistEUR <- cdfcomp(list(fwEUR,fgEUR,flnEUR,fllEUR),
                        xlab="EUR (Mm3)",
                        ylab="Cumulative Distribution Function",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))
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
GOFEUR<-gofstat(list(fwEUR,
                        fgEUR,
                        flnEUR,
                        fllEUR))
LogLikEUR<-list(logW=logLik(fwEUR),
                   logG=logLik(fgEUR),
                   logln=logLik(flnEUR),
                   logll=logLik(fllEUR))
SummaryEUR <- list(SummaryW=summary(fwEUR),
                          SummaryG=summary(fgEUR),
                          Summaryln=summary(flnEUR),
                          Summaryll=summary(fllEUR))
StatsProEUR <- list(GOF=GOFEUR,
                    LogLikelihood=LogLikEUR,
                    Summary=SummaryEUR)
#Log-Logistic is best at fitting but we will use LOG NORMAL
