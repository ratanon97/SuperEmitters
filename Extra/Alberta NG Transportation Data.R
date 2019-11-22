#Set Working Directory for Transportation Data
getwd() #See the working directory of the associated computer to import the data
setwd("C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files")
NGTransport <- read.csv("Alberta NG Transportation.csv") 
NGTransport$Facility.Province.or.Territory <- NULL #This column is literally Alberta, so this column is removed
colnames(NGTransport)[2:10] <- c("Year","Site.Name","City","Supply.Chain.Parameter","CO2tonnes","CO2eqtonnes","CH4tonnes","CH4inCO2eqtonnes","TotalEmissionsinCO2eqtonnes")
#Column names were renamed appropriately
NGTransport$CH4_percentage <- (NGTransport$CH4inCO2eqtonnes / NGTransport$TotalEmissionsinCO2eqtonnes)*100
#-------------------------------------------------------------------
#Pipeline Transportation Data. To categorise them into Stationary Fuel and Fugitive Emissions
#This could possibly be used later to create a distribution fitting them
#Stationary Fuel Combustion
NGPipelineTransport <- NGTransport[which(NGTransport$Supply.Chain.Parameter=="Pipeline Transportation of Natural Gas"),]
NGPipelineTransport$StationaryFuelCO2 <-NGPipelineTransport$CO2tonnes * 0.72 
NGPipelineTransport$StationaryFuelCO2eq <- NGPipelineTransport$CO2eqtonnes * 0.72 
NGPipelineTransport$StationaryFuelCH4 <- NGPipelineTransport$CH4tonnes * 0.72 
NGPipelineTransport$StationaryFuelCH4inCO2eq <- NGPipelineTransport$CH4inCO2eqtonnes * 0.72 
NGPipelineTransport$StationaryFuelTotal <- NGPipelineTransport$TotalEmissionsinCO2eqtonnes * 0.72 
NGPipelineTransport$StationaryFuelCH4_percent <- (NGPipelineTransport$StationaryFuelCH4inCO2eq / NGPipelineTransport$StationaryFuelTotal)*100

#Fugitive Vented Emissions 
NGPipelineTransport$FugitiveVentedCO2 <-NGPipelineTransport$CO2tonnes * 0.28 
NGPipelineTransport$FugitiveVentedCO2eq <- NGPipelineTransport$CO2eqtonnes * 0.28 
NGPipelineTransport$FugitiveVentedCH4 <- NGPipelineTransport$CH4tonnes * 0.28 
NGPipelineTransport$FugitiveVentedCH4inCO2eq <- NGPipelineTransport$CH4inCO2eqtonnes * 0.28 
NGPipelineTransport$FugitiveVentedTotal <- NGPipelineTransport$TotalEmissionsinCO2eqtonnes * 0.28 
NGPipelineTransport$FugitiveVentedCH4_percent <- (NGPipelineTransport$FugitiveVentedCH4inCO2eq / NGPipelineTransport$FugitiveVentedTotal)*100
#Natueral Gas Distribution Only (Assumed 100% Fugitive)
NGDistribution <- NGTransport[which(NGTransport$Supply.Chain.Parameter=="Natural Gas Distribution"),]
#-------------------------------------------------------------------
#Visualisation of Transportation Data
library(ggplot2) #Install ggplot2 library for visualisation
#Probability Density of Methane Emissions as % of Emissions 
NGTransportMethane <- ggplot(data=NGTransport,aes(x=CH4inCO2eqtonnes)) #Assigning the methane emissions data to an object
NGTransportMethane + 
  geom_density(aes(fill=Supply.Chain.Parameter))+
  facet_grid(Supply.Chain.Parameter~.,scales="free",space="free")+ #To create facets on Natural Gas Distribution and Pipeline Transportation
  xlab("Methane Emissions in CO2eq tonnes")+
  ylab("Density")+
  ggtitle("Alberta Methane Emissions in Natural Gas Transportation")+ #Making the graph look niceer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=c(0,1),
        legend.justification=c(0,1),
        plot.title=element_text(size=20),
        strip.text=element_text(size=15))

#----------------------------------------------- 
#CH4 % Emissions BoxPlot Version
NGTransportMethaneBox2 <- ggplot(data=NGTransport,aes(x=Supply.Chain.Parameter,y=CH4inCO2eqtonnes))
NGTransportMethaneBox2 + 
  geom_boxplot(aes(fill=Supply.Chain.Parameter),size=1.2,alpha=0.4)+
  geom_jitter(aes(colour=CH4inCO2eqtonnes,size=Year))+
  facet_grid(.~Supply.Chain.Parameter,scales="free",space="free")+
  xlab("Supply Chain Parameter")+
  ylab("Methane Emissions in CO2eq tonnes")+
  ggtitle("Alberta Methane Emissions in Natural Gas Transportation")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=c(0,1),
        legend.justification=c(0,1),
        plot.title=element_text(size=20),
        strip.text=element_text(size=15))
#----------------------------------------------- 
#CH4 % Emissions Histogram Version
NGTransportMethane + 
  geom_histogram(aes(y=stat(density),fill=Supply.Chain.Parameter),color="black")+
  #stat_function(geom="line",fun=dnorm,args=list(mean=mean(NGTransport$CH4_percentage),sd=sd(NGTransport$CH4_percentage)),lwd=2)+
  facet_grid(Supply.Chain.Parameter~.,scales="free",space="free")+ #To create facets on Natural Gas Distribution and Pipeline Transportation
  xlab("Methane Emissions in CO2eq tonnes")+
  ylab("Density")+
  ggtitle("Alberta Methane Emissions in Natural Gas Transportation")+ #Making the graph look niceer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=c(0,1),
        legend.justification=c(0,1),
        plot.title=element_text(size=20),
        strip.text=element_text(size=15))
#----------------------------------------------------
#Probability Distribution Fitting
library(fitdistrplus) #Load fitdistrplus package to create probability distributions
library(actuar) #Load actuar package to plot log-logistic distribution
#All Transportation Data
fwTransport <- fitdist(NGTransport$CH4inCO2eqtonnes,"weibull",start=list(shape=1,scale=500)) #Weibull Distribution
fgTransport <- fitdist(NGTransport$CH4inCO2eqtonnes,"gamma",start=list(shape=1,scale=500)) #Gamma Distribution
flnTransport <- fitdist(NGTransport$CH4inCO2eqtonnes,"lnorm",) #Log-Normal Distribution
fllTransport <- fitdist(NGTransport$CH4inCO2eqtonnes,"llogis",start=list(shape=1,scale=500)) #Log-Logistic Distribution 
#Stationary Fuel Combustion Data
fwStation <- fitdist(NGPipelineTransport$StationaryFuelCH4inCO2eq,"weibull",start=list(shape=1,scale=500)) 
fgStation <- fitdist(NGPipelineTransport$StationaryFuelCH4inCO2eq,"gamma",start=list(shape=1,scale=500)) 
flnStation <- fitdist(NGPipelineTransport$StationaryFuelCH4inCO2eq,"lnorm") 
fllStation <- fitdist(NGPipelineTransport$StationaryFuelCH4inCO2eq,"llogis",start=list(shape=1,scale=500))
#Fugitive Vent Data for Pipeline Transport
fwFugVent <- fitdist(NGPipelineTransport$FugitiveVentedCH4inCO2eq,"weibull",start=list(shape=1,scale=500)) 
fgFugVent <- fitdist(NGPipelineTransport$FugitiveVentedCH4inCO2eq,"gamma",start=list(shape=1,scale=500))
flnFugVent <- fitdist(NGPipelineTransport$FugitiveVentedCH4inCO2eq,"lnorm") 
fllFugVent <- fitdist(NGPipelineTransport$FugitiveVentedCH4inCO2eq,"llogis",start=list(shape=1,scale=500))
#Distribution Data
fwDistr <- fitdist(NGDistribution$CH4inCO2eqtonnes,"weibull",start=list(shape=1,scale=500)) 
fgDistr <- fitdist(NGDistribution$CH4inCO2eqtonnes,"gamma",start=list(shape=1,scale=500))
flnDistr <- fitdist(NGDistribution$CH4inCO2eqtonnes,"lnorm") 
fllDistr <- fitdist(NGDistribution$CH4inCO2eqtonnes,"llogis",start=list(shape=1,scale=500))
#----------------------------------------------------
#Plot all 4 distribution curves along with histogram 
#All Transportation Data
ProbDistTransport<-denscomp(list(fwTransport,fgTransport,flnTransport,fllTransport), xlab="Methane Emissions (%)",
               plotstyle="ggplot",
               legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
               line01lty="solid") 
CProbDistTransport<-cdfcomp(list(fwTransport,fgTransport,flnTransport,fllTransport), xlab="Methane Emissions in CO2eq tonnes",
                            plotstyle="ggplot",
                            legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                            line01lty="solid") 
#Stationary Fuel Data
ProbDistStationary<-denscomp(list(fwStation,fgStation,flnStation,fllStation), xlab="Methane Emissions in CO2eq tonnes",
                            plotstyle="ggplot",
                            legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                            line01lty="solid")
CProbDistStationary<-cdfcomp(list(fwStation,fgStation,flnStation,fllStation), xlab="Methane Emissions in CO2eq tonnes",
                             plotstyle="ggplot",
                             legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                             line01lty="solid")
#Fugitive Vented Data
ProbDistFugitiveVented<-denscomp(list(fwFugVent,fgFugVent,flnFugVent,fllFugVent), xlab="Methane Emissions in CO2eq tonnes",
                             plotstyle="ggplot",
                             legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                             line01lty="solid")
CProbDistFugitiveVented<-cdfcomp(list(fwFugVent,fgFugVent,flnFugVent,fllFugVent), xlab="Methane Emissions in CO2eq tonnes",
                                 plotstyle="ggplot",
                                 legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                                 line01lty="solid")
#NG Distribution Data
ProbDistDistr<-denscomp(list(fwDistr,fgDistr,flnDistr,fllDistr), xlab="Methane Emissions in CO2eq tonnes",
                                 plotstyle="ggplot",
                                 legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                                 line01lty="solid")
CProbDistDistr<-cdfcomp(list(fwDistr,fgDistr,flnDistr,fllDistr), xlab="Methane Emissions in CO2eq tonnes",
                                 plotstyle="ggplot",
                                 legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                                 line01lty="solid")
#Plot style is set to ggplot so this can be used with the ggplot functions
#Load the ggplot2 library as well prior to visualise the probability density distributions
#Visualise the histogram and density curves together
 ProbDistTransport+ggtitle("Histogram and Distribution Curves of Transportation Data")+ 
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
 #CDF Version
 CProbDistTransport+ggtitle("Cumulative Distribution Curves of Transportation Data")+ 
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         legend.title=element_text(size=15),
         legend.text=element_text(size=15),
         legend.justification=c(1,1),
         plot.title=element_text(size=20))
 #Pipeline Transport - Stationary Fuel Combustion
 ProbDistStationary+ggtitle("Pipeline Transporation - Stationary Combustion Data")+ 
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         legend.title=element_text(size=15),
         legend.text=element_text(size=15),
         legend.justification=c(1,1),
         plot.title=element_text(size=20))
 #CDF Version
 CProbDistStationary+ggtitle("Pipeline Transporation - Stationary Combustion Data")+ 
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         legend.title=element_text(size=15),
         legend.text=element_text(size=15),
         legend.justification=c(1,1),
         plot.title=element_text(size=20))
 #Pipeline Transport - Fugitive Vented Emissions
 ProbDistFugitiveVented+ggtitle("Pipeline Transporation - Fugitive/Vented Emissions Data")+ 
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         legend.title=element_text(size=15),
         legend.text=element_text(size=15),
         legend.justification=c(1,1),
         plot.title=element_text(size=20))
# CDF Version
 CProbDistFugitiveVented+ggtitle("Pipeline Transporation - Fugitive/Vented Emissions Data")+ 
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         legend.title=element_text(size=15),
         legend.text=element_text(size=15),
         legend.justification=c(1,1),
         plot.title=element_text(size=20))
 #Natural Gas Distribution
 ProbDistDistr+ggtitle("Natural Gas Distribution Data")+ 
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         legend.title=element_text(size=15),
         legend.text=element_text(size=15),
         legend.justification=c(1,1),
         plot.title=element_text(size=20))
 #CDF Version
 CProbDistDistr+ggtitle("Natural Gas Distribution Data")+ 
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         legend.title=element_text(size=15),
         legend.text=element_text(size=15),
         legend.justification=c(1,1),
         plot.title=element_text(size=20))
#Statistics Data from the Transportation fit.
gofstat(list(fwTransport,fgTransport,flnTransport,fllTransport))
logLik(fwTransport)
logLik(fgTransport)
logLik(flnTransport)
logLik(fllTransport)
#Statistics Data from the Stationary Fuel Fit
gofstat(list(fwStation,fgStation,flnStation,fllStation))
logLik(fwStation)
logLik(fgStation)
logLik(flnStation)
logLik(fllStation)
#Log-Logistic is described the best?
#Statistics Data from the Fugitive Vented Emissions Fit
gofstat(list(fwFugVent,fgFugVent,flnFugVent,fllFugVent))
logLik(fwStation)
logLik(fgStation)
logLik(flnStation)
logLik(fllStation)
#Log-Logistic is described the best?
#Statistics Data from the Natural Gas Distribution
gofstat(list(fwDistr,fgDistr,flnDistr,fllDistr))
logLik(fwDistr)
logLik(fgDistr)
logLik(flnDistr)
logLik(fllDistr)

