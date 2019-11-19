#--------------------------------------------
#Set working directory for Production Data
getwd()
setwd("C:\\Users\\KOMSUN\\Documents\\Files from Imperial Laptop\\Modules\\Research Project\\Data Collection\\CSV Inputs")
NGProduction <-read.csv("Alberta Production Data for Red Deer.csv",row.names=1)
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
NGProduction$CH4_percentage <- NGProduction$CH4_emission_m3 / NGProduction$Gas_production_cms#Calculate the methane emissions as % of total gas production data
colnames(NGProduction) #Check column names of the NGProduction Data Frame
#NGProduction <- NGProduction[,c(1,2,13,3,4,5,11,12,14,15,6,7,8,9,10)] #Rearranging Columns
NGProduction$CH4_emission_m3_per_day <- NGProduction$CH4_emission_m3*86400
write.csv(NGProduction,"C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files\\Red_Deer.csv")
AbsRedDeerEmission <- mean(NGProduction$CH4_emission_m3_per_day)
#--------------------------------------------------- 
#Filter out the inappropriate % methane data
infinityfilt <-NGProduction$CH4_percentage != Inf & NGProduction$CH4_percentage < 100 
NGProduction<-NGProduction[infinityfilt,] #Apply the infinity filter
#Descriptive Statistics 
RedDeerMean <- mean(NGProduction$Gas_production_cms)
RedDeerSD <- sd(NGProduction$Gas_production_cms)
Log_RDMean <- mean(NGProduction$log_gas_production_cms)
Log_RDSD <- sd(NGProduction$log_gas_production_cms)
#----------------------------------------------------
library(dplyr)
#Tracer Method
NGProduction_Tracer <- NGProduction %>% 
    dplyr::select(Method,CH4_emission_m3,Gas_production_cms,CH4_percentage,both_methods) %>% 
    filter(Method=="tracer flux") %>% 
    filter(both_methods!="a"& both_methods!="b"&both_methods!="c"&both_methods!="d") %>% 
    mutate(Of_Throughput=CH4_percentage*100)
Tracer_Desc_OTP <- order(NGProduction_Tracer$Of_Throughput)
OTP_Desc_Tracer <- NGProduction_Tracer[Tracer_Desc_OTP,]
OTP_Desc_Tracer <- OTP_Desc_Tracer %>% 
    mutate(Of_Throughput_CS=cumsum(Of_Throughput)/sum(Of_Throughput))
Tracer_Desc_Order_CH4 <- order(NGProduction_Tracer$CH4_emission_m3)
Tracer_CH4_desc <- NGProduction_Tracer[Tracer_Desc_Order_CH4,]
Tracer_CH4_desc <- Tracer_CH4_desc %>% 
    mutate(CH4_emission_m3_CS=cumsum(CH4_emission_m3)/sum(CH4_emission_m3))
#Gaussian dispersion
NGProduction_GD <- NGProduction %>% 
    dplyr::select(Method,CH4_emission_m3,Gas_production_cms,CH4_percentage,both_methods) %>% 
    filter(Method=="Gaussian dispersion") %>% 
    filter(both_methods!="a"& both_methods!="b"&both_methods!="c"&both_methods!="d") %>%
    mutate(Of_Throughput_CS=cumsum(CH4_percentage)/max(sum(CH4_percentage))) %>% 
    mutate(CH4_emission_m3_CS=cumsum(CH4_emission_m3*CH4_percentage)/max(sum(CH4_emission_m3*CH4_percentage)))
#Both Methods
NGProduction_Both_Methods <- NGProduction %>%
    dplyr::select(Method,CH4_emission_m3,Gas_production_cms,CH4_percentage,both_methods) %>% 
    filter(both_methods == "a"|both_methods == "b"| both_methods == "c" | both_methods == "d") %>% 
    mutate(Of_Throughput=CH4_percentage*100) 
Desc_Order_OTP <- order(NGProduction_Both_Methods$Of_Throughput)
OTP_Desc <- NGProduction_Both_Methods[Desc_Order_OTP,]
OTP_Desc <- OTP_Desc %>% 
    mutate(Of_Throughput_CS=cumsum(Of_Throughput))
Desc_Order_CH4 <- order(NGProduction_Both_Methods$CH4_emission_m3)
CH4_Desc <- NGProduction_Both_Methods[Desc_Order_CH4,]
CH4_Desc <- CH4_Desc %>% 
    mutate(CH4_emission_m3_CS=cumsum(CH4_emission_m3))
#Per Emitter Population
library(ineq)
plot(Lc(NGProduction$CH4_emission_m3),
     xlab="Percent Of Population",
     ylab="Percent of Total Emissions",
     main="Per Emitter for Top-Down Method Population Analysis (Red Deer)",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Red Deer"),
       col=c("red","blue","orange"),
       pch=15:18,
       horiz=F,
       border = "white")
grid(col="lightgray")
# lines(Lc(NGProduction_Both_Methods$CH4_emission_m3),
#       col="blue",
#       lwd=4)
# lines(Lc(NGProduction_GD$CH4_emission_m3),
#       col="orange",
#       lwd=4)
#Of Throughput Population
plot(Lc(NGProduction_Both_Methods$CH4_percentage),
     xlab="Percent of Of Throughput Population",
     ylab="Percent of Total Emissions",
     main="Of Throughput for Top-Down Method Population Analysis (Red Deer)",
     col="red",cex.lab=1.5,
     cex.axis=1.5,
     cex.main=1.5,
     lwd=4)
legend("topleft",inset= 0.01,
       legend=c("Red Deer"),
       col=c("red","blue","orange"),
       pch=15:18,
       horiz=F,
       border = "white")
grid(col="lightgray")
#Skewness Analysis
Skewness_RedDeer <- NGProduction %>% 
  dplyr::select(Gas_production_cmd,CH4_emission_m3_per_day,CH4_percentage) %>% 
  mutate(Rank=rank(CH4_percentage,ties.method="max")) %>% 
  filter(Rank >= 52)
Skewness_PE_RedDeer <- NGProduction %>% 
  dplyr::select(Gas_production_cmd,CH4_emission_m3_per_day,CH4_percentage) %>% 
  mutate(Rank=rank(CH4_emission_m3_per_day,ties.method="max")) %>% 
  filter(Rank >= 52) %>% 
  mutate(Region=factor("Red Deer"))
Agg_Skewness_RedDeer <- Skewness_PE_RedDeer %>% 
  dplyr::select(CH4_emission_m3_per_day,Region)
#----------------------------------------------------
#Data Visualisation
#Histogram Version
library(ggplot2)
NGProductionMethane <- ggplot(data=NGProduction,
                              aes(x=CH4_percentage)) #Assigning the methane emissions data to an object
NGProductionMethane + 
    geom_histogram(fill="LightBlue",
                 colour="Black",
                 size=1)+
    scale_x_continuous(trans="log")+
    xlab("Methane Emissions (% of Production)")+
    ylab("Density")+
    ggtitle("Alberta Methane Emissions in Natural Gas Production")+ #Making the graph look niceer using the Theme function
    theme(axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          axis.text.x=element_text(size=15,colour="black"),
          axis.text.y=element_text(size=15,colour="black"),
          plot.title=element_text(size=20))
#Cumulative Curve
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
library(fitdistrplus) #Load all the fitdistr to plot the probability distribution functions
library(actuar) #Load the Log-Logistic Distribution function
library(EnvStats)
fwRedDeer <- fitdist(NGProduction$CH4_percentage,"weibull") #Weibull Distribution
fgRedDeer <- fitdist(NGProduction$CH4_percentage,"gamma") #Gamma Distribution
flnRedDeer <- fitdist(NGProduction$CH4_percentage,"lnorm")# Log-Normal Distribution
fllRedDeer <- fitdist(NGProduction$CH4_percentage,"llogis") #Log-Logistic Distribution 
#Assigning the desncomp function to plot the probability density function

ProbDistRedDeer<-denscomp(list(fwRedDeer,
                                  fgRedDeer,
                                  flnRedDeer,
                                  fllRedDeer), 
                            xlab="Methane Emissions (% of Production)",
                            plotstyle="ggplot",
                            legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                            line01lty="solid")
CProbDistRedDeer<-cdfcomp(list(fwRedDeer,
                               fgRedDeer,
                               flnRedDeer,
                               fllRedDeer),
                             xlab="ln(Methane Emissions (m3/day))",
                             ylab="Cumulative Distribution Function",
                             plotstyle="ggplot",
                             legendtext=c("Weibull","Gamma","Log-Normal","Log-Logistic"),
                             line01lty="solid")
#Using the relevant ggplot functions
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
                            fllRedDeer))

LogLikProRedDeer<-list(logW=logLik(fwRedDeer),
                   logG=logLik(fgRedDeer),
                   logln=logLik(flnRedDeer),
                   logll=logLik(fllRedDeer))
SummaryProRedDeer <- list(SummaryW=summary(fwRedDeer),
                          SummaryG=summary(fgRedDeer),
                          Summaryln=summary(flnRedDeer),
                          Summaryll=summary(fllRedDeer))
StatsProRedDeer <- list(GOF=GOFProRedDeer,
                    LogLikelihood=LogLikProRedDeer,
                    Summary=SummaryProRedDeer)
#Distribution fitting:
#Log-Logistic is the best at fittings
#However, Logistic is used for discrete variables. Not as appropriate.
#----------------------------------------------------
#Monte Carlo Simulation
set.seed(123)
MC_Function <- function(df,mean,sd){
  RD_Methane_Throughput_MC <- (df$CH4_emission_m3_per_day/rlnorm(nrow(df),mean,sd))*100
  return(RD_Methane_Throughput_MC)
}
RedDeer_Sim <- replicate(10000,MC_Function(NGProduction,Mean_LN_EUR,SD_LN_EUR))
RedDeer_Sim <- data.frame(RedDeer_Sim)
#Calculations from Simulation
Mean_RedDeer_Sim <- sapply(RedDeer_Sim,mean)
Median_RedDeer_Sim <- sapply(RedDeer_Sim,median)
Q5_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.05))
Q25_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.25))
Q75_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.75))
Q95_RedDeer_Sim <- sapply(RedDeer_Sim,quantile,c(.95))
FilteredMean_RedDeer_Sim<-Mean_RedDeer_Sim[Mean_RedDeer_Sim <= 100]
FilteredMedian_RedDeer_Sim<-Median_RedDeer_Sim[Median_RedDeer_Sim <= 100]
FilteredQ75_RedDeer_Sim <- Q75_RedDeer_Sim[Q75_RedDeer_Sim<=100]
FilteredQ95_RedDeer_Sim <- Q95_RedDeer_Sim[Q95_RedDeer_Sim<=100]
MinFilteredMeanRD<-min(FilteredMean_RedDeer_Sim)
MaxFilteredMeanRD<-max(FilteredMean_RedDeer_Sim)
MinFilteredMedianRD<-min(FilteredMedian_RedDeer_Sim)
MaxFilteredMedianRD<-max(FilteredMedian_RedDeer_Sim)
MinFilteredQ5RD<-min(Q5_RedDeer_Sim)
MaxFilteredQ5RD<-max(Q5_RedDeer_Sim)
MinFilteredQ25RD<-min(Q25_RedDeer_Sim)
MaxFilteredQ25RD<-max(Q25_RedDeer_Sim)
#MinFilteredQ75RD<-min(FilteredQ75_RedDeer_Sim)
# MaxFilteredQ75RD<-max(FilteredQ75_RedDeer_Sim)
# MinFilteredQ95RD<-min(FilteredQ95_RedDeer_Sim)
# MaxFilteredQ95RD<-max(FilteredQ95_RedDeer_Sim)
#--------------------------------------
#Multiple Plots
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
}
for(h in 1001:2000){
  lines(ecdf(RedDeer_Sim[,h]),
        col=alpha("cadetblue2",0.3))
}
for(l in 2001:3000){
  lines(ecdf(RedDeer_Sim[,l]),
        col=alpha("darkorchid2",0.3))
}
for(p in 3001:4000){
  lines(ecdf(RedDeer_Sim[,p]),
        col=alpha("lightskyblue2",0.3))
}
for(o in 4001:5000){
  lines(ecdf(RedDeer_Sim[,o]),
        col=alpha("blueviolet",0.3))
}
for(k in 5001:6000){
  lines(ecdf(RedDeer_Sim[,k]),
        col=alpha("darkslateblue",0.3))
}
for(s in 6001:7000){
  lines(ecdf(RedDeer_Sim[,s]),
        col=alpha("darkmagenta",0.3))
}
for(u in 7001:8000){
  lines(ecdf(RedDeer_Sim[,u]),
        col=alpha("deeppink1",0.3))
}
for(x in 8001:9000){
  lines(ecdf(RedDeer_Sim[,x]),
        col=alpha("cornflowerblue",0.3))
}
for(f in 9001:10000){
  lines(ecdf(RedDeer_Sim[,f]),
        col=alpha("darkorchid",0.3))
}
#--------------------------------------
# #Visualisation Testing
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