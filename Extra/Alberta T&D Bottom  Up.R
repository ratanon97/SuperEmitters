#Set Working Directory for Transportation Data
getwd() #See the working directory of the associated computer to import the data
setwd("C:\\Users\\CE-rs2318\\Documents\\Modules\\Research Project\\Data Collection\\CSV Files")
NGTDB <- read.csv("Alberta T&D Bottom Up Data.csv") 
#---------------------------------------------------------
#Notes: This is bottom up method
#GWP Used: GWP100 of 25 for Methane and GWP100 of 298 for NO2
#Years 2013 - 2017 for this script
#----------------------------------------------------------
#Data Frame Preparation
NGTDB$Year <- factor(NGTDB$Year) #Convert the Year into categorical variable
colnames(NGTDB)[2:9] <- c("Reporting_Segment",
                          "Emission_Source_Category",
                          "CO2_tonnes",
                          "CH4_tonnes",
                          "CH4_CO2eqtonnes",
                          "N2O_tonnes",
                          "N2O_CO2eqtonnes",
                          "Total_GHG_Emissions_CO2eqtonnes") #Rename appropriate columns
CH4_density <- 0.8 #Density range <- 0.7 - 0.9 kg/m3 Averaged to be 0.8
NGTDB$CH4_kg <- NGTDB$CH4_tonnes * 1000
NGTDB$CH4_m3 <- NGTDB$CH4_kg / CH4_density
NGTDB$CH4_m3_day <- NGTDB$CH4_m3/365 #Conversion into m3/day
#DYPLR preparation of the Median Percent Calculation
library(dplyr)
NGTDB <- NGTDB %>% 
  mutate(CH4_Percent_Median = ifelse(NGTDB$Year=="2017" & Production_Gas_Vertical_Median_2017$Activity_Years == "2017",
                          NGTDB$CH4_m3_day/Production_Gas_Vertical_Median_2017$Median_Production_Mm3_per_day,
                          ifelse(NGTDB$Year=="2016" & Production_Gas_Vertical_Median_2016$Activity_Years == "2016",
                                 NGTDB$CH4_m3_day/ Production_Gas_Vertical_Median_2016$Median_Production_Mm3_per_day,
                                 ifelse(NGTDB$Year=="2015" & Production_Gas_Vertical_Median_2015$Activity_Years == "2015",
                                        NGTDB$CH4_m3_day/ Production_Gas_Vertical_Median_2015$Median_Production_Mm3_per_day,
                                        ifelse(NGTDB$Year=="2014" & Production_Gas_Vertical_Median_2014$Activity_Years == "2014",
                                               NGTDB$CH4_m3_day/ Production_Gas_Vertical_Median_2014$Median_Production_Mm3_per_day,
                                               ifelse(NGTDB$Year=="2013" & Production_Gas_Vertical_Median_2013$Activity_Years == "2013",
                                                      NGTDB$CH4_m3_day/Production_Gas_Vertical_Median_2013$Median_Production_Mm3_per_day,"")
                                               )
                                        )
                                 )  
                          )
         ) #Calculate the Methane Emissions of Production based on EUR script
NGTDB$CH4_Percent_Median <- as.numeric(NGTDB$CH4_Percent_Median)
#From the IF statement, the values were stored as character
#Converted into Numeric
#--------------------------------------------------------------------
#Mean Version
NGTDB <- NGTDB %>% 
  mutate(CH4_Percent_Mean = ifelse(NGTDB$Year=="2017" & Production_Gas_Vertical_Mean_2017$Activity_Years == "2017",
                                     NGTDB$CH4_m3_day/Production_Gas_Vertical_Mean_2017$Mean_Production_Mm3_per_day,
                                     ifelse(NGTDB$Year=="2016" & Production_Gas_Vertical_Mean_2016$Activity_Years == "2016",
                                            NGTDB$CH4_m3_day/Production_Gas_Vertical_Mean_2016$Mean_Production_Mm3_per_day,
                                            ifelse(NGTDB$Year=="2015" & Production_Gas_Vertical_Mean_2015$Activity_Years == "2015",
                                                   NGTDB$CH4_m3_day/ Production_Gas_Vertical_Mean_2015$Mean_Production_Mm3_per_day,
                                                   ifelse(NGTDB$Year=="2014" & Production_Gas_Vertical_Mean_2014$Activity_Years == "2014",
                                                          NGTDB$CH4_m3_day/ Production_Gas_Vertical_Mean_2014$Mean_Production_Mm3_per_day,
                                                          ifelse(NGTDB$Year=="2013" & Production_Gas_Vertical_Mean_2013$Activity_Years == "2013",
                                                                 NGTDB$CH4_m3_day/Production_Gas_Vertical_Mean_2013$Mean_Production_Mm3_per_day,"")
                                                   )
                                            )
                                     )  
  )
  ) #Calculate the Methane Emissions of Production based on EUR script
NGTDB$CH4_Percent_Mean <- as.numeric(NGTDB$CH4_Percent_Mean) 
#Filter Out 0 Values
NGTDB <- NGTDB %>% 
  filter(CH4_tonnes != 0)
#-------------------------------------------------
#Data Visualisation
library(ggplot2)
NGTDB_Histo <- ggplot(data=NGTDB,aes(x=CH4_Percent_Mean))
NGTDB_Histo + geom_density(aes(y=stat(density)),
                           fill="LightBlue",
                             colour="black")+
  scale_x_continuous(trans="log")+
  # facet_grid(Emission_Source_Category~.,
  #            scales="free",
  #            space="free")+
  xlab("Methane Emissions (% of Production)")+
  ylab("Density")+
  ggtitle("Alberta Transmission & Distribution Data (2013-2017)")+ #Making the graph look niceer using the Theme function
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20),
        strip.text=element_text(size=10))
#-------------------------------------------------------
#Probability/Distribution Data Fit
library(fitdistrplus)
library(actuar)
fwTD <- fitdist(NGTDB$CH4_tonnes,"weibull") #Weibull Distribution
fgTD <- fitdist(NGTDB$CH4_tonnes,"gamma",start=list(shape=1,scale=500)) #Gamma Distribution
flnTD <- fitdist(NGTDB$CH4_tonnes,"lnorm")# Log-Normal Distribution
fllTD <- fitdist(NGTDB$CH4_tonnes,"llogis") #Log-Logistic Distribution 

ProbDistTD <- denscomp(list(fwTD,fgTD,flnTD,fllTD),
                        xlab="Methane Emissions (% of Production)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))

CProbDistTD <- cdfcomp(list(fwTD,fgTD,flnTD,fllTD),
                        xlab="Methane Emissions (% of Production)",
                        plotstyle="ggplot",
                        legendtext=c("Weibull",
                                     "Gamma",
                                     "Log-Normal",
                                     "Log-Logistic"))

ProbDistTD+
  ggtitle("Histogram and Distribution Curves of Alberta Transportation and Distribution Data")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))

CProbDistTD+
ggtitle("Cumulative Distribution Curves of Alberta Transportation and Distribution Data")+
  scale_x_continuous(trans="log")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.justification=c(1,1),
        plot.title=element_text(size=20))
#Statistics Listing
GOFTD<-gofstat(list(fwTD,
                      fgTD,
                      flnTD,
                      fllTD))
LogLikTD <- list(logW=logLik(fwTD),
                logG=logLik(fgTD),
                logln=logLik(flnTD),
                logll=logLik(fllTD))
SummaryTD<-list(SummaryW=summary(fwTD),
                 SummaryG=summary(fgTD),
                 Summaryln=summary(flnTD),
                 Summaryll=summary(fllTD))
StatsTD <- list(GOF=GOFTD,
                LogLik=LogLikTD,
                Summary=SummaryTD)
#------------------------------------------------------------
#Monte Carlo and Log-Log Regression
#Generation of the Monte Carlo Table
# set.seed(123) #Set seed so that the random values doesn't change each time the script is ran
# N <- 10000 #Generate Monte Carlo Rows
# MC_EUR<-rnorm(N,TDMean,TDSD) #Monte Carlo based on EUR variability
# MC_TD_Bottom_Up <- matrix(NA, 
#                        nrow=N,
#                        ncol=nrow(NGTDB))
# #Set an empty matrix with the number of rows 
# #same as the amount of Monte Carlo simulations
# for(i in 1:ncol(MC_TD_Bottom_Up)){
#   MC_TD_Bottom_Up[,i] <- NGTDB$CH4_m3_day[i] / MC_EUR
# } #Create a for loop to fill out each column with a fixed value of the specified column of its specified dataset
#   #The fixed value is then calculated over the 10,000 values of EUR
# MC_TD_Bottom_Up <- data.frame(MC_TD_Bottom_Up) 
#The result of this loop creates a matrix
#It is desirable to use it as a data frame, so this is converted into a data frame structure
#Refined Monte Carlo Version
set.seed(123)
MC_Function_TD <- function(df,mean,sd){
  TD_Methane_Throughput <- (df$CH4_CO2eqtonnes/rlnorm(nrow(df),mean,sd))*100
  return(TD_Methane_Throughput)
}
CEPEI_Sim <- replicate(10000,MC_Function_TD(NGTDB,Mean_LN_EUR,SD_LN_EUR))
CEPEI_Sim <- data.frame(CEPEI_Sim)
#---------------------------------------------------------------
#Visualisation Testing (based on Monte Carlo Simulations)
MC_TD_Bottom_Up_Histo <- ggplot(data=MC_TD_Bottom_Up)
MC_TD_Bottom_Up_Histo + geom_histogram(aes(x=X4,y=stat(density)),
                                    fill="LightBlue",
                                    colour="Black",
                                    binwidth = 0.2,
                                    size=1)+
  scale_x_continuous(trans="log")+
  ggtitle("Monte Carlo Simulations of Alberta T&D Data")+
  xlab("Methane Emissions (% of Production)")+
  ylab("Density")+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=15,colour="black"),
        axis.text.y=element_text(size=15,colour="black"),
        plot.title=element_text(size=20))
#---------------------------------------------
#Appendix
# #Log-Log Regression
 library(caTools) #For Machine Learning Regression
 set.seed(123) #So the splitting of data can be the same
 #Splitting of Data
 ML_NGTDB <- NGTDB[NGTDB$CO2_tonnes!=0,]
 split <- sample.split(ML_NGTDB$CO2_tonnes, SplitRatio = 3/4)
 training_set <- subset(ML_NGTDB, split == TRUE)
 test_set <- subset(ML_NGTDB, split == FALSE)
 #Log-Log Regression
 Regressor <- lm(log(CO2_tonnes)~log(CH4_CO2eqtonnes),
                 training_set)
 C1 <- coef(Regressor)[[1]]
 C2 <- coef(Regressor)[[2]]
 summaryReg <- summary(Regressor)
 Methane_Pred <- predict(Regressor,test_set)
 #Training Set
 library(ggplot2)
 ggplot()+
   geom_point(aes(x=training_set$CH4_CO2eqtonnes,
                  y=training_set$CO2_tonnes),
              colour="red")+ #Observation Points Plot
   geom_line(aes(x=training_set$CH4_CO2eqtonnes,
                 y=predict(Regressor,training_set)),
             colour="blue",
             size=2)+ #Regression line
   ggtitle("CO2 Emissions and CH4 Emissions (Training Set)")+
   xlab("CH4 Emissions (CO2eqtonnes)")+
   ylab("CO2 Emissions (tonnes)")+
   scale_x_continuous(trans="log")+
   scale_y_continuous(trans="log")+
      theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         plot.title=element_text(size=20))
 #Test Set
 ggplot()+
   geom_point(aes(x=test_set$CH4_CO2eqtonnes,
                  y=test_set$CO2_tonnes),
              colour="red")+ #Observation Points Plot
   geom_line(aes(x=training_set$CH4_CO2eqtonnes,
                 y=predict(Regressor,training_set)),
             colour="blue",
             size=2)+ #Regression line
   ggtitle("CO2 Emissions and CH4 Emissions (Test Set)")+
   xlab("CH4 Emissions (CO2eqtonnes)")+
   ylab("CO2 Emissions (tonnes)")+
   scale_x_continuous(trans="log")+
   scale_y_continuous(trans="log")+
   theme(axis.title.x=element_text(size=15),
         axis.title.y=element_text(size=15),
         axis.text.x=element_text(size=15,colour="black"),
         axis.text.y=element_text(size=15,colour="black"),
         plot.title=element_text(size=20))

