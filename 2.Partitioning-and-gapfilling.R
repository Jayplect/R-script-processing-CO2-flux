library(REddyProc) 
library(dplyr)
library(ggplot2)
library(FREddyPro)

graphics.off()
#+++ Load data with 1 header and 1 unit row from 
# (tab-delimited) text file

#fileName <- getExamplePath('Example_DETha98.txt', isTryDownload = TRUE) 
#EddyDataTemp <- if (length(fileName)) fLoadTXTIntoDataframe(fileName) else 
 #or use example dataset in RData format provided with REddyProc
#Example_DETha98
#+++ Replace long runs of equal NEE values by NA

setwd("C:/Users/grad_student/OneDrive/Documents/EC_P3_2018") #change path here

mydataTemp <- read.delim ("ERS4_2018_P3_.txt") #change path here

#filter invalid values
# NEE
mydataTemp$NEE[mydataTemp$NEE <=  -50 ] <- NA
mydataTemp$NEE[mydataTemp$NEE >=  25 ] <- NA
#Rg
mydataTemp$Rg[mydataTemp$Rg < 0 ] <- NA
mydataTemp$Rg[mydataTemp$Rg > 1500 ] <- NA
#VPD
mydataTemp$VPD[mydataTemp$VPD > 50] <- NA
#Tair
mydataTemp$Tair[mydataTemp$Tair <= -50 ] <- NA
mydataTemp$Tair[mydataTemp$Tair >= 50] <- NA
#U*
mydataTemp$Ustar[mydataTemp$Ustar > 1.5 ] <- NA
mydataTemp$Ustar[mydataTemp$Ustar < 0 ] <- NA
#LE
mydataTemp$LE[mydataTemp$LE < -150 ] <- NA
mydataTemp$LE[mydataTemp$LE > 700 ] <- NA
#H
mydataTemp$H[mydataTemp$H < -200 ] <- NA   
mydataTemp$H[mydataTemp$H > 600 ] <- NA     

#replace all errors with NA
mydataTemp[mydataTemp == -9999 ] <- NA 
#EddyData.FTemp[EddyData.FTemp ==  -9999] <- NA


#EddyData <- filterLongRuns(EddyDataTemp, "NEE")
#EddyData.F <- filterLongRuns(EddyData.FTemp, "NEE")

#execution continued
EddyData.F <- filterLongRuns(mydataTemp, "NEE")
#EddyData.F <- EddyData

#+++ Add time stamp in POSIX time format 

EddyDataWithPosix.F <- fConvertTimeToPosix( 
  EddyData.F, 'YDH', Year.s = 'Year', 
  Day.s = 'DoY',Hour.s = 'Hour')

#+++ Initalize R5 reference class sEddyProc for post-processing of 
#eddy data with the variables needed later

EProc <- sEddyProc$new( 
 'DE-Tha', EddyDataWithPosix.F,
  c('NEE','Rg','Tair','VPD', 'Ustar'))

EProc$sPlotFingerprintY('NEE', Year = 2018)

#Estimating the ustar threshold distribution_1

EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.05, 0.5, 0.95))
EProc$sGetEstimatedUstarThresholdDistribution()

#Estimating the ustar threshold distribution using 
#the four u* threshold scenearios

EProc$sGetUstarScenarios()

#Gap-filling

EProc$sMDSGapFillUStarScens('NEE')

## [1] "NEE_uStar_f" "NEE_U05_f"   "NEE_U50_f"   "NEE_U95_f"  
## [1] "NEE_uStar_fsd" "NEE_U05_fsd"   "NEE_U50_fsd"   "NEE_U95_fsd"

grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EProc$sExportResults()), value = TRUE)

# A fingerprint-plot of one of the new variables
# shows that gaps have been filled.

EProc$sPlotFingerprintY('NEE_U50_f', Year = 2018) #change year

# Fill missing values in meteorological data
EProc$sSetLocationInfo(LatDeg = 56.1, LongDeg = 106.3, TimeZoneHour = -6)  
EProc$sMDSGapFill('Tair', FillAll = TRUE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('VPD', FillAll = TRUE,  minNWarnRunLength = NA) 

# Night time approach partitioning

#EProc$sMRFluxPartition(parsE0Regression = list(TempRange = 3))
#EProc$sMRFluxPartition(debug = list(fixedE0 = 122))
#EProc$sMRFluxPartition(parsE0Regression=list (TempRange.n=2.0, optimAlgorithm="LM")) 

EProc$sMRFluxPartitionUStarScens()

# The results are stored in columns Reco and GPP_f modified by the respective u??? threshold suffix.
grep("GPP.*_f$|Reco",
     names(EProc$sExportResults()), value = TRUE)

#Visualizations of the results by a fingerprint plot
 EProc$sPlotFingerprintY('GPP_U50_f', Year = 2018) #change year
 EProc$sPlotFingerprintY('Reco_U50', Year = 2018)

##Estimating the uncertainty of aggregated results
#First compute mean of GPP across all year for each u* and convert to g/Cm2/yr
FilledEddyData <- EProc$sExportResults()
uStarSuffixes <- colnames(EProc$sGetUstarScenarios())[-1]
#suffix <- uStarSuffixes[2]
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
  GPPHalfHour <- FilledEddyData[[paste0("GPP_",suffix,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365.25
print(GPPAgg)

#The difference between those aggregated values is a first estimate 
#of uncertainty range in GPP due to uncertainty of the u??? threshold.
(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg) 

#Storing the results in a csv-file
FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData.F, FilledEddyData)
write.csv(CombinedData, 'gapfilledresults2.csv')



#------------------------------------------------------------
#plot gapfilled NEE
plotgapfilled<-ggplot(data = CombinedData, aes(x = DoY, y = NEE_U50_f))+
  geom_point(size=1, colour="firebrick1")+
  scale_y_continuous(limits=c(-65,20),expand = c(0, 0))+
  scale_x_continuous(limits=c(1,366),expand=c(0,0))+
    #Labels appearance
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust=0.5),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),    
        axis.title.y = element_text(size=14, face="bold", colour = "black"),    
        axis.text.x = element_text(size=12, face="bold", colour = "black"), 
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        panel.background = element_blank()) +
  labs(title = "Gapfilled NEE for Year 2020, Plot 12",face="bold", x = "DOY",
       y = expression(bold(NEE~(µmol/s/m^{2}))))
print (plotgapfilled)

#------------------------------------------------------------
#percent of NA in NEE_filled, GPP and Reco
percentNA(CombinedData$NEE_U50_f) 
percentNA(CombinedData$GPP_U50_f)
percentNA(CombinedData$Reco_U50)
