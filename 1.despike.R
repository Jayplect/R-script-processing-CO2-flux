library(installr)
library(readxl)
library(lubridate)
library (naniar)
library (ggplot2)
library (FREddyPro)


#set working directory and file path
setwd("C:/Users/grad_student/Documents/EC_P3_2019") #change path
inputdata <- read.csv ("plot3_2019uf.csv")

#Split data into growing and non growing season
'offseason <- inputdata[1:7163,]
growingseason <- inputdata[7164:17520,]'
'growingseason <- inputdata[7164:13153,]
growingseason2 <- inputdata[13154:17520,]'

#off-season filtering
'offseason$co2_flux[offseason$co2_flux <=  -5 ] <- NA
offseason$co2_flux[offseason$co2_flux >=  5 ] <- NA'

#in-season filtering
'growingseason$co2_flux[growingseason$co2_flux <=  -60 ] <- NA
growingseason$co2_flux[growingseason$co2_flux >=  25 ] <- NA'

'#second growing-season filtering
growingseason2$co2_flux[growingseason2$co2_flux <=  -15 ] <- NA
growingseason2$co2_flux[growingseason2$co2_flux >=  15 ] <- NA'

'combineseason<-rbind(offseason,growingseason)
write.csv(combineseason,"thresholdflux.csv")'
thresholdflux<-read.csv('thresholdflux.csv')

#PLot CO2 before filtering data 
UnfillteredNEE<- ggplot(data = inputdata, aes(x = DOY, y =co2_flux))+
    geom_point(size=1.1, colour="purple",alpha = 1)+
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
    #change label by changing year and plot if applies
    labs(title = "Unfiltered NEE for Year 2018, Plot 3",face="bold", x = "DOY",
         y = expression(bold(NEE~(µmol/s/m^{2}))))
print (UnfillteredNEE)

#PLot CO2 for threshold between -60 and 30umols/s/m^2 
UnfillteredNEE_2<- ggplot(data = inputdata, aes(x = DOY, y =co2_flux))+
    geom_point(size=1.1, colour="blue",alpha = 1)+
    scale_y_continuous(limits=c(-60,30),expand = c(0, 0))+
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
    #change label by changing year and plot if applies
    labs(title = "Unfiltered NEE for Year 2019, Plot 3",face="bold", x = "DOY",
         y = expression(bold(NEE~(µmol/s/m^{2}))))
print (UnfillteredNEE_2)

#Functions
#---------------------------------------------------------
midpoints <-
    function (x, dp = 2) 
    {
        lower <- as.numeric(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", 
                                                 "", x)))
        upper <- as.numeric(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", 
                                                 "", x)))
        return(round(lower + (upper - lower)/2, dp))
    }
#---------------------------------------------------------
cleanVar <-
    function (x, data, lessThan = NULL, greaterThan = NULL) 
    {
        clean <- which(data[[x]] >= greaterThan | data[[x]] <= lessThan)
        data[[x]][clean] <- NA
        return(data)
    }
#---------------------------------------------------------
distClean <-
    function (var, hour, df) 
    {
        m <- aggregate(var ~ hour, data = df, mean)
        q <- as.data.frame(t(as.data.frame(t(aggregate(var ~ hour, 
                                                       data = df, quantile, prob = c(0.05, 0.95))))))
        p <- merge(m, q, "hour")
        names(p)[c(3, 4)] <- c("quant5", "quant95")
        for (i in 1:nrow(p)) {
            var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
                                                                  p$hour[i])] < p$quant5[i], NA, var[which(hour == 
                                                                                                               p$hour[i])])
            var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
                                                                  p$hour[i])] > p$quant95[i], NA, var[which(hour == 
                                                                                                                p$hour[i])])
        }
        return(var)
    }

#---------------------------------------------------------
sdClean <-
    function (var, p) 
    {
        positive <- which(var > 0)
        negative <- which(var < 0)
        m.positive <- mean(var[positive], na.rm = TRUE)
        m.negative <- mean(var[negative], na.rm = TRUE)
        sd.positive <- sd(var[positive], na.rm = TRUE)
        sd.negative <- sd(var[negative], na.rm = TRUE)
        filter.positive <- m.positive + p * sd.positive
        filter.negative <- m.negative - p * sd.negative
        rm.positive <- which(var > filter.positive)
        rm.negative <- which(var < filter.negative)
        var[rm.positive] <- NA
        var[rm.negative] <- NA
        return(var)
    }

#---------------------------------------------------------
cleanSecondVar <-
    function (x, y, data) 
    {
        clean <- which(is.na(data[[x]]) & !is.na(data[[y]]))
        data[[y]][clean] <- NA
        return(data)
    }
#---------------------------------------------------------
qcClean <-
    function (var, qcVar, qc) 
    {
        if (length(qc) != 1) {
            for (i in 1:length(qc)) {
                qc.index <- which(qcVar == qc[i])
                var[qc.index] <- NA
            }
        }
        else {
            qc.index <- which(qcVar == qc)
            var[qc.index] <- NA
        }
        return(var)
    }

#---------------------------------------------------------
createTimestamp <-
    function (data, timestamp = NULL) 
    {
        if (is.null(timestamp)) {
            data$timestamp <- paste(data$date, data$time, sep = " ")
            data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC")
            data$year <- year(data$timestamp)
            data$yday <- yday(data$timestamp)
            data$hour <- hour(data$timestamp) + minute(data$timestamp)/60
            data$month <- month(data$timestamp)
        }
        else {
            data[[timestamp]] <- as.POSIXct(data[[timestamp]], tz = "UTC")
            data$year <- year(data[[timestamp]])
            data$yday <- yday(data[[timestamp]])
            data$hour <- hour(data[[timestamp]]) + minute(data[[timestamp]])/60
            data$month <- month(data[[timestamp]])
        }
        return(data)
    }
#-----------------------------------------------------------------------
ustarThreshold <-
    function (data, sunset = 19, sunrise = 6) 
    {
        data = createTimestamp(data)
        data$air_temperature = data$air_temperature - 273.15
        night = which(data$hour >= sunset | data$hour <= sunrise)
        data_night = data[night, ]
        temp.breaks = as.vector(unique(quantile(data_night$air_temperature, 
                                                seq(0, 1, length = 8), na.rm = TRUE)))
        data_night$temp.class = cut(data_night$air_temperature, breaks = temp.breaks, 
                                    include.lowest = TRUE)
        unique.tc = unique(data_night$temp.class)
        t = data.frame(DOY = numeric(), ustra.class = factor())
        options(warn = -1)
        for (i in 1:length(unique.tc)) {
            index <- which(data_night$temp.class == unique.tc[i])
            if (length(index) != 0) {
                t.df = data_night[index, c("DOY", "u.", "temp.class")]
                ustar.breaks = as.vector(unique(quantile(t.df$u., seq(0, 
                                                                      1, length = 21), na.rm = TRUE)))
                t.df$ustar.class = cut(t.df$u., breaks = ustar.breaks, 
                                       include.lowest = TRUE)
                t = rbind(t, t.df[, c("DOY", "ustar.class")])
            }
        }
        data_night = merge(data_night, t, by.x = "DOY", by.y = "DOY", 
                           all.x = TRUE)
        df = aggregate(cbind(co2_flux, u.) ~ temp.class + ustar.class, 
                       data = data_night, mean)
        untc = unique(df$temp.class)
        ust = array(NA, dim = c(0, length(untc)))
        for (i in 1:length(untc)) {
            index <- which(df$temp.class == untc[i])
            sub.df <- df[index, ]
            sub.df$ustar.class.midpoints <- midpoints(sub.df$ustar.class)
            higher <- which(sub.df$u. > quantile(sub.df$u., probs = 0.1, 
                                                 na.rm = TRUE))
            M = mean(sub.df$co2_flux[higher], na.rm = TRUE)
            ust[i] = sub.df$u.[which(sub.df$co2_flux[which(sub.df$co2_flux >= 
                                                               M * 0.99)] == max(sub.df$co2_flux[which(sub.df$co2_flux >= 
                                                                                                           M * 0.99)]))]
        }
        ustar.threshold = median(ust, na.rm = TRUE)
        return(ustar.threshold)
    }

#-----------------------------------------------------------------------
cleanFluxes <-
    function (data, gas = "co2_flux", qcFlag = 2, sdCor = FALSE, 
              sdTimes = 1, distCor = FALSE, agcCor = FALSE, agcVal = NULL, 
              ustar = NULL, plot = FALSE, write = FALSE, outputFile, thresholdList = list(H = NULL, 
                                                                                          LE = NULL, Tau = NULL, h2o = NULL), timesList = list(H = NULL, 
                                                                                                                                               LE = NULL, Tau = NULL, h2o = NULL), sunset = 19, sunrise = 6, 
              na.value = "NA") 
    {
        if (!is.data.frame(data)) {
            site <- readEddyPro(data, na = na.value)
        }
        else {
            site <- data
        }
        if (!all(c("year", "yday", "hour", "month") %in% names(site))) 
            site <- createTimestamp(site)
        if (plot) {
            if (!write) {
                if (dev.cur() == 1) {
                    dev.new()
                    dev.new()
                }
            }
        }
        if (length(timesList) == 1) {
            timesList = list(H = timesList, LE = timesList, Tau = timesList, 
                             h2o = timesList)
        }
        if (plot) {
            if (write) {
                plots.before <- paste(dirname(outputFile), "/before.jpg", 
                                      sep = "")
                jpeg(plots.before, width = 1200, height = 800, res = 100)
            }
            else {
                dev.set(2)
            }
            par(mfrow = c(4, 3), oma = c(0, 0, 1.7, 0), mar = c(4, 
                                                                4, 2, 2))
            plot(co2_flux ~ DOY, data = site, type = "p", pch = 16, 
                 cex = 0.8)
            plot(H ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(LE ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(Tau ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(h2o_flux ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(ET ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(wind_speed ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(wind_dir ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(u. ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(air_temperature ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(VPD ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            mtext("Before cleaning_Plot3_2019", outer = TRUE, cex = 1)
            if (write) {
                dev.off(2)
            }
        }
        gas_qc = paste("qc_", gas, sep = "")
        site[[gas]] <- qcClean(site[[gas]], site[[gas_qc]], qcFlag)
        if (sdCor) {
            site[[gas]] <- sdClean(site[[gas]], sdTimes)
        }
        if (distCor) {
            positive <- which(site[[gas]] > 0)
            negative <- which(site[[gas]] < 0)
            site[[gas]][positive] <- distClean(site[[gas]][positive], 
                                               site$hour[positive], site[positive, ])
            site[[gas]][negative] <- distClean(site[[gas]][negative], 
                                               site$hour[negative], site[negative, ])
        }
        if (!is.null(ustar)) {
            if (is.numeric(ustar)) {
                clean.ustar <- which(site$u. <= ustar)
                site[[gas]][clean.ustar] <- NA
            }
            else if (ustar == TRUE) {
                ust <- ustarThreshold(site, sunset = sunset, sunrise = sunrise)
                print(paste("Ustar threshold: ", round(ust, 3), sep = ""), 
                      quote = FALSE)
                clean.ustar <- which(site$u. <= ust)
                site[[gas]][clean.ustar] <- NA
            }
            else if (ustar == FALSE) {
                print("No ustar filtering", quote = FALSE)
            }
        }
        if (agcCor) {
            clean.agc <- which(site[[agcVal]] < 50 | site[[agcVal]] > 
                                   60)
            site$co2_flux[clean.agc] <- NA
            site <- cleanSecondVar(x = "co2_flux", y = agcVal, data = site)
        }
        names_with_co2 = grep("co2", names(site))
        for (i in 2:length(names_with_co2)) {
            site <- cleanSecondVar(x = "co2_flux", y = names(site[, 
                                                                  names_with_co2])[i], data = site)
        }
        site$H <- qcClean(site$H, site$qc_H, qcFlag)
        if (!is.null(thresholdList$H) && is.null(timesList$H)) {
            site <- cleanVar(x = "H", data = site, lessThan = thresholdList$H[1], 
                             greaterThan = thresholdList$H[2])
            site <- cleanSecondVar(x = "H", y = "qc_H", data = site)
        }
        else if (!is.null(timesList$H) && is.null(thresholdList$H)) {
            site$H <- sdClean(site$H, timesList$H)
            site <- cleanSecondVar(x = "H", y = "qc_H", data = site)
        }
        else if (!is.null(timesList$H) && !is.null(thresholdList$H)) {
            site$H <- sdClean(site$H, timesList$H)
            site <- cleanVar(x = "H", data = site, lessThan = thresholdList$H[1], 
                             greaterThan = thresholdList$H[2])
            site <- cleanSecondVar(x = "H", y = "qc_H", data = site)
        }
        site$LE <- qcClean(site$LE, site$qc_LE, qcFlag)
        if (!is.null(thresholdList$LE) && is.null(timesList$LE)) {
            site <- cleanVar(x = "LE", data = site, lessThan = thresholdList$LE[1], 
                             greaterThan = thresholdList$LE[2])
            site <- cleanSecondVar(x = "LE", y = "qc_LE", data = site)
        }
        else if (!is.null(timesList$LE) && is.null(thresholdList$LE)) {
            site$LE <- sdClean(site$LE, timesList$LE)
            site <- cleanSecondVar(x = "LE", y = "qc_LE", data = site)
        }
        else if (!is.null(timesList$LE) && !is.null(thresholdList$LE)) {
            site$LE <- sdClean(site$LE, timesList$LE)
            site <- cleanVar(x = "LE", data = site, lessThan = thresholdList$LE[1], 
                             greaterThan = thresholdList$LE[2])
            site <- cleanSecondVar(x = "LE", y = "qc_LE", data = site)
        }
        site$Tau <- qcClean(site$Tau, site$qc_Tau, qcFlag)
        if (!is.null(thresholdList$Tau) && is.null(timesList$Tau)) {
            site <- cleanVar(x = "Tau", data = site, lessThan = thresholdList$Tau[1], 
                             greaterThan = thresholdList$Tau[2])
            site <- cleanSecondVar(x = "Tau", y = "qc_Tau", data = site)
        }
        else if (!is.null(timesList$Tau) && is.null(thresholdList$Tau)) {
            site$Tau <- sdClean(site$Tau, timesList$Tau)
            site <- cleanSecondVar(x = "Tau", y = "qc_Tau", data = site)
        }
        else if (!is.null(timesList$Tau) && !is.null(thresholdList$Tau)) {
            site$Tau <- sdClean(site$Tau, timesList$Tau)
            site <- cleanVar(x = "Tau", data = site, lessThan = thresholdList$Tau[1], 
                             greaterThan = thresholdList$Tau[2])
            site <- cleanSecondVar(x = "Tau", y = "qc_Tau", data = site)
        }
        site$h2o_flux <- qcClean(site$h2o_flux, site$qc_h2o_flux, 
                                 qcFlag)
        if (!is.null(thresholdList$h2o) && is.null(timesList$h2o)) {
            site <- cleanVar(x = "h2o_flux", data = site, lessThan = thresholdList$h2o[1], 
                             greaterThan = thresholdList$h2o[2])
            site <- cleanSecondVar(x = "h2o_flux", y = "qc_h2o_flux", 
                                   data = site)
        }
        else if (!is.null(timesList$h2o) && is.null(thresholdList$h2o)) {
            site$h2o_flux <- sdClean(site$h2o_flux, timesList$h2o)
            site <- cleanSecondVar(x = "h2o_flux", y = "qc_h2o_flux", 
                                   data = site)
        }
        else if (!is.null(timesList$h2o) && !is.null(thresholdList$h2o)) {
            site$h2o_flux <- sdClean(site$h2o_flux, timesList$h2o)
            site <- cleanVar(x = "h2o_flux", data = site, lessThan = thresholdList$h2o[1], 
                             greaterThan = thresholdList$h2o[2])
            site <- cleanSecondVar(x = "h2o_flux", y = "qc_h2o_flux", 
                                   data = site)
        }
        site <- cleanSecondVar(x = "h2o_flux", y = "ET", data = site)
        site <- cleanSecondVar(x = "LE", y = "ET", data = site)
        if (all(is.na(site[[gas]]))) {
            print("All CO2 is NA. Cleaning stopped")
        }
        if (all(is.na(site$H))) {
            print("All H is NA. Cleaning stopped")
        }
        if (all(is.na(site$LE))) {
            print("All LE is NA. Cleaning stopped")
        }
        if (all(is.na(site$h2o_flux))) {
            print("All H2O is NA. Cleaning stopped")
        }
        if (all(is.na(site$Tau))) {
            print("All Tau is NA. Cleaning stopped")
        }
        if (all(is.na(site$ET))) {
            print("All ET is NA. Cleaning stopped")
        }
        if (plot) {
            if (write) {
                plots.after <- paste(dirname(outputFile), "/after.jpg", 
                                     sep = "")
                jpeg(plots.after, width = 1200, height = 800, res = 100)
            }
            else {
                dev.set(3)
            }
            par(mfrow = c(4, 3), oma = c(0, 0, 1.7, 0), mar = c(4, 
                                                                4, 2, 2))
            plot(co2_flux ~ DOY, data = site, type = "p", pch = 16, 
                 cex = 0.8)
            plot(H ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(LE ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(Tau ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(h2o_flux ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(ET ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(wind_speed ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(wind_dir ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(u. ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            plot(air_temperature ~ DOY, data = site, col = 1, type = "p", 
                 pch = 16, cex = 0.8)
            plot(VPD ~ DOY, data = site, col = 1, type = "p", pch = 16, 
                 cex = 0.8)
            mtext("After cleaning_plot3 2019", outer = TRUE, cex = 1)
            if (write) {
                dev.off(3)
            }
        }
        if (write) {
            if (.Platform$OS.type == "unix") {
                if (missing(outputFile)) {
                    stop("No output file given while trying to write clean output. Please provide a file name.", 
                         call. = FALSE)
                }
                if (!is.data.frame(data)) {
                    header <- paste("head -n 3", file, ">", outputFile, 
                                    sep = " ")
                    system(header)
                }
                drop <- c("timestamp", "yday", "year", "month", "hour")
                site <- site[, !names(site) %in% drop]
                colNames = TRUE
                app = FALSE
                if (!is.data.frame(data)) 
                    colNames = FALSE
                if (!is.data.frame(data)) 
                    app = TRUE
                write.table(site, outputFile, col.names = colNames, 
                            row.names = FALSE, sep = ",", quote = FALSE, 
                            append = app, na = as.character(na.value))
            }
            else if (.Platform$OS.type == "windows") {
                if (missing(outputFile)) {
                    stop("No output file given while trying to write clean output. Please provide a file name.", 
                         call. = FALSE)
                }
                drop <- c("timestamp", "yday", "year", "month", "hour")
                site <- site[, !names(site) %in% drop]
                app = FALSE
                if (!is.data.frame(data)) 
                    app = TRUE
                write.table(site, outputFile, col.names = TRUE, row.names = FALSE, 
                            sep = ",", quote = FALSE, append = app, na = as.character(na.value))
            }
        }
        else {
            return(site)
        }
    }

#options(max.print = 999999999)

#inputdata<- inputdata %>%
   #replace_with_na(replace = list(co2_flux = -9999,H = -9999,LE=-9999, 
                                  # Tau=-9999,h2o_flux=-9999,ET=-9999,wind_speed=-9999,wind_dir=-9999,
                                  # u.=-9999,air_temperature=-9999,VPD=-9999))
#-----------------------------------------------------------------
#Clean fluxes
cleanFluxes(inputdata, gas = "co2_flux", qcFlag = NULL, sdCor = FALSE, sdTimes =
                20, distCor = FALSE, agcCor = FALSE, agcVal = "agc_avg", ustar = 0.1, plot= FALSE, 
            write = TRUE, 
            'C:/Users/grad_student/OneDrive/Documents/EC_P2_2018/filtereddata.csv', #change path
            sunset = 19, sunrise = 6, na.value = "NA")

#-----------------------------------------------------------------

filtereddata <- read.csv ("filtereddata.csv")
#PLot NEE after filtering data
plotfilteredNEE<- ggplot(data = filtereddata, aes(x = DOY, y =co2_flux))+
    geom_point(size=1, colour="red",alpha = 1)+ 
    scale_y_continuous(limits=c(-60,30),expand = c(0, 0))+
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
    #change label by changing year and plot if applies
    labs(title = "Filtered NEE for Year 2019, Plot 3",face="bold", x = "DOY",
         y = expression(bold(NEE~(µmol/s/m^{2}))))
print (plotfilteredNEE)


#percentage of NA in unfiltered and filtered data resp.
#percentNA(inputdata$co2_flux)
'filtereddata<-read.csv("filtereddata.csv")'
#percentNA(thresholdflux$co2_flux)
#percentNA(filtereddata$co2_flux)
'percentNA(combineseason$co2_flux)'


'graphics.off()
print (UnfillteredNEE)
print (UnfillteredNEE_2)
print (plotfilteredNEE)


g<-ggplot(filtereddata, aes(x=DOY))
g2 <- g + geom_point(aes(y=co2_flux), colour="green")

g3<-g2+geom_point(aes(y=co2_flux_f), colour="red")
print(g3)+
    scale_y_continuous(limits=c(-60,50),expand = c(0, 0))+
    scale_x_continuous(limits=c(1,366),expand=c(0,0))'
