library (ggplot2)
library (dplyr)
library(magrittr)

graphics.off()
setwd("C:/Users/grad_student/OneDrive/Documents/EC_P3_2018")#change path
gfresult <- read.csv ("gapfilledresults2.csv") 

#plot graph after daily sums calculations and half-hourly conversion to hourly

#summarise by daily sums
datasum <- gfresult  %>% 
  group_by(DoY) %>% 
  summarise_at (vars
                (NEE_U50_f,Reco_U50, GPP_U50_f), 
                funs(mean), na.rm=TRUE) 

#daily sums to hourly data
dailysums<-
  mutate (datasum, NEE_U50ds = NEE_U50_f/2,
          Reco_U50ds=Reco_U50/2, GPP_U50_fds=-(GPP_U50_f/2)
          )
#plot daily sums graph
#daily sum NEE
ggplot(data = dailysums, aes(x = DoY, y = NEE_U50ds))+
  geom_line(size=1.1, colour="blue",alpha = 1, shape=9) +
  scale_y_continuous(limits=c(-6,4), expand = c(0, 0))+
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
  labs(title = "NEE Daily Sums for Year 2020, Plot 12",face="bold", x = "DOY",
       y = expression(bold(NEE~(gCm^{-2}~day^{-1}))))

#GPP and Re daily sum
plotReGPP<- ggplot(data = dailysums, )+
  geom_line(aes(x = DoY, y = GPP_U50_fds, colour="black"), 
          size=1, alpha = 1, shape=19)+
  geom_line(aes(x = DoY, y =Reco_U50ds,colour="darkgrey"),
            size=1.1, alpha = 1, shape=19)+
  scale_color_discrete (labels=c("GPP", "Re"))+
    scale_y_continuous(limits=c(-10,5),expand = c(0, 0))+
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
          labs(title = "GPP and Re Daily Sums for Year 2020, Plot 12",face="bold", x = "DOY",
       y = expression(bold(Re~and~GPP~(gCm^{-2}~day^{-1}))))
#legend description
plotReGPP+ theme(
  legend.position = c(0.95, 0.2),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.text = element_text(size = 12, colour = "black"),
  legend.title=element_blank(),
  legend.key = element_rect(fill = "transparent", colour = "transparent")
)

#Cummulative NEE sums and plot
cumdata <-mutate(dailysums, cumNEE= cumsum(NEE_U50ds))
plotcumNEE<- ggplot(data = cumdata, aes(x = DoY, y = cumNEE))+
  geom_line(size=1.1, colour="red",alpha = 1)+
  scale_y_continuous(limits=c(-150,100),expand = c(0, 0))+
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
  labs(title = "Cummulative NEE for Year 2020, Plot 12",face="bold", x = "DOY",
       y = expression(bold(Cumulative~NEE~(gC/m^{2}))))
print (plotcumNEE)

#Cummulative GPP sums and plot
cumdata <-mutate(dailysums, cumGPP= cumsum(GPP_U50_fds))
plotcumGPP<- ggplot(data = cumdata, aes(x = DoY, y = cumGPP))+
  geom_line(size=1.1, colour="red",alpha = 1)+
  scale_y_continuous(limits=c(-1000,100),expand = c(0, 0))+
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
  labs(title = "Cummulative GPP for Year 2018, Plot 3",face="bold", x = "DOY",
       y = expression(bold(Cumulative~GPP~(gC/m^{2}))))
print (plotcumGPP)

#Cummulative Re sums and plot
cumdata <-mutate(dailysums, cumReco= cumsum(Reco_U50ds))
plotcumRe<- ggplot(data = cumdata, aes(x = DoY, y = cumReco))+
  geom_line(size=1.1, colour="red",alpha = 1) +
  scale_y_continuous(limits=c(-100,400),expand = c(0, 0))+
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
  labs(title = "Cummulative Re for Year 2020, Plot 12",face="bold", x = "DOY",
       y = expression(bold(Cumulative~Re~(gC/m^{2}))))
print (plotcumRe)

#save data
write.csv(dailysums, "C:/Users/grad_student/OneDrive/Documents/EC_P3_2018/dailysums2.csv")
