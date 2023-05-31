library(ggplot2)
library(dplyr)
## Load the data
setwd("C:/Users/grad_student/OneDrive/Documents/EC_p3_2020")
flux <- read.csv ("gapfilledresults.csv")

#----------------------------------------------------------
"Standard error function"
se <- function(x, na.rm = TRUE){
  sd(x, na.rm = TRUE)/sqrt(length(x))
}

#-------------------------------------------------------
"Monthly Data" #Remember to change doy according to the year you are visualizing
Mar <- subset(flux, DoY >= 61 & DoY <= 91,
              select=c(DoY, Hour, NEE_U50_f))
names(Mar)<-c("DoY","Hour","Mar")
Mar <- Mar %>% 
  group_by(Hour) %>% 
  summarise_at (vars
                (Mar), 
                funs(mean,se), na.rm=TRUE)
names(Mar)<-c("Hour11","Mar_mean","Mar_se")

Apr <- subset(flux, DoY >= 92 & DoY <= 121,
              select=c(DoY, Hour, NEE_U50_f))
names(Apr)<-c("DoY","Hour","Apr")
Apr <- Apr %>% 
  group_by(Hour) %>% 
  summarise_at (vars
                (Apr), 
                funs(mean,se), na.rm=TRUE)
names(Apr)<-c("Hour12","Apr_mean","Apr_se")

May <- subset(flux, DoY >= 122 & DoY <= 152,
                  select=c(DoY, Hour, NEE_U50_f))
names(May)<-c("DoY","Hour","May")
May <- May %>% 
  group_by(Hour) %>% 
  summarise_at (vars
                (May), 
                funs(mean,se), na.rm=TRUE)
names(May)<-c("Hour","May_mean","May_se")
June <- subset(flux, DoY >= 153 & DoY <= 182,
                  select=c(Hour,NEE_U50_f))
names(June)<-c("Hour1","June")
June <- June %>% 
  group_by(Hour1) %>% 
  summarise_at (vars
                (June), 
                funs(mean,se), na.rm=TRUE)
names(June)<-c("Hour1","June_mean","June_se")
July <- subset(flux, DoY >= 183 & DoY <= 213,
               select=c(Hour,NEE_U50_f))
names(July)<-c("Hour2", "July")
July <- July %>% 
  group_by(Hour2) %>% 
  summarise_at (vars
                (July), 
                funs(mean,se), na.rm=TRUE)
names(July)<-c("Hour2","July_mean","July_se")

August <- subset(flux, DoY >= 214 & DoY <= 244,
               select=c(Hour,NEE_U50_f))
names(August)<-c("Hour3","August")
August <- August %>% 
  group_by(Hour3) %>% 
  summarise_at (vars
                (August), 
                funs(mean,se), na.rm=TRUE)
names(August)<-c("Hour3","August_mean","August_se")

Sept <- subset(flux, DoY >= 245 & DoY <= 274,
                 select=c(Hour,NEE_U50_f))
names(Sept)<-c("Hour4","Sept")
Sept <- Sept %>% 
  group_by(Hour4) %>% 
  summarise_at (vars
                (Sept), 
                funs(mean,se), na.rm=TRUE)
names(Sept)<-c("Hour4","Sept_mean","Sept_se")

Octo <- subset(flux, DoY >= 275 & DoY <= 305,
                 select=c(Hour,NEE_U50_f))
names(Octo)<-c("Hour5","Octo")
Octo <- Octo %>% 
  group_by(Hour5) %>% 
  summarise_at (vars
                (Octo), 
                funs(mean,se), na.rm=TRUE)
names(Octo)<-c("Hour5","Octo_mean","Octo_se")

Novb <- subset(flux, DoY >= 306 & DoY <= 335,
                 select=c(Hour,NEE_U50_f))
names(Novb)<-c("Hour6","Novb")
Novb <- Novb %>% 
  group_by(Hour6) %>% 
  summarise_at (vars
                (Novb), 
                funs(mean,se), na.rm=TRUE)
names(Novb)<-c("Hour6","Novb_mean","Novb_se")

Decb <- subset(flux, DoY >= 336 & DoY <= 366,
                 select=c(Hour,NEE_U50_f))
names(Decb)<-c("Hour7","Decb")
Decb <- Decb %>% 
  group_by(Hour7) %>% 
  summarise_at (vars
                (Decb), 
                funs(mean,se), na.rm=TRUE)
names(Decb)<-c("Hour7","Decb_mean","Decb_se")

commonths<-cbind(Mar,Apr,May,June,July,August,Sept,Octo,Novb,Decb)

#write.csv(commonths,"C:/Users/grad_student/OneDrive/Documents/EC_p3_2018/diurnalmonths_18div.csv" )

#-----------------------------------------
# Plot the diurnal patterns for NEE
diurnplot<-ggplot(data=commonths)+
  geom_line(mapping=aes(y=Mar_mean,x= Hour,color="Mar"),size=1 ) +
  geom_line(mapping=aes(y=Apr_mean,x= Hour,color="Apr"),size=1 ) +
  geom_line(mapping=aes(y=May_mean,x= Hour,color="May"),size=1 ) +
  geom_line(mapping=aes(y=June_mean,x= Hour,color="Jun"),size=1 ) +
  geom_line(mapping=aes(y=July_mean,x= Hour,color="Jul"),size=1 ) +
  geom_line(mapping=aes(y=August_mean,x= Hour,color="Aug"),size=1 ) +
  geom_line(mapping=aes(y=Sept_mean,x= Hour,color="Sep"),size=1 ) +
  geom_line(mapping=aes(y=Octo_mean,x= Hour,color="Oct"),size=1 ) +
  geom_line(mapping=aes(y=Novb_mean,x= Hour,color="Nov"),size=1 ) +
  geom_line(mapping=aes(y=Decb_mean,x= Hour,color="Dec"),size=1 ) +
    scale_color_manual(values = c('Mar'='navyblue','Apr'='brown',
    'May' = 'Blue','Jun' = 'green','Jul' = 'red','Aug' = 'cyan',
    'Sep' = 'darkviolet','Oct' = 'gold', 'Nov' = 'orange4',
    'Dec' = 'black'))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(limits=c(-40,20),expand = c(0, 0))+
  #Labels appearance
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust=0.5),
        axis.title.x = element_text(size=14, colour = "black"),    
        axis.title.y = element_text(size=14, colour = "black"),    
        axis.text.x = element_text(size=12,  colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        panel.background = element_blank()) +
  labs(main="May",x = expression("Time of day (h)"), y = expression(Mean~NEE~(µmol/m^{2}/s)))
#legend description
diurnplot+ theme(
  legend.position = c(0.95, 0.98),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.direction="horizontal",
  legend.text = element_text(size = 14, colour = "black"),
  legend.title=element_blank(),
  legend.key = element_rect(fill = "transparent", colour = "transparent")
)
'diurnplot + 
  geom_errorbar(aes(ymin=NEEDiv_mean-NEEDiv_se, ymax=NEEDiv_mean+NEEDiv_se), width=.4)'