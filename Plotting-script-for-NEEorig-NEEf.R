library (ggplot2)
library (dplyr)

setwd("C:/Users/grad_student/Documents/EC_P12_2019")
resultfile<-read.csv ('ERS4_2019_P12_results.csv')

#plot graph after partitioning and gapfilling
ggplot(data = resultfile, aes(x = DoY, y = NEE_U50_orig))+
  geom_point(size=0.9, colour="darkred", )
ggplot(data = resultfile, aes(x = DoY, y = NEE_U50_f))+
  geom_point(size=0.9, colour="steelblue")

#original and filled NEE
'ggplot(resultfile, aes(x=DoY)) + 
  geom_point (aes(y = NEE_U50_orig), color = "red", size = 2, shape = 1, alpha = 1) + 
  geom_point (aes(y = NEE_U50_f), color="black", size = 0.8, shape=4, alpha = 0.6)

ggplot(resultfile, aes(x=DoY)) + 
  geom_point (aes(y = NEE_U50_orig), color = "red", size = 2, shape = 1, alpha = 1) + 
  geom_point (aes(y = NEE_U50_f), color="black", size = 0.8, shape=4, alpha = 0.6)'
  '
  