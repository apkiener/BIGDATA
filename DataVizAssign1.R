####LOAD LIBRARIES####

library(ggplot2)
library(ggthemes)
library(nlme)
library(gganimate)
library(gapminder)
library(ggExtra)
library(psych)
library(reshape2)
library(dplyr)
library(nycflights13)
library(ggcorrplot)
library(waffle)
library(tidyr)
library(scales)
library(ggalt)
library(data.table)
library(extrafont)
library(lubridate)
library(DT)
library(grid)
library(gridExtra)
library(prettydoc)
library(devtools)
library(tidyverse)
library(ggdark)
library(here)
library(png)
library(gifski)
library(forcats)
library(tufte)
library(colorspace)
library(viridisLite)
library(Zelig)
library(formatR)
library(DiagrammeR)
library(xaringan)
library(ggridges)
library(GGally)
library(ggpubr)


####CREATE THEME####

#Picking colors
hcl_color_picker(shiny.trace = FALSE)

#Defining the general colors  
fill_color = '#000000'
decoration_color = '#C0C0C0'
main1_color = '#FF00FF'
main2_color = '#66FFFF'

#Create a personalised theme
groupd_theme <- theme_bw() + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 14, hjust = 0.2, color = decoration_color),
  axis.title = element_text(size = 10, hjust = 0.5, color = decoration_color),
  axis.text = element_text(colour = decoration_color, size = 8),
  axis.ticks = element_blank(),
  axis.line = element_line(colour = decoration_color, size=0.3, linetype = "solid"), 
  panel.border = element_blank(),
  panel.grid = element_blank(),
  strip.text = element_text(size = 12, color = decoration_color),
  panel.background = element_rect(fill = fill_color),
  strip.background =element_rect(fill = fill_color),
  plot.background = element_rect(fill = fill_color),
  legend.background = element_rect(fill = fill_color), 
  legend.text	= element_text(size = 10, hjust = 0.5, color = decoration_color), 
  legend.position = c(0.900, 0.80),
  legend.key = element_blank(),
  legend.title = element_blank(),
)

#Set the new defined theme to the default option 
theme_set(groupd_theme)

####LOAD DATA####

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
Alcohol_Smoking <- fread("Alcohol_Smoking.csv", sep = ",", header = TRUE);
Alcohol_Smoking2 <- fread("Alcohol_Smoking2.csv", sep = ",", header = TRUE);

#Explore the data 
names(Alcohol_Smoking)
head(Alcohol_Smoking, n=10)
str(Alcohol_Smoking)
summary(Alcohol_Smoking)


####ALCOHOL####

#Trendlines per country
ggplot(Alcohol_Smoking, aes(Year, Literpercapita, group=Country)) + 
  geom_line(colour = main2_color)

#Ading a main trainline
ggplot(Alcohol_Smoking, aes(Year, Literpercapita, group=Country)) + 
  geom_line(colour = main2_color)+
  geom_smooth(aes(group = 1), colour = decoration_color, size = 1.5, method = "lm", se = FALSE)

#Advance viz. Alcohol liters per Continent 

p <- ggplot(Alcohol_Smoking[Literpercapita != "NA",list(Literpercapita= avg(Literpercapita)), by = c("Continent","Year")], aes(x = Year, y=Literpercapita,group = Continent, colour = Continent)) +
  geom_point(aes(x=Year, y=Literpercapita, color= Continent),show.legend = TRUE, alpha = 0.7, size = 4) +
  labs(x = "Year", y = "Liter per Capita")+
  theme(legend.position = c(0.90, 0.90)) +
  scale_color_manual(values=c("#C0C0C0", "#FF40FF", "#FFFF00", "#40FF40", "#00FFFF")) 

p + transition_time(Year) +
  labs(title = "Year: {frame_time}")

#Trendlines per country colors by continent 
ggplot(Alcohol_Smoking, aes(Year, Literpercapita, group=Country, colour = Continent)) + 
  geom_line()+
  scale_color_manual(values=c("#C0C0C0", "#FF40FF", "#FFFF00", "#40FF40", "#00FFFF")) +
  theme(legend.position = c(0.90, 0.83), legend.text	= element_text(size = 7, hjust = 0.5, color = decoration_color))

#Smallmultiple: Trendlines per country colors by continent 
ggplot(Alcohol_Smoking, aes(Year, Literpercapita, group=Country,color= Continent)) +
  geom_line(size=0.8, alpha=0.5)+
  facet_wrap( ~ Continent, ncol=3)+
  scale_color_manual(values=c("#C0C0C0", "#FF40FF", "#FFFF00", "#40FF40", "#00FFFF")) +
  stat_smooth(aes(group=1),color=decoration_color)+
  theme(legend.position = "none")

#All data. Trendlines for Europe per country 
ggplot() +
  geom_line(data = transform(Alcohol_Smoking, continent = NULL), aes (Year, Literpercapita, group = Country), alpha = 0.8, lwd = 0.4, colour = decoration_color, linetype = "dotted") +
  geom_line(data=Alcohol_Smoking2[Continent=="Europe"], aes (Year, Literpercapita, group = Country), lwd = 0.3, show.legend = FALSE, color= "#BEF2BF") +
  geom_smooth(data=Alcohol_Smoking2[Continent=="Europe"], aes(Year, Literpercapita, group = 1), lwd = 2, method = 'loess', span = 0.1, se = FALSE, color = "#40FF40") +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Europe")

#Boxplot European countries
ggplot(Alcohol_Smoking[Continent=="Europe"], aes(factor(Year),Literpercapita)) + 
  geom_tufteboxplot(outlier.colour="transparent", color= "#40FF40") + 
  annotate("text", x = 10, y = 25, adj=1,  family="serif", label = c("")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####SMOKING####

#Trendlines per country
ggplot(Alcohol_Smoking2, aes(Year, Gramspercapita, group=Country)) + 
  geom_line(colour = main2_color)

#Ading a main trainline
ggplot(Alcohol_Smoking2, aes(Year, Gramspercapita, group=Country)) + 
  geom_line(colour = main2_color)+
  geom_smooth(aes(group = 1), colour = decoration_color, size = 1.5, method = "lm", se = FALSE)

#Advance viz. Smoking Tobacco grams per Continent 

p <- ggplot(Alcohol_Smoking2[Gramspercapita != "NA",list(Gramspercapita= avg(Gramspercapita)), by = c("Continent","Year")], aes(x = Year, y=Gramspercapita,group = Continent, colour = Continent)) +
  geom_point(aes(x=Year, y=Gramspercapita, color= Continent),show.legend = TRUE, alpha = 0.7, size = 4) +
  labs(x = "Year", y = "Grams per Capita")+
  theme(legend.position = c(0.90, 0.90)) +
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF")) 

p + transition_time(Year) +
  labs(title = "Year: {frame_time}")

#Trendlines per country colors by continent 
ggplot(Alcohol_Smoking2, aes(Year, Gramspercapita, group=Country, colour = Continent)) + 
  geom_line()+
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF")) 

#Smallmultiple: Trendlines per country colors by continent 
ggplot(Alcohol_Smoking2, aes(Year, Gramspercapita, group=Country,color= Continent)) +
  geom_line(size=0.8, alpha=0.5)+
  facet_wrap( ~ Continent, ncol=3)+
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF")) +
  stat_smooth(aes(group=1),color=decoration_color)+
  theme(legend.position = "none") 

#All data. Trendlines for Europe per country 
ggplot() +
  geom_line(data = transform(Alcohol_Smoking2, continent = NULL), aes (Year, Gramspercapita, group = Country), alpha = 0.8, lwd = 0.4, colour = decoration_color, linetype = "dotted") +
  geom_line(data=Alcohol_Smoking2[Continent=="Europe"], aes (Year, Gramspercapita, group = Country), lwd = 0.3, show.legend = FALSE, color= "#BEF2BF") +
  geom_smooth(data=Alcohol_Smoking2[Continent=="Europe"], aes(Year, Gramspercapita, group = 1), lwd = 2, method = 'loess', span = 0.1, se = FALSE, color = "#40FF40") +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Boxplot European countries
ggplot(Alcohol_Smoking2[Continent=="Europe"], aes(factor(Year),Gramspercapita)) + 
  geom_tufteboxplot(outlier.colour="transparent", color= "#40FF40") + 
  annotate("text", x = 10, y = 25, adj=1,  family="serif", label = c("")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####RELATIONSHIP ANALYSIS####

#Fisrt counting NA
count_nas <- function(x){
  ret <- sum(is.na(x));
  return(ret);
}

sapply(Alcohol_Smoking2, count_nas)
Alcohol_Smoking2 <- as.data.table(Alcohol_Smoking2)

head(Alcohol_Smoking2)

#We eliminate rows for missing values in Liters and in Grams
Alcohol_Smoking2 <- Alcohol_Smoking2[Literpercapita != "NA"]
Alcohol_Smoking2 <- Alcohol_Smoking2[Gramspercapita != "NA"]


#Test of correlation for all the data

##First we make a basic scatter plot looking for linearity: We can see there is no a clear
#linear association between the two variables at least when we look for the entire dataset
ggplot(Alcohol_Smoking2, aes(Literpercapita, Gramspercapita)) + 
  geom_point(size=0.02, alpha=0.4, color=main2_color)  + 
  xlab("")+ 
  ylab("")+ 
  scale_x_continuous(breaks = round(as.vector(quantile(Alcohol_Smoking2$Literpercapita)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(Alcohol_Smoking2$Gramspercapita)), digits = 1))

#Test of correlation per continent

#Scatterplot using colors for continent: From the below ilustration, for each continent we still can not see
#a clear linear association between the two variables
ggplot(Alcohol_Smoking2, aes(Literpercapita, Gramspercapita, color = Continent)) +
  geom_point(alpha=0.5) +
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF"))

#To doble check that relationship we use the Smallmultiple - Scatterplot to see if indeed is no linear
#relationship between Alcohol and Smoking in each Continent. The conclusion is there is not relationship
ggplot(Alcohol_Smoking2, aes(x=Literpercapita, y=Gramspercapita, color = Continent)) +
  geom_point(size=0.8, alpha=0.25)+
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF"))+
  facet_wrap( ~ Continent, ncol=2, scales = "free")+
  theme(legend.position = "none") 

#Finally we use a Advance viz. Scatter Plot Evolution to see how is the evolution of both 
#Alcohol and Tobacco consumption at the same time among all the contintents
p <- ggplot(Alcohol_Smoking2, aes(Literpercapita, Gramspercapita, color = Continent)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF"))

p + transition_time(Year) +
  labs(title = "Year: {frame_time}")






##This is only to doble check no correlation in the data. Not need to include in the script
#Test of correlation for all the data

##First we make a basic scatter plot looking for linearity: We can see there is no a clear
#linear association between the two variables at least when we look for the entire dataset
ggplot(Alcohol_Smoking2, aes(Literpercapita, Gramspercapita)) + 
  geom_point(size=0.02, alpha=0.4, color=main2_color)  + 
  xlab("")+ 
  ylab("")+ 
  scale_x_continuous(breaks = round(as.vector(quantile(Alcohol_Smoking2$Literpercapita)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(Alcohol_Smoking2$Gramspercapita)), digits = 1))


##Second we test if the variables that we are using are normally distributed
# using the Shapiro-Wilk normality test and a visual inspection of the Q-Q plots (quantile-quantile plots)

#Shapiro-Wilk normality test for Liters per capita
shapiro.test(Alcohol_Smoking2$Literpercapita)

#Shapiro-Wilk normality test for Grams per capita
shapiro.test(Alcohol_Smoking2$Gramspercapita)

#Q-Q plot "Liter per capita""
ggqqplot(Alcohol_Smoking2$Literpercapita, ylab = "Liter per capita")

#Q-Q plot "Grams per capita""
ggqqplot(Alcohol_Smoking2$Gramspercapita, ylab = "Grams per capita")

#Finally we run the test of correlation: We can see that for all the data the variables have no correlation at all
cor.test(Alcohol_Smoking2$Literpercapita, Alcohol_Smoking2$Gramspercapita, method="kendall")

#Test of correlation per continent

#Scatterplot using colors for continent: From the below ilustration we can see that apparently
#the only continent that have a linear association between those variable is America
ggplot(Alcohol_Smoking2, aes(Literpercapita, Gramspercapita, color = Continent)) +
  geom_point(alpha=0.5) +
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF"))

#To doble check that relationship we use the Smallmultiple - Scatterplot to see the linear
#relationship for each Continent
ggplot(Alcohol_Smoking2, aes(x=Literpercapita, y=Gramspercapita, color = Continent)) +
  geom_point(size=0.8, alpha=0.25)+
  scale_color_manual(values=c("#FF40FF", "#FFFF00", "#40FF40", "#00FFFF"))+
  facet_wrap( ~ Continent, ncol=2, scales = "free")+
  theme(legend.position = "none") 

#Shapiro-Wilk normality test for Liters per capita

#America
shapiro.test(Alcohol_Smoking2[Continent=="America",Literpercapita])
#Asia
shapiro.test(Alcohol_Smoking2[Continent=="Asia",Literpercapita])
#Europe
shapiro.test(Alcohol_Smoking2[Continent=="Europe",Literpercapita])
#Oceania
shapiro.test(Alcohol_Smoking2[Continent=="Oceania",Literpercapita])


#Shapiro-Wilk normality test for Grams per capita

#America
shapiro.test(Alcohol_Smoking2[Continent=="America",Gramspercapita])
#Asia
shapiro.test(Alcohol_Smoking2[Continent=="Asia",Gramspercapita])
#Europe
shapiro.test(Alcohol_Smoking2[Continent=="Europe",Gramspercapita])
#Oceania
shapiro.test(Alcohol_Smoking2[Continent=="Oceania",Gramspercapita])


#Finally we run the test of correlation: We can see that for all the data the variables have no correlation at all

#America
cor.test(Alcohol_Smoking2[Continent=="America",Literpercapita], Alcohol_Smoking2[Continent=="America",Gramspercapita], method="kendall")
#Asia
cor.test(Alcohol_Smoking2[Continent=="Asia",Literpercapita], Alcohol_Smoking2[Continent=="Asia",Gramspercapita], method="kendall")
#Europe
cor.test(Alcohol_Smoking2[Continent=="Europe",Literpercapita], Alcohol_Smoking2[Continent=="Europe",Gramspercapita], method="kendall")
#Oceania
cor.test(Alcohol_Smoking2[Continent=="Oceania",Literpercapita], Alcohol_Smoking2[Continent=="Oceania",Gramspercapita], method="kendall")


