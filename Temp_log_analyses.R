rm(list=ls())
dir.exists("//storage-dh.slu.se/home$/sedi0002/My Documents/Job/Solab/Temp_log/Temp_log_data")
setwd("//storage-dh.slu.se/home$/sedi0002/My Documents/Job/Solab/Temp_log/Temp_log_data")

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
#library(tidyr)
library(gplots)
library(lattice)
library(nlme)
# library(MASS) # potenital name clash problem for function select in dplyr
library(piecewiseSEM)
library(lme4)
library(car)
library(visreg)
library(ggeffects)


#####
# Read Datasets
#####

# read the whole dataset with Swedish cHaracters 
# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("Antal dygn mindre än 0,2 grader.txt", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### read datasets
#my0<- read.csv2("Antal dygn mindre än 0,2 grader.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my1<- read.csv2("Årsmedel_ -max och -min.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my2<- read.csv2("Dygnsmedel_ - max_ min (sommarhalvår).txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my3<- read.csv2("Dygnsmedel_ - max_ min (vinterhalvår).txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my4<- read.csv2("Dygnsmedel_ - max_ min_ differens.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my5<- read.csv2("Månadsmedel_ -max och -min.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my6<- read.csv2("Mätdatum.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my7<- read.csv2("Periodmedel sommarhalvår.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my8<- read.csv2("Periodmedel vinterhalvår.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my10<- read.csv2("Vattendrag och lokal.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
#my11<- read.csv2("vattendrag.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 

my<- read.csv2("Totaldata.txt",fileEncoding ="ISO-8859-1",  header=TRUE, sep=";", dec=".") 
head(my)
summary(my)

# exploration plots - don't run
#ggplot(my, aes(x = År, y = Temperatur)) +
#  geom_point(size=2)+ 
#  facet_wrap(~Namn)+
#  theme_bw(base_size=15)

unique(my$Namn) # 38 rivers
unique(my$Lokalnamn) # 39 sites
table(my$Namn, my$Lokalnamn) # 1 site per river (ca)

hist(my$Frekvens..mätningar.dygn.) # majority has measurements every two hours, few every 4, fewer every 6
table(my$Frekvens..mätningar.dygn., my$Månad) # but well distributed throughout the year
table(my$Frekvens..mätningar.dygn., my$Dygn) # the day of the month
table(my$Frekvens..mätningar.dygn., my$Timme) # some hours are covered best
hist(my$Timme)
table(my$Lokalnamn, my$År) # variable time serie length

unique(my$Instrumenttyp) # three types
hist(my$Djup) # OBS

# subset
my_site<-filter(my, Lokalnamn == "1 km S Aggarps skola") %>%
  filter(År == 1998)  %>%
  filter(Månad == 9)

# OBS: Frekvens..mätningar.dygn. does not correspond to the actual number of measurements in the 
# dataset, better to calculate my own variable!

# aggregate
my_day<-my %>% 
  group_by(Lokalnamn, År, Månad, Dygn) %>%
  summarise(avg_day_Temperatur=mean(Temperatur ,na.rm=TRUE),
            sd_day_Temperatur = sd(Temperatur ,na.rm=TRUE),
            avg_day_Djup=mean(Djup ,na.rm=TRUE),
            sd_day_Djup=sd(Djup ,na.rm=TRUE), # it will be zero if only 1 measurement was taken
            avg_Lat=mean(Xkoordinat,na.rm=TRUE),
            N_in_day= n()
  ) 

my_month<-my %>% 
  group_by(Lokalnamn, År, Månad) %>%
  summarise(avg_month_Temperatur=mean(Temperatur ,na.rm=TRUE),
            sd_month_Temperatur = sd(Temperatur ,na.rm=TRUE),
            avg_month_Djup=mean(Djup ,na.rm=TRUE),
            sd_month_Djup=sd(Djup ,na.rm=TRUE),
            avg_Lat=mean(Xkoordinat,na.rm=TRUE),
            N_in_month= n()
  ) 


my_year<-my %>% 
  group_by(Lokalnamn, År) %>%
  summarise(avg_year_Temperatur=mean(Temperatur ,na.rm=TRUE),
            sd_year_Temperatur = sd(Temperatur ,na.rm=TRUE),
            avg_year_Djup=mean(Djup ,na.rm=TRUE),
            sd_year_Djup=sd(Djup ,na.rm=TRUE),
            avg_Lat=mean(Xkoordinat,na.rm=TRUE),
            N_in_year= n()
  ) 

ggplot(my_year, aes(x = År, y = avg_year_Temperatur)) +
  geom_point(size=2)+ 
  geom_smooth(method = "loess")+ 
  facet_wrap(~Lokalnamn)+
  theme_bw(base_size=15)
ggplot(my_year, aes(x = År, y = avg_year_Temperatur)) +
  geom_point(size=2)+ 
  geom_smooth(method = "loess")+ 
  facet_wrap(~avg_Lat)+
  theme_bw(base_size=15)
ggplot(my_year, aes(x = År, y = N_in_year)) +
  geom_point(size=2)+ 
  facet_wrap(~Lokalnamn)+
  theme_bw(base_size=15)
ggplot(my_year, aes(x = År, y = sd_year_Temperatur)) +
  geom_point(size=2)+ 
  facet_wrap(~Lokalnamn)+
  theme_bw(base_size=15)

ggplot(my_month, aes(x = År, y = avg_month_Temperatur, colour = Månad)) +
  geom_point(size=2)+ 
  facet_wrap(~Lokalnamn)+
  geom_smooth(aes(group = Månad), method = "loess")+
  theme_bw(base_size=15)

ggplot(my_year, aes(x = avg_year_Djup, y = avg_year_Temperatur)) +
  geom_point(size=2)+ 
  facet_wrap(~Lokalnamn)+
  theme_bw(base_size=15)

# explore variation introduced by depth, different instruments, different n of replicates 
# plot sd of temp
# explore finer level of aggregation, aggregate by season
# plot sd vs temp by site, and depth vs temp by site and year/month/day/hour


