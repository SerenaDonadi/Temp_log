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
library(mgcv)

# Save entire workspace (creates my_workspace.RData)
save.image(file = "my_workspace.RData")

# Restore
load("my_workspace.RData")        # restores all objects


#####
# Read Datasets and subsets
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

my_season<-my %>%
  mutate(
    Månad = as.integer(Månad),
    season = case_when(
      Månad %in% c(12,1,2) ~ "Winter",
      Månad %in% c(3,4,5)   ~ "Spring",
      Månad %in% c(6,7,8)   ~ "Summer",
      Månad %in% c(9,10,11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    season_year = if_else(Månad == 12, År + 1L, År) # If the month is December, assign the observation to next year; otherwise, keep the current year.
  ) 

# aggregate
my_day<-my_season %>% 
  group_by(Lokalnamn, År, Månad, Dygn,season,season_year,Instrumenttyp) %>%
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

##### 
# exploration plots
#####
ggplot(my_year, aes(x = År, y = avg_year_Temperatur)) +
  geom_point(size=2)+ 
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


# mean year temp by site, with error bars of sd, ordered by latitude
library(dplyr)
library(ggplot2)

site_means <- my_year |>
  group_by(Lokalnamn) |>
  summarise(
    mean_temp = mean(avg_year_Temperatur, na.rm = TRUE),
    sd_temp   = sd(avg_year_Temperatur, na.rm = TRUE),
    avg_Lat   = mean(avg_Lat, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(avg_Lat) |>
  mutate(Lokalnamn = factor(Lokalnamn, levels = Lokalnamn))

p <- ggplot(site_means, aes(x = mean_temp, y = Lokalnamn)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = mean_temp - sd_temp, xmax = mean_temp + sd_temp), height = 0.2, alpha = 0.6) +
  labs(x = "Mean annual temperature (°C)", y = NULL, title = "Mean annual temperature by site") +
  theme_bw(base_size = 12)

p

# mean seasonal temperature per site ordered by latitude
site_order <- my_year |>
  group_by(Lokalnamn) |>
  summarise(mean_lat = mean(avg_Lat, na.rm = TRUE), .groups = "drop") |>
  arrange(mean_lat) |>
  pull(Lokalnamn)

seas_data <- my |>
  mutate(
    Månad = as.integer(Månad),
    season = case_when(
      Månad %in% c(12,1,2) ~ "Winter",
      Månad %in% c(3,4,5)   ~ "Spring",
      Månad %in% c(6,7,8)   ~ "Summer",
      Månad %in% c(9,10,11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    season_year = if_else(Månad == 12, År + 1L, År) # If the month is December, assign the observation to next year; otherwise, keep the current year.
  ) |>
  filter(!is.na(season)) |>
  group_by(Lokalnamn, season, season_year) |>
  summarise(
    mean_temp = mean(Temperatur, na.rm = TRUE),
    sd_temp   = sd(Temperatur, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(Lokalnamn = factor(Lokalnamn, levels = site_order))

p_season <- ggplot(seas_data, aes(x = season_year, y = mean_temp, color = season, group = season)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), width = 0.2, alpha = 0.6) +
  geom_point(size = 1.8) +
  facet_wrap(~Lokalnamn) +
  labs(x = "Year", y = "Mean temperature (°C)", color = "Season") +
  theme_bw(base_size = 12)

p_season

# influence of depth on temperature, by site
depth_plot_data <- my |>
  mutate(
    Månad = as.integer(Månad),
    season = case_when(
      Månad %in% c(12,1,2) ~ "Winter",
      Månad %in% c(3,4,5)   ~ "Spring",
      Månad %in% c(6,7,8)   ~ "Summer",
      Månad %in% c(9,10,11) ~ "Autumn",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(Djup), !is.na(season))

p_depth <- ggplot(depth_plot_data, aes(x = Djup, fill = season)) +
  geom_histogram(bins = 30, color = "white", position = "stack") +
  facet_wrap(~Xkoordinat, scales = "free_y") +
  labs(x = "Depth (Djupt)", y = "Count", fill = "Season") +
  theme_bw(base_size = 12)

p_depth

# R
scatter_data <- my |>
  mutate(
    Månad = as.integer(Månad),
    season = case_when(
      Månad %in% c(12,1,2) ~ "Winter",
      Månad %in% c(3,4,5)   ~ "Spring",
      Månad %in% c(6,7,8)   ~ "Summer",
      Månad %in% c(9,10,11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    Djup = as.numeric(as.character(Djup)),
    Lokalnamn = factor(Lokalnamn, levels = site_order)
  ) |>
  filter(!is.na(Djup), !is.na(Temperatur), !is.na(season))

p_depth_scatter <- ggplot(scatter_data, aes(x = Djup, y = Temperatur, color = season)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.6, size = 1.25) +
  facet_wrap(~Lokalnamn, scales = "free_x") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Djup (m)", y = "Temperatur (°C)", color = "Season") +
  theme_bw(base_size = 12)

p_depth_scatter



##### add and fix time variables before modeling ####

# build a "date" variable for the my_day dataset
my_day$date <- as.Date(
  paste(my_day$År, my_day$Månad, my_day$Dygn, sep = "-"),
  format = "%Y-%m-%d"
)
class(my_day$date)
# check for invalid dates:
sum(is.na(my_day$date))

# remove Feb 29 (so that each year has the same range, hence starting/ending point for 
# cyclic splines, for day of the year) :
my_day2 <- my_day[format(my_day$date, "%m-%d") != "02-29", ]
# shift all days after Feb 28 in leap years back by 1.
library(lubridate)
my_day2$doy <- yday(my_day2$date)
# Identify leap years
is_leap <- leap_year(my_day2$date)
# Collapse leap years to 365-day cycle
my_day2$doy[is_leap & my_day2$doy > 59] <-my_day2$doy[is_leap & my_day2$doy > 59] - 1
range(my_day2$doy) # check

# TO EXPLORE TEMPORAL TREND
# create a time variable that can handle missing years, Ciclicity (the vicinity of dec31 and
# jan 1, and retain daily resolution:
my_day2$time <- as.numeric(my_day2$date - min(my_day2$date))

# TO EXPLORE SEASONALITY
# create a variable as ordinal number for the day within a year. Use
my_day2$doy 


######
# Modelling
######

lmc <- lmeControl(niterEM = 5000,msMaxIter = 1000)

unique(my_day$Lokalnamn)

# make lokal as factor:
is.factor(my_day2$Lokalnamn)
levels(my_day2$Lokalnamn)
table(my_day2$Lokalnamn)

my_day2$Lokalnamn <- factor(my_day2$Lokalnamn)
my_day2 <- my_day2[!is.na(my_day2$Lokalnamn), ] # there are no NAs anyways

# model: s(time) → common long‑term trend,s(doy) → common seasonality,
# s(location) → location‑specific baseline differences. This is modeled as random!
# bs=re means: odel this variable as a random effect with shrinkage.
# GAM equivalent of a random intercept in a mixed‑effects model. 
# it is an average temperature offset per location. 

# without corr str, and with site as random intercept
M1<-gam(avg_day_Temperatur ~ avg_day_Djup +
    s(time, k = 30) +
    s(doy, bs = "cc", k = 10) +
    s(Lokalnamn, bs = "re"),
  data = my_day2,
  method = "REML"
)

summary(M1)
anova(M1) # useful when I have a factor, gives a overall F test
plot(M1)

# without corr str, and with site as random slope over time
M2<-gam(avg_day_Temperatur ~ avg_day_Djup +
          s(time, k = 30) +
          s(doy, bs = "cc", k = 10) +
          s(time, Lokalnamn, bs = "fs"),
        data = my_day2,
        method = "REML"
)
summary(M2)
anova(M2) # useful when I have a factor, gives a overall F test
plot(M2)

# without corr str, and with site as random slope over seasonality (location-specific seasonality)
M3<-gam(avg_day_Temperatur ~ avg_day_Djup +
          s(time, k = 30) +
          s(doy, bs = "cc", k = 10) +
          s(doy, Lokalnamn, bs = "fs"),
        data = my_day2,
        method = "REML"
)
summary(M3)
anova(M3) # useful when I have a factor, gives a overall F test
plot(M3)







# model with no temporal correl  - wrong, don't run
M1<-gam(avg_day_Temperatur ~ avg_day_Djup*season + 
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "1 km S Aggarps skola")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "300 m ovan väg")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "700 m uppströms Mörtsjön")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Arålund")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "E4:an Nr 2")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Fredriksdal")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "G:a Stenbron")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "G:a järnvägsbron")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Gamla Kvarnen")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Gravbacka")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Huvudfåran")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Hästgången")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Jockara Fäbod")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Kopparhemmet")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Källsjöklack")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Laxbäcken Övre")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Lyckemyran")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Munkhättan")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Mynningsnära")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ned Sågen")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Nedre")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Nedströms Bosgårdsfallet")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ovan E4:an")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ovan väg")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ryerna")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Sandån Nedre")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Sandån övre")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Storsele")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Stråfulnäset övre")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Sågverket")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Tangådalsstugan Ö fåran")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Vedema")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Vid Vägen")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Vingäng nedre")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Västra Lövås")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Västra Styberget")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ängarna")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Åbro")) +
          s(År,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Övre Ejgst")), 
        method = "REML", data=my_day)

summary(M1)
anova(M1) # useful when I have a factor, gives a overall F test
#The fx and k means that the amount of smoothing is not fixed to a predeternmined value;hence, cross-validation is used to estimate the optimal amount of smoothing. 
# bs is for cubic regression spline to be used. try either "cr" or "cs".
#if convergence problems: 
# The option "control = lmc," can be used to ensure convergence
# One option is to increase the number of iterations in the routine or reduce the convergence criteria, see the help file of gamm
# Other options are to fix the degrees of freedom (and not use cross-validation) 

# plots
plot(M1)


