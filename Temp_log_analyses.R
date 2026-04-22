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


######
# Modelling
######

#