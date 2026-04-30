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
# create a time variable that can handle missing years, ciclicity (the vicinity of dec31 and
# jan 1, and retain daily resolution:
my_day2$time <- as.numeric(my_day2$date - min(my_day2$date))

# TO EXPLORE SEASONALITY
# create a variable as ordinal number for the day within a year. Use
my_day2$doy 

min(my_day2$date)
# to check what date corresponds to a certain numeric value of "time":
as.Date(min(my_day2$date) + 3000)
as.Date(min(my_day2$date) + 4800)


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
# s(location) → location‑specific baseline differences. 

##### site as random ####

# for random intercept use:bs=re means: model this variable as a random effect with shrinkage.
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

# risks:data per site are sparse or irregular across the year. check:
#####
# Number of observations per site: differs bc different number of sampling years
site_n <- my_day2 %>%
  group_by(Lokalnamn) %>%
  summarise(n_obs = n())
site_n
# Seasonal coverage (range of DOY): all have 365!
site_doy_range <- my_day2 %>%
  group_by(Lokalnamn) %>%
  summarise(
    doy_min = min(doy, na.rm = TRUE),
    doy_max = max(doy, na.rm = TRUE),
    doy_span = doy_max - doy_min + 1
  )
site_doy_range
# Are there large gaps in the seasonal cycle? no
site_doy_gaps <- my_day2 %>%
  arrange(Lokalnamn, doy) %>%
  group_by(Lokalnamn) %>%
  summarise(
    max_gap = max(diff(sort(unique(doy))), na.rm = TRUE)
  )
site_doy_gaps
# How much of the year is actually observed? 100%
site_doy_coverage <- my_day2 %>%
  group_by(Lokalnamn) %>%
  summarise(
    n_unique_doy = n_distinct(doy),
    coverage_frac = n_unique_doy / 365
  )
site_doy_coverage

library(ggplot2)

ggplot(my_day2, aes(x = doy, y = Lokalnamn)) +
  geom_point(alpha = 0.3, size = 0.7) +
  labs(x = "Day of year", y = "Site") +
  theme_bw()
#####
# without corr str, and with site as random slope over time AND over season
M6 <- gam(
  avg_day_Temperatur ~ avg_day_Djup +
    s(time, k = 30) +
    s(doy, bs = "cc", k = 10) +
    s(time, Lokalnamn, bs = "fs", k = 10) +
    s(doy, Lokalnamn, bs = "fs", k = 5),
  data = my_day2,
  method = "REML"
)
summary(M6)
plot(M6)
#mgcv‑specific check
summary(M6)$s.table
# Red flags: EDF ≈ 0 for many site smooths. Very large SEs. 
# Warnings about rank deficiency

# check for Residual temporal autocorrelation:
# extract normalized (Pearson) residuals
res <- resid(M6, type = "pearson")
time <- my_day2$time
site <- my_day2$Lokalnamn
acf(res, lag.max = 100)
# Spikes outside the confidence bands → autocorrelation. 
# Strong spike at lag 1 → AR(1)-type process
# Slow decay → under-smoothed long-term trend
#️ This mixes all sites — it’s only diagnostic, not definitive.
# Asses autocorrelation within-site 
# check if time is monotonic within sites
time_check <- my_day2 %>%
  arrange(Lokalnamn, time) %>%
  group_by(Lokalnamn) %>%
  summarise(
    time_ok = all(diff(time) >= 0),
    .groups = "drop"
  )
time_check # ok

par(mfrow = c(1, 1))
for (s in unique(site)) {
  idx <- site == s
  if (sum(idx) > 20) {   # avoid tiny series
    acf(res[idx], main = paste("ACF:", s))
  }
}
par(mfrow = c(1, 1))
# Many sites show lag‑1 correlation → real temporal structure
# Only a few sites → consider site‑specific solutions
# Formal test: Durbin–Watson–type diagnostic (by site)
my_day2$res <- resid(M6, type = "pearson")
dw_by_site <- my_day2 %>%
  arrange(Lokalnamn, time) %>%
  group_by(Lokalnamn) %>%
  summarise(
    dw = sum(diff(res)^2, na.rm = TRUE) /
      sum(res^2, na.rm = TRUE),
    n = n()
  )
dw_by_site # strong autocorrelation
# DW ≈ 2 → no autocorrelation, DW < 1.5 → positive autocorrelation, DW < 1.2 → strong autocorrelation

# Is autocorrelation actually a problem? Did the smooths already soak it up?
gam.check(M6)
# Look for: residual patterns over time, k‑index warnings, patterns in 
# residual vs time plots.  If increasing k for s(time) removes 
# autocorrelation → prefer that solution.If autocorrelation remains

# when you have time (> 18 hours) run this (where k is higher)
M6_k <- gam(
  avg_day_Temperatur ~ avg_day_Djup +
    s(time, k = 50) +
    s(doy, bs = "cc", k = 15) +
    s(time, Lokalnamn, bs = "fs", k = 15) +
    s(doy, Lokalnamn, bs = "fs", k = 8),
  data = my_day2,
  method = "REML"
)
summary(M6_k)
plot(M6_k)
# check corr within sites as above
#mgcv‑specific check
summary(M6_k)$s.table
# Red flags: EDF ≈ 0 for many site smooths. Very large SEs. 
# Warnings about rank deficiency

# check for Residual temporal autocorrelation:
# extract normalized (Pearson) residuals
res_k <- resid(M6_k, type = "pearson")
time <- my_day2$time
site <- my_day2$Lokalnamn
acf(res_k, lag.max = 100)
# Spikes outside the confidence bands → autocorrelation. 
# Strong spike at lag 1 → AR(1)-type process
# Slow decay → under-smoothed long-term trend
#️ This mixes all sites — it’s only diagnostic, not definitive.
# Asses autocorrelation within-site 
par(mfrow = c(1, 1))
for (s in unique(site)) {
  idx <- site == s
  if (sum(idx) > 20) {   # avoid tiny series
    acf(res_k[idx], main = paste("ACF:", s))
  }
}
par(mfrow = c(1, 1))
# Many sites show lag‑1 correlation → real temporal structure
# Only a few sites → consider site‑specific solutions
# Formal test: Durbin–Watson–type diagnostic (by site)
my_day2$res_k <- resid(M6_k, type = "pearson")
dw_by_site <- my_day2 %>%
  arrange(Lokalnamn, time) %>%
  group_by(Lokalnamn) %>%
  summarise(
    dw = sum(diff(res_k)^2, na.rm = TRUE) /
      sum(res_k^2, na.rm = TRUE),
    n = n()
  )
dw_by_site # strong autocorrelation
# DW ≈ 2 → no autocorrelation, DW < 1.5 → positive autocorrelation, DW < 1.2 → strong autocorrelation

# Is autocorrelation actually a problem? Did the smooths already soak it up?
gam.check(M6_k)
# Look for: residual patterns over time, k‑index warnings, patterns in 
# residual vs time plots.If autocorrelation remains:

##### include temp autocorrelation: bam ####

# bam model with site as random interecpt
M1_ar1 <- bam(
  avg_day_Temperatur ~ avg_day_Djup +
    s(time, k = 30) +
    s(doy, bs = "cc", k = 10) +
    s(Lokalnamn, bs = "re"),
  data = my_day2,
  method = "REML",
  rho = 0.7,          # guess estimated 
  AR.start = my_day2$start_event
)
summary(M1_ar1)
plot(M1_ar1)
# alternatives, or additions:
# increase flexibility with K in: s(time, Lokalnamn, bs = "fs", k = 15)
# don't use gamm() with many sites (slow, unstable)
# if Only few sites affected, model them seapartely
# overall, If autocorrelation exists: Prefer bam() with AR(1) for 
# short‑term dependence, Prefer richer smooths for long‑term dependence

# include temp autocorrelation: AR(1) model with bam()
# create start_event, which must be TRUE at the start of each site time series
my_day2 <- my_day2 %>%
  arrange(Lokalnamn, time) %>%
  group_by(Lokalnamn) %>%
  mutate(start_event = row_number() == 1) %>%
  ungroup()
# Estimate rho from residuals of the non‑AR model:
rho_est <- acf(res, plot = FALSE)$acf[2]
rho_est

M6_ar1 <- bam(
  avg_day_Temperatur ~ avg_day_Djup +
    s(time, k = 30) +
    s(doy, bs = "cc", k = 10) +
    s(time, Lokalnamn, bs = "fs")+
    s(doy, Lokalnamn, bs = "fs"),
  data = my_day2,
  method = "REML",
  rho = 0.9179998,          # estimated from resid
  AR.start = my_day2$start_event
)
summary(M6_ar1)
plot(M6_ar1)

# Validate again
acf(resid(M6_ar1, type = "pearson"))
# DOES IT DIFFERE FROM PREVIOUS? YES
anova(M6_ar1,M1_ar1,test = "Chisq")
# but since s(time, Lokalnamn) is NOT much supported, I try:

# same model as above with a different rho:
M3_ar1 <- bam(
  avg_day_Temperatur ~ avg_day_Djup +
    s(time, k = 30) +
    s(doy, bs = "cc", k = 10) +
    s(time, Lokalnamn, bs = "fs")+
    s(doy, Lokalnamn, bs = "fs"),
  data = my_day2,
  method = "REML",
  rho = 0.8,          # guess estimated 
  AR.start = my_day2$start_event
)
summary(M3_ar1)
plot(M3_ar1)
anova(M6_ar1,M3_ar1,test = "Chisq")
acf(resid(M3_ar1, type = "pearson"))
summary(M3_ar1)$s.table

# find which sites differ
# visual inspectioon: to fix and re run
draw(M3_ar1)
sapply(M3_ar1$smooth, function(s) s$label) # [1] "s(time)","s(doy)","s(time,Lokalnamn)"
# [4] "s(doy,Lokalnamn)" 

# extract site‑specific smooth estimates for season
library(gratia)
seasonal_sites <- smooth_estimates(
  M3_ar1,
  smooth = "s(doy,Lokalnamn)"
)
# Large values → strong site‑specific seasonality
# Identify sites with non‑zero seasonal effects
seasonal_site_aggregated <- seasonal_sites %>%
  group_by(Lokalnamn) %>%
  summarise(
    max_abs = max(abs(.estimate)),
    mean_abs = mean(abs(.estimate))
  ) %>%
  arrange(desc(mean_abs))
seasonal_site_aggregated
print(seasonal_site_aggregated,n =39)

# do the same for deviations of sites from global trend over time
# extract site‑specific smooth estimates for season
library(gratia)
time_sites <- smooth_estimates(
  M3_ar1,
  smooth = "s(time,Lokalnamn)"
)
# Identify sites with non‑zero seasonal effects
time_site_aggregated <- time_sites %>%
  group_by(Lokalnamn) %>%
  summarise(
    max_abs = max(abs(.estimate)),
    mean_abs = mean(abs(.estimate))
  ) %>%
  arrange(desc(mean_abs))
time_site_aggregated
print(time_site_aggregated,n =39)


##### site as fixed ####
# using different smoothers for different sites (I think site is now fixed, not random):
# run for few sites at a time to shorten computational time, skip seasonal component for now but introduce interaction depth*season:
M4<-gam(avg_day_Temperatur ~ avg_day_Djup*season + 
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "1 km S Aggarps skola")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "300 m ovan väg")),
        data = my_day2, method = "REML")
summary(M4)
plot(M4)
# run for few sites at a time to shorten computational time, use seasonal component as smoother, no interaction depth*season:
M5<-gam(avg_day_Temperatur ~ avg_day_Djup + 
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "1 km S Aggarps skola")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "300 m ovan väg"))+
          s(doy, bs = "fs"),
        data = my_day2, method = "REML")
summary(M5)
plot(M5)

# check autocorrelation


# full
M0<-gam(avg_day_Temperatur ~ avg_day_Djup + 
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "1 km S Aggarps skola")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "300 m ovan väg")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "700 m uppströms Mörtsjön")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Arålund")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "E4:an Nr 2")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Fredriksdal")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "G:a Stenbron")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "G:a järnvägsbron")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Gamla Kvarnen")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Gravbacka")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Huvudfåran")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Hästgången")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Jockara Fäbod")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Kopparhemmet")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Källsjöklack")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Laxbäcken Övre")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Lyckemyran")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Munkhättan")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Mynningsnära")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ned Sågen")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Nedre")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Nedströms Bosgtimedsfallet")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ovan E4:an")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ovan väg")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ryerna")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Sandån Nedre")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Sandån övre")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Storsele")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Stråfulnäset övre")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Sågverket")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Tangådalsstugan Ö ftimean")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Vedema")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Vid Vägen")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Vingäng nedre")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Västra Lövås")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Västra Styberget")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Ängarna")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Åbro")) +
          s(time,fx = FALSE, k = -1, bs = "cr", by = as.numeric(Lokalnamn == "Övre Ejgst"))+
          s(doy, bs = "fs"), 
        method = "REML", data=my_day2)

summary(M0)
anova(M0) # useful when I have a factor, gives a overall F test
#The fx and k means that the amount of smoothing is not fixed to a predeternmined value;hence, cross-validation is used to estimate the optimal amount of smoothing. 
# bs is for cubic regression spline to be used. try either "cr" or "cs".
#if convergence problems: 
# The option "control = lmc," can be used to ensure convergence
# One option is to increase the number of iterations in the routine or reduce the convergence criteria, see the help file of gamm
# Other options are to fix the degrees of freedom (and not use cross-validation) 

# plots
plot(M0)

#####
# data SERS
#####
library(readr)
guess_encoding("SERS_Dalarna.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""
Blekinge<- read.csv2("SERS_Blekinge.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Dalarna<- read.csv2("SERS_Dalarna.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Gotland<- read.csv2("SERS_Gotland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Gävleborg<- read.csv2("SERS_Gävleborg.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Halland<- read.csv2("SERS_Halland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Jämtland<- read.csv2("SERS_Jämtland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Jönköping<- read.csv2("SERS_Jönköping.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Kalmar<- read.csv2("SERS_Kalmar.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Kronoberg<- read.csv2("SERS_Kronoberg.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Norrbotten<- read.csv2("SERS_Norrbotten.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Skåne<- read.csv2("SERS_Skåne.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Stockholm<- read.csv2("SERS_Stockholm.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Södermanland<- read.csv2("SERS_Södermanland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Uppsala<- read.csv2("SERS_Uppsala.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Värmland<- read.csv2("SERS_Värmland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Västerbotten<- read.csv2("SERS_Västerbotten.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Västernorrland<- read.csv2("SERS_Västernorrland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Västmanland<- read.csv2("SERS_Västmanland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Västra_Götaland<- read.csv2("SERS_Västra_Götaland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Örebro<- read.csv2("SERS_Örebro.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
Östergötland<- read.csv2("SERS_Östergötland.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
head(Uppsala)

