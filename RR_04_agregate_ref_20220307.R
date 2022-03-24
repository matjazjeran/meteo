################################################################################
# Agregiranje referencnih dnevnih, mesecnih in letnih statistik vremenskih
# podatkov za Ljubljano, Kredarico in Rakican (Mursko Soboto) prebranih iz
# www.meteo.si in shranjenih v lokalni bazi statisticnih podatkov v datoteki
# ./Data/Location_stat.RData
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
# Referencne podatke shrani za analizo drugih programov.
################################################################################
# Aggregation of daily, monthly and yearly reference statistical weather data
# for Ljubljana, Kredarica and Rakican (Murska Sobota) previously saved in
# statistical files
# ./Data/Location_stat.RData
# The start date is dependent on location when the first measurement took place.
# For other locations the program must be adapted to.
# The aggregated file is saved for further analysis.
################################################################################
# Input: ./Data/'Location'_stat.RData : day.term, month.stat, year.stat
# Output: /Data/'Location'_ref.RData : ref.days.1951.1980, ref.days.1981.2010,
#                                      ref.months.1951.1980, ref.months.1981.2010,
#                                      ref.years.1951.1980, ref.years.1981.2010
# Matjaz Jeran
# 07.03.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux") setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

library (plyr)

Location <- "Ljubljana"
# Location <- "Kredarica"
# Location <- "Rakican"

# starting years of observations

if (Location == "Ljubljana") { y.loc.start <- 1948
} else if (Location == "Kredarica") { y.loc.start <- 1955
} else if (Location == "Rakican") { y.loc.start <- 1956 }


load (file = paste0 ("./Data/", Location, "_stat.RData"))  # day.term, month.stat, year.stat

# compute daily reference statistics for various year intervals
# year 2000 is a leap year so a ref day for all days for all years
# can be presented as day in year 2000


day.ref.1951.1980 <- subset (day.term, subset = "1951-01-01" <= date & date <= "1980-12-31")
day.ref.1981.2010 <- subset (day.term, subset = "1981-01-01" <= date & date <= "2010-12-31")

ref.days.1951.1980 <- plyr::ddply (.data = day.ref.1951.1980,
                                   .variables = .(day.str = paste0 ("2000-", substr (date.str, start = 6, stop = 10)),
                                                  day = as.Date (paste0 ("2000-", format (date, "%m-%d")))), summarize,
                                   Num.days = length (format (date, "%m-%d")),
                                   Dmin.min.temp.5cm.C = min (min.temp.5cm.C, na.rm = TRUE),
                                   Dmedian.min.temp.5cm.C = median (min.temp.5cm.C, na.rm = TRUE),
                                   Dmean.min.temp.5cm.C = mean (min.temp.5cm.C, na.rm = TRUE),
                                   Dmax.min.temp.5cm.C = max (min.temp.5cm.C, na.rm = TRUE),
                                   Dmin.min.temp.C = min (min.temp.C, na.rm = TRUE),
                                   Dmedian.min.temp.C = median (min.temp.C, na.rm = TRUE),
                                   Dmean.min.temp.C = mean (min.temp.C, na.rm = TRUE),
                                   Dmax.min.temp.C = max (min.temp.C, na.rm = TRUE),
                                   Dmin.ave.temp.C = min (ave.temp.C, na.rm = TRUE),
                                   Dmedian.ave.temp.C = median (ave.temp.C, na.rm = TRUE),
                                   Dmean.ave.temp.C = mean (ave.temp.C, na.rm = TRUE),
                                   Dmax.ave.temp.C = max (ave.temp.C, na.rm = TRUE),
                                   Dmin.max.temp.C = min (max.temp.C, na.rm = TRUE),
                                   Dmedian.max.temp.C = median (max.temp.C, na.rm = TRUE),
                                   Dmean.max.temp.C = mean (max.temp.C, na.rm = TRUE),
                                   Dmax.max.temp.C = max (max.temp.C, na.rm = TRUE),
                                   Dmin.rel.humid = min (rel.humid, na.rm = TRUE),
                                   Dmedian.rel.humid = median (rel.humid, na.rm = TRUE),
                                   Dmax.rel.humid = max (rel.humid, na.rm = TRUE),
                                   Dmin.pressure.hPa = min (pressure.hPa, na.rm = TRUE),
                                   Dmedian.pressure.hPa = median (pressure.hPa, na.rm = TRUE),
                                   Dmean.pressure.hPa = mean (pressure.hPa, na.rm = TRUE),
                                   Dmax.pressure.hPa = max (pressure.hPa, na.rm = TRUE),
                                   Dmin.precipit.mm = min (precipit.mm, na.rm = TRUE),
                                   Dmedian.precipit.mm = median (precipit.mm, na.rm = TRUE),
                                   Dmean.precipit.mm = mean (precipit.mm, na.rm = TRUE),
                                   Dmax.precipit.mm = max (precipit.mm, na.rm = TRUE),
                                   Dmin.snow.level.cm = min (snow.level.cm, na.rm = TRUE),
                                   Dmedian.snow.level.cm = median (snow.level.cm, na.rm = TRUE),
                                   Dmean.snow.level.cm = mean (snow.level.cm, na.rm = TRUE),
                                   Dmax.snow.level.cm = max (snow.level.cm, na.rm = TRUE),
                                   Dmin.new.snow.cm = min (new.snow.cm, na.rm = TRUE),
                                   Dmedian.new.snow.cm = median (new.snow.cm, na.rm = TRUE),
                                   Dmean.new.snow.cm = mean (new.snow.cm, na.rm = TRUE),
                                   Dmax.new.snow.cm = max (new.snow.cm, na.rm = TRUE),
                                   Dmin.sun.h = min (sun.h, na.rm = TRUE),
                                   Dmedian.sun.h = median (sun.h, na.rm = TRUE),
                                   Dmean.sun.h = mean (sun.h, na.rm = TRUE),
                                   Dmax.sun.h = max (sun.h, na.rm = TRUE),
                                   Dmin.temp.C.7 = min (temp.C.7, na.rm = TRUE),
                                   Dmedian.temp.C.7 = median (temp.C.7, na.rm = TRUE),
                                   Dmax.temp.C.7 = max (temp.C.7, na.rm = TRUE),
                                   Dmin.temp.C.14 = min (temp.C.14, na.rm = TRUE),
                                   Dmedian.temp.C.14 = median (temp.C.14, na.rm = TRUE),
                                   Dmax.temp.C.14 = max (temp.C.14, na.rm = TRUE),
                                   Dmin.temp.C.21 = min (temp.C.21, na.rm = TRUE),
                                   Dmedian.temp.C.21 = median (temp.C.21, na.rm = TRUE),
                                   Dmean.temp.C.21 = mean (temp.C.21, na.rm = TRUE),
                                   Dmax.temp.C.21 = max (temp.C.21, na.rm = TRUE),
                                   Prob.strong.wind = sum (strong.wind, na.rm = FALSE),
                                   Prob.stormy.wind = sum (stormy.wind, na.rm = FALSE),
                                   Prob.rain = sum (rain, na.rm = FALSE),
                                   Prob.drizzle = sum (drizzle, na.rm = FALSE),
                                   Prob.rain.shower = sum (rain.shower, na.rm = FALSE),
                                   Prob.storm = sum (storm, na.rm = FALSE),
                                   Prob.thunder = sum (thunder, na.rm = FALSE),
                                   Prob.lightning = sum (lightning, na.rm = FALSE),
                                   Prob.freezing.rain = sum (freezing.rain, na.rm = FALSE),
                                   Prob.freezing.drizzle = sum (freezing.drizzle, na.rm = FALSE),
                                   Prob.needles = sum (needles, na.rm = FALSE),
                                   Prob.snow = sum (snow, na.rm = FALSE),
                                   Prob.snow.pellets = sum (snow.pellets, na.rm = FALSE),
                                   Prob.snow.storm = sum (snow.storm, na.rm = FALSE),
                                   Prob.rain.and.snow = sum (rain.and.snow, na.rm = FALSE),
                                   Prob.sleet = sum (sleet, na.rm = FALSE),
                                   Prob.rain.shower.with.snow = sum (rain.shower.with.snow, na.rm = FALSE),
                                   Prob.hail = sum (hail, na.rm = FALSE),
                                   Prob.wet.snow = sum (wet.snow, na.rm = FALSE),
                                   Prob.fog = sum (fog, na.rm = FALSE),
                                   Prob.fog.open.sky = sum (fog.open.sky, na.rm = FALSE),
                                   Prob.icy.fog = sum (icy.fog, na.rm = FALSE),
                                   Prob.mist = sum (mist, na.rm = FALSE),
                                   Prob.dry.mist = sum (dry.mist, na.rm = FALSE),
                                   Prob.surface.fog = sum (surface.fog, na.rm = FALSE),
                                   Prob.dew = sum (dew, na.rm = FALSE),
                                   Prob.frost = sum (frost, na.rm = FALSE),
                                   Prob.ice = sum (ice, na.rm = FALSE),
                                   Prob.ice.surface = sum (ice.surface, na.rm = FALSE),
                                   Prob.rime.ice = sum (frost, na.rm = FALSE),
                                   Prob.hard.rime = sum (frost, na.rm = FALSE),
                                   Prob.precipitation = sum (precipitation, na.rm = FALSE))

ref.days.1951.1980$Dmin.min.temp.5cm.C [is.infinite(ref.days.1951.1980$Dmin.min.temp.5cm.C)] <- NA
ref.days.1951.1980$Dmean.min.temp.5cm.C [is.nan(ref.days.1951.1980$Dmean.min.temp.5cm.C)] <- NA
ref.days.1951.1980$Dmax.min.temp.5cm.C [is.infinite(ref.days.1951.1980$Dmax.min.temp.5cm.C)] <- NA

ref.days.1951.1980$Prob.strong.wind <- ref.days.1951.1980$Prob.strong.wind / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.stormy.wind <- ref.days.1951.1980$Prob.stormy.wind / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.rain <- ref.days.1951.1980$Prob.rain / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.drizzle <- ref.days.1951.1980$Prob.drizzle / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.rain.shower <- ref.days.1951.1980$Prob.rain.shower / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.storm <- ref.days.1951.1980$Prob.storm / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.thunder <- ref.days.1951.1980$Prob.thunder / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.lightning <- ref.days.1951.1980$Prob.lightning / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.freezing.rain <- ref.days.1951.1980$Prob.freezing.rain / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.freezing.drizzle <- ref.days.1951.1980$Prob.freezing.drizzle / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.needles <- ref.days.1951.1980$Prob.needles / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.snow <- ref.days.1951.1980$Prob.snow / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.snow.pellets <- ref.days.1951.1980$Prob.snow.pellets / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.snow.storm <- ref.days.1951.1980$Prob.snow.storm / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.rain.and.snow <- ref.days.1951.1980$Prob.rain.and.snow / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.sleet <- ref.days.1951.1980$Prob.sleet / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.rain.shower.with.snow <- ref.days.1951.1980$Prob.rain.shower.with.snow / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.hail <- ref.days.1951.1980$Prob.hail / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.wet.snow <- ref.days.1951.1980$Prob.wet.snow / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.fog <- ref.days.1951.1980$Prob.fog / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.fog.open.sky <- ref.days.1951.1980$Prob.fog.open.sky / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.icy.fog <- ref.days.1951.1980$Prob.icy.fog / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.mist <- ref.days.1951.1980$Prob.mist / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.dry.mist <- ref.days.1951.1980$Prob.dry.mist / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.surface.fog <- ref.days.1951.1980$Prob.surface.fog / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.dew <- ref.days.1951.1980$Prob.dew / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.frost <- ref.days.1951.1980$Prob.frost / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.ice <- ref.days.1951.1980$Prob.ice / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.ice.surface <- ref.days.1951.1980$Prob.ice.surface / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.rime.ice <- ref.days.1951.1980$Prob.rime.ice / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.hard.rime <- ref.days.1951.1980$Prob.hard.rime / ref.days.1951.1980$Num.days
ref.days.1951.1980$Prob.precipitation <- ref.days.1951.1980$Prob.precipitation / ref.days.1951.1980$	Num.days


ref.days.1981.2010 <- plyr::ddply (.data = day.ref.1981.2010,
                                   .variables = .(day.str = paste0 ("2000-", substr (date.str, start = 6, stop = 10)),
                                                 day = as.Date (paste0 ("2000-", format (date, "%m-%d")))), summarize,
                                   Num.days = length (format (date, "%m-%d")),
                                   Dmin.min.temp.5cm.C = min (min.temp.5cm.C, na.rm = TRUE),
                                   Dmedian.min.temp.5cm.C = median (min.temp.5cm.C, na.rm = TRUE),
                                   Dmean.min.temp.5cm.C = mean (min.temp.5cm.C, na.rm = TRUE),
                                   Dmax.min.temp.5cm.C = max (min.temp.5cm.C, na.rm = TRUE),
                                   Dmin.min.temp.C = min (min.temp.C, na.rm = TRUE),
                                   Dmedian.min.temp.C = median (min.temp.C, na.rm = TRUE),
                                   Dmean.min.temp.C = mean (min.temp.C, na.rm = TRUE),
                                   Dmax.min.temp.C = max (min.temp.C, na.rm = TRUE),
                                   Dmin.ave.temp.C = min (ave.temp.C, na.rm = TRUE),
                                   Dmedian.ave.temp.C = median (ave.temp.C, na.rm = TRUE),
                                   Dmean.ave.temp.C = mean (ave.temp.C, na.rm = TRUE),
                                   Dmax.ave.temp.C = max (ave.temp.C, na.rm = TRUE),
                                   Dmin.max.temp.C = min (max.temp.C, na.rm = TRUE),
                                   Dmedian.max.temp.C = median (max.temp.C, na.rm = TRUE),
                                   Dmean.max.temp.C = mean (max.temp.C, na.rm = TRUE),
                                   Dmax.max.temp.C = max (max.temp.C, na.rm = TRUE),
                                   Dmin.rel.humid = min (rel.humid, na.rm = TRUE),
                                   Dmedian.rel.humid = median (rel.humid, na.rm = TRUE),
                                   Dmax.rel.humid = max (rel.humid, na.rm = TRUE),
                                   Dmin.pressure.hPa = min (pressure.hPa, na.rm = TRUE),
                                   Dmedian.pressure.hPa = median (pressure.hPa, na.rm = TRUE),
                                   Dmean.pressure.hPa = mean (pressure.hPa, na.rm = TRUE),
                                   Dmax.pressure.hPa = max (pressure.hPa, na.rm = TRUE),
                                   Dmin.precipit.mm = min (precipit.mm, na.rm = TRUE),
                                   Dmedian.precipit.mm = median (precipit.mm, na.rm = TRUE),
                                   Dmean.precipit.mm = mean (precipit.mm, na.rm = TRUE),
                                   Dmax.precipit.mm = max (precipit.mm, na.rm = TRUE),
                                   Dmin.snow.level.cm = min (snow.level.cm, na.rm = TRUE),
                                   Dmedian.snow.level.cm = median (snow.level.cm, na.rm = TRUE),
                                   Dmean.snow.level.cm = mean (snow.level.cm, na.rm = TRUE),
                                   Dmax.snow.level.cm = max (snow.level.cm, na.rm = TRUE),
                                   Dmin.new.snow.cm = min (new.snow.cm, na.rm = TRUE),
                                   Dmedian.new.snow.cm = median (new.snow.cm, na.rm = TRUE),
                                   Dmean.new.snow.cm = mean (new.snow.cm, na.rm = TRUE),
                                   Dmax.new.snow.cm = max (new.snow.cm, na.rm = TRUE),
                                   Dmin.sun.h = min (sun.h, na.rm = TRUE),
                                   Dmedian.sun.h = median (sun.h, na.rm = TRUE),
                                   Dmean.sun.h = mean (sun.h, na.rm = TRUE),
                                   Dmax.sun.h = max (sun.h, na.rm = TRUE),
                                   Dmin.temp.C.7 = min (temp.C.7, na.rm = TRUE),
                                   Dmedian.temp.C.7 = median (temp.C.7, na.rm = TRUE),
                                   Dmax.temp.C.7 = max (temp.C.7, na.rm = TRUE),
                                   Dmin.temp.C.14 = min (temp.C.14, na.rm = TRUE),
                                   Dmedian.temp.C.14 = median (temp.C.14, na.rm = TRUE),
                                   Dmax.temp.C.14 = max (temp.C.14, na.rm = TRUE),
                                   Dmin.temp.C.21 = min (temp.C.21, na.rm = TRUE),
                                   Dmedian.temp.C.21 = median (temp.C.21, na.rm = TRUE),
                                   Dmean.temp.C.21 = mean (temp.C.21, na.rm = TRUE),
                                   Dmax.temp.C.21 = max (temp.C.21, na.rm = TRUE),
                                   Prob.strong.wind = sum (strong.wind, na.rm = FALSE),
                                   Prob.stormy.wind = sum (stormy.wind, na.rm = FALSE),
                                   Prob.rain = sum (rain, na.rm = FALSE),
                                   Prob.drizzle = sum (drizzle, na.rm = FALSE),
                                   Prob.rain.shower = sum (rain.shower, na.rm = FALSE),
                                   Prob.storm = sum (storm, na.rm = FALSE),
                                   Prob.thunder = sum (thunder, na.rm = FALSE),
                                   Prob.lightning = sum (lightning, na.rm = FALSE),
                                   Prob.freezing.rain = sum (freezing.rain, na.rm = FALSE),
                                   Prob.freezing.drizzle = sum (freezing.drizzle, na.rm = FALSE),
                                   Prob.needles = sum (needles, na.rm = FALSE),
                                   Prob.snow = sum (snow, na.rm = FALSE),
                                   Prob.snow.pellets = sum (snow.pellets, na.rm = FALSE),
                                   Prob.snow.storm = sum (snow.storm, na.rm = FALSE),
                                   Prob.rain.and.snow = sum (rain.and.snow, na.rm = FALSE),
                                   Prob.sleet = sum (sleet, na.rm = FALSE),
                                   Prob.rain.shower.with.snow = sum (rain.shower.with.snow, na.rm = FALSE),
                                   Prob.hail = sum (hail, na.rm = FALSE),
                                   Prob.wet.snow = sum (wet.snow, na.rm = FALSE),
                                   Prob.fog = sum (fog, na.rm = FALSE),
                                   Prob.fog.open.sky = sum (fog.open.sky, na.rm = FALSE),
                                   Prob.icy.fog = sum (icy.fog, na.rm = FALSE),
                                   Prob.mist = sum (mist, na.rm = FALSE),
                                   Prob.dry.mist = sum (dry.mist, na.rm = FALSE),
                                   Prob.surface.fog = sum (surface.fog, na.rm = FALSE),
                                   Prob.dew = sum (dew, na.rm = FALSE),
                                   Prob.frost = sum (frost, na.rm = FALSE),
                                   Prob.ice = sum (ice, na.rm = FALSE),
                                   Prob.ice.surface = sum (ice.surface, na.rm = FALSE),
                                   Prob.rime.ice = sum (frost, na.rm = FALSE),
                                   Prob.hard.rime = sum (frost, na.rm = FALSE),
                                   Prob.precipitation = sum (precipitation, na.rm = FALSE))

ref.days.1981.2010$Dmin.min.temp.5cm.C [is.infinite(ref.days.1981.2010$Dmin.min.temp.5cm.C)] <- NA
ref.days.1981.2010$Dmean.min.temp.5cm.C [is.nan(ref.days.1981.2010$Dmean.min.temp.5cm.C)] <- NA
ref.days.1981.2010$Dmax.min.temp.5cm.C [is.infinite(ref.days.1981.2010$Dmax.min.temp.5cm.C)] <- NA

ref.days.1981.2010$Prob.strong.wind <- ref.days.1981.2010$Prob.strong.wind / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.stormy.wind <- ref.days.1981.2010$Prob.stormy.wind / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.rain <- ref.days.1981.2010$Prob.rain / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.drizzle <- ref.days.1981.2010$Prob.drizzle / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.rain.shower <- ref.days.1981.2010$Prob.rain.shower / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.storm <- ref.days.1981.2010$Prob.storm / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.thunder <- ref.days.1981.2010$Prob.thunder / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.lightning <- ref.days.1981.2010$Prob.lightning / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.freezing.rain <- ref.days.1981.2010$Prob.freezing.rain / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.freezing.drizzle <- ref.days.1981.2010$Prob.freezing.drizzle / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.needles <- ref.days.1981.2010$Prob.needles / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.snow <- ref.days.1981.2010$Prob.snow / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.snow.pellets <- ref.days.1981.2010$Prob.snow.pellets / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.snow.storm <- ref.days.1981.2010$Prob.snow.storm / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.rain.and.snow <- ref.days.1981.2010$Prob.rain.and.snow / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.sleet <- ref.days.1981.2010$Prob.sleet / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.rain.shower.with.snow <- ref.days.1981.2010$Prob.rain.shower.with.snow / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.hail <- ref.days.1981.2010$Prob.hail / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.wet.snow <- ref.days.1981.2010$Prob.wet.snow / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.fog <- ref.days.1981.2010$Prob.fog / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.fog.open.sky <- ref.days.1981.2010$Prob.fog.open.sky / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.icy.fog <- ref.days.1981.2010$Prob.icy.fog / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.mist <- ref.days.1981.2010$Prob.mist / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.dry.mist <- ref.days.1981.2010$Prob.dry.mist / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.surface.fog <- ref.days.1981.2010$Prob.surface.fog / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.dew <- ref.days.1981.2010$Prob.dew / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.frost <- ref.days.1981.2010$Prob.frost / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.ice <- ref.days.1981.2010$Prob.ice / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.ice.surface <- ref.days.1981.2010$Prob.ice.surface / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.rime.ice <- ref.days.1981.2010$Prob.rime.ice / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.hard.rime <- ref.days.1981.2010$Prob.hard.rime / ref.days.1981.2010$Num.days
ref.days.1981.2010$Prob.precipitation <- ref.days.1981.2010$Prob.precipitation / ref.days.1981.2010$	Num.days


# compute reference months

ref.months.1951.1980 <- plyr::ddply (.data = day.ref.1951.1980,
                                     .variables = .(month.str = paste0 ("2000-", substr (date.str, start = 6, stop = 7), "-01"),
                                                    month = as.Date (paste0 ("2000-", format (date, "%m"), "-01"))), summarize,
                                     Mmin.min.temp.5cm.C = min (min.temp.5cm.C, na.rm = TRUE),
                                     Mmedian.min.temp.5cm.C = median (min.temp.5cm.C, na.rm = TRUE),
                                     Mmean.min.temp.5cm.C = mean (min.temp.5cm.C, na.rm = TRUE),
                                     Mmax.min.temp.5cm.C = max (min.temp.5cm.C, na.rm = TRUE),
                                     Mmin.min.temp.C = min (min.temp.C, na.rm = TRUE),
                                     Mmedian.min.temp.C = median (min.temp.C, na.rm = TRUE),
                                     Mmean.min.temp.C = mean (min.temp.C, na.rm = TRUE),
                                     Mmax.min.temp.C = max (min.temp.C, na.rm = TRUE),
                                     Mmin.ave.temp.C = min (ave.temp.C, na.rm = TRUE),
                                     Mmedian.ave.temp.C = median (ave.temp.C, na.rm = TRUE),
                                     Mmean.ave.temp.C = mean (ave.temp.C, na.rm = TRUE),
                                     Mmax.ave.temp.C = max (ave.temp.C, na.rm = TRUE),
                                     Mmin.max.temp.C = min (max.temp.C, na.rm = TRUE),
                                     Mmedian.max.temp.C = median (max.temp.C, na.rm = TRUE),
                                     Mmean.max.temp.C = mean (max.temp.C, na.rm = TRUE),
                                     Mmax.max.temp.C = max (max.temp.C, na.rm = TRUE),
                                     Mmin.rel.humid = min (rel.humid, na.rm = TRUE),
                                     Mmedian.rel.humid = median (rel.humid, na.rm = TRUE),
                                     Mmean.rel.humid = mean (rel.humid, na.rm = TRUE),
                                     Mmax.rel.humid = max (rel.humid, na.rm = TRUE),
                                     Mmin.pressure.hPa = min (pressure.hPa, na.rm = TRUE),
                                     Mmedian.pressure.hPa = median (pressure.hPa, na.rm = TRUE),
                                     Mmean.pressure.hPa = mean (pressure.hPa, na.rm = TRUE),
                                     Mmax.pressure.hPa = max (pressure.hPa, na.rm = TRUE),
                                     Mmin.precipit.mm = min (precipit.mm, na.rm = TRUE),
                                     Mmedian.precipit.mm = median (precipit.mm, na.rm = TRUE),
                                     Mmean.precipit.mm = mean (precipit.mm, na.rm = TRUE),
                                     Mmax.precipit.mm = max (precipit.mm, na.rm = TRUE),
                                     Msum.precipit.mm = as.numeric (NA),
                                     Mmin.snow.level.cm = min (snow.level.cm, na.rm = TRUE),
                                     Mmedian.snow.level.cm = median (snow.level.cm, na.rm = TRUE),
                                     Mmean.snow.level.cm = mean (snow.level.cm, na.rm = TRUE),
                                     Mmax.snow.level.cm = max (snow.level.cm, na.rm = TRUE),
                                     Mmin.new.snow.cm = min (new.snow.cm, na.rm = TRUE),
                                     Mmedian.new.snow.cm = median (new.snow.cm, na.rm = TRUE),
                                     Mmean.new.snow.cm = mean (new.snow.cm, na.rm = TRUE),
                                     Mmax.new.snow.cm = max (new.snow.cm, na.rm = TRUE),
                                     Msum.new.snow.cm = as.numeric (NA),
                                     Mmin.sun.h = min (sun.h, na.rm = TRUE),
                                     Mmedian.sun.h = median (sun.h, na.rm = TRUE),
                                     Mmean.sun.h = mean (sun.h, na.rm = TRUE),
                                     Mmax.sun.h = max (sun.h, na.rm = TRUE),
                                     Msum.sun.h = as.numeric (NA),
                                     Mmin.temp.C.7 = min (temp.C.7, na.rm = TRUE),
                                     Mmedian.temp.C.7 = median (temp.C.7, na.rm = TRUE),
                                     Mmean.temp.C.7 = mean (temp.C.7, na.rm = TRUE),
                                     Mmax.temp.C.7 = max (temp.C.7, na.rm = TRUE),
                                     Mmin.temp.C.14 = min (temp.C.14, na.rm = TRUE),
                                     Mmedian.temp.C.14 = median (temp.C.14, na.rm = TRUE),
                                     Mmean.temp.C.14 = mean (temp.C.14, na.rm = TRUE),
                                     Mmax.temp.C.14 = max (temp.C.14, na.rm = TRUE),
                                     Mmin.temp.C.21 = min (temp.C.21, na.rm = TRUE),
                                     Mmedian.temp.C.21 = median (temp.C.21, na.rm = TRUE),
                                     Mmean.temp.C.21 = mean (temp.C.21, na.rm = TRUE),
                                     Mmax.temp.C.21 = max (temp.C.21, na.rm = TRUE))

ref.months.1951.1980$Mmin.min.temp.5cm.C [is.infinite(ref.months.1951.1980$Mmin.min.temp.5cm.C)] <- NA
ref.months.1951.1980$Mmean.min.temp.5cm.C [is.nan(ref.months.1951.1980$Mmean.min.temp.5cm.C)] <- NA
ref.months.1951.1980$Mmax.min.temp.5cm.C [is.infinite(ref.months.1951.1980$Mmax.min.temp.5cm.C)] <- NA

tmp <- subset (month.stat, subset = "1951-01-01" <= month & month <= "1980-12-31")
tmp.agg <- plyr::ddply (.data = tmp,
                        .variables = .(month.str = paste0 ("2000-", substr (month.str, start = 6, stop = 7), "-01"),
                                       month = as.Date (paste0 ("2000-", format (month, "%m"), "-01"))), summarize,
                        Msum.precipit.mm = mean (Msum.precipit.mm, na.rm = TRUE),
                        Msum.new.snow.cm = mean (Msum.new.snow.cm, na.rm = TRUE),
                        Msum.sun.h = mean (Msum.sun.h, na.rm = TRUE))
                        
ref.months.1951.1980$Msum.precipit.mm <- tmp.agg$Msum.precipit.mm
ref.months.1951.1980$Msum.new.snow.cm <- tmp.agg$Msum.new.snow.cm
ref.months.1951.1980$Msum.sun.h <- tmp.agg$Msum.sun.h

ref.months.1981.2010 <- plyr::ddply (.data = day.ref.1981.2010,
                                     .variables = .(month.str = paste0 ("2000-", substr (date.str, start = 6, stop = 7), "-01"),
                                                    month = as.Date (paste0 ("2000-", format (date, "%m"), "-01"))), summarize,
                                     Mmin.min.temp.5cm.C = min (min.temp.5cm.C, na.rm = TRUE),
                                     Mmedian.min.temp.5cm.C = median (min.temp.5cm.C, na.rm = TRUE),
                                     Mmean.min.temp.5cm.C = mean (min.temp.5cm.C, na.rm = TRUE),
                                     Mmax.min.temp.5cm.C = max (min.temp.5cm.C, na.rm = TRUE),
                                     Mmin.min.temp.C = min (min.temp.C, na.rm = TRUE),
                                     Mmedian.min.temp.C = median (min.temp.C, na.rm = TRUE),
                                     Mmean.min.temp.C = mean (min.temp.C, na.rm = TRUE),
                                     Mmax.min.temp.C = max (min.temp.C, na.rm = TRUE),
                                     Mmin.ave.temp.C = min (ave.temp.C, na.rm = TRUE),
                                     Mmedian.ave.temp.C = median (ave.temp.C, na.rm = TRUE),
                                     Mmean.ave.temp.C = mean (ave.temp.C, na.rm = TRUE),
                                     Mmax.ave.temp.C = max (ave.temp.C, na.rm = TRUE),
                                     Mmin.max.temp.C = min (max.temp.C, na.rm = TRUE),
                                     Mmedian.max.temp.C = median (max.temp.C, na.rm = TRUE),
                                     Mmean.max.temp.C = mean (max.temp.C, na.rm = TRUE),
                                     Mmax.max.temp.C = max (max.temp.C, na.rm = TRUE),
                                     Mmin.rel.humid = min (rel.humid, na.rm = TRUE),
                                     Mmedian.rel.humid = median (rel.humid, na.rm = TRUE),
                                     Mmean.rel.humid = mean (rel.humid, na.rm = TRUE),
                                     Mmax.rel.humid = max (rel.humid, na.rm = TRUE),
                                     Mmin.pressure.hPa = min (pressure.hPa, na.rm = TRUE),
                                     Mmedian.pressure.hPa = median (pressure.hPa, na.rm = TRUE),
                                     Mmean.pressure.hPa = mean (pressure.hPa, na.rm = TRUE),
                                     Mmax.pressure.hPa = max (pressure.hPa, na.rm = TRUE),
                                     Mmin.precipit.mm = min (precipit.mm, na.rm = TRUE),
                                     Mmedian.precipit.mm = median (precipit.mm, na.rm = TRUE),
                                     Mmean.precipit.mm = mean (precipit.mm, na.rm = TRUE),
                                     Mmax.precipit.mm = max (precipit.mm, na.rm = TRUE),
                                     Msum.precipit.mm = as.numeric (NA),
                                     Mmin.snow.level.cm = min (snow.level.cm, na.rm = TRUE),
                                     Mmedian.snow.level.cm = median (snow.level.cm, na.rm = TRUE),
                                     Mmean.snow.level.cm = mean (snow.level.cm, na.rm = TRUE),
                                     Mmax.snow.level.cm = max (snow.level.cm, na.rm = TRUE),
                                     Mmin.new.snow.cm = min (new.snow.cm, na.rm = TRUE),
                                     Mmedian.new.snow.cm = median (new.snow.cm, na.rm = TRUE),
                                     Mmean.new.snow.cm = mean (new.snow.cm, na.rm = TRUE),
                                     Mmax.new.snow.cm = max (new.snow.cm, na.rm = TRUE),
                                     Msum.new.snow.cm = as.numeric (NA),
                                     Mmin.sun.h = min (sun.h, na.rm = TRUE),
                                     Mmedian.sun.h = median (sun.h, na.rm = TRUE),
                                     Mmean.sun.h = mean (sun.h, na.rm = TRUE),
                                     Mmax.sun.h = max (sun.h, na.rm = TRUE),
                                     Msum.sun.h = as.numeric (NA),
                                     Mmin.temp.C.7 = min (temp.C.7, na.rm = TRUE),
                                     Mmedian.temp.C.7 = median (temp.C.7, na.rm = TRUE),
                                     Mmean.temp.C.7 = mean (temp.C.7, na.rm = TRUE),
                                     Mmax.temp.C.7 = max (temp.C.7, na.rm = TRUE),
                                     Mmin.temp.C.14 = min (temp.C.14, na.rm = TRUE),
                                     Mmedian.temp.C.14 = median (temp.C.14, na.rm = TRUE),
                                     Mmean.temp.C.14 = mean (temp.C.14, na.rm = TRUE),
                                     Mmax.temp.C.14 = max (temp.C.14, na.rm = TRUE),
                                     Mmin.temp.C.21 = min (temp.C.21, na.rm = TRUE),
                                     Mmedian.temp.C.21 = median (temp.C.21, na.rm = TRUE),
                                     Mmean.temp.C.21 = mean (temp.C.21, na.rm = TRUE),
                                     Mmax.temp.C.21 = max (temp.C.21, na.rm = TRUE))

ref.months.1981.2010$Mmin.min.temp.5cm.C [is.infinite(ref.months.1981.2010$Mmin.min.temp.5cm.C)] <- NA
ref.months.1981.2010$Mmean.min.temp.5cm.C [is.nan(ref.months.1981.2010$Mmean.min.temp.5cm.C)] <- NA
ref.months.1981.2010$Mmax.min.temp.5cm.C [is.infinite(ref.months.1981.2010$Mmax.min.temp.5cm.C)] <- NA


tmp <- subset (month.stat, subset = "1981-01-01" <= month & month <= "2010-12-31")
tmp.agg <- plyr::ddply (.data = tmp,
                        .variables = .(month.str = paste0 ("2000-", substr (month.str, start = 6, stop = 7), "-01"),
                                       month = as.Date (paste0 ("2000-", format (month, "%m"), "-01"))), summarize,
                        Msum.precipit.mm = mean (Msum.precipit.mm, na.rm = TRUE),
                        Msum.new.snow.cm = mean (Msum.new.snow.cm, na.rm = TRUE),
                        Msum.sun.h = mean (Msum.sun.h, na.rm = TRUE))

ref.months.1981.2010$Msum.precipit.mm <- tmp.agg$Msum.precipit.mm
ref.months.1981.2010$Msum.new.snow.cm <- tmp.agg$Msum.new.snow.cm
ref.months.1981.2010$Msum.sun.h <- tmp.agg$Msum.sun.h


# compute yearly references

tmp <- subset (year.stat, subset = "1951-01-01" <= year & year <= "1980-12-31")
ref.years.1951.1980 <- data.frame (
  year = "1951-1980",
  Ymin.min.temp.5cm.C = min (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymedian.min.temp.5cm.C = median (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymean.min.temp.5cm.C = mean (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymax.min.temp.5cm.C = max (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymin.min.temp.C = min (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymedian.min.temp.C = median (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymean.min.temp.C = mean (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymax.min.temp.C = max (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymin.ave.temp.C = min (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymedian.ave.temp.C = median (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymean.ave.temp.C = mean (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymax.ave.temp.C = max (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymin.max.temp.C = min (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymedian.max.temp.C = median (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymean.max.temp.C = mean (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymax.max.temp.C = max (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymin.rel.humid = min (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymedian.rel.humid = median (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymean.rel.humid = mean (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymax.rel.humid = max (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymin.pressure.hPa = min (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymedian.pressure.hPa = median (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymean.pressure.hPa = mean (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymax.pressure.hPa = max (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymin.precipit.mm = min (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ymedian.precipit.mm = median (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ymean.precipit.mm = mean (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ymax.precipit.mm = max (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ysum.precipit.mm = mean (tmp$Ysum.precipit.mm, na.rm = TRUE),
  Ymin.snow.level.cm = min (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymedian.snow.level.cm = median (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymean.snow.level.cm = mean (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymax.snow.level.cm = max (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymin.new.snow.cm = min (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ymedian.new.snow.cm = median (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ymean.new.snow.cm = mean (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ymax.new.snow.cm = max (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ysum.new.snow.cm = mean (tmp$Ysum.new.snow.cm, na.rm = TRUE),
  Ymin.sun.h = min (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ymedian.sun.h = median (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ymean.sun.h = mean (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ymax.sun.h = max (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ysum.sun.h = mean (tmp$Ysum.sun.h, na.rm = TRUE),
  Ymin.temp.C.7 = min (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymedian.temp.C.7 = median (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymean.temp.C.7 = mean (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymax.temp.C.7 = max (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymin.temp.C.14 = min (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymedian.temp.C.14 = median (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymean.temp.C.14 = mean (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymax.temp.C.14 = max (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymin.temp.C.21 = min (day.ref.1981.2010$temp.C.21, na.rm = TRUE),
  Ymedian.temp.C.21 = median (day.ref.1981.2010$temp.C.21, na.rm = TRUE),
  Ymean.temp.C.21 = mean (day.ref.1981.2010$temp.C.21, na.rm = TRUE),
  Ymax.temp.C.21 = max (day.ref.1981.2010$temp.C.21, na.rm = TRUE))

ref.years.1951.1980$Ymin.min.temp.5cm.C [is.infinite(ref.years.1951.1980$Ymin.min.temp.5cm.C)] <- NA
ref.years.1951.1980$Ymean.min.temp.5cm.C [is.nan(ref.years.1951.1980$Ymean.min.temp.5cm.C)] <- NA
ref.years.1951.1980$Ymax.min.temp.5cm.C [is.infinite(ref.years.1951.1980$Ymax.min.temp.5cm.C)] <- NA


tmp <- subset (year.stat, subset = "1981-01-01" <= year & year <= "2010-12-31")
ref.years.1981.2010 <- data.frame (
  year = "1981-2010",
  Ymin.min.temp.5cm.C = min (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymedian.min.temp.5cm.C = median (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymean.min.temp.5cm.C = mean (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymax.min.temp.5cm.C = max (day.ref.1981.2010$min.temp.5cm.C, na.rm = TRUE),
  Ymin.min.temp.C = min (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymedian.min.temp.C = median (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymean.min.temp.C = mean (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymax.min.temp.C = max (day.ref.1981.2010$min.temp.C, na.rm = TRUE),
  Ymin.ave.temp.C = min (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymedian.ave.temp.C = median (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymean.ave.temp.C = mean (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymax.ave.temp.C = max (day.ref.1981.2010$ave.temp.C, na.rm = TRUE),
  Ymin.max.temp.C = min (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymedian.max.temp.C = median (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymean.max.temp.C = mean (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymax.max.temp.C = max (day.ref.1981.2010$max.temp.C, na.rm = TRUE),
  Ymin.rel.humid = min (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymedian.rel.humid = median (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymean.rel.humid = mean (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymax.rel.humid = max (day.ref.1981.2010$rel.humid, na.rm = TRUE),
  Ymin.pressure.hPa = min (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymedian.pressure.hPa = median (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymean.pressure.hPa = mean (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymax.pressure.hPa = max (day.ref.1981.2010$pressure.hPa, na.rm = TRUE),
  Ymin.precipit.mm = min (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ymedian.precipit.mm = median (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ymean.precipit.mm = mean (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ymax.precipit.mm = max (day.ref.1981.2010$precipit.mm, na.rm = TRUE),
  Ysum.precipit.mm = mean (tmp$Ysum.precipit.mm, na.rm = TRUE),
  Ymin.snow.level.cm = min (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymedian.snow.level.cm = median (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymean.snow.level.cm = mean (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymax.snow.level.cm = max (day.ref.1981.2010$snow.level.cm, na.rm = TRUE),
  Ymin.new.snow.cm = min (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ymedian.new.snow.cm = median (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ymean.new.snow.cm = mean (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ymax.new.snow.cm = max (day.ref.1981.2010$new.snow.cm, na.rm = TRUE),
  Ysum.new.snow.cm = mean (tmp$Ysum.new.snow.cm, na.rm = TRUE),
  Ymin.sun.h = min (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ymedian.sun.h = median (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ymean.sun.h = mean (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ymax.sun.h = max (day.ref.1981.2010$sun.h, na.rm = TRUE),
  Ysum.sun.h = mean (tmp$Ysum.sun.h, na.rm = TRUE),
  Ymin.temp.C.7 = min (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymedian.temp.C.7 = median (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymean.temp.C.7 = mean (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymax.temp.C.7 = max (day.ref.1981.2010$temp.C.7, na.rm = TRUE),
  Ymin.temp.C.14 = min (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymedian.temp.C.14 = median (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymean.temp.C.14 = mean (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymax.temp.C.14 = max (day.ref.1981.2010$temp.C.14, na.rm = TRUE),
  Ymin.temp.C.21 = min (day.ref.1981.2010$temp.C.21, na.rm = TRUE),
  Ymedian.temp.C.21 = median (day.ref.1981.2010$temp.C.21, na.rm = TRUE),
  Ymean.temp.C.21 = mean (day.ref.1981.2010$temp.C.21, na.rm = TRUE),
  Ymax.temp.C.21 = max (day.ref.1981.2010$temp.C.21, na.rm = TRUE))

ref.years.1981.2010$Ymin.min.temp.5cm.C [is.infinite(ref.years.1981.2010$Ymin.min.temp.5cm.C)] <- NA
ref.years.1981.2010$Ymean.min.temp.5cm.C [is.nan(ref.years.1981.2010$Ymean.min.temp.5cm.C)] <- NA
ref.years.1981.2010$Ymax.min.temp.5cm.C [is.infinite(ref.years.1981.2010$Ymax.min.temp.5cm.C)] <- NA

# save reference statistics depending on measurement terms

save (ref.days.1951.1980, ref.days.1981.2010, ref.months.1951.1980, ref.months.1981.2010,
      ref.years.1951.1980, ref.years.1981.2010, file = paste0 ("./Data/", Location, "_ref.RData"))
