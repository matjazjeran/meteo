################################################################################
# Zlivanje dnevnih povprecnih in dnevnih terminskih vremenskih podatkov za
# Ljubljano, Kredarico in Rakican (Mursko Soboto) ze prebranih iz www.meteo.si
# Po zlivanju izracun mesecnih in letnih statistik.
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Podatki za Rakican so iz dveh virov iz portala in po najboljsih moceh
# sestavljeni skupaj.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
################################################################################
# Reading daily average weather and daily term data for Ljubljana, Kredarica
# and Rakican (Murska Sobota).
# After merging computing monthy and yearly statistics.
# The start date is dependent on location when the first measurement took place.
# Rakican data is a composite of two sources from the portal www.meteo.si
# For other locations the program must be adapted to.
################################################################################
# Input:  ./Data/Location_D.RData : daily1, daily2, daily3
#         ./Data/Location_term.RData : term
# Output: ./Data/Location_stat.RData : day.term, month.stat, year.stat
#
# Matjaz Jeran
# 03.03.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux") setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

library (plyr)

# reading daily data for selected location
# comment out the desired location

Location <- "Ljubljana"
# Location <- "Kredarica"
# Location <- "Rakican"

# term data
load (file = paste0 ("./Data/", Location, "_term.RData"))  # term

# daily average data
load (file = paste0 ("./Data/", Location, "_D.RData"))  # daily1, daily2, daily3

daily12 <- merge (daily1, daily2)
daily123 <- merge (daily12, daily3)

day.term <- merge (daily123, term)

rm (daily12, daily123)

# make monthly statistics
# expect warnings for computing with NA

month.stat <- plyr::ddply (.data = day.term,
                           .variables = .(month.str = paste0 (format (date, "%Y-%m"), "-01"),
                                          month = as.Date (paste0 (format (date, "%Y-%m"), "-01"))), summarize,
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
                           Mmax.rel.humid  = max (rel.humid, na.rm = TRUE),
                           Mmin.pressure.hPa = min (pressure.hPa, na.rm = TRUE),
                           Mmedian.pressure.hPa = median (pressure.hPa, na.rm = TRUE),
                           Mmean.pressure.hPa = mean (pressure.hPa, na.rm = TRUE),
                           Mmax.pressure.hPa = max (pressure.hPa, na.rm = TRUE),
                           Mmin.precipit.mm  = min (precipit.mm, na.rm = TRUE),
                           Mmedian.precipit.mm = median (precipit.mm, na.rm = TRUE),
                           Mmean.precipit.mm = mean (precipit.mm, na.rm = TRUE),
                           Mmax.precipit.mm = max (precipit.mm, na.rm = TRUE),
                           Msum.precipit.mm = sum (precipit.mm, na.rm = TRUE),
                           Mmin.snow.level.cm = min (snow.level.cm, na.rm = TRUE),
                           Mmedian.snow.level.cm = median (snow.level.cm, na.rm = TRUE),
                           Mmean.snow.level.cm = mean (snow.level.cm, na.rm = TRUE),
                           Mmax.snow.level.cm = max (snow.level.cm, na.rm = TRUE),
                           Mmin.new.snow.cm = min (new.snow.cm, na.rm = TRUE),
                           Mmedian.new.snow.cm = median (new.snow.cm, na.rm = TRUE),
                           Mmean.new.snow.cm = mean (new.snow.cm, na.rm = TRUE),
                           Mmax.new.snow.cm = max (new.snow.cm, na.rm = TRUE),
                           Msum.new.snow.cm = sum (new.snow.cm, na.rm = TRUE),
                           Mmin.sun.h = min (sun.h, na.rm = TRUE),
                           Mmedian.sun.h = median (sun.h, na.rm = TRUE),
                           Mmean.sun.h = mean (sun.h, na.rm = TRUE),
                           Mmax.sun.h = max (sun.h, na.rm = TRUE),
                           Msum.sun.h = sum (sun.h, na.rm = TRUE),
                           Mmin.clouds = min (clouds, na.rm = TRUE),
                           Mmedian.clouds = median (clouds, na.rm = TRUE),
                           Mmean.clouds = mean (clouds, na.rm = TRUE),
                           Mmax.clouds = max (clouds, na.rm = TRUE),
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

# replace all -Inf +Inf and NaN  with NAs

month.stat$Mmin.min.temp.5cm.C [is.infinite (month.stat$Mmin.min.temp.5cm.C)] <- NA
month.stat$Mmean.min.temp.5cm.C [is.nan (month.stat$Mmean.min.temp.5cm.C)] <- NA
month.stat$Mmax.min.temp.5cm.C [is.infinite (month.stat$Mmax.min.temp.5cm.C)] <- NA

month.stat$Mmin.min.temp.C [is.infinite (month.stat$Mmin.min.temp.C)] <- NA
month.stat$Mmean.min.temp.C [is.nan (month.stat$Mmean.min.temp.C)] <- NA
month.stat$Mmax.min.temp.C [is.infinite (month.stat$Mmax.min.temp.C)] <- NA

month.stat$Mmin.ave.temp.C [is.infinite (month.stat$Mmin.ave.temp.C)] <- NA
month.stat$Mmean.ave.temp.C [is.nan (month.stat$Mmean.ave.temp.C)] <- NA
month.stat$Mmax.ave.temp.C [is.infinite (month.stat$Mmax.ave.temp.C)] <- NA

month.stat$Mmin.max.temp.C [is.infinite (month.stat$Mmin.max.temp.C)] <- NA
month.stat$Mmean.max.temp.C [is.nan (month.stat$Mmean.max.temp.C)] <- NA
month.stat$Mmax.max.temp.C [is.infinite (month.stat$Mmax.max.temp.C)] <- NA

month.stat$Mmin.rel.humid [is.infinite (month.stat$Mmin.rel.humid)] <- NA
month.stat$Mmean.rel.humid [is.nan (month.stat$Mmean.rel.humid)] <- NA
month.stat$Mmax.rel.humid [is.infinite (month.stat$Mmax.rel.humid)] <- NA

month.stat$Mmin.pressure.hPa [is.infinite (month.stat$Mmin.pressure.hPa)] <- NA
month.stat$Mmean.pressure.hPa [is.nan (month.stat$Mmean.pressure.hPa)] <- NA
month.stat$Mmax.pressure.hPa [is.infinite (month.stat$Mmax.pressure.hPa)] <- NA

month.stat$Mmin.precipit.mm [is.infinite (month.stat$Mmin.precipit.mm)] <- NA
month.stat$Mmean.precipit.mm [is.nan (month.stat$Mmean.precipit.mm)] <- NA
month.stat$Mmax.precipit.mm [is.infinite (month.stat$Mmax.precipit.mm)] <- NA

month.stat$Mmin.snow.level.cm [is.infinite (month.stat$Mmin.snow.level.cm)] <- NA
month.stat$Mmean.snow.level.cm [is.nan (month.stat$Mmean.snow.level.cm)] <- NA
month.stat$Mmax.snow.level.cm [is.infinite (month.stat$Mmax.snow.level.cm)] <- NA

month.stat$Mmin.new.snow.cm [is.infinite (month.stat$Mmin.new.snow.cm)] <- NA
month.stat$Mmean.new.snow.cm [is.nan (month.stat$Mmean.new.snow.cm)] <- NA
month.stat$Mmax.new.snow.cm [is.infinite (month.stat$Mmax.new.snow.cm)] <- NA

month.stat$Mmin.sun.h [is.infinite(month.stat$Mmin.sun.h)] <- NA
month.stat$Mmean.sun.h [is.nan(month.stat$Mmean.sun.h)] <- NA
month.stat$Mmax.sun.h [is.infinite(month.stat$Mmax.sun.h)] <- NA


# make yearly statistics
# expect warnings for computing with NA

year.stat <- plyr::ddply (.data = day.term,
                          .variables = .(year.str = substr (date.str, 1, 4), 
                                         year = as.Date (paste0 (substr (date.str, 1, 4), "-01-01"))),
                          summarize,
                          Ymin.min.temp.5cm.C = min (min.temp.5cm.C, na.rm = TRUE),
                          Ymedian.min.temp.5cm.C = median (min.temp.5cm.C, na.rm = TRUE),
                          Ymean.min.temp.5cm.C = mean (min.temp.5cm.C, na.rm = TRUE),
                          Ymax.min.temp.5cm.C = max (min.temp.5cm.C, na.rm = TRUE),
                          Ymin.min.temp.C = min (min.temp.C, na.rm = TRUE),
                          Ymedian.min.temp.C = median (min.temp.C, na.rm = TRUE),
                          Ymean.min.temp.C = mean (min.temp.C, na.rm = TRUE),
                          Ymax.min.temp.C = max (min.temp.C, na.rm = TRUE),
                          Ymin.ave.temp.C = min (ave.temp.C, na.rm = TRUE),
                          Ymedian.ave.temp.C = median (ave.temp.C, na.rm = TRUE),
                          Ymean.ave.temp.C = mean (ave.temp.C, na.rm = TRUE),
                          Ymax.ave.temp.C = max (ave.temp.C, na.rm = TRUE),
                          Ymin.max.temp.C = min (max.temp.C, na.rm = TRUE),
                          Ymedian.max.temp.C = median (max.temp.C, na.rm = TRUE),
                          Ymean.max.temp.C = mean (max.temp.C, na.rm = TRUE),
                          Ymax.max.temp.C = max (max.temp.C, na.rm = TRUE),
                          Ymin.rel.humid = min (rel.humid, na.rm = TRUE),
                          Ymedian.rel.humid = median (rel.humid, na.rm = TRUE),
                          Ymean.rel.humid = mean (rel.humid, na.rm = TRUE),
                          Ymax.rel.humid  = max (rel.humid, na.rm = TRUE),
                          Ymin.pressure.hPa = min (pressure.hPa, na.rm = TRUE),
                          Ymedian.pressure.hPa = median (pressure.hPa, na.rm = TRUE),
                          Ymean.pressure.hPa = mean (pressure.hPa, na.rm = TRUE),
                          Ymax.pressure.hPa = max (pressure.hPa, na.rm = TRUE),
                          Ymin.precipit.mm  = min (precipit.mm, na.rm = TRUE),
                          Ymedian.precipit.mm = median (precipit.mm, na.rm = TRUE),
                          Ymean.precipit.mm = mean (precipit.mm, na.rm = TRUE),
                          Ymax.precipit.mm = max (precipit.mm, na.rm = TRUE),
                          Ysum.precipit.mm = sum (precipit.mm, na.rm = TRUE),
                          Ymin.snow.level.cm = min (snow.level.cm, na.rm = TRUE),
                          Ymedian.snow.level.cm = median (snow.level.cm, na.rm = TRUE),
                          Ymean.snow.level.cm = mean (snow.level.cm, na.rm = TRUE),
                          Ymax.snow.level.cm = max (snow.level.cm, na.rm = TRUE),
                          Ymin.new.snow.cm = min (new.snow.cm, na.rm = TRUE),
                          Ymedian.new.snow.cm = median (new.snow.cm, na.rm = TRUE),
                          Ymean.new.snow.cm = mean (new.snow.cm, na.rm = TRUE),
                          Ymax.new.snow.cm = max (new.snow.cm, na.rm = TRUE),
                          Ysum.new.snow.cm = sum (new.snow.cm, na.rm = TRUE),
                          Ymin.sun.h = min (sun.h, na.rm = TRUE),
                          Ymedian.sun.h = median (sun.h, na.rm = TRUE),
                          Ymean.sun.h = mean (sun.h, na.rm = TRUE),
                          Ymax.sun.h = max (sun.h, na.rm = TRUE),
                          Ysum.sun.h = sum (sun.h, na.rm = TRUE),
                          Ymin.clouds = min (clouds, na.rm = TRUE),
                          Ymedian.clouds = median (clouds, na.rm = TRUE),
                          Ymean.clouds = mean (clouds, na.rm = TRUE),
                          Ymax.clouds = max (clouds, na.rm = TRUE),
                          Ymin.temp.C.7 = min (temp.C.7, na.rm = TRUE),
                          Ymedian.temp.C.7 = median (temp.C.7, na.rm = TRUE),
                          Ymean.temp.C.7 = mean (temp.C.7, na.rm = TRUE),
                          Ymax.temp.C.7 = max (temp.C.7, na.rm = TRUE),
                          Ymin.temp.C.14 = min (temp.C.14, na.rm = TRUE),
                          Ymedian.temp.C.14 = median (temp.C.14, na.rm = TRUE),
                          Ymean.temp.C.14 = mean (temp.C.14, na.rm = TRUE),
                          Ymax.temp.C.14 = max (temp.C.14, na.rm = TRUE),
                          Ymin.temp.C.21 = min (temp.C.21, na.rm = TRUE),
                          Ymedian.temp.C.21 = median (temp.C.21, na.rm = TRUE),
                          Ymean.temp.C.21 = mean (temp.C.21, na.rm = TRUE),
                          Ymax.temp.C.21 = max (temp.C.21, na.rm = TRUE))

year.stat$Ymin.min.temp.5cm.C [is.infinite(year.stat$Ymin.min.temp.5cm.C)] <- NA
year.stat$Ymedian.min.temp.5cm.C [is.infinite(year.stat$Ymedian.min.temp.5cm.C)] <- NA
year.stat$Ymean.min.temp.5cm.C [is.nan(year.stat$Ymean.min.temp.5cm.C)] <- NA
year.stat$Ymax.min.temp.5cm.C [is.infinite(year.stat$Ymax.min.temp.5cm.C)] <- NA

save (day.term, month.stat, year.stat, file = paste0 ("./Data/", Location, "_stat.RData"))
