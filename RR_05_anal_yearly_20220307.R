################################################################################
# Analiza letnih statistik vremenskih podatkov za Ljubljano, Kredarico
# in Rakican (Mursko Soboto) prebranih iz www.meteo.si in
# shranjenih v lokalni bazi statisticnih podatkov v datoteki
# ./Data/Location_stat.RData in referencni datoteki za obdobja
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
# 
# Program izracuna referecne statistike za obdobje od zacetka zbiranja podatkov
# do konca leta 2021, za obdobje 1981 do 2010 in za 1951 do 1980. Za te izdela
# graficne letne prikaze za temperature, padavine in soncno sevanje.
# Prav tako napravi primerjavo statistik zadnjega leta z referencnimi.
################################################################################
# Analyis of yearly statistical weather data for Ljubljana, Kredarica and
# Rakican (Murska Sobota) previously saved in statistical files
# ./Data/Location_stat.RData and reference files for defined year intervals
# The start date is dependent on location when the first measurement took place.
# For other locations the program must be adapted to.
#
# The program computes reference statistics for all measured data until end of
# the year 2021, for reference years 1981-2010 and for 1951-1980 for temperature,
# precipitation and sunshine.
# It makes comparisons of last year statistics with the reference year
################################################################################
# Input: ./Data/Location_stat.RData : day.term, month.stat, year.stat
#        ./Data/'Location'_ref.RData : ref.days.1951.1980, ref.days.1981.2010,
#                                      ref.months.1951.1980, ref.months.1981.2010,
#                                      ref.years.1951.1980, ref.years.1981.2010
# Output: line graphs and boxplots of yearly weather statistics
#
# Matjaz Jeran
# 07.03.2022

# zrihtati mesecne minimume in maksimume?
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux") setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

# reading reference and current weather data for desired location - select location manually

Location <- "Ljubljana"
# Location <- "Kredarica"
# Location <- "Rakican"


load (file = paste0 ("./Data/", Location, "_ref.RData")) # ref.months.1948.2021, ref.months.1951.1980, ref.months.1981.2010
load (file = paste0 ("./Data/", Location, "_stat.RData"))  # day.term, month.stat, year.stat

# all texts used in the analysis

txt.time.days <- "Čas (dnevi)"
txt.time.months <- "Čas (meseci)"
txt.time.years <- "Čas (leta)"
txt.months.years <- "Meseci v letih"
txt.temp <- "Temperatura (st C)"
txt.min.day.temp <- "mesečni minimumi minimalne dnevne temperature po letih"
txt.min.day.temp.deg <- "Mesečni minimumi minimalne dnevne temperature (st C)"
txt.ave.day.temp <- "mesečna povprečja povprečne dnevne temperature po letih"
txt.ave.day.temp.deg <- "Mesečna povprečja povprečne dnevne temperature (st C)"
txt.max.day.temp <- "mesečni maksimumi maksimalne dnevne temperature po letih"
txt.max.day.temp.deg <- "Mesečni maksimimi maksimalne dnevne temperature (st C)"
txt.min.year.temp <- "povprečne letne minimalne dnevne temperature"
txt.min.year.temp.deg <- "Povprečne letne minimalne temperature (st C)"
txt.ave.year.temp <- "povprečne letne temperature"
txt.ave.year.temp.deg <- "Povprečne letne temperature (st C)"
txt.max.year.temp <- "povprečne letne maksimalne dnevne temperature"
txt.max.year.temp.deg <- "Povprečne letne maksimalne temperature (st C)"
txt.max.year.max.day <- "mesečni maksimumi dnevnih maksimalnih temperatur"
txt.range.day.temp <- "razpon povprečnih dnevnih temperatur po mesecih"
txt.ave.month.ave.day <- "mesečna povprečja dnevnih temperatur"
txt.ave.month.ave.day.deg <- "Mesečna povprečja dnevnih temperatur (st C)"
txt.cmp.year.ave.day <- "primerjava povprečnih mesečnih temperatur z referenčnimi"
txt.ave.year.temp <- "povprečne letne temperature"
txt.ave.minmax.year.temp <- "povprečne in skrajne letne temperature"
txt.year.temp <- "letne temperature"
txt.rain.mm <- "Padavine (mm)"
txt.rain.day.year <- "dnevne padavine po mesecih"
txt.month.rain <- "skupne mesečne padavine po letih"
txt.month.rain.mm <- "Skupne mesečne padavine (mm)"
txt.year.rain <- "skupne letne padavine"
txt.year.rain.mm <- "Skupne letne padavine (mm))"
txt.cmp.year.rain <- "primerjava ref. skupnih mesečnih padavin z zadnjimi"
txt.year.rain <- "letne padavine"
txt.sun.day.year.h <- "Dnevno sončno sevanje po letih (h)"
txt.sum.sun.month <- "skupno mesečno trajanje sončnega obsevanja po letih"
txt.sum.sun.month.h <- "Skupno mesečno trajanje sončnega obsevanja (h)"
txt.year.sun <- "skupno letno trajanje sončnega obsevanja"
txt.year.sun.h <- "Skupno letno trajanje sončnega obsevanja (h)"
txt.cmp.year.sun <- "primerjava ref. skupnega letnega trajanja\nsončnega obsevanja z zadnjim"
txt.ref.year <- "Referenčno leto"
txt.ref.years <- "Referenčno leto za obdobje"
txt.year.temp.deg <- "Letna temperatura (stopinje)"
txt.year.ave <- "letno povprečje"
txt.amount.year <- "letna količina"
txt.long.year <- "letno trajanje"
txt.ref.int <- paste ("referenčno obdobje", "1981:2010")
txt.distr.min.temp.year <- "porazdelitev minimalnih dnevnih temperatur po letih"
txt.distr.ave.temp.year <- "porazdelitev povprečnih dnevnih temperatur po letih"
txt.distr.max.temp.year <- "porazdelitev maksimalnih dnevnih temperatur po letih"
txt.distr.precipit.year <- "porazdelitev dnevnih padavin po letih"
txt.distr.sun.year <- "porazdelitev dnevnega sončnega sevanja po letih"

# select reference years interval and latest year

ref.years <- ref.years.1981.2010
latest.year <- 2021
ref.months <- ref.months.1981.2010
ref.day.term <- subset (day.term, subset = "1980-01-01" < date & date <= "2010-12-31")

# plot yearly data

tmp.year.stat <- subset (year.stat, subset = year <= paste0 (latest.year, "-12-31"))

plot (Ymean.min.temp.C ~ year, data = tmp.year.stat, type = "l", col = "black",
      main = paste (Location, txt.min.year.temp),
      xlab = txt.time.years, ylab = txt.min.year.temp.deg)
 abline (h = ref.years$Ymean.min.temp.C, col = "red")
abline (h = seq (from = -50, to = 50, by = 1), col = "lightgrey")
legend ("topleft", legend = c (txt.year.ave, txt.ref.int), lty = c (1), col = c ("black", "red"))

plot (Ymean.ave.temp.C ~ year, data = tmp.year.stat, type = "l", 
      main = paste (Location, txt.ave.year.temp),
      xlab = txt.time.years, ylab = txt.ave.year.temp.deg)
abline (h = ref.years$Ymean.ave.temp.C, col = "red")
abline (h = seq (from = -50, to = 50, by = 1), col = "lightgrey")
legend ("topleft", legend = c (txt.year.ave, txt.ref.int), lty = c (1), col = c ("black", "red"))

plot (Ymean.max.temp.C ~ year, data = tmp.year.stat, type = "l", 
      main = paste (Location, txt.max.year.temp),
      xlab = txt.time.years, ylab = txt.max.year.temp.deg)
abline (h = ref.years$Ymean.max.temp.C, col = "red")
abline (h = seq (from = -50, to = 50, by = 1), col = "lightgrey")
legend ("topleft", legend = c (txt.year.ave, txt.ref.int), lty = c (1), col = c ("black", "red"))


plot (Ysum.precipit.mm ~ year, data = tmp.year.stat, type = "l",
      main = paste (Location, txt.year.rain),
      xlab = txt.time.years, ylab = txt.year.rain.mm)
abline (h = ref.years$Ysum.precipit.mm, col = "red")
abline (h = seq (from = 0, to = 3000, by = 200), col = "lightgrey")
legend ("topleft", legend = c (txt.amount.year, txt.ref.int), lty = c (1), col = c ("black", "red"))

if (Location == "Ljubljana") { y.loc.start <- 1949
} else if (Location == "Kredarica") { y.loc.start <- 1955
} else if (Location == "Rakican") { y.loc.start <- 1956 }
tmp.year.stat <- subset (year.stat, subset = paste0 (y.loc.start, "-12-31") <= year & year <= paste0 (latest.year, "-12-31"))
plot (Ysum.sun.h ~ year, data = tmp.year.stat, type = "l",
      main = paste (Location, txt.year.sun),
      xlab = txt.time.years, ylab = txt.year.sun.h)
abline (h = ref.years$Ysum.sun.h, col = "red")
abline (h = seq (from = 0, to = 3000, by = 200), col = "lightgrey")
legend ("topleft", legend = c (txt.long.year, txt.ref.int), lty = c (1), col = c ("black", "red"))


# draw monthly boxplots for all years when measurement were made

tmp.month.stat <- subset (month.stat, subset = month <= paste0 (latest.year, "-12-31"))

boxplot (Mmin.min.temp.C ~ format (month, "%Y"), data = tmp.month.stat, varwidth = TRUE,
         main = paste (Location, txt.min.day.temp),
      xlab = txt.time.years, ylab = txt.min.day.temp.deg)
# abline (h = mean (ref.years$Ymean.min.temp.C), col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

boxplot (Mmean.ave.temp.C ~ format (month, "%Y"), data = tmp.month.stat, varwidth = TRUE,
         main = paste (Location, txt.ave.day.temp),
         xlab = txt.time.years, ylab = txt.ave.day.temp.deg)
abline (h = ref.years$Ymean.ave.temp.C, col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

boxplot (Mmax.max.temp.C ~ format (month, "%Y"), data = tmp.month.stat, varwidth = TRUE,
         main = paste (Location, txt.max.day.temp),
         xlab = txt.time.years, ylab = txt.max.day.temp.deg)
# abline (h = max (ref.years$Ymax.max.temp.C), col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

boxplot (Msum.precipit.mm ~ format (month, "%Y"), data = tmp.month.stat, varwidth = TRUE,
         main = paste (Location, txt.month.rain),
         xlab = txt.time.years, ylab = txt.month.rain.mm)
abline (h = mean (ref.months$Msum.precipit.mm), col = "red")
abline (h = seq (from = 0, to = 1000, by = 100), col = "lightgrey")

boxplot (Msum.sun.h ~ format (month, "%Y"), data = tmp.month.stat, varwidth = TRUE,
         main = paste (Location, txt.sum.sun.month),
         ylim = c (0, 400),
         xlab = txt.time.years, ylab = txt.sum.sun.month.h)
abline (h = mean (ref.months$Msum.sun.h), col = "red")
abline (h = seq (from = 0, to = 400, by = 100), col = "lightgrey")


# draw yearly boxplots of daily data

cur.days <- subset (day.term, subset = date < "2022-01-01")

boxplot (min.temp.C ~ format (date, "%Y"), data = cur.days, varwidth = TRUE,
         main = paste (Location, txt.distr.min.temp.year),
         xlab = txt.time.years, ylab = txt.temp)
abline (h = mean (ref.day.term$min.temp.C), col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

boxplot (ave.temp.C ~ format (date, "%Y"), data = cur.days, varwidth = TRUE,
         main = paste (Location, txt.distr.ave.temp.year),
         xlab = txt.time.years, ylab = txt.temp)
abline (h = mean (ref.day.term$ave.temp.C), col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

boxplot (max.temp.C ~ format (date, "%Y"), data = cur.days, varwidth = TRUE,
         main = paste (Location, txt.distr.max.temp.year),
         xlab = txt.time.years, ylab = txt.temp)
abline (h = mean (ref.day.term$max.temp.C), col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

boxplot (precipit.mm ~ format (date, "%Y"), data = cur.days, varwidth = TRUE,
         main = paste (Location, txt.distr.precipit.year),
         xlab = txt.time.years, ylab = txt.rain.mm)
abline (h = mean (ref.day.term$precipit.mm), col = "red")
abline (h = seq (from = 0, to = 150, by = 20), col = "lightgrey")

boxplot (sun.h ~ format (date, "%Y"), data = cur.days, varwidth = TRUE,
         main = paste (Location, txt.distr.sun.year),
         ylim = c (0, 15),
         xlab = txt.time.years, ylab = txt.sun.day.year.h)
abline (h = mean (ref.day.term$sun.h, na.rm = TRUE), col = "red")
abline (h = seq (from = 0, to = 15, by = 2), col = "lightgrey")
