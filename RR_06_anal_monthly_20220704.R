################################################################################
# Analiza mesecnih statistik vremenskih podatkov za Ljubljano, Kredarico
# in Rakican (Mursko Soboto) prebranih iz www.meteo.si in
# shranjenih v lokalni bazi statisticnih podatkov v datoteki
# ./Data/Location_stat.RData in referencni datoteki za obdobja
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
# 
# Program izracuna referecne statistike za obdobje od zacetka zbiranja podatkov
# do konca leta 2021, za obdobje 1981 do 2010 in za 1951 do 1980. Za te izdela
# graficne prikaze tipicnih mesecev za temperature, padavine in soncno sevanje.
# Prav tako napravi primerjavo statistik zadnjega leta z referencnimi.
################################################################################
# Analyis of monthly statistical weather data for Ljubljana, Kredarica and
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
# Output: line graphs and boxplots of monthly weather statistics
#
# Matjaz Jeran
# 04.07.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux") setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

library (ggplot2)


# reading reference and current weather data for desired location - select location manually

Location <- "Ljubljana"
# Location <- "Kredarica"
# Location <- "Rakican"


load (file = paste0 ("./Data/", Location, "_ref.RData")) # ref.months.1948.2021, ref.months.1951.1980, ref.months.1981.2010
load (file = paste0 ("./Data/", Location, "_stat.RData"))  # day.term, month.stat, year.stat


# select reference years interval and latest year

ref.months <- ref.months.1981.2010
ref.years <- ref.years.1981.2010
latest.year <- 2022


# all texts used in the analysis

txt.time.days <- "Čas (dnevi)"
txt.time.months <- "Čas (meseci)"
txt.time.years <- "Čas (leta)"
txt.months.years <- "Meseci v letih"
txt.temp <- "Temperatura (st C)"
txt.max <- "max"
txt.ave <- "povprečne"
txt.min <- "min"
txt.ave.day.temp <- "povprečne dnevne temperature"
txt.ave.day.temp.deg <- "Povprečne dnevne temperature (st C)"
txt.min.month.temp <- "povprečne mesečne minimalne temperature"
txt.min.month.temp.deg <- "Povprečne mesečne minimalne temperature (st C)"
txt.ave.month.temp <- "povprečne mesečne temperature"
txt.ave.month.temp.deg <- "Povprečne mesečne temperature (st C)"
txt.max.month.temp <- "povprečne mesečne maksimalne temperature"
txt.max.month.temp.deg <- "Povprečne mesečne maksimalne temperature (st C)"
txt.max.month.max.day <- "mesečni maksimumi dnevnih maksimalnih temperatur"
txt.range.day.temp <- "razpon povprečnih dnevnih temperatur po mesecih"
txt.ave.month.ave.day <- "mesečna povprečja dnevnih temperatur"
txt.ave.month.ave.day.deg <- "Mesečna povprečja dnevnih temperatur (st C)"
txt.cmp.month.ave.day <- "primerjava povprečnih mesečnih temperatur z referenčnimi"
txt.ave.year.temp <- "povprečne letne temperature"
txt.ave.minmax.year.temp <- "povprečne in skrajne letne temperature"
txt.rain.mm <- "Padavine (mm)"
txt.rain.day.month <- "dnevne padavine po mesecih"
txt.month.rain <- "skupne mesečne padavine"
txt.month.rain.mm <- "Skupne mesečne padavine (mm))"
txt.cmp.month.rain <- "primerjava ref. skupnih mesečnih padavin z zadnjimi"
txt.year.rain <- "letne padavine"
txt.sun.day.h <- "Dnevno sončno obsevanje (h)"
txt.sun.day.month <- "dnevno sončno obsevanje po mesecih"
txt.sun.day.month.h <- "Dnevno sončno obsevanje po mesecih (h)"
txt.month.sun <- "skupno mesečno trajanje sončnega obsevanja"
txt.month.sun.h <- "Skupno mesečno trajanje sončnega obsevanja (h)"
txt.cmp.month.sun <- "primerjava ref. skupnega mesečnega trajanja\nsončnega obsevanja z zadnjim"
txt.all.data <- "obdobje vseh podatkov"
txt.r <- "ref. obdobje"
txt.ref <- paste ("ref. obdobje", "1981:2010")
txt.last.year <- paste ("zadnje leto", latest.year)


# select typical months

type.month = format (month.stat$month, "%m")
type.month.d = format (day.term$date, "%m")


# research differences among reference intervals
# adaptation needed for locations other than Ljubljana

plot (Mmean.ave.temp.C ~ month, data = ref.months.1951.1980, type = "l", lty = 1, col = "blue",
      ylim = c (min (ref.months$Mmin.ave.temp.C), max (ref.months$Mmax.ave.temp.C)),
      main = paste (Location, txt.ave.month.ave.day),
      xlab = txt.months.years, ylab = txt.ave.month.temp.deg)
lines (Mmean.ave.temp.C ~ month, data = ref.months.1981.2010, lty = 4, col = "red")
legend ("topleft", legend = c (paste (txt.r, "1981:2010"), paste (txt.r, "1951:1980")),
        lty = c (2, 1), col = c ("red", "blue"))


# plot monthly data

plot (Mmean.min.temp.C ~ month, data = month.stat, type = "l", 
      main = paste (Location, txt.min.month.temp),
      xlab = txt.time.months, ylab = txt.ave.month.temp.deg)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

plot (Mmean.ave.temp.C ~ month, data = month.stat, type = "l", 
      main = paste (Location, txt.ave.month.temp),
      xlab = txt.time.months, ylab = txt.ave.month.temp.deg)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

plot (Mmean.max.temp.C ~ month, data = month.stat, type = "l", 
      main = paste (Location, txt.max.month.temp),
      xlab = txt.time.months, ylab = txt.ave.month.temp.deg)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

plot (Msum.precipit.mm ~ month, data = month.stat, type = "l",
      main = paste (Location, txt.month.rain),
      xlab = txt.time.months, ylab = txt.month.rain.mm)
abline (h = seq (from = 0, to = 800, by = 100), col = "lightgrey")

plot (Msum.sun.h ~ month, data = month.stat, type = "l",
      main = paste (Location, txt.month.sun),
      xlab = txt.time.months, ylab = txt.month.sun.h)
abline (h = seq (from = 0, to = 350, by = 50), col = "lightgrey")


# draw boxplots for all months when measurement were made

boxplot (Mmean.ave.temp.C ~ type.month, data = month.stat, varwidth = TRUE,
         main = paste (Location, txt.ave.month.ave.day, txt.all.data),
      xlab = txt.months.years, ylab = txt.ave.month.ave.day.deg)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

boxplot (Msum.precipit.mm ~ type.month, data = month.stat, varwidth = TRUE,
         main = paste (Location, txt.month.rain, txt.all.data),
         xlab = txt.months.years, ylab = txt.month.rain.mm)
abline (h = seq (from = 0, to = 800, by = 100), col = "lightgrey")

boxplot (Mmean.sun.h ~ type.month, data = month.stat, varwidth = TRUE,
         main = paste (Location, txt.sun.day.month, txt.all.data),
         ylim = c (0, 12),
         xlab = txt.months.years, ylab = txt.sun.day.h)
abline (h = seq (from = 0, to = 12, by = 2), col = "lightgrey")


# plot reference monthly temperatures

plot (Mmean.ave.temp.C ~ month, data = ref.months, type = "l", 
      ylim = c (min (ref.months$Mmin.ave.temp.C), max (ref.months$Mmax.ave.temp.C)),
      main = paste (Location, txt.ave.month.ave.day, txt.ref),
      xlab = txt.months.years, ylab = txt.ave.month.temp.deg)
lines (Mmin.ave.temp.C ~ month, data = ref.months, col = "blue")
lines (Mmax.ave.temp.C ~ month, data = ref.months, col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")
legend ("topleft", legend = c (txt.max, txt.ave, txt.min), lty = c (1), col = c ("red", "black", "blue"))


# compare weather characteristics of previous year with the reference data
# compare temperatures first

tmp.ref <- subset (ref.months.1981.2010, select = c (month.str, Mmean.ave.temp.C))
tmp.ref <- cbind (tmp.ref, key = substr (tmp.ref$month.str, start = 6, stop = 10))
tmp.curr <- subset (month.stat, subset = paste0 (latest.year, "-01-01") <= month & month <= paste0 (latest.year, "-12-01"))
tmp.curr <- cbind (tmp.curr, key = substr (tmp.curr$month.str, start = 6, stop = 10))
tmp <- merge (tmp.ref, tmp.curr, by = "key")

plot (Mmean.ave.temp.C.x ~ month, data = tmp, type = "l", col = "red",
      ylim = c (-20, 30),
      main = paste (Location, txt.cmp.month.ave.day),
      xlab = txt.time.months, ylab = txt.ave.day.temp.deg)
lines (Mmean.ave.temp.C.y ~ month, data = tmp)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")
legend ("topleft", legend = c (txt.last.year, txt.ref), lty = 1, col = c ("black", "red"))

boxplot (Mmean.ave.temp.C ~ format (month, paste0 (latest.year, "-%m-%d")), data = month.stat, varwidth = TRUE,
      ylim = c (-20, 30),
      main = paste (Location, txt.cmp.month.ave.day),
      xlab = txt.time.months, ylab = txt.ave.day.temp.deg)
stripchart (Mmean.ave.temp.C.y ~ month, data = tmp, vertical = TRUE,
            add = TRUE, pch = 16, col = "red")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")
legend ("topleft", legend = c (txt.ref, txt.last.year), pch = 15, col = c ("black", "red"))



# compare precipitations

tmp.ref <- subset (ref.months.1981.2010, select = c (month.str, Msum.precipit.mm))
tmp.ref <- cbind (tmp.ref, key = substr (tmp.ref$month.str, start = 6, stop = 10))
tmp.curr <- subset (month.stat, subset = paste0 (latest.year, "-01-01") <= month & month <= paste0 (latest.year, "-12-01"))
tmp.curr <- cbind (tmp.curr, key = substr (tmp.curr$month.str, start = 6, stop = 10))
tmp <- merge (tmp.ref, tmp.curr, by = "key")

plot (Msum.precipit.mm.x ~ month, data = tmp, type = "l", col = "red",
      ylim = c (0, 700),
      main = paste (Location, txt.cmp.month.rain),
      xlab = txt.time.months, ylab = txt.month.rain.mm)
lines (Msum.precipit.mm.y ~ month, data = tmp)
abline (h = seq (from = 0, to = 700, by = 100), col = "lightgrey")
legend ("topleft", legend = c (txt.last.year, txt.ref), lty = 1, col = c ("black", "red"))

boxplot (Msum.precipit.mm ~ format (month, paste0 (latest.year, "-%m-%d")), data = month.stat, varwidth = TRUE,
         ylim = c (0, 700),
         main = paste (Location, txt.cmp.month.rain),
         xlab = txt.time.months, ylab = txt.month.rain.mm)
stripchart (Msum.precipit.mm.y ~ month, data = tmp, vertical = TRUE,
            add = TRUE, pch = 16, col = "red")
abline (h = seq (from = 0, to = 700, by = 100), col = "lightgrey")
legend ("topleft", legend = c (txt.ref, txt.last.year), pch = 15, col = c ("black", "red"))


# compare reference sunshine with the latest year

tmp.ref <- subset (ref.months.1981.2010, select = c (month.str, Msum.sun.h))
tmp.ref <- cbind (tmp.ref, key = substr (tmp.ref$month.str, start = 6, stop = 10))
tmp.curr <- subset (month.stat, subset = paste0 (latest.year, "-01-01") <= month & month <= paste0 (latest.year, "-12-01"))
tmp.curr <- cbind (tmp.curr, key = substr (tmp.curr$month.str, start = 6, stop = 10))
tmp <- merge (tmp.ref, tmp.curr, by = "key")

plot (Msum.sun.h.x ~ month, data = tmp, type = "l", col = "red",
      ylim = c (0, 370),
      main = paste (Location, txt.cmp.month.sun),
      xlab = txt.time.months, ylab = txt.month.sun.h)
lines (Msum.sun.h.y ~ month, data = tmp)
abline (h = seq (from = 0, to = 350, by = 50), col = "lightgrey")
legend ("topleft", legend = c (txt.last.year, txt.ref), lty = 1, col = c ("black", "red"))

boxplot (Msum.sun.h ~ format (month, paste0 (latest.year, "-%m-%d")), data = month.stat, varwidth = TRUE,
         ylim = c (0, 370),
         main = paste (Location, txt.cmp.month.sun),
         xlab = txt.time.months, ylab = txt.month.sun.h)
stripchart (Msum.sun.h.y ~ month, data = tmp, vertical = TRUE,
            add = TRUE, pch = 16, col = "red")
abline (h = seq (from = 0, to = 350, by = 100), col = "lightgrey")
legend ("topleft", legend = c (txt.ref, txt.last.year), pch = 15, col = c ("black", "red"))


## warning:  tmp.start and tmp.end must be in the same year!
tmp.start <- "2022-01-01"
tmp.end <- "2022-06-01"
tmp.ref <- subset (ref.months.1981.2010, select = c (month.str, Msum.sun.h))
tmp.ref <- cbind (tmp.ref, key = substr (tmp.ref$month.str, start = 6, stop = 10))
tmp.curr <- subset (month.stat, subset = paste0 (latest.year, "-01-01") <= month & month <= paste0 (latest.year, "-12-01"))
tmp.curr <- cbind (tmp.curr, key = substr (tmp.curr$month.str, start = 6, stop = 10))
tmp <- merge (tmp.ref, tmp.curr, by = "key")
tmp <- subset (tmp, subset = tmp.start <= paste0 (substr (tmp.start, 1, 4), "-", tmp$key) &
                                          paste0 (substr (tmp.end, 1, 4), "-", tmp$key) <= tmp.end)
tmp.stat <- subset (month.stat, subset = "1981-01-01" <= month & month <= "2010-12-31")
tmp.stat <- subset (tmp.stat, 
                    subset = substr (tmp.start, 6, 10) <= format (tmp.stat$month, "%m-%d") &
                             format (tmp.stat$month, "%m-%d") <= substr (tmp.end, 6, 10))

plot (Msum.sun.h.x ~ month, data = tmp, type = "l", col = "red",
      ylim = c (0, 370),
      main = paste (Location, txt.cmp.month.sun),
      xlab = txt.time.months, ylab = txt.month.sun.h)
lines (Msum.sun.h.y ~ month, data = tmp)
abline (h = seq (from = 0, to = 350, by = 50), col = "lightgrey")
legend ("topleft", legend = c (txt.last.year, txt.ref), lty = 1, col = c ("black", "red"))

boxplot (Msum.sun.h ~ format (month, paste0 (latest.year, "-%m-%d")), data = tmp.stat, varwidth = TRUE,
         ylim = c (0, 370),
         main = paste (Location, txt.cmp.month.sun),
         xlab = txt.time.months, ylab = txt.month.sun.h)
stripchart (Msum.sun.h.y ~ month, data = tmp, vertical = TRUE,
            add = TRUE, pch = 16, col = "red")
abline (h = seq (from = 0, to = 350, by = 100), col = "lightgrey")
legend ("topleft", legend = c (txt.ref, txt.last.year), pch = 15, col = c ("black", "red"))
