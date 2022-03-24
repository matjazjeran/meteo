################################################################################
# Analiza dnevnih povprecnih dnevnih vremenskih podatkov za Ljubljano, Kredarico
# in Rakican (Mursko Soboto) prebranih iz www.meteo.si in shranjenih v
# lokalni bazi statisticnih podatkov v datoteki # ./Data/Location_stat.RData in
# bazi referencnih podatkov ./Data/'Location'_ref.RData
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
# Program nariše linijske grafe in okvire z ročaji za dnevne vremenske podatke
################################################################################
# Analyis of daily average weather data for Ljubljana, Kredarica and Rakican
# (Murska Sobota) previously saved in statistical files
# ./Data/Location_stat.RData and reference data in ./Data/'Location'_ref.RData
# The start date is dependent on location when the first measurement took place.
# For other locations the program must be adapted to.
# The program draws line graphs and boxplots of daily weather data
################################################################################
# Input: ./Data/'Location'_stat.RData : day.term, month.stat, year.stat
#        ./Data/'Location'_ref.RData : ref.days.1951.1980, ref.days.1981.2010,
#                                      ref.months.1951.1980, ref.months.1981.2010,
#                                      ref.years.1951.1980, ref.years.1981.2010
# Output: line graphs and boxplots
#
# Matjaz Jeran
# 23.03.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux") setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

library (ggplot2)

Location <- "Ljubljana"
# Location <- "Kredarica"
# Location <- "Rakican"

# starting years of observations

if (Location == "Ljubljana") { y.loc.start <- 1948
} else if (Location == "Kredarica") { y.loc.start <- 1955
} else if (Location == "Rakican") { y.loc.start <- 1956 }


# all texts used in the analysis

txt.time.days <- "Čas (dnevi)"
txt.time.months <- "Čas (meseci)"
txt.time.years <- "Čas (leta)"
txt.temp <- "Temperatura (st C)"
txt.max <- "max"
txt.ave <- "povprečje"
txt.min <- "min"
txt.obd <- "obdobje"
txt.day.temp <- "dnevne temperature"
txt.ave.day.temp <- "povprečne dnevne temperature"
txt.ave.day.temp.deg <- "Povprečne dnevne temperature (st C)"
txt.ave.month.temp <- "povprečne mesečne temperature"
txt.max.month.max.day <- "mesečni maksimumi dnevnih maksimalnih temperatur"
txt.range.day.temp <- "razpon povprečnih dnevnih temperatur po mesecih"
txt.ave.year.temp <- "povprečne letne temperature"
txt.ave.minmax.year.temp <- "povprečne in skrajne letne temperature"
txt.day.rain <- "dnevne padavine"
txt.rain.mm <- "Padavine (mm)"
txt.day.rain.mm <- "Dnevne padavine (mm)"
txt.rain.day.month <- "dnevne padavine po mesecih"
txt.month.rain <- "Skupne mesečne padavine"
txt.month.rain.mm <- "skupne mesečne padavine (mm))"
txt.year.rain <- "letne padavine"
txt.sun.day <- "dnevno sončno obsevanje"
txt.sun.day.h <- "Dnevno sončno obsevanje (h)"
txt.month.sun <- "skupno mesečno trajanje sončnega obsevanja"
txt.month.sun.h <- "skupno mesečno trajanje sončnega obsevanja (h)"
txt.day.sun <- "dnevno sončno obsevanje"
txt.day.sun.h <- "Dnevno sončno obsevanje (h)"
txt.med.ave.temp.ref <- "mediane meritev dnevnih temperatur\npo dnevih v letu za referenčno obdobje"
txt.ave.max.rain.ref <- "povprečja in maksimumi vseh dnevnih padavin po dnevih v letu"
txt.min.temp.day <- "minimalne dnevne temperature po dnevih v letu"
txt.min.temp.day.deg <- "Minimalna dnevna temperatura (st C)"
txt.ave.temp.day <- "povprečne dnevne temperature po dnevih v letu"
txt.ave.temp.day.deg <- "Povprečna dnevna temperatura (st C)"
txt.max.temp.day <- "maksimalne dnevne temperature po dnevih v letu"
txt.max.temp.day.deg <- "Maksimalna dnevna temperatura (st C)"
txt.rain.day <- "dnevne padavine po dnevih v letu"
txt.rain.day.mm <- "Dnevne padavine (mm)"
txt.sun.day <- "dnevno obsevanje sonca po dnevih v letu"
txt.sun.day.h <- "Dnevno obsevanje sonca (h)"
txt.prob.rain.day <- "verjetnost dežja po dnevih v letu"
txt.prob.rain <- "Verjetnost dežja"
txt.mean.precipit <- "povprečne dnevne padavine po dnevih v referenčnem letu"
txt.mean.sun.days <- "povprečno dnevno sončno obsevanje po dnevih v referenčnem letu"
txt.cmp.temp.ref <- "primerjava povprečnih dnevnih temperatur z referenčnimi v letu"
txt.cmp.precipit.ref <- "primerjava povprečnih dnevnih padavin z referenčnimi v letu"
txt.day.rain.mm <- "Dnevne padavine (mm)"


load (file = paste0 ("./Data/", Location, "_stat.RData"))  # day.term, month.stat, year.stat

load (file = paste0 ("./Data/", Location, "_ref.RData"))  # 

# search for characteristic days in a year
# datea are assiciated with leap year 2000 to be able to draw graphs with calendar dates

# reference years:
# years from 1981 to 2010
# years from 1951 to 1980


# current days

cur.days <- subset (day.term, subset = date < "2022-01-01")

Ref.dates <- "1981:2010"
latest.year <- 2022

# cur.day.ref <- day.ref.1981.2010
ref.days  <- ref.days.1981.2010
ref.years <- ref.years.1981.2010

cur.ref.days  <- ref.days.1981.2010
cur.box.days <- subset (day.term, subset = "1981-01-01" <= date & date <= "2010-12-31")

type.month = format (month.stat$month, "%m")
type.month.d = format (day.term$date, "%m")


# average daily temperature, precipitation and sunshine by days for all data

plot (ave.temp.C ~ date, data = day.term, type = "l", col = "darkgrey",
      main = paste (Location, txt.ave.day.temp),
      xlab = txt.time.days, ylab = txt.ave.day.temp.deg,
      ylim = c (min (day.term$min.temp.C, na.rm = TRUE),
                max (day.term$max.temp.C, na.rm = TRUE)))
abline (h = ref.years$Ymean.ave.temp.C, col = "red")
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")

plot (precipit.mm ~ date, data = day.term, type = "l", col = "darkgrey",
      main = paste (Location, txt.day.rain),
      xlab = txt.time.days, ylab = txt.day.rain.mm,
      ylim = c (0, max (day.term$precipit.mm, na.rm = TRUE)))
abline (h = mean (ref.days$Dmean.precipit.mm), col = "red")
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")

plot (sun.h ~ date, data = day.term, type = "l", col = "darkgrey",
      main = paste (Location, txt.day.sun),
      xlab = txt.time.days, ylab = txt.day.sun.h,
      ylim = c (0, max (day.term$sun.h, na.rm = TRUE)))
abline (h = mean (ref.days$Dmean.sun.h), col = "red")
abline (h = seq (from = 0, to = 15, by = 5), col = "lightgrey")


# mean daily average temperature by days in a year

plot (Dmean.ave.temp.C ~ day, data = cur.ref.days, type = "l", col = "darkgrey",
      main = paste (Location, txt.med.ave.temp.ref, Ref.dates),
      xlab = txt.time.days, ylab = txt.temp,
      ylim = c (min (cur.ref.days$Dmean.min.temp.C, na.rm = TRUE),
                max (cur.ref.days$Dmean.max.temp.C, na.rm = TRUE)))
lines (Dmean.max.temp.C ~ day, data = cur.ref.days, col = "red")
lines (Dmean.min.temp.C ~ day, data = cur.ref.days, col = "blue")
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")
legend ("topleft", legend = c (txt.max, txt.ave, txt.min),
        lty = c (1), col = c ("red", "darkgrey", "blue"))


tmp.days <- subset (day.term, subset = "2021-01-01" <= date & date <= "2021-12-31")
plot (ave.temp.C ~ date, data = tmp.days, type = "l", col = "darkgrey",
      main = paste (Location, txt.ave.day.temp, latest.year),
      xlab = txt.time.days, ylab = txt.temp,
      ylim = c (min (tmp.days$min.temp.C, na.rm = TRUE),
                max (tmp.days$max.temp.C, na.rm = TRUE)))
lines (max.temp.C ~ date, data = tmp.days, type = "l", col = "red")
lines (min.temp.C ~ date, data = tmp.days, type = "l", col = "blue")
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")
legend ("topleft", legend = c (txt.max, txt.ave, txt.min),
        lty = c (1), col = c ("red", "darkgrey", "blue"))


tmp.days <- subset (day.term, subset = "2021-01-01" <= date & date <= "2021-01-31")
plot (ave.temp.C ~ date, data = tmp.days, type = "l", col = "darkgrey",
      main = paste (Location, txt.ave.day.temp, latest.year),
      xlab = txt.time.days, ylab = txt.temp,
      ylim = c (min (tmp.days$min.temp.C, na.rm = TRUE),
                max (tmp.days$max.temp.C, na.rm = TRUE)))
lines (max.temp.C ~ date, data = tmp.days, type = "l", col = "red")
lines (min.temp.C ~ date, data = tmp.days, type = "l", col = "blue")
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")
legend ("topleft", legend = c (txt.max, txt.ave, txt.min),
        lty = c (1), col = c ("red", "darkgrey", "blue"))



# means of precipitation by days in a year

plot (Dmean.precipit.mm ~ day, data = cur.ref.days, type = "l", col = "black",
      main = paste (Location, txt.ave.max.rain.ref),
      xlab = txt.time.days, ylab = txt.day.rain.mm,
      ylim = c (0, max (cur.ref.days$Dmax.precipit.mm, na.rm = TRUE)))
lines (Dmax.precipit.mm ~ day, data = cur.ref.days, col = "red")
lines (Dmin.precipit.mm ~ day, data = cur.ref.days, col = "blue")
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")
legend ("topleft", legend = c (txt.max, txt.ave, txt.min),
        lty = c (1), col = c ("red", "darkgrey", "blue"))


tmp.days <- subset (day.term, subset = "2021-01-01" <= date & date <= "2021-12-31")
plot (precipit.mm ~ date, data = tmp.days, type = "l",
      main = paste (Location, txt.day.rain, latest.year),
      xlab = txt.time.days, ylab = txt.day.rain.mm,
      ylim = c (0, max (day.term$precipit.mm, na.rm = TRUE)))
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")


# daily sunshine

plot (Dmean.sun.h ~ day, data = cur.ref.days, type = "l",
      main = paste (Location, txt.sun.day),
      xlab = txt.time.days, ylab = txt.sun.day.h,
      ylim = c (0, 15))
lines (Dmax.sun.h ~ day, data = cur.ref.days, col = "red")
lines (Dmin.sun.h ~ day, data = cur.ref.days, col = "blue")
abline (h = seq (from = 0, to = 15, by = 5), col = "lightgrey")

tmp.days <- subset (day.term, subset = "2021-01-01" <= date & date <= "2021-12-31")
plot (sun.h ~ date, data = tmp.days, type = "l",
      main = paste (Location, txt.sun.day, latest.year),
      xlab = txt.time.days, ylab = txt.sun.day.h,
      ylim = c (0, 15))
abline (h = seq (from = 0, to = 15, by = 5), col = "lightgrey")


# boxplots of temperature, precipitation and sunshine by days in a year

boxplot (min.temp.C ~ format (date, "%m-%d"), data = cur.days,
         main = paste (Location, txt.min.temp.day),
         xlab = txt.time.days, ylab = txt.min.temp.day.deg)
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")

boxplot (ave.temp.C ~ format (date, "%m-%d"), data = cur.days,
         main = paste (Location, txt.ave.temp.day),
         xlab = txt.time.days, ylab = txt.ave.temp.day.deg)
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")

boxplot (max.temp.C ~ format (date, "%m-%d"), data = cur.days,
         main = paste (Location, txt.max.temp.day),
         xlab = txt.time.days, ylab = txt.max.temp.day.deg)
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")

boxplot (precipit.mm ~ format (date, "%m-%d"), data = cur.days,
         main = paste (Location, txt.rain.day),
         xlab = txt.time.days, ylab = txt.rain.day.mm)
abline (h = seq (from = 0, to = 150, by = 10), col = "lightgrey")

boxplot (sun.h ~ format (date, "%m-%d"), data = cur.days,
         main = paste (Location, txt.sun.day),
         xlab = txt.time.days, ylab = txt.sun.day.h)
abline (h = seq (from = 0, to = 15, by = 5), col = "lightgrey")


# probability of rain by days in a year

plot (Prob.rain ~ day, data = cur.ref.days, type = "l",
      main = paste (Location, txt.prob.rain.day),
      xlab = txt.time.days, ylab = txt.prob.rain,
      ylim = c (0, 1))
abline (h = seq (from = 0, to = 1, by = 0.2), col = "lightgrey")


# medians of average daily temperature by days in a year

tmp.ref.days <- subset (cur.ref.days, subset = format (cur.ref.days$day, "%m") %in% c ("01", "02"))
plot (Dmedian.ave.temp.C ~ day, data = tmp.ref.days, type = "l", col = "darkgrey",
      main = paste (Location, txt.med.ave.temp.ref),
      xlab = txt.time.days, ylab = txt.temp,
      ylim = c (min (tmp.ref.days$Dmedian.min.temp.C, na.rm = TRUE),
                max (tmp.ref.days$Dmedian.max.temp.C, na.rm = TRUE)))
lines (Dmedian.max.temp.C ~ day, data = tmp.ref.days, col = "red")
lines (Dmedian.min.temp.C ~ day, data = tmp.ref.days, col = "blue")
legend ("topleft", 
        legend = c (txt.max, txt.ave, txt.min),
        lty = c (1), col = c ("red", "darkgrey", "blue"))
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")

# daily precipitations

tmp.ref.days <- subset (cur.ref.days, subset = format (cur.ref.days$day, "%m") %in% c ("01", "02"))
plot (Dmean.precipit.mm ~ day, data = tmp.ref.days, type = "l", col = "darkgrey",
      main = paste (Location, txt.mean.precipit),
      xlab = txt.time.days, ylab = txt.day.rain.mm,
      ylim = c (0, max (tmp.ref.days$Dmax.precipit.mm, na.rm = TRUE)))
lines (Dmax.precipit.mm ~ day, data = tmp.ref.days, col = "red")
lines (Dmin.precipit.mm ~ day, data = tmp.ref.days, col = "blue")
abline (h = seq (from = 0, to = 150, by = 10), col = "lightgrey")

# daily sunshine

tmp.ref.days <- subset (cur.ref.days, subset = format (cur.ref.days$day, "%m") %in% c ("01", "02"))
plot (Dmean.sun.h ~ day, data = tmp.ref.days, type = "l", col = "darkgrey",
      main = paste (Location, txt.mean.sun.days),
      xlab = txt.time.days, ylab = txt.day.sun.h,
      ylim = c (0, max (tmp.ref.days$Dmax.sun.h, na.rm = TRUE)))
lines (Dmax.sun.h ~ day, data = tmp.ref.days, col = "red")
lines (Dmin.sun.h ~ day, data = tmp.ref.days, col = "blue")
abline (h = seq (from = 0, to = 15, by = 2), col = "lightgrey")


# comparison of current year daily data with the reference year daily data

latest.year <- 2021
tmp1 <- subset (day.term, subset = "2021-01-01" <= date & date <= "2021-12-31", select = c (date, ave.temp.C))
tmp1 <- cbind (d = format (tmp1$date, "%m-%d"), tmp1)
tmp2 <- subset (cur.ref.days,
                subset = "2021-01-01" <= paste0 (latest.year, format (day, "-%m-%d")) &
                                         paste0 (latest.year, format (day, "-%m-%d")) <= "2021-12-31")
tmp2 <- cbind (d = format (tmp2$day, "%m-%d"), tmp2)
tmp <- merge (tmp1, tmp2, by = "d")

plot (Dmean.ave.temp.C ~ date, data = tmp, type = "l", col = "darkgrey",
      main = paste (Location, txt.cmp.temp.ref),
      xlab = txt.time.days, ylab = txt.ave.day.temp.deg,
      ylim = c (min (tmp$Dmin.min.temp.C, na.rm = TRUE),
                max (tmp$Dmax.max.temp.C, na.rm = TRUE)))
lines (Dmean.max.temp.C ~ date, data = tmp, col = "red")
lines (Dmean.min.temp.C ~ date, data = tmp, col = "blue")
lines (ave.temp.C ~ date, data = tmp, col = "black", lwd = 2)
legend ("topleft", 
        legend = c (txt.max, txt.ave, txt.min, "2021"),
        lty = c (1), col = c ("red", "darkgrey", "blue", "black"))
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")


ggplot2::ggplot (data = tmp, aes (x = date)) + geom_line (aes (y = Dmean.max.temp.C), colour = "red") +
geom_line (aes (x = date, y = Dmean.ave.temp.C), colour = "grey") +
geom_line (aes (x = date, y = Dmean.min.temp.C), colour = "blue") +
geom_line (aes (x = date, y = ave.temp.C), colour = "black", size = 1) +
ggtitle (paste (Location, txt.cmp.temp.ref)) +
xlab (txt.time.days) + ylab (txt.ave.day.temp.deg) + theme_light()


# plot of selected dates only

## warning:  tmp.start and tmp.end must be in the same year!
tmp.start <- "2021-01-01"
tmp.end <- "2021-12-31"
tmp1 <- subset (day.term, subset = tmp.start <= date & date <= tmp.end, select = c (date, ave.temp.C))
tmp1 <- cbind (d = format (tmp1$date, "%m-%d"), tmp1)
tmp2 <- cur.ref.days
tmp2 <- cbind (d = format (tmp2$day, "%m-%d"), tmp2)
tmp <- merge (tmp1, tmp2, by = "d")
tmp <- subset (tmp, subset = tmp.start <= date & date <= tmp.end)
ref.box <- subset (cur.box.days, subset = substr (tmp.start, 6, 10) <= substr (date.str, 6, 10) &
                     substr (date.str, 6, 10) <= substr (tmp.end, 6, 10))


plot (Dmean.ave.temp.C ~ date, data = tmp, type = "l", col = "darkgrey",
      main = paste (Location, txt.cmp.temp.ref), xlab = txt.time.days, ylab = txt.ave.day.temp.deg,
      ylim = c (min (tmp$Dmin.min.temp.C, na.rm = TRUE),
                max (tmp$Dmax.max.temp.C, na.rm = TRUE)))
lines (Dmean.max.temp.C ~ date, data = tmp, col = "red")
lines (Dmean.min.temp.C ~ date, data = tmp, col = "blue")
lines (ave.temp.C ~ date, data = tmp, col = "black", lwd = 2)
legend ("topleft", 
        legend = c (txt.max, txt.ave, txt.min, "2022"),
        lty = c (1), col = c ("red", "darkgrey", "blue", "black"))
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")


boxplot (ave.temp.C ~ format (date, paste0 (latest.year, "-%m-%d")), data = ref.box, col = "lightgrey",
         main = paste (Location, txt.cmp.temp.ref), xlab = txt.time.days, ylab = txt.ave.day.temp.deg)
stripchart (ave.temp.C ~ date, data = tmp, vertical = TRUE, add = T, pch = 16, col = "red")
legend ("topleft", 
        legend = c (txt.obd, "2021"),
        lty = c (1), col = c ("grey", "red"))
abline (h = seq (from = -50, to = 50, by = 10), col = "lightgrey")


# compare precipitations

## warning:  tmp.start and tmp.end must be in the same year!
tmp.start <- "2022-01-01"
tmp.end <- "2022-03-21"

tmp1 <- subset (day.term, subset = tmp.start <= date & date <= tmp.end, select = c (date, precipit.mm))
tmp1 <- cbind (d = format (tmp1$date, "%m-%d"), tmp1)
tmp2 <- cur.ref.days
tmp2 <- cbind (d = format (tmp2$day, "%m-%d"), tmp2)
tmp <- merge (tmp1, tmp2, by = "d")
tmp <- subset (tmp, subset = tmp.start <= date & date <= tmp.end)
ref.box <- subset (cur.box.days, subset = substr (tmp.start, 6, 10) <= substr (date.str, 6, 10) &
                     substr (date.str, 6, 10) <= substr (tmp.end, 6, 10))

plot (Dmean.precipit.mm ~ date, data = tmp, type = "l", col = "darkgrey",
      main = paste (Location, txt.cmp.precipit.ref),
      xlab = txt.time.days, ylab = txt.day.rain.mm,
      ylim = c (min (tmp$Dmin.precipit.mm, na.rm = TRUE),
                max (tmp$Dmax.precipit.mm, na.rm = TRUE)))
lines (Dmin.precipit.mm ~ date, data = tmp, col = "blue", lwd = 2)
lines (Dmax.precipit.mm ~ date, data = tmp, col = "red", lwd = 2)
lines (precipit.mm ~ date, data = tmp, col = "black", lwd = 2)

legend ("topleft", 
        legend = c (txt.max, txt.ave, txt.min, "2022"),
        lty = c (1), col = c ("red", "darkgrey", "blue", "black"))
abline (h = seq (from = 0, to = 150, by = 20), col = "lightgrey")

boxplot (precipit.mm ~ format (date, paste0 (latest.year, "-%m-%d")), data = ref.box, col = "lightgrey",
         main = paste (Location, txt.cmp.precipit.ref), xlab = txt.time.days, ylab = txt.day.rain.mm)
stripchart (precipit.mm ~ date, data = tmp, vertical = TRUE, add = T, pch = 16, col = "red")
legend ("topleft", 
        legend = c (txt.obd, "2022"), lty = c (1), col = c ("grey", "red"))
abline (h = seq (from = 0, to = 150, by = 20), col = "lightgrey")


# compare sunshine

## warning:  tmp.start and tmp.end must be in the same year!
tmp.start <- "2021-01-01"
tmp.end <- "2021-12-31"

tmp1 <- subset (day.term, subset = tmp.start <= date & date <= tmp.end, select = c (date, sun.h))
tmp1 <- cbind (d = format (tmp1$date, "%m-%d"), tmp1)
tmp2 <- cur.ref.days
tmp2 <- cbind (d = format (tmp2$day, "%m-%d"), tmp2)
tmp <- merge (tmp1, tmp2, by = "d")
tmp <- subset (tmp, subset = tmp.start <= date & date <= tmp.end)
ref.box <- subset (cur.box.days, subset = substr (tmp.start, 6, 10) <= substr (date.str, 6, 10) &
                                          substr (date.str, 6, 10) <= substr (tmp.end, 6, 10))

plot (Dmean.sun.h ~ date, data = tmp, type = "l", col = "red",
      main = paste (Location, txt.mean.sun.days), xlab = txt.time.days, ylab = txt.sun.day.h,
      ylim = c (min (tmp$Dmin.sun.h, na.rm = TRUE), max (tmp$Dmax.sun.h, na.rm = TRUE)))
lines (Dmin.sun.h ~ date, data = tmp, col = "blue", lwd = 2)
lines (Dmax.sun.h ~ date, data = tmp, col = "red", lwd = 2)
lines (sun.h ~ date, data = tmp, col = "black", lwd = 2)
legend ("topleft", 
        legend = c (txt.ave, "2021"), lty = c (1), col = c ("red", "black"))
abline (h = seq (from = 0, to = 16, by = 2), col = "lightgrey")

boxplot (sun.h ~ format (date, paste0 (latest.year, "-%m-%d")), data = ref.box, col = "lightgrey",
         main = paste (Location, txt.mean.sun.days), xlab = txt.time.days, ylab = txt.sun.day.h)
stripchart (sun.h ~ date, data = tmp, vertical = TRUE, add = T, pch = 16, col = "red")
legend ("topleft", 
        legend = c (txt.obd, "2021"), lty = c (1), col = c ("grey", "red"))
abline (h = seq (from = -50, to = 50, by = 2), col = "lightgrey")
