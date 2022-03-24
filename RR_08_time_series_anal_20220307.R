################################################################################
# Graficna analiza dnevnih povprecnih dnevnih vremenskih podatkov za
# Ljubljano, Kredarico in Rakican (Mursko Soboto) prebranih iz www.meteo.si in
# shranjenih v lokalni bazi statisticnih podatkov v datoteki
# ./Data/Location_stat.RData
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
# Program narise boxplote in crtne grafe za temperature, padavine in 
# soncno obsevanje 
# Na koncu naredi se casovno analizo temperatur z metodami analize casovnih vrst
################################################################################
# Graphical analyis of daily average weather data for Ljubljana, Kredarica
# and Rakican (Murska Sobota) previously saved in statistical files
# ./Data/Location_stat.RData
# After merging computing monthy and yearly statistics.
# The start date is dependent on location when the first measurement took place.
# For other locations the program must be adapted to.
# The program draws boxplots and line graphs of daily average data on
# temperature, precipitation and sunshine hours.
# Finaly it maxes time analysis of temperature as time series
################################################################################
# Input: ./Data/Location_stat.RData : day.term, month.stat, year.stat
# Output: line graphs and boxplots and time decomposition graphs
#
# Matjaz Jeran
# 07.03.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

library (ggplot2)
library (forecast)


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
txt.ave.day.temp <- "povprečne dnevne temperature"
txt.ave.month.temp <- "povprečne mesečne temperature po letih"
txt.max.month.max.day <- "mesečni maksimumi dnevnih maksimalnih temperatur"
txt.range.day.temp <- "razpon povprečnih dnevnih temperatur po mesecih"
txt.ave.year.temp <- "povprečne letne temperature"
txt.ave.minmax.month.temp <- "povprečne in skrajne mesečne temperature"
txt.ave.minmax.year.temp <- "povprečne in skrajne letne temperature"
txt.rain.mm <- "Padavine (mm)"
txt.rain.day.month <- "dnevne padavine po mesecih"
txt.month.rain <- "skupne mesečne padavine po letih"
txt.month.rain.mm <- "Skupne mesečne padavine (mm))"
txt.year.rain <- "letne padavine"
txt.sun.day.month <- "dnevno sončno sevanje po mesecih"
txt.sun.day.month.h <- "dnevno sončno sevanje po mesecih (h)"
txt.month.sun <- "skupno mesečno trajanje sončnega obsevanja v letih"
txt.month.sun.h <- "skupno mesečno trajanje sončnega obsevanja (h)"


load (file = paste0 ("./Data/", Location, "_stat.RData"))  # day.term, month.stat, year.stat


# start with yearly analysis
# temperatures

data <- subset (year.stat, subset = year.str < "2022",
                select = c (year, Ymean.min.temp.C, Ymean.ave.temp.C, Ymean.max.temp.C))

plot (Ymean.ave.temp.C ~ year, data = data,
      ylim = c (min (Ymean.min.temp.C, na.rm = FALSE), max (Ymean.max.temp.C, na.rm = FALSE)), type = "l",
      main = paste (Location, txt.ave.minmax.year.temp), xlab = txt.time.years, ylab = txt.temp)
lines (Ymean.min.temp.C ~ year, data = data, col = "blue")
lines (Ymean.max.temp.C ~ year, data = data, col = "red")

ggplot2::ggplot (data = data, aes (x = year, y = Ymean.ave.temp.C)) + geom_line() +
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.ave.year.temp)) +
  xlab (txt.time.years) + ylab (txt.temp) + theme_light()

# boxplots

data <- subset (month.stat, subset = month < "2022-01-01",
                select = c (month, Mmean.min.temp.C, Mmean.ave.temp.C, Mmean.max.temp.C))
boxplot (Mmean.ave.temp.C ~ format (month, "%Y-01-01"), data = data,
# ylim = c (min (Mmean.min.temp.C, na.rm = FALSE), max (Mmean.max.temp.C, na.rm = FALSE)), type = "l",
      main = paste (Location, txt.ave.month.temp),
      xlab = txt.time.months, ylab = txt.temp)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgrey")

# precipitation

data <- subset (year.stat, subset = year.str < "2022",
                select = c (year, Ysum.precipit.mm))

plot (Ysum.precipit.mm ~ year, data = data, type = "l",
      ylim = c (0, 3000), 
      main = paste (Location, txt.year.rain), xlab = txt.time.years, ylab = txt.rain.mm)
abline (h = seq (from = 0, to = 3000, by = 500), col = "lightgrey")

ggplot2::ggplot (data = data, aes (x = year, y = Ysum.precipit.mm)) + geom_line() +
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.year.rain)) +
  xlab (txt.time.years) + ylab (txt.rain.mm) + theme_light()

# boxplots

data <- subset (month.stat, subset = month < "2022-01-01",
                select = c (month, Msum.precipit.mm))
boxplot (Msum.precipit.mm ~ format (month, "%Y-01-01"), data = data,
#      ylim = c (min (Mmean.min.temp.C, na.rm = FALSE), max (Mmean.max.temp.C, na.rm = FALSE)), type = "l",
         main = paste (Location, txt.month.rain),
         xlab = txt.time.months, ylab = txt.month.rain.mm)
abline (h = seq (from = 0, to = 700, by = 100), col = "lightgrey")

# sunshine

data <- subset (year.stat, subset = year.str < "2022",
                select = c (year, Ysum.sun.h))

plot (Ysum.sun.h ~ year, data = data, type = "l",
      main = paste (Location, txt.month.sun),
      ylim = c (0, 3000), 
      xlab = txt.time.years, ylab = txt.month.sun.h)
abline (h = seq (from = 0, to = 3000, by = 500), col = "lightgrey")

ggplot2::ggplot (data = data, aes (x = year, y = Ysum.sun.h)) + geom_line() +
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.month.sun)) +
  xlab (txt.time.years) + ylab (txt.month.sun.h) + theme_light()


# boxplots

data <- subset (month.stat, subset = month < "2022-01-01",
                select = c (month, Msum.sun.h))
boxplot (Msum.sun.h ~ format (month, "%Y-01-01"), data = data,
# ylim = c (min (Mmean.min.temp.C, na.rm = FALSE), max (Mmean.max.temp.C, na.rm = FALSE)), type = "l",
         main = paste (Location, txt.month.sun),
         xlab = txt.time.months, ylab = txt.month.sun.h)
abline (h = seq (from = 0, to = 400, by = 50), col = "lightgrey")

# monthly analysis
# temperature

data <- subset (month.stat, subset = month >= "2010-01-01" & month < "2022-01-01",
                select = c (month, Mmean.min.temp.C, Mmean.ave.temp.C, Mmean.max.temp.C))
plot (Mmean.ave.temp.C ~ month, data = data,
      ylim = c (min (Mmean.min.temp.C, na.rm = FALSE), max (Mmean.max.temp.C, na.rm = FALSE)), type = "l",
      main = paste (Location, txt.ave.minmax.month.temp),
      xlab = txt.time.months, ylab = txt.temp)
lines (Mmean.min.temp.C ~ month, data = data, col = "blue")
lines (Mmean.max.temp.C ~ month, data = data, col = "red")
abline (h = seq (from = -30, to = 300, by = 5), col = "lightgrey")

ggplot2::ggplot (data = data, aes (x = month, y = Mmean.ave.temp.C)) +
  geom_line () + 
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.ave.month.temp)) +
  xlab (txt.time.months) + ylab (txt.temp) + theme_light ()



# monthly boxplots
data <- subset (day.term, subset = date >= "2020-01-01" & date < "2022-01-01",
                select = c (date, min.temp.C, ave.temp.C, max.temp.C))

boxplot (ave.temp.C ~ format (date, "%Y-%m-01"), data = data,
         main = paste (Location, txt.range.day.temp),
         xlab = txt.time.months, ylab = txt.temp)
abline (h = seq (from = -30, to = 30, by = 5), col = "lightgrey")


data <- subset (day.term, subset = date >= "2021-01-01" & date < "2022-01-01",
                select = c (date, min.temp.C, ave.temp.C, max.temp.C))

boxplot (ave.temp.C ~ format (date, "%Y-%m-01"), data = data,
         main = paste (Location, txt.range.day.temp),
         xlab = txt.time.months, ylab = txt.temp)
abline (h = seq (from = -30, to = 30, by = 5), col = "lightgrey")

# precipitation

data <- subset (month.stat, subset = month >= "2010-01-01" & month < "2022-01-01",
                select = c (month, Msum.precipit.mm))
plot (Msum.precipit.mm ~ month, data = data,
      ylim = c (0, max (Msum.precipit.mm, na.rm = FALSE)), type = "l",
      main = paste (Location, txt.month.rain), xlab = txt.time.months, ylab = txt.rain.mm)
abline (h = seq (from = 0, to = 800, by = 100), col = "lightgrey")


ggplot2::ggplot (data = data, aes (x = month, y = Msum.precipit.mm)) +
  geom_line () + 
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.month.rain)) +
  xlab (txt.time.months) + ylab (txt.month.rain.mm) + theme_light ()


data <- subset (day.term, subset = date >= "2021-01-01" & date < "2022-01-01",
                select = c (date, precipit.mm))

boxplot (precipit.mm ~ format (date, "%Y-%m-01"), data = data,
         main = paste (Location, txt.rain.day.month),
         xlab = txt.time.months, ylab = txt.rain.mm)
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")

# sunshine

data <- subset (month.stat, select = c (month, Msum.sun.h))

plot (Msum.sun.h ~ month, data = data, type = "l",
      main = paste (Location, txt.month.sun),
      ylim = c (0, 350), 
      xlab = txt.time.months, ylab = txt.month.sun.h)
abline (h = seq (from = 0, to = 400, by = 50), col = "lightgrey")

ggplot2::ggplot (data = data, aes (x = month, y = Msum.sun.h)) + geom_line() +
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.month.sun)) +
  xlab (txt.time.years) + ylab (txt.month.sun.h) + theme_light()


data <- subset (day.term, subset = date >= "2021-01-01" & date < "2022-01-01",
                select = c (date, sun.h))

boxplot (sun.h ~ format (date, "%Y-%m-01"), data = data,
         main = paste (Location, txt.sun.day.month),
         xlab = txt.time.months, ylab = txt.sun.day.month.h)
abline (h = seq (from = 0, to = 15, by = 5), col = "lightgrey")


# analysis as time series

# averages
ts.Mmean.ave.temp.C <- ts (data = month.stat$Mmean.ave.temp.C, start = y.loc.start, frequency = 12)
plot (ts.Mmean.ave.temp.C, main = paste (Location, txt.ave.month.temp),
      xlab = txt.time.months, ylab = txt.temp)

dc.ts.Mmean.ave.temp.C <- decompose (ts.Mmean.ave.temp.C, type = "multiplicative")
plot (dc.ts.Mmean.ave.temp.C)

# season plot
forecast::ggseasonplot (ts.Mmean.ave.temp.C, polar = TRUE)
forecast::ggseasonplot (ts.Mmean.ave.temp.C, polar = FALSE)

# forecasting two years ahead
fct.Mmean.ave.temp.C <- forecast::forecast (ts.Mmean.ave.temp.C)
autoplot (fct.Mmean.ave.temp.C)
autoplot (fct.Mmean.ave.temp.C, include = 3*12)


# maxima
ts.Mmax.max.temp.C <- ts (data = month.stat$Mmax.max.temp.C, start = y.loc.start, frequency = 12)
plot (ts.Mmean.ave.temp.C, main = paste (Location, txt.max.month.max.day),
      xlab = txt.time.months, ylab = txt.temp)

dcm.ts.Mmax.max.temp.C <- decompose (ts.Mmax.max.temp.C, type = "multiplicative")
plot (dcm.ts.Mmax.max.temp.C)

forecast::ggseasonplot (ts.Mmax.max.temp.C, polar = FALSE)

fct.Mmax.max.temp.C <- forecast::forecast (ts.Mmax.max.temp.C)
autoplot (fct.Mmax.max.temp.C)
autoplot (fct.Mmax.max.temp.C, include = 3*12)

# time series on daily data

dts.ave.temp.C <- ts (day.term$ave.temp.C, start = c (y.loc.start, 1), frequency = 365.25) # 
plot (dts.ave.temp.C, main = paste (Location, txt.ave.day.temp),
      xlab = txt.time.days, ylab = txt.temp)

dca.dts.ave.temp.C <- decompose (dts.ave.temp.C, type = "additive")
plot (dca.dts.ave.temp.C)

dcm.dts.ave.temp.C <- decompose (dts.ave.temp.C, type = "multiplicative")
plot (dcm.dts.ave.temp.C)


# can be computed - but no use because confidence interval is huge

forecast::ggseasonplot (dts.ave.temp.C, polar = FALSE)
fct.dts.ave.temp.C <- forecast::forecast (dts.ave.temp.C)
autoplot (fct.dts.ave.temp.C, include = 3*360)
