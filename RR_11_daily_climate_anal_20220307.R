################################################################################
# Analiza klimatskih podatkov za Ljubljano, Kredarico in Rakican (Mursko Soboto)
# prebranih iz www.meteo.si in shranjenih v lokalni bazi statisticnih podatkov v
# datoteki ./Data/'Location'_stat.RData in v bazi referencnih podatkov 
# ./Data/'Location'_ref.RData
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
# Program narise grafe stevila tropskih noci, mrzlih dni, dezevnih dni
# snezenih dni v letu in sorodne klimatoloske izracune.
################################################################################
# Analyis of daily average weather data for Ljubljana, Kredarica and Rakican
# (Murska Sobota) previously saved in statistical files
# ./Data/'Location'_stat.RData and
# ./Data/'Location'_ref.RData
# The start date is dependent on location when the first measurement took place.
# For other locations the program must be adapted to.

################################################################################
# Input: ./Data/'Location'_stat.RData : day.term, month.stat, year.stat
#        ./Data/'Location'_ref.RData : ref.days.1951.1980, ref.days.1981.2010,
#                                      ref.months.1951.1980, ref.months.1981.2010,
#                                      ref.years.1951.1980, ref.years.1981.2010
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

Location <- "Ljubljana"
#Location <- "Kredarica"
#Location <- "Rakican"

# start year of observations

if (Location == "Ljubljana") { y.loc.start <- 1948
} else if (Location == "Kredarica") { y.loc.start <- 1955
} else if (Location == "Rakican") { y.loc.start <- 1956 }


# all texts used in the analysis

txt.time.days <- "Čas (dnevi)"
txt.time.months <- "Čas (meseci)"
txt.time.years <- "Čas (leta)"
txt.temp <- "Temperatura (st C)"
txt.ave.day.temp <- "povprečne dnevne temperature"
txt.ave.day.temp.deg <- "Povprečna dnevna temperatura (st C)"
txt.ave.month.temp <- "povprečne mesečne temperature"
txt.max.month.max.day <- "mesečni maksimumi dnevnih maksimalnih temperatur"
txt.range.day.temp <- "razpon povprečnih dnevnih temperatur po mesecih"
txt.ave.year.temp <- "povprečne letne temperature"
txt.ave.minmax.year.temp <- "povprečne in skrajne letne temperature"
txt.rain.mm <- "Količine padavin (mm)"
txt.rain.day.month <- "dnevne padavine po mesecih"
txt.month.rain <- "Skupne mesečne padavine"
txt.month.rain.mm <- "skupne mesečne padavine (mm))"
txt.year.rain <- "letne padavine"
txt.sun.day.month <- "dnevno sončno sevanje po mesecih"
txt.sun.day.month.h <- "dnevno sončno sevanje po mesecih (h)"
txt.month.sun <- "skupno mesečno trajanje sončnega obsevanja"
txt.month.sun.h <- "skupno mesečno trajanje sončnega obsevanja (h)"
txt.tropic.nights <- "tropske noči"
txt.num.tropic.nights <- "Število tropskih noči v letu"
txt.no.tropic.nights <- "Ni tropskih noči."
txt.freezing.days <- "dnevi z zmrzaljo"
txt.num.freezing.days <- "število dni z zmrzaljo v letu"
txt.rain.days <- "dnevi s padavinami"
txt.num.Rain.days <- "Število močno padavinskih dni"
txt.num.rain.days <- "število močno padavinskih dni"
txt.num.freezing.days <- "število dni z zmrzaljo"
txt.no.freezing.days <- "Ni dni z zmrzaljo"
txt.max.precip.day <- "maksimalne količine padavin najbolj mokrih dni v letu"
txt.num.Snowy.days <- "Število dni s snegom"
txt.num.snowy.days <- "število dni s snegom"
txt.num.Snow.cover <- "Število dni s snežno odejo v letu"
txt.num.snow.cover <- "število dni s snežno odejo v letu"

load (file = paste0 ("./Data/", Location, "_stat.RData"))  # day.term, month.stat, year.stat

load (file = paste0 ("./Data/", Location, "_ref.RData"))  # 


# current days

cur.days <- subset (day.term, subset = date < "2022-01-01")

Ref.dates <- "1981:2010"
# cur.day.ref <- day.ref.1981.2010
cur.ref.days  <- ref.days.1981.2010


# tropical nights - average daily temp >= 20 degrees C
# count tropical nights per year

tropic <- subset (cur.days, subset = min.temp.C >= 20)
if (dim (tropic)[1] > 0) {
  plot (max.temp.C ~ date, data = tropic, main = paste (Location, txt.tropic.nights),
        xlab = txt.time.years, ylab = "Povprečna dnevna temperatura (st C)", col = "red",
        ylim = c (min (tropic$min.temp.C), max (tropic$max.temp.C)))
  points (ave.temp.C ~ date, data = tropic)
  points (min.temp.C ~ date, data = tropic, col = "blue")
  
  # postej jih po letih
  count.tropic <- aggregate (x = cur.days$min.temp.C >= 20,
                             by = list (year = format (cur.days$date, "%Y")),
                             FUN = sum, na.rm = TRUE)
  count.tropic$year <- as.Date (paste0 (count.tropic$year, "-01-01"))
  plot (x ~ year, data = count.tropic, 
        main = paste (Location, txt.tropic.nights),
        xlab = txt.time.years, ylab = txt.num.tropic.nights, xlim = range (cur.days$date),
        ylim = c (0, max (count.tropic$x)))
  abline (h = seq (from = 0, to = 15, by = 5), col = "lightgrey")
  
  ggplot2::ggplot (data = count.tropic, aes (x = year, y = x)) + geom_point(colour = "red") + 
    geom_smooth (method = "loess", formula = y ~ x) + 
    ggtitle (paste (Location, txt.tropic.nights)) + xlab (txt.time.years) +
    ylab (txt.num.tropic.nights) + theme_light()
} else print (txt.no.tropic.nights)


# freezing days - all day temp <= 0 degrees C
# count freezing days per year

cold.days <- subset (cur.days, subset = max.temp.C <= 0)
if (dim (cold.days) [1] > 0) {
  plot (max.temp.C ~ date, data = cold.days, main = paste (Location, txt.freezing.days),
        xlab = txt.time.years, ylab = txt.ave.day.temp.deg, col = "red",
        ylim = c (min (cold.days$min.temp.C), max (cold.days$max.temp.C)))
  points (ave.temp.C ~ date, data = cold.days)
  points (min.temp.C ~ date, data = cold.days, col = "blue")
  
  # postej jih po letih
  count.cold.days <- aggregate (x = cur.days$max.temp.C <= 0,
                                by = list (year = format (cur.days$date, "%Y")),
                                FUN = sum, na.rm = TRUE)
  count.cold.days$year <- as.Date (paste0 (count.cold.days$year, "-01-01"))
  plot (x ~ year, data = count.cold.days, 
        main = paste (Location, txt.freezing.days),
        xlab = txt.time.years, ylab = txt.num.freezing.days, xlim = range (cur.days$date),
        ylim = c (0, max (count.cold.days$x)))
  abline (h = seq (from = 0, to = 150, by = 10), col = "lightgrey")
  
  ggplot2::ggplot (data = count.cold.days, aes (x = year, y = x)) + geom_point(colour = "blue") + 
    geom_smooth (method = "loess", formula = y ~ x) + 
    ggtitle (paste (Location, txt.freezing.days )) + xlab (txt.time.years) +
    ylab (txt.num.freezing.days) + theme_light()
} else print (txt.no.freezing.days)


# top list of wet days in a year
# averages

wet.days <- subset (cur.ref.days, subset = Dmean.precipit.mm > 0)

sort.wet.days <- wet.days [order (- wet.days$Dmean.precipit.mm), ]
tmp <- subset (sort.wet.days, select = c (day, Dmean.precipit.mm, Dmean.ave.temp.C))
print (data.frame (dan=format(tmp$day, "%d-%m"), padavin = tmp$Dmean.precipit.mm, temp = tmp$Dmean.ave.temp.C)[1:20, ])

# medians
sort.wet.days <- wet.days [order (- wet.days$Dmedian.precipit.mm), ]
tmp <- subset (sort.wet.days, select = c (day, Dmedian.precipit.mm, Dmedian.ave.temp.C))
print (data.frame (dan=format(tmp$day, "%d-%m"), padavin = tmp$Dmedian.precipit.mm, temp = tmp$Dmedian.ave.temp.C)[1:20, ])


flood <- subset (day.term, subset = precipit.mm >= 20)
plot (precipit.mm ~ date, data = flood, main = paste (Location, txt.rain.days),
      xlab = txt.time.years, ylab = txt.rain.mm, col = "red",
      ylim = c (0, max (flood$precipit.mm)))
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")

flood <- subset (day.term, subset = precipit.mm >= 20)
count.flood <- aggregate (x = flood$precipit.mm >= 20,
                           by = list (year = format (flood$date, "%Y")),
                           FUN = sum, na.rm = TRUE)
count.flood$year <- as.Date (paste0 (count.flood$year, "-01-01"))
plot (x ~ year, data = count.flood, 
      main = paste (Location, txt.num.rain.days),
      xlab = txt.time.years, ylab = txt.num.Rain.days,
      xlim = range (day.term$date), ylim = c (0, max (count.flood$x)))
abline (h = seq (from = 0, to = 50, by = 5), col = "lightgrey")

# amount of maximal rain days per year

max.flood <- aggregate (x = cur.days$precipit.mm,
                          by = list (year = format (cur.days$date, "%Y")),
                          FUN = max, na.rm = TRUE)
max.flood$year <- as.Date (paste0 (max.flood$year, "-01-01"))
plot (x ~ year, data = max.flood, 
      main = paste (Location, txt.max.precip.day),
      xlab = txt.time.years, ylab = txt.rain.mm,
      xlim = range (cur.days$date), ylim = c (0, max (max.flood$x)))
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")

ggplot2::ggplot (data = max.flood, aes (x = year, y = x)) + geom_point() + 
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.max.precip.day)) +
  xlab (txt.time.years) + ylab (txt.rain.mm) + theme_light()


# number of days with rain per year

wet <- subset (cur.days, subset = precipit.mm > 0)
count.wet <- aggregate (x = cur.days$precipit.mm > 0,
                           by = list (year = format (cur.days$date, "%Y")),
                           FUN = sum, na.rm = TRUE)
count.wet$year <- as.Date (paste0 (count.wet$year, "-01-01"))
plot (x ~ year, data = count.wet, 
      main = paste (Location, txt.num.rain.days),
      xlab = txt.time.years, ylab = txt.num.rain.days, xlim = range (day.term$date),
      ylim = c (0, max (count.wet$x)))
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")

ggplot2::ggplot (data = count.wet, aes (x = year, y = x)) + geom_point(colour = "blue") + 
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.num.rain.days)) + xlab (txt.time.years) +
  ylab (txt.num.rain.days) + theme_light()


# number of days with snow

snowy <- subset (cur.days, subset = new.snow.cm > 0)
count.snowy <- aggregate (x = cur.days$new.snow.cm > 0,
                        by = list (year = format (cur.days$date, "%Y")),
                        FUN = sum, na.rm = TRUE)
count.snowy$year <- as.Date (paste0 (count.snowy$year, "-01-01"))
plot (x ~ year, data = count.snowy, 
      main = paste (Location, txt.num.snowy.days),
      xlab = txt.time.years, ylab = txt.num.Snowy.days, xlim = range (day.term$date),
      ylim = c (0, max (count.snowy$x)))
abline (h = seq (from = 0, to = 200, by = 10), col = "lightgrey")

ggplot2::ggplot (data = count.snowy, aes (x = year, y = x)) + geom_point(colour = "blue") + 
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.num.snowy.days)) + xlab ("Leta") + 
  ylab (txt.num.Snowy.days) + theme_light()


# number of days with snow cover

snow.cover <- subset (cur.days, subset = snow.level.cm > 0)
count.snow.cover <- aggregate (x = cur.days$snow.level.cm > 0,
                          by = list (year = format (cur.days$date, "%Y")),
                          FUN = sum, na.rm = TRUE)
count.snow.cover$year <- as.Date (paste0 (count.snow.cover$year, "-01-01"))
plot (x ~ year, data = count.snow.cover, 
      main = paste (Location, txt.num.snow.cover),
      xlab = txt.time.years, ylab = txt.num.Snow.cover, xlim = range (day.term$date),
      ylim = c (0, max (count.snow.cover$x)))
abline (h = seq (from = 0, to = 200, by = 20), col = "lightgrey")

ggplot2::ggplot (data = count.snow.cover, aes (x = year, y = x)) + geom_point(colour = "blue") + 
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.num.snow.cover)) + xlab ("Leta") + 
  ylab (txt.num.Snow.cover) + theme_light()


# top list of days with flood

sort.wet <- cur.days [order (- cur.days$precipit.mm), ]
print (subset (sort.wet, select = c (date, precipit.mm, ave.temp.C))[1:20, ])

sort.new.snow <- cur.days [order (- cur.days$new.snow.cm), ]
print (subset (sort.new.snow, select = c (date, new.snow.cm, snow.level.cm, precipit.mm, ave.temp.C))[1:20, ])

sort.snow.level <- cur.days [order (- cur.days$snow.level.cm), ]
print (subset (sort.snow.level, select = c (date, snow.level.cm, new.snow.cm, precipit.mm, ave.temp.C))[1:20, ])

sort.cold <- cur.days [order (cur.days$ave.temp.C), ]
print (subset (sort.cold, select = c (date, ave.temp.C, min.temp.C, max.temp.C, snow.level.cm, new.snow.cm))[1:20, ])

sort.record.cold <- cur.days [order (cur.days$min.temp.C), ]
print (subset (sort.record.cold, select = c (date, min.temp.C, ave.temp.C, max.temp.C, snow.level.cm, new.snow.cm))[1:20, ])

sort.hot <- cur.days [order (- cur.days$ave.temp.C), ]
print (subset (sort.hot, select = c (date, ave.temp.C, min.temp.C, max.temp.C, rel.humid, precipit.mm))[1:20, ])

sort.record.hot <- cur.days [order (- cur.days$max.temp.C), ]
print (subset (sort.record.hot, select = c (date, max.temp.C, min.temp.C, ave.temp.C, rel.humid, precipit.mm))[1:20, ])


# draught - number of days without no rain
## to do
