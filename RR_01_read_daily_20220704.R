################################################################################
# Branje dnevnih povprecnih vremenskih podatkov za Ljubljano, Kredarico in
# Rakican (Mursko Soboto) iz arhiva ARSO portala www.meteo.si
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Podatki za Rakican so iz dveh virov iz portala in po najboljsih moceh
# sestavljeni skupaj.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
################################################################################
# Reading daily average weather data for Ljubljana, Kredarica and Rakican
# (Murska Sobota). The start date is dependent on location when the first
# measurement took place.
# Rakican data is a composite of two sources from the portal www.meteo.si
# For other locations the program mus be adapted to.
################################################################################
# Input: Downloaded text files ./Data/Location/D_fromyear_toyear.txt
#                              ./Data/Location/DN_fromyear_toyear.txt
#                              ./Data/Location/DNN_fromyear_toyear.txt
# Output: ./Data/Location_D.RData : daily1, daily2, daily3
#
# Matjaz Jeran
# 04.07.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux") setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

# reading daily data for selected location
# comment out the desired location

Location <- "Ljubljana"
 Location <- "Kredarica"
 Location <- "Rakican"

da.ne.2.logical <- function (x)
{
  y <- NA [1:length(x)]
  y [x == "da"] <- TRUE
  y [x == "ne"] <- FALSE
  return(y)
}

# column names for all second files
c.names <- c ("station.id", "station.name", "valid",
              "močan.veter", "viharni.veter", "dež", "rosenje", "ploha.dežja", "nevihta",
              "grmenje", "bliskanje", "dež..ki.zmrzuje", "rosenje..ki.zmrzuje", "iglice",
              "sneg", "zrnat.sneg", "ploha.snega", "dež.s.snegom", "babje.pšeno",
              "ploha.dežja.s.snegom", "toča", "sodra")

if (Location == "Ljubljana") {
  file_1 <- "1948_1957.txt"
  file_2 <- "1958_1967.txt"
  file_3 <- "1968_1977.txt"
  file_4 <- "1978_1987.txt"
  file_5 <- "1988_1997.txt"
  file_6 <- "1998_2007.txt"
  file_7 <- "2008_2017.txt"
  file_8 <- "2018_2022.txt"
  
# read first part of daily data
  for (i in 1:8)
    eval (expr = parse (
      text = paste0 (
        "data",
        i,
        " <- read.table (file = paste0 ('./Data/Ljubljana/D_', file_",
        i,
        '), header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE, 
        stringsAsFactors = FALSE, fileEncoding = "utf8")'
      )
    ))
  
  tmp1 <- rbind (data1, data2, data3, data4, data5, data6, data7, data8)
  
  any (duplicated (tmp1))
  
  rm (data1, data2, data3, data4, data5, data6, data7, data8)
  
# read second part of daily data
  for (i in 1:8)
    eval (expr = parse (
      text = paste0 ("{ data", i, 
                     " <- read.table (file = paste0 ('./Data/Ljubljana/DN_', file_", i,
                     '), header = TRUE, skip = 1, sep = ",", na.strings = "NA", dec = ".", 
                                       strip.white = TRUE, stringsAsFactors = FALSE, fileEncoding = "utf8")\n',
                     'colnames (data', i, ') <- c.names }')))
  
  tmp2 <- rbind (data1, data2, data3, data4, data5, data6, data7, data8)
  
  any (duplicated (tmp2))
  
  rm (data1, data2, data3, data4, data5, data6, data7, data8)
  
# read third part of daily data
  for (i in 1:8)
    eval (expr = parse (
      text = paste0 ("data", i,
                     " <- read.table (file = paste0 ('./Data/Ljubljana/DNN_', file_", i, '),
        header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE,
        stringsAsFactors = FALSE, fileEncoding = "utf8")'
      )))
  
  tmp3 <- rbind (data1, data2, data3, data4, data5, data6, data7, data8)
  
  any (duplicated (tmp3))
  
  rm (data1, data2, data3, data4, data5, data6, data7, data8)
} else if (Location == "Kredarica") {
  file_1 <- "1955_1964.txt"
  file_2 <- "1965_1974.txt"
  file_3 <- "1975_1984.txt"
  file_4 <- "1985_1994.txt"
  file_5 <- "1995_2004.txt"
  file_6 <- "2005_2014.txt"
  file_7 <- "2015_2022.txt"
  
# read first part of daily data
  for (i in 1:7)
    eval (expr = parse (
      text = paste0 ("data", i, " <- read.table (file = paste0 ('./Data/Kredarica/D_', file_", i, '),
                     header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE,
                     stringsAsFactors = FALSE, fileEncoding = "utf8")'
      )))
  
  tmp1 <- rbind (data1, data2, data3, data4, data5, data6, data7)
  
  any (duplicated (tmp1))
  
  rm (data1, data2, data3, data4, data5, data6, data7)
  
# read second part of daily data
  for (i in 1:7)
    eval (expr = parse (
      text = paste0 ("{ data", i, 
                     " <- read.table (file = paste0 ('./Data/Kredarica/DN_', file_", i,
                     '), header = TRUE, skip = 1, sep = ",", na.strings = "NA", dec = ".", 
                     strip.white = TRUE, stringsAsFactors = FALSE, fileEncoding = "utf8")\n',
                     'colnames (data', i, ') <- c.names }')))
  
  tmp2 <- rbind (data1, data2, data3, data4, data5, data6, data7)
  
  any (duplicated (tmp2))
  
  rm (data1, data2, data3, data4, data5, data6, data7)
  
# read third part of daily data
  for (i in 1:7)
    eval (expr = parse (
      text = paste0 ("data", i, " <- read.table (file = paste0 ('./Data/Kredarica/DNN_', file_", i, '),
                     header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE,
                     stringsAsFactors = FALSE, fileEncoding = "utf8")'
      )))
  
  tmp3 <- rbind (data1, data2, data3, data4, data5, data6, data7)
  
# check for any duplicated data
  any (duplicated (tmp3))
  
# dodaj preverjanje, ce kak datum manjka
  rm (data1, data2, data3, data4, data5, data6, data7)
} else if (Location == "Rakican") {
  file_1 <- "1956_1965.txt"
  file_2 <- "1966_1975.txt"
  file_3 <- "1976_1985_levi.txt"
  file_4 <- "1976_1985_vsredi.txt"
  file_5 <- "1986_1995.txt"
  file_6 <- "1996_2005.txt"
  file_7 <- "2006_2015.txt"
  file_8 <- "2016_2022.txt"
  
# read first part of data
  for (i in 1:8)
    eval (expr = parse (
      text = paste0 ("data", i, 
                     " <- read.table (file = paste0 ('./Data/Rakican/D_', file_", i, '), 
                          header = TRUE, sep = ",", na.strings = "NA", dec = ".", 
                          strip.white = TRUE, stringsAsFactors = FALSE, fileEncoding = "utf8")')))
  
  dat3 <- subset (data3, subset = ! is.na (povp..dnevna.T...C.))
  dat4 <- subset (data4, subset = ! is.na (povp..dnevna.T...C.))
  
  tmp1 <- rbind (data1, data2, dat3, dat4, data5, data6, data8)
  
  any (duplicated (tmp1))
  
  rm (dat3, dat4)
  rm (data1, data2, data3, data4, data5, data6, data7, data8)
  
# read second part of data
  for (i in 1:8)
    eval (expr = parse (
      text = paste0 ("{ data", i, 
                     " <- read.table (file = paste0 ('./Data/Rakican/DN_', file_", i,
                     '), header = TRUE, skip = 1, sep = ",", na.strings = "NA", dec = ".", 
                     strip.white = TRUE, stringsAsFactors = FALSE, fileEncoding = "utf8")\n',
                     'colnames (data', i, ') <- c.names }')))
  
  dat3 <- subset (data3, subset = ! is.na (viharni.veter))
  dat4 <- subset (data4, subset = ! is.na (viharni.veter))
  
  tmp2 <- rbind (data1, data2, dat3, dat4, data5, data6, data8)
  
  any (duplicated (tmp2))
  
  rm (dat3, dat4)
  rm (data1, data2, data3, data4, data5, data6, data7, data8)
  
# read third part of data
  for (i in 1:8)
    eval (expr = parse (
      text = paste0 ("data", i, 
                     " <- read.table (file = paste0 ('./Data/Rakican/DNN_', file_", i, '), 
                                       header = TRUE, sep = ",", na.strings = "NA", dec = ".", 
                                       strip.white = TRUE, stringsAsFactors = FALSE, fileEncoding = "utf8")')))
  
  dat3 <- subset (data3, subset = ! is.na (megla))
  dat4 <- subset (data4, subset = ! is.na (megla))
  
  tmp3 <- rbind (data1, data2, dat3, dat4, data5, data6, data8)
  
  any (duplicated (tmp3))
  
  rm (dat3, dat4)
  rm (data1, data2, data3, data4, data5, data6, data7, data8)
}

daily1 <- data.frame (
  date.str       = tmp1$valid,
  date           = as.Date (tmp1$valid, format = "%Y-%m-%d"),
  min.temp.5cm.C = tmp1$min..T.na.5cm...C.,
  min.temp.C     = tmp1$min..T...C.,
  ave.temp.C     = tmp1$povp..dnevna.T...C.,
  max.temp.C     = tmp1$max..T...C.,
  rel.humid      = tmp1$povp..rel..vla.....,
  pressure.hPa   = tmp1$povp..tlak..hPa.,
  precipit.mm    = tmp1$količina.padavin..mm.,
  snow.level.cm  = tmp1$višina.snežne.odeje..cm.,
  new.snow.cm    = tmp1$višina.novega.snega..cm.,
  sun.h          = tmp1$trajanje.sonca..h.,
  clouds         = tmp1$oblačnost....)

tmp <- data.frame (
  date.str              = tmp2$valid,
  date                  = as.Date (tmp2$valid, format = "%Y-%m-%d"),
  strong.wind           = da.ne.2.logical (tmp2$močan.veter),
  gale                  = da.ne.2.logical (tmp2$viharni.veter),
  rain                  = da.ne.2.logical (tmp2$dež),
  drizzle               = da.ne.2.logical (tmp2$rosenje),
  rain.shower           = da.ne.2.logical (tmp2$ploha.dežja),
  storm                 = da.ne.2.logical (tmp2$nevihta),
  thunder               = da.ne.2.logical (tmp2$grmenje),
  lightning             = da.ne.2.logical (tmp2$bliskanje),
  freezing.rain         = da.ne.2.logical (tmp2$dež..ki.zmrzuje),
  freezing.drizzle      = da.ne.2.logical (tmp2$rosenje..ki.zmrzuje),
  needles               = da.ne.2.logical (tmp2$iglice),
  snow                  = da.ne.2.logical (tmp2$sneg),
  snow.pellets          = da.ne.2.logical (tmp2$zrnat.sneg),
  snow.storm            = da.ne.2.logical (tmp2$ploha.snega),
  rain.and.snow         = da.ne.2.logical (tmp2$dež.s.snegom),
  sleet                 = da.ne.2.logical (tmp2$babje.pšeno),
  rain.shower.with.snow = da.ne.2.logical (tmp2$ploha.dežja.s.snegom),
  hail                  = da.ne.2.logical (tmp2$toča),
  wet.snow              = da.ne.2.logical (tmp2$sodra))


daily2 <- data.frame (
  date.str              = tmp2$valid,
  date                  = as.Date (tmp2$valid, format = "%Y-%m-%d"),
  strong.wind           = da.ne.2.logical (tmp2$močan.veter),
  stormy.wind           = da.ne.2.logical (tmp2$viharni.veter),
  rain                  = da.ne.2.logical (tmp2$dež),
  drizzle               = da.ne.2.logical (tmp2$rosenje),
  rain.shower           = da.ne.2.logical (tmp2$ploha.dežja),
  storm                 = da.ne.2.logical (tmp2$nevihta),
  thunder               = da.ne.2.logical (tmp2$grmenje),
  lightning             = da.ne.2.logical (tmp2$bliskanje),
  freezing.rain         = da.ne.2.logical (tmp2$dež..ki.zmrzuje),
  freezing.drizzle      = da.ne.2.logical (tmp2$rosenje..ki.zmrzuje),
  needles               = da.ne.2.logical (tmp2$iglice),
  snow                  = da.ne.2.logical (tmp2$sneg),
  snow.pellets          = da.ne.2.logical (tmp2$zrnat.sneg),
  snow.storm            = da.ne.2.logical (tmp2$ploha.snega),
  rain.and.snow         = da.ne.2.logical (tmp2$dež.s.snegom),
  sleet                 = da.ne.2.logical (tmp2$babje.pšeno),
  rain.shower.with.snow = da.ne.2.logical (tmp2$ploha.dežja.s.snegom),
  hail                  = da.ne.2.logical (tmp2$toča),
  wet.snow              = da.ne.2.logical (tmp2$sodra))


daily3 <- data.frame (
  date.str       = tmp3$valid,
  date           = as.Date (tmp3$valid, format = "%Y-%m-%d"),
  fog            = da.ne.2.logical (tmp3$megla),
  fog.open.sky   = da.ne.2.logical (tmp3$megla.z.vidnim.nebom),
  icy.fog        = da.ne.2.logical (tmp3$ledena.megla),
  mist           = da.ne.2.logical (tmp3$meglica),
  dry.mist       = da.ne.2.logical (tmp3$suha.motnost),
  surface.fog    = da.ne.2.logical (tmp3$talna.megla),
  dew            = da.ne.2.logical (tmp3$rosa),
  frost          = da.ne.2.logical (tmp3$slana),
  ice            = da.ne.2.logical (tmp3$poledica),
  ice.surface    = da.ne.2.logical (tmp3$poledica.na.tleh),
  rime.ice       = da.ne.2.logical (tmp3$ivje),
  hard.rime      = da.ne.2.logical (tmp3$trdo.ivje),
  precipitation  = da.ne.2.logical (tmp3$padavine),
  snow.cover     = da.ne.2.logical (tmp3$snežna.odeja))


save (daily1, daily2, daily3, file = paste0 ("./Data/", Location, "_D.RData"))
