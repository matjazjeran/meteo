################################################################################
# Branje terminskih vremenskih podatkov za Ljubljano, Kredarico in
# Rakican (Mursko Soboto) iz arhiva ARSO portala www.meteo.si
# Dnevni terminski podatki ob 07, 13 in 21h.
# Zacetni datum je odvisen od kraja, kjer so zaceli meriti podatke.
# Podatki za Rakican so iz dveh virov iz portala in po najboljsih moceh
# sestavljeni skupaj.
# Za se druge kraje je treba ustrezno adaptirati program in nabrati datoteke.
################################################################################
# Reading daily weather term data for Ljubljana, Kredarica and Rakican
# (Murska Sobota). Daily measurements at 07, 13 and 21h.
# The start date is dependent on location when the first
# measurement took place.
# Rakican data is a composite of two sources from the portal meteo.si
# For other locations the program mus be adapted to.
################################################################################
# Input: Downloaded text files .Data/Location/term_fromyear_toyear.txt
# Output: ./Data/Location_term.RData : term
#
# Matjaz Jeran
# 07.03.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux") setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

# reading daily data for selected location
# comment out the desired location

Location <- "Ljubljana"
# Location <- "Kredarica"
# Location <- "Rakican"

if (Location == "Ljubljana") {
  file_1 <- "1948_1950.txt"
  file_2 <- "1951_1952.txt"
  file_3 <- "1953_1954.txt"
  file_4 <- "1955_1956.txt"
  file_5 <- "1957_1958.txt"
  file_6 <- "1959_1960.txt"
  file_7 <- "1961_1962.txt"
  file_8 <- "1963_1964.txt"
  file_9 <- "1965_1966.txt"
  file_10 <- "1967_1968.txt"
  file_11 <- "1969_1970.txt"
  file_12 <- "1971_1973.txt"
  file_13 <- "1974_1978.txt"
  file_14 <- "1979_1984.txt"
  file_15 <- "1985_1990.txt"
  file_16 <- "1991_1996.txt"
  file_17 <- "1997_2000.txt"
  file_18 <- "2001_2006.txt"
  file_19 <- "2007_2011.txt"
  file_20 <- "2012_2017.txt"
  file_21 <- "2018_2022.txt"
  
  for (i in 1:21)
    eval (expr = parse (
      text = paste0 ("data", i,
                     " <- read.table (file = paste0 ('./Data/Ljubljana/term_', file_", i,
                     '), header = TRUE, sep = ",", na.strings = "NA", dec = ".",
                     strip.white = TRUE, fileEncoding = "utf8")')))
  
  tmp <- rbind (data1, data2, data3, data4, data5, data6, data7, data8, data9, data10,
                data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21)
  
  any (duplicated (tmp))
  
  rm (data1, data2, data3, data4, data5, data6, data7, data8, data9, data10,
      data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21)
} else if (Location == "Kredarica") {
  file_1 <- "1955_1961.txt"
  file_2 <- "1962_1967.txt"
  file_3 <- "1968_1974.txt"
  file_4 <- "1975_1981.txt"
  file_5 <- "1982_1988.txt"
  file_6 <- "1989_1995.txt"
  file_7 <- "1996_2002.txt"
  file_8 <- "2003_2009.txt"
  file_9 <- "2010_2016.txt"
  file_10 <- "2017_2022.txt"
  for (i in 1:10)
    eval (expr = parse (
      text = paste0 ("data", i,
                     " <- read.table (file = paste0 ('./Data/Kredarica/term_', file_", i,
                     '), header = TRUE, sep = ",", na.strings = "NA", dec = ".",
                     strip.white = TRUE, fileEncoding = "utf8")')))
  
  tmp <- rbind (data1, data2, data3, data4, data5, data6, data7, data8, data9, data10)
  
  any (duplicated (tmp))
  
  rm (data1, data2, data3, data4, data5, data6, data7, data8, data9, data10)
} else if (Location == "Rakican") {
  file_1 <- "1948_1953.txt"
  file_2 <- "1954_1960.txt"
  file_3 <- "1961_1965.txt"
  file_4 <- "1966_1971.txt"
  file_5 <- "1972_1976.txt"
  file_6 <- "1977_1981.txt"
  file_7 <- "1982_1986.txt"
  file_8 <- "1987_1991.txt"
  file_9 <- "1992_1996.txt"
  file_10 <- "1997_2001.txt"
  file_11 <- "2002_2006.txt"
  file_12 <- "2007_2011.txt"
  file_13 <- "2012_2017.txt"
  file_14 <- "2018_2022.txt"
  for (i in 1:14)
    eval (expr = parse (
      text = paste0 ("data", i,
                     " <- read.table (file = paste0 ('./Data/Rakican/term_', file_", i,
                     '), header = TRUE, sep = ",", na.strings = "NA", dec = ".",
                     strip.white = TRUE, fileEncoding = "utf8")')))
  
  tmp <- rbind (data1, data2, data3, data4, data5, data6, data7, data8, data9,
                data10, data11, data12, data13, data14)
  
  any (duplicated (tmp))
  
  rm (data1, data2, data3, data4, data5, data6, data7, data8, data9, data10,
      data11, data12, data14)
}

term <- data.frame (
  date.str       = substr (tmp$valid, 1, 10),
  time.str       = substr (tmp$valid, 12, 16),
  temp.C         = tmp$T...C,
  rel.humid      = tmp$rel..vlaga....,
  dir.wind       = tmp$smer.vetra....,
  wind.speed.m.s = tmp$hitrost.vetra..m.s.)

# manually combine three columns at 7 14 and 21h
tmp7 <- subset (term, subset = time.str == "07:00")
tmp14 <- subset (term, subset = time.str == "14:00")
tmp21 <- subset (term, subset = time.str == "21:00")

tmp7.w <- data.frame (
  date.str          = tmp7$date.str,
  time.str          = tmp7$time.str,
  temp.C.7          = tmp7$temp.C,
  rel.humid.7       = tmp7$rel.humid,
  dir.wind.7        = tmp7$dir.wind,
  wind.speed.m.s.7  = tmp7$wind.speed.m.s)

tmp14.w <- data.frame (
  date.str          = tmp14$date.str,
  time.str          = tmp14$time.str,
  temp.C.14         = tmp14$temp.C,
  rel.humid.14      = tmp14$rel.humid,
  dir.wind.14       = tmp14$dir.wind,
  wind.speed.m.s.14 = tmp14$wind.speed.m.s)

tmp21.w <- data.frame (
  date.str          = tmp21$date.str,
  time.str          = tmp21$time.str,
  temp.C.21         = tmp21$temp.C,
  rel.humid.21      = tmp21$rel.humid,
  dir.wind.21       = tmp21$dir.wind,
  wind.speed.m.s.21 = tmp21$wind.speed.m.s)

tmp.term <- merge (tmp7.w, merge (tmp14.w, tmp21.w, by = "date.str"), by = "date.str")

term <- rbind (data.frame (
  date.str       = tmp.term$date.str,
  date           = as.Date (substr (tmp.term$date.str, 1, 10)),
  subset (tmp.term, select = c (temp.C.7:wind.speed.m.s.7,
                                temp.C.14:wind.speed.m.s.14,
                                temp.C.21:wind.speed.m.s.21))))

save (term, file = paste0 ("./Data/", Location, "_term.RData"))
