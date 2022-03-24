################################################################################
# Analiza razsirjenih mesecnih statistik podatkov temperatur samo za Ljubljano
# pridobljeno od dr Lucke Kajfez Bogataj
# Program izracuna mesecne statistike za Ljubljano obdobje od 1851 do zacetka
# zbiranja podatkov 1948 in naprej do konca leta 2021.
# Program narise boxplote in crtne grafe za temperature in naredi se
# casovno analizo temperatur z metodami analize casovnih vrst
################################################################################
# Analyis of monthly average temperature data only for Ljubljana from data
# obtained from dr Lucka Kajfez Bogataj.
# The start date iz 1851 until 1948 when routine data are available.
# For other locations the program must be adapted to.
# The program draws boxplots and line graphs of daily average data on
# temperature and finaly maxes time analysis of temperature as time series
################################################################################
# Input: ./Data/LJU_add_month_stat.RData : add.month.stat
#        ./Data/Location_stat.RData : day.term, month.stat, year.stat
# Output: line graphs and boxplots of monthly weather statistics for Ljubljana
#
# Matjaz Jeran
# 07.03.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

library (plyr)
library (ggplot2)
library (forecast)


Location <- "Ljubljana"

# define start year

if (Location == "Ljubljana") { y.loc.start <- 1851  # Lucka's extend back from 1948
} else if (Location == "Kredarica") { y.loc.start <- 1955
} else if (Location == "Rakican") { y.loc.start <- 1956 }


# all texts used in the analysis

txt.time.days <- "Čas (dnevi)"
txt.time.months <- "Čas (meseci)"
txt.time.years <- "Čas (leta)"
txt.range.years <- "Obdobja let"
txt.months.years <- "Meseci v letih"
txt.temp <- "Temperatura (st C)"
txt.ave.day.temp <- "povprečne dnevne temperature"
txt.ave.day.temp.deg <- "Povprečne dnevne temperature (st C)"
txt.ave.month.temp <- "povprečne mesečne temperature"
txt.ave.month.temp.deg <- "Povprečne mesečne temperature (st C)"
txt.max.month.max.day <- "mesečni maksimumi dnevnih maksimalnih temperatur"
txt.range.day.temp <- "razpon povprečnih dnevnih temperatur po mesecih"
txt.ave.month.ave.day <- "mesečna povprečja povprečnih dnevnih temperatur"
txt.ave.month.ave.day.deg <- "Mesečna povprečja povprečnih dnevnih temperatur (st C)"
txt.rang.month.temp.year <- "razpon povprečnih mesečnih temperatur po letih"
txt.rang.month.temp.range <- "razpon povprečnih mesečnih temperatur po obdobjih"
txt.cmp.month.ave.day <- "primerjava mesečnih povprečij povprečnih dnevnih temperatur"
txt.ave.year.temp <- "povprečne letne temperature"
txt.ave.minmax.year.temp <- "povprečne in skrajne letne temperature"
txt.rain.mm <- "Padavine (mm)"
txt.rain.day.month <- "dnevne padavine po mesecih"
txt.month.rain <- "skupne mesečne padavine"
txt.month.rain.mm <- "Skupne mesečne padavine (mm))"
txt.cmp.month.rain <- "Primerjava skupnih mesečnih padavin"
txt.year.rain <- "letne padavine"
txt.sun.day.h <- "Dnevno sončno sevanje (h)"
txt.sun.day.month <- "dnevno sončno sevanje po mesecih"
txt.sun.day.month.h <- "Dnevno sončno sevanje po mesecih (h)"
txt.month.sun <- "skupno mesečno trajanje sončnega obsevanja"
txt.month.sun.h <- "Skupno mesečno trajanje sončnega obsevanja (h)"
txt.cmp.month.sun <- "primerjava skupnega mesečnega trajanja sončnega obsevanja"


load (file = paste0 ("./Data/", Location, "_stat.RData"))  # day.term, month.stat, year.stat

load (file = "./Data/LJU_add_month_stat.RData")  # add.month.stat


# combine all data
tmp.month <- rbind (subset (add.month.stat, subset = month.str <= "1947-12-01"),
                    subset (month.stat, subset = month.str < "2022-01-01",
                            select = c (month.str, month, Mmean.ave.temp.C)))

# compute extended yearly statistics

year.stat <- plyr::ddply (.data = tmp.month,
                          .variables = .(year.str = substr (month.str, 1, 4), 
                                         year = as.Date (paste0 (substr (month.str, 1, 4), "-01-01"))),
                          summarize,
                          Ymean.ave.temp.C = mean (Mmean.ave.temp.C, na.rm = TRUE))
                          
# start with yearly analysis

ydata <- subset (year.stat, subset = year.str < "2022", select = c (year, Ymean.ave.temp.C))

plot (Ymean.ave.temp.C ~ year, data = ydata,
      ylim = range (Ymean.ave.temp.C, na.rm = FALSE), type = "l",
      main = paste (Location, txt.ave.year.temp), xlab = txt.time.years, ylab = txt.temp)
abline (h = seq(from = -50, to = 50, by = 1), col = "lightgrey")

ggplot2::ggplot (data = ydata, aes (x = year, y = Ymean.ave.temp.C)) + geom_line() +
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.ave.year.temp)) + xlab (txt.time.years) + ylab (txt.temp) + 
  theme_light ()

# monthly analysis

mdata <- subset (tmp.month, subset = month < "2022-01-01", select = c (month, Mmean.ave.temp.C))
plot (Mmean.ave.temp.C ~ month, data = mdata,
      ylim = range (Mmean.ave.temp.C, na.rm = FALSE), type = "l",
      main = paste (Location, txt.ave.month.temp), xlab = txt.time.months, ylab = txt.temp)
abline (h = seq(from = -20, to = 30, by = 5), col = "lightgrey")

ggplot2::ggplot (data = mdata, aes (x = month, y = Mmean.ave.temp.C)) + geom_line() + 
  geom_smooth (method = "loess", formula = y ~ x) + 
  ggtitle (paste (Location, txt.ave.month.temp)) + xlab (txt.time.months) + ylab (txt.temp) + theme_light ()

# yearly boxplots
data <- subset (tmp.month, subset = month < "2022-01-01",
                select = c (month, Mmean.ave.temp.C))

boxplot (Mmean.ave.temp.C ~ format (month, "%Y"), data = data,
         main = paste (Location, txt.rang.month.temp.year),
         xlab = txt.time.months, ylab = txt.temp)
abline (h = seq(from = -50, to = 50, by = 5), col = "lightgrey")


month.breaks.20 <- as.Date (c ("1850-01-01", "1860-01-01", "1880-01-01", "1900-01-01", "1920-01-01",
                               "1940-01-01", "1960-01-01", "1980-01-01", "2000-01-01", "2020-01-01",
                               "2040-01-01"))
month.labels.20 <-  c ("1850-1859", "1860-1879", "1880-1899", "1900-1919", "1920-1939", "1940-1959",
                       "1960-1979", "1980-1999", "2000-2019", "2020-2021")
int.years <- cut (data$month,
                  breaks = month.breaks.20,
                  labels = month.labels.20)
boxplot (data$Mmean.ave.temp.C ~ int.years, varwidth = TRUE,
         main = paste (Location, txt.rang.month.temp.range),
         xlab = txt.range.years, ylab = txt.temp)
abline (h = seq(from = -50, to = 50, by = 5), col = "lightgrey")



month.breaks.25 <- as.Date (c ("1850-01-01", "1875-01-01", "1900-01-01",
                               "1925-01-01", "1950-01-01", "1975-01-01", "2000-01-01", "2025-01-01"))
month.labels.25 <-  c ("1851-1874", "1875-1899", "1900-1924", "1925-1949", "1950-1974", "1975-1999", "2000-2021")
int.years <- cut (data$month,
                  breaks = month.breaks.25,
                  labels = month.labels.25)
boxplot (data$Mmean.ave.temp.C ~ int.years, varwidth = TRUE,
         main = paste (Location, txt.rang.month.temp.range),
         xlab = txt.range.years, ylab = txt.temp)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgray")


month.breaks.50 <- as.Date (c ("1850-01-01", "1900-01-01",
                               "1950-01-01", "2000-01-01", "2025-01-01"))
month.labels.50 <-  c ("1851-1899", "1900-1949", "1950-1999", "2000-2021")
int.years <- cut (data$month,
                    breaks = month.breaks.50,
                    labels = month.labels.50)
boxplot (data$Mmean.ave.temp.C ~ int.years, varwidth = TRUE,
         main = paste (Location, txt.rang.month.temp.range),
         xlab = txt.range.years, ylab = txt.temp)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgray")


month.breaks.split <- as.Date (c ("1850-01-01", "1975-01-01", "2025-01-01"))
month.labels.split <-  c ("1851-1974", "1975-2021")
int.years <- cut (data$month,
                  breaks = month.breaks.split,
                  labels = month.labels.split)
boxplot (data$Mmean.ave.temp.C ~ int.years, varwidth = TRUE,
         main = paste (Location, txt.rang.month.temp.range),
         xlab = txt.range.years, ylab = txt.temp)
abline (h = seq (from = -50, to = 50, by = 5), col = "lightgray")


# top warm and cold months

print (data.frame (month = format (tmp.month$month, "%Y-%m"), temp = tmp.month$Mmean.ave.temp.C) [order (tmp.month$Mmean.ave.temp.C), ] [1:20,])
print (data.frame (month = format (tmp.month$month, "%Y-%m"), temp = tmp.month$Mmean.ave.temp.C) [order (-tmp.month$Mmean.ave.temp.C), ] [1:20,])

# analysis as time series


# averages
ts.Mmean.ave.temp.C <- ts (data = tmp.month$Mmean.ave.temp.C, start = y.loc.start, frequency = 12)
plot (ts.Mmean.ave.temp.C, main = paste (Location, txt.ave.month.ave.day),
      xlab = txt.time.months, ylab = txt.temp)

adc.ts.Mmean.ave.temp.C <- decompose (ts.Mmean.ave.temp.C, type = "additive")
plot (adc.ts.Mmean.ave.temp.C)

mdc.ts.Mmean.ave.temp.C <- decompose (ts.Mmean.ave.temp.C, type = "multiplicative")
plot (mdc.ts.Mmean.ave.temp.C)

# season plot
forecast::ggseasonplot (ts.Mmean.ave.temp.C, polar = TRUE)
forecast::ggseasonplot (ts.Mmean.ave.temp.C, polar = FALSE)

# forecasting two years ahead
fct.Mmean.ave.temp.C <- forecast::forecast (ts.Mmean.ave.temp.C)
forecast::autoplot (fct.Mmean.ave.temp.C)
forecast::autoplot (fct.Mmean.ave.temp.C, include = 3*12)
