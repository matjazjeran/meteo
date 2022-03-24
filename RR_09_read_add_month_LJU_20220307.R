################################################################################
# Branje mesecnih povprecij povprecnih dnevnih temperatur za Ljubljano dobljenih
# od Lucke Kajfez Bogataj in shranjevanje v formatu za nadaljne obdelave
# casovnih vrst
################################################################################
# Reading monthly averages of daily average temperature for Ljubljana obtained
# from Lucka Kajfez Bogataj and saving data in format for further time analysis
################################################################################
# Input: ./Data/Ljubljana/LJ T mesecne.xlsx
# Output: ./Data/LJU_add_month_stat.RData : add.month.stat
#
# Matjaz Jeran
# 07.03.2022
################################################################################

rm (list = ls (all = TRUE))

getwd ()

# if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/LJU_temperature/")
# if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Dokumenti/UTZO/Zgodovina/")

library (RcmdrMisc)
library (reshape2)


tmp_wide <- RcmdrMisc::readXL ("./Data/Ljubljana/LJ T mesecne.xlsx", rownames = FALSE, header = TRUE, na = "",
sheet = "List1", stringsAsFactors = FALSE)

colnames (tmp_wide) <- c ("year", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# convert to long form compatible with the 

tmp_long <- reshape2::melt (tmp_wide, id.vars = "year", measure.vars = colnames (tmp_wide) [2:13])

tmp <- data.frame (
  month.str = paste0 (tmp_long$year, "-", tmp_long$variable, "-01"),
    month = NA,
  Mmean.ave.temp.C = tmp_long$value)

tmp$month <- as.Date (tmp$month.str)

add.month.stat <- tmp [order (tmp$month.str),]

save (add.month.stat, file = "./Data/LJU_add_month_stat.RData")
