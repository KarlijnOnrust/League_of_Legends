#libraries
library(rvest)
library(dplyr)
library(openxlsx)

#get data
r <-
  read_html("http://leagueoflegends.wikia.com/wiki/List_of_champions/Base_statistics")
Data <-
  r %>% html_node("table.wikitable") %>% html_table(fill = TRUE)
#clean data
c <- data.frame(Data$Champions)
c <- data.frame(word(c$Data.Champions, 1, sep = " "))
Data$`AS+` = substr(Data$`AS+`, 2, nchar(Data$`AS+`) - 1)
Data$`AS+` = as.numeric(Data$`AS+`) / 100
Data <- data.frame(data.matrix(Data))
Data$Champions <- c$word.c.Data.Champions..1..sep.......

baselvl1 <-
  data.frame(
    Data$Champions,
    Data$HP,
    Data$HP.,
    Data$HP5,
    Data$HP5.,
    Data$MP,
    Data$MP.,
    Data$MP5,
    Data$MP5.,
    Data$AD,
    Data$AD.,
    Data$AS,
    Data$AS.,
    Data$AR,
    Data$AR.,
    Data$MR,
    Data$MR.,
    Data$MS,
    Data$Range
  )
colnames(baselvl1) <-
  c(
    "Champions",
    "HP",
    "HP per lvl",
    "HP5",
    "HP5 per lvl",
    "MP",
    "MP per lvl",
    "MP5",
    "MP5 per lvl",
    "AD",
    "AD per lvl",
    "AS",
    "AS per lvl",
    "AR",
    "AR per lvl",
    "MR",
    "MR per lvl",
    "MS",
    "Range"
  )

baselvl18 <-
  data.frame(
    Data$Champions,
    Data$HP + Data$HP. * 18,
    Data$HP5 + Data$HP5. * 18,
    Data$MP + Data$MP. * 18,
    Data$MP5 + Data$MP5. * 18,
    Data$AD + Data$AD. * 18,
    Data$AS * (+Data$AS. * 18),
    Data$AR + Data$AR. * 18,
    Data$MR + Data$MR. * 18,
    Data$MS,
    Data$Range
  )
colnames(baselvl18) <-
  c("Champions",
    "HP",
    "HP5",
    "MP",
    "MP5",
    "AD",
    "AS",
    "AR",
    "MR",
    "MS",
    "Range")

#to excel
League_stats <- createWorkbook()
addWorksheet(League_stats, "lvl 1")
addWorksheet(League_stats, "lvl 18")
writeDataTable(League_stats, "lvl 1", x = baselvl1)
writeDataTable(League_stats, "lvl 18", x = baselvl18)
saveWorkbook(League_stats, "League_stats.xlsx", overwrite = TRUE)


