# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK

# lab 2

# загружаем пакеты
library("memisc") # две и более регрессий в одной табличке
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики
library("sgof")
library("ggplot2") # графики
library("foreign") # загрузка данных в разных форматах
library("car")
library("hexbin") # графики
library("rlms") # загрузка данных в формате rlms (spss)
library(devtools)
devtools::install_github("bdemeshev/rlms",force=TRUE)


help(rlms)

h <- rlms_read("r21i_os_42.sav")
saveRDS(h,"r21i_os_42.rds")
help(readxls)

library("memisc")
library("dplyr")
library("psych")
library("lmtest") 
library("sjPlot") 
library("sgof")
library("ggplot2") 
library("foreign") 
library("car")
library("hexbin") 
library("rlms") 
library(devtools)
devtools::install_github("bdemeshev/rlms")

