library(dplyr)
library(readr)
library(tidyverse)

dados <- read.csv("dados1.csv",header = TRUE, sep = ";", dec = ",")

dados$Coluna1 <- NULL

dados$Data <- as.Date(dados$Data, "%Y-%m-%d")

temperatura <- select(dados, Temp)

tempAlta <- filter(temperatura, Temp>26)

mean(temperatura$Temp, na.rm=TRUE)

min(temperatura$Temp, na.rm=TRUE)

max(temperatura$Temp, na.rm=TRUE)

resumo <- summary(temperatura$Temp)[]

range(temperatura$Temp, na.rm=TRUE)

dados$Dia <- as.numeric(format(dados$Data, format="%d"))
dados$Mes <- as.numeric(format(dados$Data, format="%m"))
dados$Ano <- as.numeric(format(dados$Data, format="%Y"))

anos <- dados %>% 
  group_by(Ano) %>% summarise(Temp = 
                               mean(Temp, na.rm=TRUE))

ano2016 <- dados %>% filter(Ano==2016)

ano2016_2017 <- dados %>% filter(Ano==2016|Ano==2017)

head(anos+1)

anos <- dados %>% filter(Ano==2020) %>%
      group_by(Mes) %>% summarise(Temp = 
                                  mean(Temp, na.rm=TRUE))

plot(dados$Data, dados$Temp, main="Temp. de Rio Grande", xlab = "Anos", ylab = "Temp. (C)")

boxplot(dados$Temp)

save.image("Aula01R.RData")

#rm(temperatura)

#head(dados)

#tail(dados)

#summary(dados)

#str(dados)

#View(dados)