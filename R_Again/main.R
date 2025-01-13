library(tidyverse)
library(dplyr)
library(plotly)

dados <- read.csv("dadosFloripa.csv", header = TRUE, skip = 9, sep = ";", dec = ".", check.names = FALSE)
dados2 <- read.csv("dadosCacador.csv", header = TRUE, skip = 9, sep = ";", dec = ".", check.names = FALSE)

dados[9] <- NULL
dados2[9] <- NULL

names(dados) <- c("Data", "PrecTotal", "TempMax", "TempMedia", "TempMin", "Umidade", "VentoMax", "VentoMedio")
names(dados2) <- c("Data", "PrecTotal", "TempMax", "TempMedia", "TempMin", "Umidade", "VentoMax", "VentoMedio")

dados <- dados %>%
  mutate(
    Data = as.Date(Data),
    PrecTotal = as.numeric(PrecTotal),
    TempMax = as.numeric(TempMax),
    TempMedia = as.numeric(TempMedia),
    TempMin = as.numeric(TempMin),
    Umidade = as.numeric(Umidade),
    VentoMax = as.numeric(VentoMax),
    VentoMedio = as.numeric(VentoMedio)
  )

dados2 <- dados2 %>%
  mutate(
    Data = as.Date(Data),
    PrecTotal = as.numeric(PrecTotal),
    TempMax = as.numeric(TempMax),
    TempMedia = as.numeric(TempMedia),
    TempMin = as.numeric(TempMin),
    Umidade = as.numeric(Umidade),
    VentoMax = as.numeric(VentoMax),
    VentoMedio = as.numeric(VentoMedio)
  )

dados$ano <- as.numeric(format(dados$Data, format = "%Y"))
dados$mes <- as.numeric(format(dados$Data, format = "%m"))

precMeses <- dados %>% filter(dados$ano == "2022") %>% group_by(mes) %>%
  summarise(PrecTotal = mean(PrecTotal, na.rm = TRUE))

tempMeses <- dados %>% filter(dados$ano == "2022") %>% group_by(mes) %>%
  summarise(TempMedia = mean(TempMedia, na.rm = TRUE))

umiMeses <- dados %>% filter(dados$ano == "2022") %>% group_by(mes) %>%
  summarise(Umidade = mean(Umidade, na.rm = TRUE))

ventoMeses <- dados %>% filter(dados$ano == "2022") %>% group_by(mes) %>%
  summarise(VentoMedio = mean(VentoMedio, na.rm = TRUE))

precAnos <- dados %>% group_by(ano) %>%
  summarise(PrecTotal = mean(PrecTotal, na.rm = TRUE))

ventoAnos <- dados %>% group_by(ano) %>%
    summarise(VentoMedio = mean(VentoMedio, na.rm = TRUE))

umidadeAnos <- dados %>% group_by(ano) %>%
  summarise(Umidade = mean(Umidade, na.rm = TRUE))

temperaturaAnos <- dados %>% group_by(ano) %>%
  summarise(TempMedia = mean(TempMedia, na.rm = TRUE))

plotPrecipitacao <- ggplot(data=precMeses, aes(x = precMeses$mes, y = precMeses$PrecTotal))+
  geom_line() +
  geom_point(aes(color=precMeses$PrecTotal, scale_shape_binned=precMeses$PrecTotal)) +
  xlab("Meses") +  ylab("Precipitação Média") +
  ggtitle("Precipitação média por mês")

plotTemperatura <- ggplot(data=tempMeses, aes(x = tempMeses$mes, y = tempMeses$TempMedia))+
  geom_point(aes(color=tempMeses$TempMedia, scale_shape_binned=tempMeses$TempMedia)) +
  xlab("Meses") +  ylab("Temperatura Médias") +
  ggtitle("Temperatura média por mês")

plotVento <- ggplot(data=ventoMeses, aes(x = ventoMeses$mes, y = ventoMeses$VentoMedio))+
  geom_point(aes(color=ventoMeses$VentoMedio, scale_shape_binned=ventoMeses$VentoMedio)) +
  xlab("Meses") +  ylab("Velocidades Médias do Vento") +
  ggtitle("Velocidade Médias do Vento por mês")

plotUmidade <- ggplot(data=umiMeses, aes(x = umiMeses$mes, y = umiMeses$Umidade))+
  geom_point(aes(color=umiMeses$Umidade, scale_shape_binned=umiMeses$Umidade)) +
  xlab("Meses") +  ylab("Umidade Média") +
  ggtitle("Umidade média por ano")

ggplotly(plotPrecipitacao)


