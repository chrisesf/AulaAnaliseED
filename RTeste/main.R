library(tidyverse)
library(dplyr)
library(plotly)

dados <- read.csv("dados_A806_D_2013-12-01_2024-11-30.csv", header = TRUE, skip = 9, sep = ";", dec = ".", check.names = FALSE)

dados[9] <- NULL

names(dados) <- c("Data", "PrecTotal", "TempMax", "TempMedia", "TempMin", "Umidade", "VentoMax", "VentoMedio")

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

temperatura <- dados$TempMedia

max(temperatura, na.rm = TRUE)
mean(temperatura, na.rm = TRUE)
min(temperatura, na.rm = TRUE)

dados$ano <- as.numeric(format(dados$Data, format = "%Y"))

anos <- dados %>% group_by(ano) %>%
  summarise(TempMedia = mean(TempMedia, na.rm = TRUE))

library(plotly) 

fig2 <- ggplot(data=dados, aes(x = dados$Data, y = dados$TempMedia))+
  geom_point(aes(color=dados$TempMedia, scale_shape_binned=dados$TempMedia)) +
  xlab("Anos") +  ylab("Temperatura Médias") +
  ggtitle("Temperatura média por ano")


ggplotly(fig2)



