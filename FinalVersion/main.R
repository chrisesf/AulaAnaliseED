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

dados2$mes <- as.numeric(format(dados2$Data, format = "%m"))

precMeses <- dados %>% 
  group_by(mes) %>%
  summarise(
    Media = mean(PrecTotal, na.rm = TRUE),
    Maxima = max(PrecTotal, na.rm = TRUE),
    Minima = min(PrecTotal, na.rm = TRUE),
    Mediana = median(PrecTotal, na.rm = TRUE),
    q1 = quantile(PrecTotal, 0.25, na.rm = TRUE),
    q3 = quantile(PrecTotal, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "mm")

# Temperatura
tempMeses <- dados %>% 
  group_by(mes) %>%
  summarise(
    Media = mean(TempMedia, na.rm = TRUE),
    Maxima = max(TempMedia, na.rm = TRUE),
    Minima = min(TempMedia, na.rm = TRUE),
    Mediana = median(TempMedia, na.rm = TRUE),
    q1 = quantile(TempMedia, 0.25, na.rm = TRUE),
    q3 = quantile(TempMedia, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "°C")

# Vento
ventoMeses <- dados %>% 
  group_by(mes) %>%
  summarise(
    Medio = mean(VentoMedio, na.rm = TRUE),
    Maximo = max(VentoMax, na.rm = TRUE),
    Rajada = max(VentoMax, na.rm = TRUE),
    Minimo = min(VentoMax, na.rm = TRUE),
    Mediana = median(VentoMedio, na.rm = TRUE),
    q1 = quantile(VentoMedio, 0.25, na.rm = TRUE),
    q3 = quantile(VentoMedio, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "m/s")

# Umidade
umiMeses <- dados %>% 
  group_by(mes) %>%
  summarise(
    Media = mean(Umidade, na.rm = TRUE),
    Maxima = max(Umidade, na.rm = TRUE),
    Minima = min(Umidade, na.rm = TRUE),
    Mediana = median(Umidade, na.rm = TRUE),
    q1 = quantile(Umidade, 0.25, na.rm = TRUE),
    q3 = quantile(Umidade, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "%")


# Dados caçador

precMeses2 <- dados2 %>% 
  group_by(mes) %>%
  summarise(
    Media = mean(PrecTotal, na.rm = TRUE),
    Maxima = max(PrecTotal, na.rm = TRUE),
    Minima = min(PrecTotal, na.rm = TRUE),
    Mediana = median(PrecTotal, na.rm = TRUE),
    q1 = quantile(PrecTotal, 0.25, na.rm = TRUE),
    q3 = quantile(PrecTotal, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "mm")

tempMeses2 <- dados2 %>% 
  group_by(mes) %>%
  summarise(
    Media = mean(TempMedia, na.rm = TRUE),
    Maxima = max(TempMedia, na.rm = TRUE),
    Minima = min(TempMedia, na.rm = TRUE),
    Mediana = median(TempMedia, na.rm = TRUE),
    q1 = quantile(TempMedia, 0.25, na.rm = TRUE),
    q3 = quantile(TempMedia, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "°C")

ventoMeses2 <- dados2 %>% 
  group_by(mes) %>%
  summarise(
    Medio = mean(VentoMedio, na.rm = TRUE),
    Maximo = max(VentoMax, na.rm = TRUE),
    Rajada = max(VentoMax, na.rm = TRUE),
    Minimo = min(VentoMax, na.rm = TRUE),
    Mediana = median(VentoMedio, na.rm = TRUE),
    q1 = quantile(VentoMedio, 0.25, na.rm = TRUE),
    q3 = quantile(VentoMedio, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "m/s")

umiMeses2 <- dados2 %>% 
  group_by(mes) %>%
  summarise(
    Media = mean(Umidade, na.rm = TRUE),
    Maxima = max(Umidade, na.rm = TRUE),
    Minima = min(Umidade, na.rm = TRUE),
    Mediana = median(Umidade, na.rm = TRUE),
    q1 = quantile(Umidade, 0.25, na.rm = TRUE),
    q3 = quantile(Umidade, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Unidade = "%")

# Temperatura (C)
floripa_temp <- tempMeses$Media
cacador_temp <- tempMeses2$Media

plot(floripa_temp, main = "Média da Temperatura Mensal das Cidades", col.main = "black", 
     type = "b", xlab = "Mês", ylab = "Temperatura (°C)", lwd = 5, col = "red", xaxt = "n", ylim = range(c(floripa_temp, cacador_temp)))

axis(1, at = 1:12, labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
lines(cacador_temp, type = "b", col = "blue", lwd = 5)

legend("topright", legend = c("Florianópolis", "Caçador"), col = c("red", "blue"), lty = 1, lwd = 4)

# Precipitação (mm)
floripa_prec <- precMeses$Media
cacador_prec <- precMeses2$Media

plot(floripa_prec, main = "Média da Precipitação Mensal das Cidades", col.main = "black", 
     type = "b", xlab = "Mês", ylab = "Precipitação (mm)", lwd = 5, col = "red", xaxt = "n", ylim = range(c(floripa_prec, cacador_prec)))

axis(1, at = 1:12, labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
lines(cacador_prec, type = "b", col = "blue", lwd = 5)

legend("topright", legend = c("Florianópolis", "Caçador"), col = c("red", "blue"), lty = 1, lwd = 4)

# Vento médio (m/s)
floripa_vento <- ventoMeses$Medio
cacador_vento <- ventoMeses2$Medio

plot(floripa_vento, main = "Média da Velocidade do Vento Mensal das Cidades", col.main = "black", 
     type = "b", xlab = "Mês", ylab = "Vento (m/s)", lwd = 5, col = "red", xaxt = "n", ylim = range(c(floripa_vento, cacador_vento)))

axis(1, at = 1:12, labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
lines(cacador_vento, type = "b", col = "blue", lwd = 5)

legend("topright", legend = c("Florianópolis", "Caçador"), col = c("red", "blue"), lty = 1, lwd = 4)

# Umidade relativa (%)
floripa_umidade <- umiMeses$Media
cacador_umidade <- umiMeses2$Media

plot(floripa_umidade, main = "Média da Umidade Mensal das Cidades", col.main = "black", 
     type = "b", xlab = "Mês", ylab = "Umidade (%)", lwd = 5, col = "red", xaxt = "n", ylim = range(c(floripa_umidade, cacador_umidade)))

axis(1, at = 1:12, labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
lines(cacador_umidade, type = "b", col = "blue", lwd = 5)

legend("topright", legend = c("Florianópolis", "Caçador"), col = c("red", "blue"), lty = 1, lwd = 4)


# extra
dados2022<-dados %>% filter(ano=="2022")
dadosresto<-dados%>% filter(ano!="2022")

dados %>% 
  ggplot(aes(y = factor(-mes, labels = c("Dez", "Nov", "Out", "Set", "Ago", "Jul", 
                                         "Jun", "Mai", "Abr", "Mar", "Fev", "Jan")), 
             x = PrecTotal)) +
  geom_point(aes(color = '2013'), size = 3, alpha = .2) +
  geom_point(data = dados2022, aes(x = PrecTotal, color = '2022'), alpha = .3) +
  scale_color_manual(
    name = 'Anos',
    values = c('2013' = 'red', '2022' = 'blue'),
    labels = c('2013-2024', '2022')
  ) +
  labs(
    title = "Comparação da Precipitação Média em 2022\ncom demais anos de Florianópolis",
    x = "\nPrecipitação Média (mm)", y = "Meses\n"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 16)
  )




