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

precMeses <- dados %>% group_by(mes) %>%
  summarise(PrecTotal = mean(PrecTotal, na.rm = TRUE))

tempMeses <- dados %>% group_by(mes) %>%
  summarise(TempMedia = mean(TempMedia, na.rm = TRUE))

tempMeses2 <- dados2 %>% group_by(mes) %>%
  summarise(TempMedia = mean(TempMedia, na.rm = TRUE))

umiMeses <- dados %>% group_by(mes) %>%
  summarise(Umidade = mean(Umidade, na.rm = TRUE))

ventoMeses <- dados %>% group_by(mes) %>%
  summarise(VentoMedio = mean(VentoMedio, na.rm = TRUE))

# grafico temp

floripa <- tempMeses$TempMedia
cacador <- tempMeses2$TempMedia

plot(floripa, main="Média da temperatura mensal das cidades", col.main="red", 
     type="b", xlab="Mês", ylab="Temperatura (°C)", lwd=5, col="red", xaxt="n", ylim=range(c(floripa, cacador)))

axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11,12), labels= c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))

lines(cacador, type="b", col="blue", lwd=5)

legend("topright", legend = c("Florianópolis", "Caçador"), col = c("red", "blue"), lty = 1, lwd = 4)

# grafico prec

dados %>% ggplot(aes(month.abb[Mes],Prec)) +
  scale_x_discrete(limits = month.abb) +
  scale_fill_brewer(palette="Set1") +
  hrbrthemes::theme_ipsum_rc(grid="X") +
  geom_violin(alpha = 0.5, )  +
  theme(legend.position = "none") +
  labs(
    x = "", y = "Volume de chuvas", fill = NULL,
    title = "Chuvas por mês",
    caption = ""
  )

# extra

dados2022<-dados %>% filter(ano=="2022")
dadosresto<-dados%>% filter(ano!="2022")

dados %>% 
  ggplot(aes(y=reorder(-mes,-PrecTotal), PrecTotal)) + # ggplot(aes(y=reorder(Mes,-Temp), Temp)) +
  geom_point(aes(color='2013'), size=3, alpha=.2 ) +
  geom_point(data=dados2022, aes(x=PrecTotal, color='2022'),  alpha=.3) +
  scale_color_manual(name='Anos',
                     values=c('2013'='red','2022'='blue'),
                     labels=c('2013-2024','2022')) +
  labs(title="Comparação da Precpitação Média em 2015\ncom demais anos de Rio Grande", 
       x="\nTemperatura Média (°C)", y="Meses\n") +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        text=element_text(size=16))


