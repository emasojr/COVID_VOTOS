#Carregando Pacotes
library(ggplot2)
library(dplyr)

#Importando dados
dados=read.csv2("C:\\Users\\evani\\Documents\\Pastas\\COVID.csv")
votos=read.csv2("C:\\Users\\evani\\Documents\\Pastas\\cev.csv")
votos=votos[,c(1,3)]
dados=dados[is.na(dados$codRegiaoSaude)==FALSE,c(-15:-17)]
colnames(dados)=c("Região","Estado","Município","CodUF","CodMun",
                  "Cod_R_Saude","Nom_Saude","Data","SemanaEPI",
                  "População","Casos_Acum","Casos","Obitos_Acum","Obitos")
dados$Obtos_100khabit=(dados$Obitos_Acum/dados$População)*100000
dados_recentes=dados[dados$Data==max(dados$Data),]
colnames(votos)=c("CodMun","Votos")
CEV=merge(dados_recentes,votos,by="CodMun")

#Visualizando outliers
boxplot(CEV$Obtos_100khabit)
boxplot(CEV$Votos)

#Removendo Outliers
outliers=boxplot(CEV$Obtos_100khabit, plot=FALSE)$out
CEV=CEV[-which(CEV$Obtos_100khabit %in% outliers),]
boxplot(CEV$Obtos_100khabit)


#Correlação entre votos em Bolsonaro no 2º turno e morte por Covid-19
cor(CEV$Obtos_100khabit,CEV$Votos)
summary(lm(CEV$Obtos_100khabit~CEV$Votos))

#Plotando Gráficos
ggplot(CEV) +
  aes(x = Votos, y = Obtos_100khabit) +
  geom_point(size = 1.8, colour = "#fa9e3b") +
  geom_smooth(method="lm",span = 0.5) +
  labs(x = "Proporção de Votos em Bolsonaro", y = "Óbitos por Covid-19 (por 100k de habitantes)", title = "Resultado das Eleições de 2018 e Óbitos por Covid-19 no Brasil", subtitle = "Por Município") +
  theme_minimal()

ggplot(CEV) +
  aes(x = Votos, y = Obtos_100khabit, colour = Região) +
  geom_point(size = 1.8) +
  scale_color_hue() +
  labs(x = "Proporção de Votos em Bolsonaro", y = "Óbitos por Covid-19 (por 100k de habitantes)", title = "Resultado das Eleições de 2018 e Óbitos por Covid-19 no Brasil", subtitle = "Por Município") +
  theme_minimal()

CEV %>%
  filter(Região %in% "Nordeste") %>%
  ggplot() +
  aes(x = Votos, y = Obtos_100khabit) +
  geom_point(size = 1.8, colour = "#fa9e3b") +
  labs(x = "Proporção de Votos em Bolsonaro", y = "Óbitos por Covid-19 (por 100k de habitantes)", title = "Resultado das Eleições de 2018 e Óbitos por Covid-19 no Nordeste", subtitle = "Por Município") +
  theme_minimal()

CEV %>%
  filter(Região %in% "Nordeste") %>%
  ggplot() +
  aes(x = Votos, y = Obtos_100khabit, colour = Estado) +
  geom_point(size = 1.8) +
  scale_color_hue() +
  labs(x = "Proporção de Votos em Bolsonaro", y = "Óbitos por Covid-19 (por 100k de habitantes)", title = "Resultado das Eleições de 2018 e Óbitos por Covid-19 no Nordeste", subtitle = "Por Município") +
  theme_minimal()

CEV %>%
  filter(Região %in% "Nordeste") %>%
  filter(Estado %in% "BA") %>%
  ggplot() +
  aes(x = Votos, y = Obtos_100khabit) +
  geom_point(size = 1.8, colour = "#fa9e3b") +
  geom_smooth(method="lm",span = 0.5) +
  labs(x = "Proporção de Votos em Bolsonaro", y = "Óbitos por Covid-19 (por 100k de habitantes)", title = "Resultado das Eleições de 2018 e Óbitos por Covid-19 na Bahia", subtitle = "Por Município") +
  theme_minimal()

CEV %>%
  filter(Região %in% c("Sul","Sudeste")) %>%
  ggplot() +
  aes(x = Votos, y = Obtos_100khabit) +
  geom_point(size = 1.8, colour = "#fa9e3b") +
  labs(x = "Proporção de Votos em Bolsonaro", y = "Óbitos por Covid-19 (por 100k de habitantes)", title = "Resultado das Eleições de 2018 e Óbitos por Covid-19 no Nordeste", subtitle = "Por Município") +
  theme_minimal()

