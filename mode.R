

##Se cargan las librerías necesarias


if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, lubridate, scales,forecast)


pib<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/forecast/master/pibvar.csv",
             encoding="latin",header=TRUE,check.names=FALSE)%>%
  mutate(trim=seq(ymd("1994/1/1"), ymd("2020/12/30"), by = "quarter"))%>%
  mutate(trim=quarter(trim,with_year=T))

##Se crea el dataset para el modelo
pibmod<-pib%>%
  filter(trim<="2020.1")%>%
  ts(.,start=c(1994,1),frequency=4)

##Se establecen el vector de regresores
xreg<-pib%>%
  filter(trim>"2020.1")%>%
  ts(.,start=c(2020,2),frequency=4)



##Se crea el modelo

modelo<-auto.arima(pibmod[,"const"],
                   xreg=pibmod[,"pib"])

#Pronóstico

fcast<-forecast(modelo,xreg=xreg[,"pib"])%>%
  as.data.frame()%>%
  rename(const=1)%>%
  select(const)%>%
  mutate(tipo="Estimado")%>%
  mutate(trim=seq(ymd("2020/4/1"), ymd("2020/12/30"), by = "quarter"))%>%
  mutate(trim=quarter(trim,with_year=T))

##Crear la dataframe

datos<-pibmod%>%
  as.data.frame()%>%
  select(const,trim)%>%
  mutate(tipo="Observado")

datos<-rbind(datos,fcast)


datos%>%
  ggplot(., aes(trim, const)) +
  geom_line(aes(color = tipo, linetype = tipo),size=3) 

  
  