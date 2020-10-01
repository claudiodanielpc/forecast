#Modelo de producción de vivienda


##Se carga la paquetería necesaria
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, forecast,
               lubridate, scales,ggfortify)

##Se carga el archivo con los datos de producción
datos<-read.csv("C:/Users/ALIENWARE/Downloads/infonavit.csv",
              encoding="latin",header=TRUE,check.names=FALSE)

#Estimar componentes----
###Se comenzará por obtener los modelos de cada componente del modelo de producción 
#para posteriormente, estimar los valores a futuro


infonavit<-datos%>%
  select(infonavit)%>%
  ##Se transforma a serie de tiempo
  ts(.,start=c(2015,1),frequency=12)


#Modelo
modinfonavit = tbats(infonavit)
#Forecast
modinfonavitf= forecast(modinfonavit, h=17)
###Generar dataframe de las estimaciones
infonavitf<-fortify(modinfonavitf)
x1<-infonavitf%>%
  filter(Index>="2020-08-01")%>%
  select(`Point Forecast`)%>%
  rename(infonavit=1)
  

####
#Fovissste
fovissste<-datos%>%
  select(fovissste)%>%
  ##Se transforma a serie de tiempo
  ts(.,start=c(2015,1),frequency=12)

fovissste<-fovissste%>%
  ##Limpiar outliers
  tsclean()

##Modelo
modfovissste=auto.arima(fovissste)

#Forecast
modfovissstef=forecast(modfovissste,h=17)
###Generar dataframe de las estimaciones
fovissstef<-fortify(modfovissstef)
x2<-fovissstef%>%
  filter(Index>="2020-08-01")%>%
  select(`Point Forecast`)%>%
  rename(fovissste=1)



###Banca
banca<-datos%>%
  select(banca)%>%
  ##Se transforma a serie de tiempo
  ts(.,start=c(2015,1),frequency=12)

modbanca = tbats(banca)
modbancaf = forecast(modbanca, h=17)
###Generar dataframe de las estimaciones
bancaf<-fortify(modbancaf)
x3<-bancaf%>%
  filter(Index>="2020-08-01")%>%
  select(`Point Forecast`)%>%
  rename(banca=1)



###Subsidio
subsidio<-datos%>%
  select(subsidio)%>%
  ##Se transforma a serie de tiempo
  ts(.,start=c(2015,1),frequency=12)
modsubsidio = auto.arima(subsidio)
modsubsidiof=forecast(modsubsidio,h=17)
###Generar dataframe de las estimaciones
subsidiof<-fortify(modsubsidiof)
x4<-subsidiof%>%
  filter(Index>="2020-08-01")%>%
  select(`Point Forecast`)%>%
  rename(subsidio=1)%>%
  mutate(subsidio=1)



###Tasa
tasa<-datos%>%
  select(tasa)%>%
  ##Se transforma a serie de tiempo
  ts(.,start=c(2015,1),frequency=12)
modtasa = auto.arima(tasa)
modtasaf=forecast(modtasa,h=17)
tasaf<-fortify(modtasaf)
x5<-tasaf%>%
  filter(Index>="2020-08-01")%>%
  select(`Point Forecast`)%>%
  rename(tasa=1)



###Construcción del modelo de estimación de producción----

##Covariables para modelo
xreg<-datos%>%
  select(infonavit,banca,subsidio)%>%
  as.matrix()

##Covariables para estimación
xregf<-cbind(x1,x3,x4)%>%
  as.matrix()

training<-datos%>%
  ts(.,start=c(2015,1),frequency=12)

regprod<-lm(prod~infonavit+banca+subsidio,data=training)

##Traer los regresores
regresor<-cbind(x1,x3,x4)%>%
  ts(.,start=c(2020,8),frequency=12)

##Predicción
regprodf<-predict(regprod,newdata = regresor)%>%
  as.data.frame()%>%
  rename(prod=1)%>%
  mutate(tipo="Estimado")


##Crear la base de datos
baseprod<-fortify(regprod)%>%
  select(prod)%>%
  mutate(tipo="Observado")

prodvivi<-rbind(baseprod,regprodf)%>%
  mutate(fecha=seq(as.Date("2015/1/1"),as.Date("2021/12/01"),
                   by = "month"))





modprod<-auto.arima(training[,"prod"],xreg=xreg)
modprod
autoplot(modprod)

modprodf<-forecast(modprod,xreg=xregf)%>%
  fortify()


#Modelo de créditos individuales----
###Se cargan los datos de crédito
creddatos<-read.csv("C:/Users/ALIENWARE/Downloads/cred.csv",
                    encoding="latin",header=TRUE,check.names=FALSE)

  
  xregcred<-creddatos%>%
  select(infonavit,fovissste,banca,subsidio,tasa)%>%
  as.matrix()

    training<-creddatos%>%
      select(credcofin)%>%
    ts(.,start=c(2015,1),frequency=12)
    dec<-decompose(training, "additive")
    
    h<-auto.arima(training)
    fh<-forecast(h,17)%>%
      fortify()
    write.csv(fh,"fh.csv")
  
##Modelo
reg<-lm(credcofin~infonavit+fovissste+banca+tasa+subsidio,data=training)
reg
summary(reg)

##Forecast
###Traer los datos a futuro de los regresores
regresor<-cbind(x1,x2,x3,x4,x5)%>%
  ts(.,start=c(2020,8),frequency=12)

##Predicción
regf<-predict(reg,newdata = regresor)%>%
  as.data.frame()%>%
  rename(credcofin=1)%>%
  mutate(tipo="Estimado")

##Crear la base de datos
basereg<-fortify(reg)%>%
  select(credcofin)%>%
  mutate(tipo="Observado")

credvivi<-rbind(basereg,regf)%>%
  mutate(fecha=seq(as.Date("2015/1/1"),as.Date("2021/12/01"),
                   by = "month"))




