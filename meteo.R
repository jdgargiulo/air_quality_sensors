## Meteorología##

library(tidyverse)
library(openair)

meteo_1_al_7_09 <- read_csv("~/Documentos/air_quality_sensors/meteo/meteo_1_al_7_09.csv")
meteo_8_al_14_09 <- read_csv("~/Documentos/air_quality_sensors/meteo/meteo_8_al_14_09.csv")
meteo_15_al_21_09 <- read_csv("~/Documentos/air_quality_sensors/meteo/meteo_15_al_21_09.csv")
meteo_22_al_28_09 <- read_csv("~/Documentos/air_quality_sensors/meteo/meteo_22_al_28_09.csv")

meteo_SEP_23<-rbind(meteo_1_al_7_09, meteo_8_al_14_09, meteo_15_al_21_09, meteo_22_al_28_09)

meteo_SEP_23<-meteo_SEP_23 %>% filter(Medición=="Temperatura del aire"|
                                      Medición=="Humedad del aire"  |
                                      Medición=="Velocidad del viento"|
                                        Medición=="Dirección del viento") %>% 
  select(Timestamp, Valor, Medición)

str(meteo_SEP_23)


?openair

## reorganizo la tabla
meteo_SEP_23<-meteo_SEP_23 %>% spread(Medición, Valor)


meteo_SEP_23<- meteo_SEP_23 %>% rename(date="Timestamp",
                        wd="Dirección del viento",
                        Hum="Humedad del aire",
                        Temp="Temperatura del aire",
                        ws="Velocidad del viento")


str(meteo_SEP_23)

## Convierto a formato date la columna fecha
meteo_SEP_23$date<-ymd_hms(meteo_SEP_23$date)

## Convierto a datos del tipo numéricos
# meteo_SEP_23$wd<-as.numeric(meteo_SEP_23$wd)
# meteo_SEP_23$Hum<-as.numeric(meteo_SEP_23$Hum)
# meteo_SEP_23$Temp<-as.numeric(meteo_SEP_23$Temp)
# meteo_SEP_23$ws<-as.numeric(meteo_SEP_23$ws)


summary(meteo_SEP_23)


## Correción de valores anómalos
which(meteo_SEP_23$wd==999.0)
which(meteo_SEP_23$Hum==-99.999)


meteo_SEP_23[550,"wd"]<-190
meteo_SEP_23[3888,"Hum"]<-99.9

## Gráficos

meteo_SEP_23 %>% ggplot(aes(date,Hum))+
  geom_line()

meteo_SEP_23 %>% ggplot(aes(date,wd))+
  geom_line()
meteo_SEP_23 %>% ggplot(aes(date,Temp))+
  geom_line()

windRose(meteo_SEP_23, type = "month", layout=c(1,1))

par(mfrow=c(1,2))
windRose(tandil_met, type = "month")
windRose(filter(tandil_met,date > "2023-09-01"&date<"2023-09-30"),
         type = "month")


library(worldmet)
getMeta(lat = -37.3, lon = -59.13, returnMap = TRUE)



tandil_met<-importNOAA(code="876450-99999", year = 2023)
tandil_met

met_rel<-left_join(b, tandil_met, by="date")


plot(met_rel$Hum, met_rel$RH)
plot(met_rel$ws.x, met_rel$ws.y)

str(met_rel)
cor(met_rel$Hum, met_rel$RH, use = "complete.obs")


summary(tandil_met$precip)
