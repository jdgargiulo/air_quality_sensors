## Datos de sensores

ID1_1_al_7_09 <- read_csv("~/Documentos/air_quality_sensors/sensors_data/ID1/ID1_1_al_7_09.csv")
ID1_7_al_14_09 <- read_csv("~/Documentos/air_quality_sensors/sensors_data/ID1/ID1_7_al_14_09.csv")
ID1_14_al_21_09 <- read_csv("~/Documentos/air_quality_sensors/sensors_data/ID1/ID1_14_al_21_09.csv")
ID1_21_al_28_09 <- read_csv("~/Documentos/air_quality_sensors/sensors_data/ID1/ID1_21_al_28_09.csv")



# Merge todos los set de datos
id1_sep<-rbind(ID1_1_al_7_09, ID1_7_al_14_09, ID1_14_al_21_09, ID1_21_al_28_09)
head(id1_sep)

## Elimino el nombre del sensor
id1_sep<-id1_sep %>% select(-`Sensor ID`)

summary(id1_sep)

# cambio el campo Fecha a formato date
id1_sep$`Fecha/Hora`<-dmy_hm(id1_sep$`Fecha/Hora`)

# Agrego 3 horas por diferencia horaria Mex/Arg
id1_sep$`Fecha/Hora`<-id1_sep$`Fecha/Hora`+10800



# Renombro el campo "Fecha/Hora" como "date"
id1_sep<-id1_sep %>% rename(date=`Fecha/Hora`)

str(id1_sep)

id1_sep %>% group_by(hour(date)) %>% 
  summarise(avg=mean(PM10),SD=sd(PM10) ,N=n()) %>% 
  print(n=28)



id1_PM10_hourly<-id1_sep %>% filter(PM10<200) %>%
  group_by(time=floor_date(date, '1 hour')) %>% 
  summarise(avg=mean(PM10)) %>% 
  print(n=100) 


id1_PM10_hourly %>% 
ggplot(aes(time, avg))+
  geom_line()+geom_point(col="red")

id1_sep_long<-gather(id1_sep, MP, valor, -date)

id1_sep_long %>% filter(valor<100) %>%
  group_by(time=floor_date(date, '1 hour')) %>% 
  ggplot(aes(date, valor, color=MP)) +
  geom_line()

str(id1_sep_long)  


cor(id1_sep$PM1, id1_sep$PM10)
cor(id1_sep$PM2, id1_sep$PM10)
cor(id1_sep$PM1, id1_sep$PM2)


#Filtro valores de PM
id1_filter <-id1_sep %>% filter(PM1<200, PM2<200, PM10<200)


pairs(id1_filter)
