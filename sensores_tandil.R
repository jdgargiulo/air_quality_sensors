# Relaciones PM vs condiciones atmosféricas

head(meteo_SEP_23)
head(id1_filter)

a<-id1_filter %>% group_by(date=floor_date(date, '1 hour')) %>% 
  summarise(pm1=mean(PM1), 
            pm2=mean(PM2), 
            pm10=mean(PM10))

head(b)
b<-meteo_SEP_23 %>% group_by(date=floor_date(date, '1 hour')) %>% 
  summarise(Hum=mean(Hum),
            wd=mean(wd),
            Temp=mean(Temp),
            ws=mean(ws))


data_id1_SEP_con_meteo<-left_join(a,b, by="date")

summary(data_id1_SEP_con_meteo)

cor(data_id1_SEP_con_meteo$pm2, data_id1_SEP_con_meteo$Temp)

pairs(data_id1_SEP_con_meteo)

plot(data_id1_SEP_con_meteo$media_H, data_id1_SEP_con_meteo$media_PM2)

windRose(data_id1_SEP_con_meteo, type = "pm1", layout=c(4,1))

pollutionRose(data_id1_SEP_con_meteo, pollutant = "pm10", statistic="prop.mean")

polarPlot(data_id1_SEP_con_meteo, type = "month", poll = "pm2")

timePlot(head(data_id1_SEP_con_meteo, 96), pollutant = c("pm2", "pm10"), 
         windflow = list(scale = 0.1, lwd = 2, 
                         col = "turquoise4"), 
         lwd = 3, group = FALSE, 
         ylab = "concentration (ug/m3)")

timePlot(data_id1_SEP_con_meteo,
         pollutant = c("pm1", "pm2", "pm10", "ws", "Temp"),
         y.relation = "free")

timeVariation(data_id1_SEP_con_meteo, 
              pollutant = c("pm2", "pm10","Temp"),
              )

scatterPlot(data_id1_SEP_con_meteo, x="date", y="pm2", z="wd")

summaryPlot(data_id1_SEP_con_meteo)
