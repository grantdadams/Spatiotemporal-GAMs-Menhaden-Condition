load(  file = "winter_and_spring_discharge.RData")
df.menhaden <- read.csv("df_menhaden_full4.csv")
df.menhaden <- subset(df.menhaden, select = - c(miss.discharge.Nov.to.March.y, atch.discharge.Nov.to.March.y, mean.monthly.spring.discharge.of.year, chl_9km_modis , SST_9km_modis, chl_9km_scaled, SST_9km_scaled , optional.1, optional))


for ( i  in 1 :dim(df.menhaden)[1]){
  df.menhaden$mean.monthly.March.to.June.miss.scaled.Year.of.Birth[i] = river.discharge[which(river.discharge[,1]== (df.menhaden$year[i] - df.menhaden$age[i])),4]
  df.menhaden$miss.discharge.Nov.to.March.scaled.Year.of.Birth[i] <- river.discharge[which(river.discharge[,1]== (df.menhaden$year[i] - df.menhaden$age[i])),2]
}

i =1

write.csv(df.menhaden, file = "df_menhaden_full5.csv")