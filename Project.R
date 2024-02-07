getwd()
df <- read.csv("C:/Users/Utente/Desktop/STATISTICAL ANALYSIS AND MODELLING/exam/project/20221213_QDV2022_001 (4).csv", dec=".", sep=",", stringsAsFactors = T)
df

Piste_ciclabili<-df[df$INDICATORE=="Piste ciclabili", c(1,3,5)]
Piste_ciclabili<-Piste_ciclabili[order(Piste_ciclabili$CODICE.PROVINCIA.ISTAT..STORICO.),]

Tasso_motorizzazione<-df[df$INDICATORE=="Tasso di motorizzazione",c(1,3,5)]
Tasso_motorizzazione<-Tasso_motorizzazione[order(Tasso_motorizzazione$CODICE.PROVINCIA.ISTAT..STORICO.),]

Verde_urbano<-df[df$INDICATORE=="Verde urbano fruibile",c(1,3,5)]
Verde_urbano<-Verde_urbano[order(Verde_urbano$CODICE.PROVINCIA.ISTAT..STORICO.),]

Qualità_aria<-df[df$INDICATORE=="Qualità dell'aria",c(1,3,5)]
Qualità_aria<-Qualità_aria[order(Qualità_aria$CODICE.PROVINCIA.ISTAT..STORICO.),]

Isole_pedonali <-df[df$INDICATORE=="Isole pedonali ",c(1,3,5)]
Isole_pedonali<-Isole_pedonali[order(Isole_pedonali$CODICE.PROVINCIA.ISTAT..STORICO.),]

Ecosistema_urbano <- df[df$INDICATORE=="Ecosistema urbano ", c(1,3,5)]
Ecosistema_urbano <- Ecosistema_urbano[order(Ecosistema_urbano$CODICE.PROVINCIA.ISTAT..STORICO.),]


newdf<-data.frame(Piste_ciclabili[,c(1,2,3)], Tasso_motorizzazione[,3], 
                  Verde_urbano[,3], Qualità_aria[,3], 
                  Isole_pedonali[,3], Ecosistema_urbano[,3])
newdf

names(newdf) <- c("Province", "Province code", "Cycling lanes", "Motorization rate", 
                  "Urban green", "Air quality", 
                  "Pedestrian areas", "Urban ecosystem")
names(newdf)

#separazione dell'italia https://www.tuttitalia.it/statistiche/nord-centro-mezzogiorno-italia/

Nord <- newdf[c(1:40, 93, 96:99, 103:104),]
Nord

Centro <- newdf[c(41:60, 100, 105),]
Centro

Mezzogiorno <- newdf[c(61:92, 94:95, 101:102, 106:107),]
Mezzogiorno

nrow(Nord)
nrow(Centro)
nrow(Mezzogiorno)

mean(Nord$`Cycling lanes`)
mean(Centro$`Cycling lanes`)
mean(Mezzogiorno$`Cycling lanes`)

mean(Nord$`Motorization rate`)
mean(Centro$`Motorization rate`)
mean(Mezzogiorno$`Motorization rate`)

mean(Nord$`Urban green`)
mean(Centro$`Urban green`)
mean(Mezzogiorno$`Urban green`)

mean(Nord$`Air quality`)
mean(Centro$`Air quality`)
mean(Mezzogiorno$`Air quality`)

mean(Nord$`Pedestrian areas`)
mean(Centro$`Pedestrian areas`)
mean(Mezzogiorno$`Pedestrian areas`)


#mean of variable "cycling lanes"
mean_cycling_lanes<-mean(newdf$`Cycling lanes`)
mean_cycling_lanes_nord<-mean(Nord$`Cycling lanes`)
mean_cycling_lanes_centro<-mean(Centro$`Cycling lanes`)
mean_cycling_lanes_mezzogiorno<-mean(Mezzogiorno$`Cycling lanes`)

#variance of variable "cycling lanes"// var=M(x^2)-M(x)^2
variance_cycling_lanes<-mean(newdf$`Cycling lanes`^2)-mean(newdf$`Cycling lanes`)^2
variance_cycling_lanes_nord<-mean(Nord$`Cycling lanes`^2)-mean(Nord$`Cycling lanes`)^2
variance_cycling_lanes_centro<-mean(Centro$`Cycling lanes`^2)-mean(Centro$`Cycling lanes`)^2
variance_cycling_lanes_mezzogiorno<-mean(Mezzogiorno$`Cycling lanes`^2)-mean(Mezzogiorno$`Cycling lanes`)^2

#standard deviation of variable "cycling lanes" // sd=sqrt(var)
sd_cycling_lanes<-sqrt(variance_cycling_lanes)
sd_cycling_lanes_nord<-sqrt(variance_cycling_lanes_nord)
sd_cycling_lanes_centro<-sqrt(variance_cycling_lanes_centro)
sd_cycling_lanes_mezzogiorno<-sqrt(variance_cycling_lanes_mezzogiorno)

#coefficient of variation of variable "cycling lanes" // cv=sd/mean
cv_cycling_lanes<-sd_cycling_lanes/mean_cycling_lanes
cv_cycling_lanes_nord<-sd_cycling_lanes_nord/mean_cycling_lanes_nord
cv_cycling_lanes_centro<-sd_cycling_lanes_centro/mean_cycling_lanes_centro
cv_cycling_lanes_mezzogiorno<-sd_cycling_lanes_mezzogiorno/mean_cycling_lanes_mezzogiorno

#mean of variable "motorization rate"
mean_motorization_rate<-mean(newdf$`Motorization rate`)
mean_motorization_rate_nord<-mean(Nord$`Motorization rate`)
mean_motorization_rate_centro<-mean(Centro$`Motorization rate`)
mean_motorization_rate_mezzogiorno<-mean(Mezzogiorno$`Motorization rate`)

#variance of variable "Motorization rate"// var=M(x^2)-M(x)^2
variance_motorization_rate<-mean(newdf$`Motorization rate`^2)-mean(newdf$`Motorization rate`)^2
variance_motorization_rate_nord<-mean(Nord$`Motorization rate`^2)-mean(Nord$`Motorization rate`)^2
variance_motorization_rate_centro<-mean(Centro$`Motorization rate`^2)-mean(Centro$`Motorization rate`)^2
variance_motorization_rate_mezzogiorno<-mean(Mezzogiorno$`Motorization rate`^2)-mean(Mezzogiorno$`Motorization rate`)^2

#standard deviation of variable "Motorization rate" // sd=sqrt(var)
sd_motorization_rate<-sqrt(variance_motorization_rate)
sd_motorization_rate_nord<-sqrt(variance_motorization_rate_nord)
sd_motorization_rate_centro<-sqrt(variance_motorization_rate_centro)
sd_motorization_rate_mezzogiorno<-sqrt(variance_motorization_rate_mezzogiorno)

#coefficient of variation of variable "Motorization rate" // cv=sd/mean
cv_motorization_rate<-sd_motorization_rate/mean_motorization_rate
cv_motorization_rate_nord<-sd_motorization_rate/mean_motorization_rate_nord
cv_motorization_rate_centro<-sd_motorization_rate/mean_motorization_rate_centro
cv_motorization_rate_mezzogiorno<-sd_motorization_rate/mean_motorization_rate_mezzogiorno


mean(Nord$`Urban ecosystem`)
mean(Centro$`Urban ecosystem`)
mean(Mezzogiorno$`Urban ecosystem`)

