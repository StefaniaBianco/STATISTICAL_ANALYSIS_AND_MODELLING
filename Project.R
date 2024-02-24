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

############################Stefania#################################
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

#########################Gioia########################
###########################
#mean of variable "urban ecosystem"
mean_urban_ecosystem<-mean(newdf$`Urban ecosystem`)
mean_urban_ecosystem_centro
mean_urban_ecosystem_nord<-mean(Nord$`Urban ecosystem`)
mean_urban_ecosystem_centro<-mean(Centro$`Urban ecosystem`)
mean_urban_ecosystem_mezzogiorno<-mean(Mezzogiorno$`Urban ecosystem`)
mean_urban_ecosystem_values<-c(0.53, 0.59, 0.53, 0.46)

#variance of variable "urban ecosystem"// var=M(x^2)-M(x)^2
variance_urban_ecosystem<-mean(newdf$`Urban ecosystem`^2)-(mean_urban_ecosystem)^2
variance_urban_ecosystem_nord<-mean(Nord$`Urban ecosystem`^2)- (mean_urban_ecosystem_nord)^2
variance_urban_ecosystem_centro<-mean(Centro$`Urban ecosystem`^2)-(mean_urban_ecosystem_centro)^2
variance_urban_ecosystem_mezzogiorno<-mean(Mezzogiorno$`Urban ecosystem`^2)- (mean_urban_ecosystem_mezzogiorno)^2

#standard deviation of variable "cycling lanes" // sd=sqrt(var)
sd_urban_ecosystem<-sqrt(variance_urban_ecosystem)
sd_urban_ecosystem_nord<-sqrt(variance_urban_ecosystem_nord)
sd_urban_ecosystem_centro<-sqrt(variance_urban_ecosystem_centro)
sd_urban_ecosystem_mezzogiorno<-sqrt(variance_urban_ecosystem_mezzogiorno)

#coefficient of variation of variable "cycling lanes" // cv=sd/mean
cv_urban_ecosystem<-sd_urban_ecosystem/mean_urban_ecosystem
cv_urban_ecosystem_nord<-sd_urban_ecosystem_nord/mean_urban_ecosystem_nord
cv_urban_ecosystem_centro<-sd_urban_ecosystem_centro/mean_urban_ecosystem_centro
cv_urban_ecosystem_mezzogiorno<-sd_urban_ecosystem_mezzogiorno/mean_urban_ecosystem_mezzogiorno

#mean of variable "pedestrian areas"
mean_pedestrian_areas<-mean(newdf$`Pedestrian areas`)
mean_pedestrian_areas_mezzogiorno
mean_pedestrian_areas_nord<-mean(Nord$`Pedestrian areas`)
mean_pedestrian_areas_centro<-mean(Centro$`Pedestrian areas`)
mean_pedestrian_areas_mezzogiorno<-mean(Mezzogiorno$`Pedestrian areas`)
mean_pedestrian_areas_values<-c(0.47, 0.50, 0.70, 0.29)

#variance of variable "Motorization rate"// var=M(x^2)-M(x)^2
variance_pedestrian_areas<-mean(newdf$`Pedestrian areas`^2)- (mean_pedestrian_areas)^2
variance_pedestrian_areas_nord<-mean(Nord$`Pedestrian areas`^2)-(mean_pedestrian_areas_nord)^2
variance_pedestrian_areas_centro<-mean(Centro$`Pedestrian areas`^2)-(mean_pedestrian_areas_centro)^2
variance_pedestrian_areas_mezzogiorno<-mean(Mezzogiorno$`Pedestrian areas`^2)-(mean_pedestrian_areas_mezzogiorno)^2

#standard deviation of variable "Motorization rate" // sd=sqrt(var)
sd_pedestrian_areas<-sqrt(variance_pedestrian_areas)
sd_pedestrian_areas_nord<-sqrt(variance_pedestrian_areas_nord)
sd_pedestrian_areas_centro<-sqrt(variance_pedestrian_areas_centro)
sd_pedestrian_areas_mezzogiorno<-sqrt(variance_pedestrian_areas_mezzogiorno)

#coefficient of variation of variable "Motorization rate" // cv=sd/mean
cv_pedestrian_areas<-sd_pedestrian_areas/mean_pedestrian_areas
cv_pedestrian_areas_nord<-sd_pedestrian_areas/mean_pedestrian_areas_nord
cv_pedestrian_areas_centro<-sd_pedestrian_areas/mean_pedestrian_areas_centro
cv_pedestrian_areas_mezzogiorno<-sd_pedestrian_areas/mean_pedestrian_areas_mezzogiorno

#barplot with the mean of variable "Urban Ecosystem"
bp_mean_urban_ecosystem<-barplot(mean_urban_ecosystem_values, 
                               names.arg = c("Italy", "North", "Centre", "South"), 
                               col="light green", ylab="Number of U.A. ???")
title("Barplot of Urban Ecosystem")
abline(h=9.67)

#barplot with the mean of variable "Pedestrian Areas"
bp_mean_pedestrian_areas<-barplot(mean_pedestrian_areas_values, 
                                   names.arg = c("Italy", "North", "Centre", "South"), 
                                   col="light green", ylab="Number of P.A. every 100 habitants")
title("Barplot of Pedestrian Areas")
abline(h=65.54)

#correlation                            
plot(x=newdf$`Urban ecosystem`, y=newdf$`Air quality`,    
     xlab = "Urban Ecosystem ", ylab="Air quality", 
     main="Scatterplot of U.E. and Air quality", 
     cex.main=1.4, font.main=2, 
     col.main="navyblue")
cor(newdf$`Urban ecosystem`, newdf$`Air quality`)     #value 0.05, no general correlation 

#but what about in the different areas of Italy?
plot(x=Nord$`Urban ecosystem`, y=Nord$`Air quality`)
cor(x=Nord$`Urban ecosystem`, y=Nord$`Air quality`)  #negative -0.3, poor correlation

plot(x=Centro$`Urban ecosystem`, y=Centro$`Air quality`)
cor(x=Centro$`Urban ecosystem`, y=Centro$`Air quality`)  #-0.06, no correlation

plot(x=Mezzogiorno$`Urban ecosystem`, y=Mezzogiorno$`Air quality`)
cor(x=Mezzogiorno$`Urban ecosystem`, y=Mezzogiorno$`Air quality`) #-0.6 good negative correlation

#########################################Mattia##########
#############################


