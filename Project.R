#Getting our working directory and importing our dataset with the function read.csv
getwd()
df <- read.csv("C:/Users/Utente/Desktop/STATISTICAL ANALYSIS AND MODELLING/exam/project/20221213_QDV2022_001 (4).csv", dec=".", sep=",", stringsAsFactors = T)
df


#Selecting the indicators of interests, the columns wanted and then ordering them in order of provence code. 
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


#Creating a new dataframe with the indicators, the columns wanted, and assigning new names 
newdf<-data.frame(Piste_ciclabili[,c(1,2,3)], Tasso_motorizzazione[,3], 
                  Verde_urbano[,3], Qualità_aria[,3], 
                  Isole_pedonali[,3], Ecosistema_urbano[,3])
names(newdf) <- c("Province", "Province code", "Cycling lanes", "Motorization rate", 
                  "Urban green", "Air quality", 
                  "Pedestrian areas", "Urban ecosystem")
newdf


#Creating three new dataframes by separating Italy in Nord, Centre and South
###PER IL REPORT SOLAMENTE#separazione dell'italia https://www.tuttitalia.it/statistiche/nord-centro-mezzogiorno-italia/
Nord <- newdf[c(1:40, 93, 96:99, 103:104),]
Nord

Centro <- newdf[c(41:60, 100, 105),]
Centro

Mezzogiorno <- newdf[c(61:92, 94:95, 101:102, 106:107),]
Mezzogiorno

#Checking if we took all the provences
nrow(Nord)+nrow(Centro)+nrow(Mezzogiorno)


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

# Mean of variable "Urban green"
mean_urban_green<-mean(newdf$`Urban green`)
mean_urban_green_nord<-mean(Nord$`Urban green`)
mean_urban_green_centro<-mean(Centro$`Urban green`)
mean_urban_green_mezzogiorno<-mean(Mezzogiorno$`Urban green`)
mean_urban_green_values<-c(22.40, 29.79, 19.87, 14.74)

# Variance of variable "Urban green" // var=M(x^2)-M(x)^2
variance_urban_green<-mean(newdf$`Urban green`^2)-(mean_urban_green)^2
variance_urban_green_nord<-mean(Nord$`Urban green`^2)- (mean_urban_green_nord)^2
variance_urban_green_centro<-mean(Centro$`Urban green`^2)-(mean_urban_green_centro)^2
variance_urban_green_mezzogiorno<-mean(Mezzogiorno$`Urban green`^2)- (mean_urban_green_mezzogiorno)^2

#standard deviation of variable "Urban green" // sd=sqrt(var)
sd_urban_green<-sqrt(variance_urban_green)
sd_urban_green_nord<-sqrt(variance_urban_green_nord)
sd_urban_green_centro<-sqrt(variance_urban_green_centro)
sd_urban_green_mezzogiorno<-sqrt(variance_urban_green_mezzogiorno)

#coefficient of variation of variable "Urban green" // cv=sd/mean
cv_urban_green<-sd_urban_green/mean_urban_green
cv_urban_green_nord<-sd_urban_green_nord/mean_urban_green_nord
cv_urban_green_centro<-sd_urban_green_centro/mean_urban_green_centro
cv_urban_green_mezzogiorno<-sd_urban_green_mezzogiorno/mean_urban_green_mezzogiorno

#barplot with the mean of variable "Urban green"
bp_mean_urban_green<-barplot(mean_urban_green_values, 
                                 names.arg = c("Italy", "North", "Centre", "South"), 
                                 col="light green", ylab="Number of U.A. ???")
title("Barplot of Urban green")
abline(h=9.67)

# Mean of variable "Air quality"
mean_air_quality<-mean(newdf$`Air quality`)
mean_air_quality_nord<-mean(Nord$`Air quality`)
mean_air_quality_centro<-mean(Centro$`Air quality`)
mean_air_quality_mezzogiorno<-mean(Mezzogiorno$`Air quality`)
mean_air_quality_values<-c(51.32, 62.41, 43.83, 41.95)

# Variance of variable "Air quality" // var=M(x^2)-M(x)^2
variance_air_quality<-mean(newdf$`Air quality`^2)-(mean_air_quality)^2
variance_air_quality_nord<-mean(Nord$`Air quality`^2)- (mean_air_quality_nord)^2
variance_air_quality_centro<-mean(Centro$`Air quality`^2)-(mean_air_quality_centro)^2
variance_air_quality_mezzogiorno<-mean(Mezzogiorno$`Air quality`^2)- (mean_air_quality_mezzogiorno)^2

#standard deviation of variable "Air quality" // sd=sqrt(var)
sd_air_quality<-sqrt(variance_air_quality)
sd_air_quality_nord<-sqrt(variance_air_quality_nord)
sd_air_quality_centro<-sqrt(variance_air_quality_centro)
sd_air_quality_mezzogiorno<-sqrt(variance_air_quality_mezzogiorno)

#coefficient of variation of variable "Air quality" // cv=sd/mean
cv_air_quality<-sd_air_quality/mean_air_quality
cv_air_quality_nord<-sd_air_quality_nord/mean_air_quality_nord
cv_air_quality_centro<-sd_air_quality_centro/mean_air_quality_centro
cv_air_quality_mezzogiorno<-sd_air_quality_mezzogiorno/mean_air_quality_mezzogiorno

#barplot with the mean of variable "Air quality"
bp_mean_air_quality<-barplot(mean_air_quality_values, 
                                 names.arg = c("Italy", "North", "Centre", "South"), 
                                 col="light green", ylab="Number of U.A. ???")
title("Barplot of Air quality")
abline(h=9.67)

#correlation between motorization rate and cycling lanes                           
cor(newdf$`Motorization rate`, newdf$`Cycling lanes`) #value -0.07, no correlation

plot(x=Nord$`Motorization rate`, y=Nord$`Cycling lanes`)
cor(x=Nord$`Motorization rate`, y=Nord$`Cycling lanes`) #value 0.32, poor correlation

plot(x=Centro$`Motorization rate`, y=Centro$`Cycling lanes`)
cor(x=Centro$`Motorization rate`, y=Centro$`Cycling lanes`)  #-0.05, no correlation

plot(x=Mezzogiorno$`Motorization rate`, y=Mezzogiorno$`Cycling lanes`)
cor(x=Mezzogiorno$`Motorization rate`, y=Mezzogiorno$`Cycling lanes`) #-0.01, no correlation
#############################


