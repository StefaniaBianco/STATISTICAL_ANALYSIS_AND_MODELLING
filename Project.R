###DATA IMPORT AND CLEANING

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

#Creating three new dataframes by separating Italy in Nord, Centre and South
Nord <- newdf[c(1:40, 93, 96:99, 103:104),]
Centro <- newdf[c(41:60, 100, 105),]
Mezzogiorno <- newdf[c(61:92, 94:95, 101:102, 106:107),]

#Checking if we took all the provences
nrow(Nord)+nrow(Centro)+nrow(Mezzogiorno)


###DESCRIPTIVE STATISTICS

##DESCRIPTIVE STATISTICS OF THE VARIABLE CYCLING LANES
#mean of variable "cycling lanes"
mean_cycling_lanes<-mean(newdf$`Cycling lanes`)
mean_cycling_lanes_nord<-mean(Nord$`Cycling lanes`)
mean_cycling_lanes_centro<-mean(Centro$`Cycling lanes`)
mean_cycling_lanes_mezzogiorno<-mean(Mezzogiorno$`Cycling lanes`)

#barplot with the mean of variable "cycling lanes"
bp_mean_cycling_lanes<-barplot(mean_cycling_lanes_values, #### NON è DEFINITA 
                               names.arg = c("Italy", "North", "Centre", "South"), 
                               col="GREEN", ylab="Equivalent metres every 100 inhabitant")
title("Barplot of cycling lanes ")
abline(h=9.67)
abline(h=0)

#Grouped frequency distribution of the variable cycling lanes
range(newdf$`Cycling lanes`)
cycling_lanes_cat<-cut(newdf$`Cycling lanes`, breaks = c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat)

cycling_lanes_cat_nord<-cut(Nord$`Cycling lanes`, breaks =c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat_nord)

cycling_lanes_cat_centro<-cut(Centro$`Cycling lanes`, breaks = c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat_centro)

cycling_lanes_cat_sud<-cut(Mezzogiorno$`Cycling lanes`, breaks = c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat_sud)

library(RColorBrewer) #choosing colours
coul <- brewer.pal(5, "Set2")

barplot(table(cycling_lanes_cat), xlab="Equivalent metres every 100 inhabitants", ylab="Number of cycling lanes", main="Cycling lanes Italy", col=coul, border="red")
par(mfrow=c(1,3))
barplot(table(cycling_lanes_cat_nord), xlab="Equivalent metres every 100 inhabitants", ylab="Number of cycling lanes", ylim=c(0,35), main="Cycling lanes Nord", col=coul, border="red")
barplot(table(cycling_lanes_cat_centro), xlab="Equivalent metres every 100 inhabitants", ylab="Number of cycling lanes", ylim=c(0,35), main="Cycling lanes Centre", col=coul, border="red")
barplot(table(cycling_lanes_cat_sud), xlab="Equivalent metres every 100 inhabitants", ylab="Number of cycling lanes", ylim=c(0,35), main="Cycling lanes South", col=coul, border="red")
dev.off()

#Histogram of the variable cycling lanes
hist(newdf$`Cycling lanes`, freq=F, xlab="Equivalent metres every 100 inhabitants", ylab="Number of cycling lanes", main="Cycling lanes Italy", col=coul, border="red")
lines(density(newdf$`Cycling lanes`), lwd=2) 
abline(v=mean(newdf$`Cycling lanes`), col='red', lwd=3)
curve(dnorm(x, mean=mean(newdf$`Cycling lanes`), 
            sd=sd(newdf$`Cycling lanes`)), 
      add=T, col="orange", lwd=2)
shapiro.test(newdf$`Cycling lanes`) 

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

#DESCRIPTIVE STATISTICS OF THE VARIABLE MOTORIZATION RATE
#mean of variable "motorization rate"
mean_motorization_rate<-mean(newdf$`Motorization rate`)
mean_motorization_rate_nord<-mean(Nord$`Motorization rate`)
mean_motorization_rate_centro<-mean(Centro$`Motorization rate`)
mean_motorization_rate_mezzogiorno<-mean(Mezzogiorno$`Motorization rate`)

#barplot with the mean of variable "motorization rate"
bp_mean_motorization_rate<-barplot(mean_motorization_rate_values, 
                                   names.arg = c("Italy", "North", "Centre", "South"), 
                                   col="GREEN", ylab="Number of cars per 100 inhabitant")
title("Barplot of motorization rate means")
abline(h=65.54)
abline(h=0)

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

#DESCRIPTIVE ANALYSIS OF THE VARIABLE URBAN GREEN
# Mean of variable "Urban green"
mean_urban_green<-mean(newdf$`Urban green`)
mean_urban_green_nord<-mean(Nord$`Urban green`)
mean_urban_green_centro<-mean(Centro$`Urban green`)
mean_urban_green_mezzogiorno<-mean(Mezzogiorno$`Urban green`)
mean_urban_green_values<-c(22.40, 29.79, 19.87, 14.74)

#barplot with the mean of variable "Urban green"
bp_mean_urban_green<-barplot(mean_urban_green_values, 
                             names.arg = c("Italy", "North", "Centre", "South"), 
                             col="light green", ylab="m^2 per inhabitant")
title("Barplot of Urban green")
abline(h=22.40)

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

#DESCRIPTIVE ANALYSIS OF THE VARIABLE AIR QUALITY
# Mean of variable "Air quality"
mean_air_quality<-mean(newdf$`Air quality`)
mean_air_quality_nord<-mean(Nord$`Air quality`)
mean_air_quality_centro<-mean(Centro$`Air quality`)
mean_air_quality_mezzogiorno<-mean(Mezzogiorno$`Air quality`)
mean_air_quality_values<-c(51.32, 62.41, 43.83, 41.95)

#barplot with the mean of variable "Air quality"
bp_mean_air_quality<-barplot(mean_air_quality_values, 
                             names.arg = c("Italy", "North", "Centre", "South"), 
                             col="light green", ylab="Index based on PM10, NO2 and O3 data")
title("Barplot of Air quality")
abline(h=51.32)

#Grouped frequency distribution of the variable Air quality
range(newdf$`Air quality`)

air_quality_rate_cat<-cut(newdf$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("20-34", "34-48", "48-62","62-76", "76-90"))
table(air_quality_rate_cat)

air_quality_rate_cat_nord<-cut(Nord$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("20-34", "34-48", "48-62","62-76", "76-90"))
table(air_quality_rate_cat_nord)

air_quality_rate_cat_centro<-cut(Centro$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("20-34", "34-48", "48-62","62-76", "76-90"))
table(air_quality_rate_cat_centro)

air_quality_rate_cat_sud<-cut(Mezzogiorno$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("20-34", "34-48", "48-62","62-76", "76-90"))
table(air_quality_rate_cat_sud)

library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

par(mfrow=c(2,2))
barplot(table(air_quality_rate_cat), xlab="Index on Pm10, nitrogen dioxide and ozone data", ylab="Number of cities", ylim = c(0,50), main="Air quality Italy", col=coul, border="black")
barplot(table(air_quality_rate_cat_nord), xlab="Index on Pm10, nitrogen dioxide and ozone data", ylab="Number of cities", ylim = c(0,25), main="Air quality North Italy", col=coul, border="black")
barplot(table(air_quality_rate_cat_centro), xlab="Index on Pm10, nitrogen dioxide and ozone data", ylab="Number of cities", ylim = c(0,25), main="Air quality Centre Italy", col=coul, border="black")
barplot(table(air_quality_rate_cat_sud), xlab="Index on Pm10, nitrogen dioxide and ozone data", ylab="Number of cities", ylim = c(0,25), main="Air quality South Italy", col=coul, border="black")
dev.off()

#Histogram with the variable Air quality
hist(newdf$`Air quality`, freq=F, xlab="Index on Pm10, nitrogen dioxide and ozone data", ylab="Number of cities", main="Air quality Italy", col=coul, border="black")
lines(density(newdf$`Air quality`), lwd=2)
abline(v=mean(newdf$`Air quality`), col='red', lwd=3)

curve(dnorm(x, mean=mean(newdf$`Air quality`), 
            sd=sd(newdf$`Air quality`)), 
      add=T, col="orange", lwd=2)
shapiro.test(newdf$`Air quality`)

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

#DESCRIPTIVE ANALYSIS OF THE VARIABLE PEDESTRIAN AREAS
#mean of variable "pedestrian areas"
mean_pedestrian_areas<-mean(newdf$`Pedestrian areas`)
mean_pedestrian_areas_nord<-mean(Nord$`Pedestrian areas`)
mean_pedestrian_areas_centro<-mean(Centro$`Pedestrian areas`)
mean_pedestrian_areas_mezzogiorno<-mean(Mezzogiorno$`Pedestrian areas`)
mean_pedestrian_areas_values<-c(0.47, 0.50, 0.70, 0.29)

#barplot with the mean of variable "Pedestrian Areas"
bp_mean_pedestrian_areas<-barplot(mean_pedestrian_areas_values, 
                                  names.arg = c("Italy", "North", "Centre", "South"), 
                                  col="light green", ylab="Number of P.A. every 100 habitants")
title("Barplot of Pedestrian Areas")
abline(h=65.54)

#now we want to divide our indicator "pedestrian areas" in four main categories
#poor, medium, good and very good by looking at the squared metres per inhabitant.
range(newdf$`Pedestrian areas`) #between 0 and 6.8
pedestrian_areas_cat <-cut(newdf$`Pedestrian areas`, 
                           breaks=c(0, 2, 4, 6, 7), 
                           labels=c("poor", "medium", "good", "very good"))
barcat <-table(pedestrian_areas_cat)

# Northern Italy table
range(Nord$`Pedestrian areas`) #wide range, between 0.00 and 5
5.19-0.01#5.18
pedestrian_areas_cat_nord<-cut(Nord$`Pedestrian areas`, breaks=c(0, 2, 4, 6, 7), labels=c("poor", "medium", "good", "very good"))
barcat_nord <-table(pedestrian_areas_cat_nord)

# Center Italy table
range(Centro$`Pedestrian areas`) #widest range, between 0.1 and 6.79 highest value)
6.79-0.11 #6.68
pedestrian_areas_cat_centro <-cut(Centro$`Pedestrian areas`, 
                                  breaks=c(0, 2, 4, 6, 7), 
                                  labels=c("poor", "medium", "good", "very good"))
barcat_centro <-table(pedestrian_areas_cat_centro)

#Southern Italy table
range(Mezzogiorno$`Pedestrian areas`) #extermely low values, maximum value at 1.66
1.66-0.0 #1.66
pedestrian_areas_cat_mezz <- cut(Mezzogiorno$`Pedestrian areas`, 
                                 breaks=c(0, 2, 4, 6, 7), 
                                 labels=c("poor", "medium", "good", "very good")) 
barcat_mezz <-table(pedestrian_areas_cat_mezz)

#we define a colour palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

par(mfrow=c(2,2))
barplot(barcat, xlab = "m^2 per inhabitant", ylab= "Number of province", 
        main="PAs in Italy", col=coul, border="red")
barplot(barcat_nord, xlab = "m^2 per inhabitant", ylab= "Number of province", 
        main="PAs in Northern Italy", col=coul, border="red" )
barplot(barcat_centro, xlab = "m^2 per inhabitant", ylab= "Number of province", 
        main="PAs in Central Italy", col=coul, border="red")
barplot(barcat_mezz, xlab = "m^2 per inhabitant", ylab= "Number of province",
        main="PAs in Southern Italy", col=coul, border="red")
#from this we can see how the situation is pretty limited all around Italy, 
#with the worst situation in southern Italy

#histogram of the variable pedestrian areas
hist(newdf$`Pedestrian areas`, freq=F, xlab="m^2 per inhabitant", ylab="% of province", 
     main="Pedestrian Areas Italy", col=coul, border="red")
lines(density(newdf$`Pedestrian areas`), lwd=) 
abline(v=mean(newdf$`Pedestrian areas`), col='red', lwd=3)
curve(dnorm(x, mean=mean(newdf$`Pedestrian areas`), 
            sd=sd(newdf$`Pedestrian areas`)), 
      add=T, col="orange", lwd=2)
shapiro.test(newdf$`Pedestrian areas`)

#variance of variable "pedestrian areas"// var=M(x^2)-M(x)^2
variance_pedestrian_areas<-mean(newdf$`Pedestrian areas`^2)- (mean_pedestrian_areas)^2
variance_pedestrian_areas_nord<-mean(Nord$`Pedestrian areas`^2)-(mean_pedestrian_areas_nord)^2
variance_pedestrian_areas_centro<-mean(Centro$`Pedestrian areas`^2)-(mean_pedestrian_areas_centro)^2
variance_pedestrian_areas_mezzogiorno<-mean(Mezzogiorno$`Pedestrian areas`^2)-(mean_pedestrian_areas_mezzogiorno)^2

#standard deviation of variable "pedestrian areas" // sd=sqrt(var)
sd_pedestrian_areas<-sqrt(variance_pedestrian_areas)
sd_pedestrian_areas_nord<-sqrt(variance_pedestrian_areas_nord)
sd_pedestrian_areas_centro<-sqrt(variance_pedestrian_areas_centro)
sd_pedestrian_areas_mezzogiorno<-sqrt(variance_pedestrian_areas_mezzogiorno)

#coefficient of variation of variable "Pedestrian areas" // cv=sd/mean
cv_pedestrian_areas<-sd_pedestrian_areas/mean_pedestrian_areas
cv_pedestrian_areas_nord<-sd_pedestrian_areas/mean_pedestrian_areas_nord
cv_pedestrian_areas_centro<-sd_pedestrian_areas/mean_pedestrian_areas_centro
cv_pedestrian_areas_mezzogiorno<-sd_pedestrian_areas/mean_pedestrian_areas_mezzogiorno

#DESCRIPTIVE STATISTICS OF THE VARIABLE URBAN ECOSYSTEMS
#mean of variable "urban ecosystem"
mean_urban_ecosystem<-mean(newdf$`Urban ecosystem`)
mean_urban_ecosystem_nord<-mean(Nord$`Urban ecosystem`)
mean_urban_ecosystem_centro<-mean(Centro$`Urban ecosystem`)
mean_urban_ecosystem_mezzogiorno<-mean(Mezzogiorno$`Urban ecosystem`)
mean_urban_ecosystem_values<-c(0.53, 0.59, 0.53, 0.46)

#barplot with the mean of variable "Urban Ecosystem"
bp_mean_urban_ecosystem<-barplot(mean_urban_ecosystem_values, 
                                 names.arg = c("Italy", "North", "Centre", "South"), 
                                 col="light green", ylab="??")
title("Barplot of Urban Ecosystem")
abline(h=9.67)

#variance of variable "urban ecosystem"// var=M(x^2)-M(x)^2
variance_urban_ecosystem<-mean(newdf$`Urban ecosystem`^2)-(mean_urban_ecosystem)^2
variance_urban_ecosystem_nord<-mean(Nord$`Urban ecosystem`^2)- (mean_urban_ecosystem_nord)^2
variance_urban_ecosystem_centro<-mean(Centro$`Urban ecosystem`^2)-(mean_urban_ecosystem_centro)^2
variance_urban_ecosystem_mezzogiorno<-mean(Mezzogiorno$`Urban ecosystem`^2)- (mean_urban_ecosystem_mezzogiorno)^2

#standard deviation of variable "urban ecosystem" // sd=sqrt(var)
sd_urban_ecosystem<-sqrt(variance_urban_ecosystem)
sd_urban_ecosystem_nord<-sqrt(variance_urban_ecosystem_nord)
sd_urban_ecosystem_centro<-sqrt(variance_urban_ecosystem_centro)
sd_urban_ecosystem_mezzogiorno<-sqrt(variance_urban_ecosystem_mezzogiorno)

#coefficient of variation of variable "urban ecosystem" // cv=sd/mean
cv_urban_ecosystem<-sd_urban_ecosystem/mean_urban_ecosystem
cv_urban_ecosystem_nord<-sd_urban_ecosystem_nord/mean_urban_ecosystem_nord
cv_urban_ecosystem_centro<-sd_urban_ecosystem_centro/mean_urban_ecosystem_centro
cv_urban_ecosystem_mezzogiorno<-sd_urban_ecosystem_mezzogiorno/mean_urban_ecosystem_mezzogiorno

##CORRELATIONS
#1.correlation between air quality (PM10, NO2 and O3 in the air) and urban green (Squared metres per inhabitant)                       
plot(x=newdf$`Urban green`, y=newdf$`Air quality`,    
     xlab = "Urban Green", ylab="PM10, NO2 and 03", 
     main="Scatterplot of Urban Green and Air quality", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
abline(model_italy)
cor(newdf$`Urban green`, newdf$`Air quality`)   
cor.test(newdf$`Urban green`, newdf$`Air quality`) #p-value=0.17-->no evidence against H0=true correlation is equal to 0
model_italy <- lm(newdf$`Urban green`~ newdf$`Air quality`)
summary(model_italy)
dev.off()

plot(x=Nord$`Urban green`, y=Nord$`Air quality`, xlab = "Urban Green", ylab="PM10, NO2 and 03", 
     main="NORD // Scatterplot of Urban Green and Air quality", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Nord$`Urban green`, y=Nord$`Air quality`)  
cor.test(x=Nord$`Urban green`, y=Nord$`Air quality`) 
model_nord <- lm(Nord$`Urban green`~ Nord$`Air quality`)
summary(model_nord)

plot(x=Centro$`Urban green`, y=Centro$`Air quality`,
     xlab = "Urban Green", ylab="PM10, NO2 and 03", 
     main="CENTRE // Scatterplot of Urban Green and Air quality", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Centro$`Urban green`, y=Centro$`Air quality`)  
cor.test(x=Centro$`Urban green`, y=Centro$`Air quality`)
model_centro <- lm(Centro$`Urban green`~ Centro$`Air quality`)
summary(model_centro)

plot(x=Mezzogiorno$`Urban green`, y=Mezzogiorno$`Air quality`, 
     xlab = "Urban Green", ylab="PM10, NO2 and 03", 
     main="Scatterplot of Urban Green and Air quality", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Mezzogiorno$`Urban green`, y=Mezzogiorno$`Air quality`) 
cor.test(x=Mezzogiorno$`Urban green`, y=Mezzogiorno$`Air quality`) #P-value=0.005-->strong evidence against H0, so they are correlated
model_mezzogiorno <- lm(Mezzogiorno$`Urban green`~ Mezzogiorno$`Air quality`)
summary(model_mezzogiorno)
abline(model_mezzogiorno)
dev.off()

#2.correlation between pedestrian areas and motorization rate
#Pedestrian areas=squared metres per inhabitant, motorization rate=cars every 100 inhabitants
plot(x=newdf$`Pedestrian areas`, y=newdf$`Motorization rate`, xlab = "Pedestrian areas", ylab="Motorization rate", 
     main="Scatterplot of Pedestrian areas and Motorization rate", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=newdf$`Pedestrian areas`, y=newdf$`Motorization rate`)
cor.test(x=newdf$`Pedestrian areas`, y=newdf$`Motorization rate`) #p-value=0.07, they might be correlated
model_italy_pa_mr<-lm (newdf$`Pedestrian areas`~newdf$`Motorization rate`)
summary(model_italy_pa_mr)
dev.off()

plot(x=Nord$`Pedestrian areas`, y=Nord$`Motorization rate`, xlab = "Pedestrian areas", ylab="Motorization rate", 
     main="NORD // Scatterplot of Pedestrian areas and Motorization rate", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Nord$`Pedestrian areas`, y=Nord$`Motorization rate`)  
cor.test(x=Nord$`Pedestrian areas`, y=Nord$`Motorization rate`) 
model_nord_correlation <- lm(Nord$`Pedestrian areas`~ Nord$`Motorization rate`)
summary(model_nord_correlation)

plot(x=Centro$`Pedestrian areas`, y=Centro$`Motorization rate`, xlab = "Pedestrian areas", ylab="Motorization rate", 
     main="CENTRO // Scatterplot of Pedestrian areas and Motorization rate", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Centro$`Pedestrian areas`, y=Centro$`Motorization rate`)  
cor.test(x=Centro$`Pedestrian areas`, y=Centro$`Motorization rate`) 
model_centro_correlation <- lm(Centro$`Pedestrian areas`~ Centro$`Motorization rate`)
summary(model_centro_correlation)

plot(x=Mezzogiorno$`Pedestrian areas`, y=Mezzogiorno$`Motorization rate`, xlab = "Pedestrian areas", ylab="Motorization rate", 
     main="Mezzogiorno // Scatterplot of Pedestrian areas and Motorization rate", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Mezzogiorno$`Pedestrian areas`, y=Mezzogiorno$`Motorization rate`)  
cor.test(x=Mezzogiorno$`Pedestrian areas`, y=Mezzogiorno$`Motorization rate`) 
model_mezzogiorno_correlation <- lm(Mezzogiorno$`Pedestrian areas`~ Mezzogiorno$`Motorization rate`)
summary(model_mezzogiorno_correlation)

#3.correlation between motorization rate and cycling lanes                          
# Plot to see if there is correlation
plot(x=newdf$`Motorization rate`, y=newdf$`Cycling lanes`,    
     xlab = "Motorization rate", ylab="Cycling lanes", 
     main="Scatterplot of Motorization rate and Cycling lanes", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(newdf$`Motorization rate`, newdf$`Cycling lanes`) #value -0.07, no correlation

# different areas of Italy
plot(x=Nord$`Motorization rate`, y=Nord$`Cycling lanes`)
cor(x=Nord$`Motorization rate`, y=Nord$`Cycling lanes`) #value 0.32, poor correlation

plot(x=Centro$`Motorization rate`, y=Centro$`Cycling lanes`)
cor(x=Centro$`Motorization rate`, y=Centro$`Cycling lanes`)  #-0.05, no correlation

plot(x=Mezzogiorno$`Motorization rate`, y=Mezzogiorno$`Cycling lanes`)
cor(x=Mezzogiorno$`Motorization rate`, y=Mezzogiorno$`Cycling lanes`) #-0.01 good negative correlation

model_mot_cyc<-lm(formula = newdf$`Motorization rate` ~ newdf$`Cycling lanes`)
summary(model_mot_cyc)

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




###INFERENCE

library(EnvStats)
library(BSDA)
require(BSDA)

#z test between population means for cycling lanes
shapiro.test(Nord$`Cycling lanes`)
shapiro.test(Mezzogiorno$`Cycling lanes`)
shapiro.test(Centro$`Cycling lanes`)

var(Nord$`Cycling lanes`)
sqrt(var(Nord$`Cycling lanes`))

var(Mezzogiorno$`Cycling lanes`)
sqrt(var(Mezzogiorno$`Cycling lanes`))

z.test(x=Nord$`Cycling lanes` , y = Mezzogiorno$`Cycling lanes`,
       alternative = "two.sided", sigma.x=10.09, sigma.y=6.06, mu=0, conf.level = 0.99)

#test between population means for motorization rate
shapiro.test(Nord$`Motorization rate`) #not normal distribution
shapiro.test(Centro$`Motorization rate`) #normal 
shapiro.test(Mezzogiorno$`Motorization rate`) #normal

var(Nord$`Motorization rate`)
sqrt(var(Nord$`Motorization rate`))
var(Mezzogiorno$`Motorization rate`)
sqrt(var(Mezzogiorno$`Motorization rate`))

z.test(x=Nord$`Motorization rate` , y = Mezzogiorno$`Motorization rate`, alternative = "two.sided",
       sigma.x = 6.15, sigma.y= 6.41, mu=0, conf.level = 0.99) #not normal and normal, so maybe z.test is fine?
t.test(x=Mezzogiorno$`Motorization rate` , y = Centro$`Motorization rate`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99) #normal distribution so t test is fine

#t test between population means for urban green
t.test(x=Nord$`Urban green` , y = Mezzogiorno$`Urban green`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Nord$`Urban green`, y = Centro$`Urban green`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Mezzogiorno$`Urban green`, y = Centro$`Urban green`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)

#t test between population means for air quality
t.test(x=Nord$`Air quality` , y = Mezzogiorno$`Air quality`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Nord$`Air quality`, y = Centro$`Air quality`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Mezzogiorno$`Air quality`, y = Centro$`Air quality`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)

#t test between population means for pedestrian areas
t.test(x=Nord$`Pedestrian areas` , y = Mezzogiorno$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Nord$`Pedestrian areas`, y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Mezzogiorno$`Pedestrian areas`, y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
