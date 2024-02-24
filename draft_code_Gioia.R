###########################
#nota che ho fatto la correlazione tra urban ecosystem e air quality perché almeno veniva fuori qualcosa e non tra
#pedestrian areas ed air quality perché usciva sempre correlazione nulla
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

#still missing last part
