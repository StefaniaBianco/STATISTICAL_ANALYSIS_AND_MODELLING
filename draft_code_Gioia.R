###########################
#nota che ho fatto la correlazione tra urban ecosystem e air quality perché almeno veniva fuori qualcosa e non tra
#pedestrian areas ed air quality perché usciva sempre correlazione nulla
###########################
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

#now we want to divide our indicator "pedestrian areas" in four main categories
#poor, medium, good and very good by looking at the squared metres per inhabitant.
#general table:
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

#we will use the data collected on our newdf but importing them with a new funciton

hist(newdf$`Pedestrian areas`, freq=F, xlab="m^2 per inhabitant", ylab="Number of province", 
     main="Pedestrian Areas Italy", col=coul, border="red")
lines(density(newdf$`Pedestrian areas`), lwd=) 
abline(v=mean(newdf$`Pedestrian areas`), col='red', lwd=3)
curve(dnorm(x, mean=mean(newdf$`Pedestrian areas`), 
            sd=sd(newdf$`Pedestrian areas`)), 
      add=T, col="orange", lwd=2)
shapiro.test(newdf$`Pedestrian areas`)

#t test between population means for pedestrian areas
t.test(x=Nord$`Pedestrian areas` , y = Mezzogiorno$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Nord$`Pedestrian areas`, y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Mezzogiorno$`Pedestrian areas`, y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)

t.test(x=Nord$`Pedestrian areas` , y = Mezzogiorno$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Nord$`Pedestrian areas` , y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
t.test(x=Mezzogiorno$`Pedestrian areas` , y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
