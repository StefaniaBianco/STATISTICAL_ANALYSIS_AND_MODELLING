
###########################
###UNIVARIATE
# Mean of variable "urban ecosystem"
mean_urban_ecosystem<-mean(newdf$`Urban ecosystem`)
mean_urban_ecosystem_nord<-mean(Nord$`Urban ecosystem`)
mean_urban_ecosystem_centro<-mean(Centro$`Urban ecosystem`)
mean_urban_ecosystem_mezzogiorno<-mean(Mezzogiorno$`Urban ecosystem`)
mean_urban_ecosystem_values<-c(0.53, 0.59, 0.53, 0.46)

# Variance of variable "urban ecosystem"// var=M(x^2)-M(x)^2
variance_urban_ecosystem<-mean(newdf$`Urban ecosystem`^2)-(mean_urban_ecosystem)^2
variance_urban_ecosystem_nord<-mean(Nord$`Urban ecosystem`^2)- (mean_urban_ecosystem_nord)^2
variance_urban_ecosystem_centro<-mean(Centro$`Urban ecosystem`^2)-(mean_urban_ecosystem_centro)^2
variance_urban_ecosystem_mezzogiorno<-mean(Mezzogiorno$`Urban ecosystem`^2)- (mean_urban_ecosystem_mezzogiorno)^2

# Standard deviation of variable "urban ecosystems" // sd=sqrt(var)
sd_urban_ecosystem<-sqrt(variance_urban_ecosystem)
sd_urban_ecosystem_nord<-sqrt(variance_urban_ecosystem_nord)
sd_urban_ecosystem_centro<-sqrt(variance_urban_ecosystem_centro)
sd_urban_ecosystem_mezzogiorno<-sqrt(variance_urban_ecosystem_mezzogiorno)

# Coefficient of variation of variable "urban ecosystems" // cv=sd/mean
cv_urban_ecosystem<-sd_urban_ecosystem/mean_urban_ecosystem
cv_urban_ecosystem_nord<-sd_urban_ecosystem_nord/mean_urban_ecosystem_nord
cv_urban_ecosystem_centro<-sd_urban_ecosystem_centro/mean_urban_ecosystem_centro
cv_urban_ecosystem_mezzogiorno<-sd_urban_ecosystem_mezzogiorno/mean_urban_ecosystem_mezzogiorno

# Mean of variable "Pedestrian areas"
mean_pedestrian_areas<-mean(newdf$`Pedestrian areas`)
mean_pedestrian_areas_nord<-mean(Nord$`Pedestrian areas`)
mean_pedestrian_areas_centro<-mean(Centro$`Pedestrian areas`)
mean_pedestrian_areas_mezzogiorno<-mean(Mezzogiorno$`Pedestrian areas`)
mean_pedestrian_areas_values<-c(0.47, 0.50, 0.70, 0.29)

# Variance of variable "Pedestrian areas"// var=M(x^2)-M(x)^2
variance_pedestrian_areas<-mean(newdf$`Pedestrian areas`^2)- (mean_pedestrian_areas)^2
variance_pedestrian_areas_nord<-mean(Nord$`Pedestrian areas`^2)-(mean_pedestrian_areas_nord)^2
variance_pedestrian_areas_centro<-mean(Centro$`Pedestrian areas`^2)-(mean_pedestrian_areas_centro)^2
variance_pedestrian_areas_mezzogiorno<-mean(Mezzogiorno$`Pedestrian areas`^2)-(mean_pedestrian_areas_mezzogiorno)^2

# Standard deviation of variable "Pedestrian areas" // sd=sqrt(var)
sd_pedestrian_areas<-sqrt(variance_pedestrian_areas)
sd_pedestrian_areas_nord<-sqrt(variance_pedestrian_areas_nord)
sd_pedestrian_areas_centro<-sqrt(variance_pedestrian_areas_centro)
sd_pedestrian_areas_mezzogiorno<-sqrt(variance_pedestrian_areas_mezzogiorno)

# Coefficient of variation of variable "Pedestrian areas" // cv=sd/mean
cv_pedestrian_areas<-sd_pedestrian_areas/mean_pedestrian_areas
cv_pedestrian_areas_nord<-sd_pedestrian_areas/mean_pedestrian_areas_nord
cv_pedestrian_areas_centro<-sd_pedestrian_areas/mean_pedestrian_areas_centro
cv_pedestrian_areas_mezzogiorno<-sd_pedestrian_areas/mean_pedestrian_areas_mezzogiorno

####BARPLOT
# Barplot with the mean of variable "Urban Ecosystem"
bp_mean_urban_ecosystem<-barplot(mean_urban_ecosystem_values, 
                               names.arg = c("Italy", "North", "Centre", "South"), 
                               col="dark green", ylab="Urban Ecosystem Synthetic Index",
                               ylim =c(0, 0.7) )
title("Barplot of Urban Ecosystem")
abline(h=0.53)

# Barplot with the mean of variable "Pedestrian Areas"
# The highest PAs are present in the Central areas, followed by the Northern region and, at the latest place, we have the Southern 
# which value is above the italian mean.
bp_mean_pedestrian_areas<-barplot(mean_pedestrian_areas_values, 
                                   names.arg = c("Italy", "North", "Centre", "South"), 
                                   col="dark green", ylab="m^2 every 100 habitants",
                                  ylim = c(0, 0.8))
title("Barplot of Pedestrian Areas")
abline(h=0.47)


#BIVARIATE
####CORRELATION                         
plot(x=newdf$`Urban ecosystem`, y=newdf$`Air quality`,    
     xlab = "Urban Ecosystem ", ylab="Air quality", 
     main="Scatterplot of U.E. and Air quality", 
     cex.main=1.4, font.main=2, 
     col.main="navyblue")
cor(newdf$`Urban ecosystem`, newdf$`Air quality`)     #value 0.05, no general correlation 
cor.test(newdf$`Urban ecosystem`, newdf$`Air quality`) #p-value, no evidence against H0= no correlation
model_italy_2 <- lm(newdf$`Urban ecosystem`~ newdf$`Air quality`)
summary(model_italy_2)
dev.off()

# But what about in the different areas of Italy?

plot(x=Nord$`Urban ecosystem`, y=Nord$`Air quality`,    
     xlab = "Urban Ecosystem ", ylab="Air quality", 
     main="Scatterplot of U.E. and Air quality Northern Italy", 
     cex.main=1.4, font.main=2, 
     col.main="navyblue")
?plot
cor(x=Nord$`Urban ecosystem`, y=Nord$`Air quality`)  # negative -0.3, poor correlation
cor.test(Nord$`Urban ecosystem`, Nord$`Air quality`) # p-value, no evidence against H0= no correlation
model_italy_3 <- lm(Nord$`Urban ecosystem`~ Nord$`Air quality`)
summary(model_italy_3)


plot(x=Centro$`Urban ecosystem`, y=Centro$`Air quality`,    
     xlab = "Urban Ecosystem ", ylab="Air quality", 
     main="Scatterplot of U.E. and Air quality central Italy", 
     cex.main=1.4, font.main=2, 
     col.main="navyblue")
cor(x=Centro$`Urban ecosystem`, y=Centro$`Air quality`) #-0.06, no correlation
cor.test(Centro$`Urban ecosystem`, Centro$`Urban ecosystem`) # p-value = 2.2 x10^(-12)
model_italy_4 <- lm(Centro$`Urban ecosystem`~ Centro$`Air quality`)
summary(model_italy_4)
dev.off()

#probably no correlation because the samples are not enough (only 20, compared to 45 in the north and 35 in the south)

plot(x=Mezzogiorno$`Urban ecosystem`, y=Mezzogiorno$`Air quality`,    
     xlab = "Urban Ecosystem ", ylab="Air quality", 
     main="Scatterplot of U.E. and Air quality Southern Italy", 
     cex.main=1.4, font.main=2, 
     col.main="navyblue")
cor(x=Mezzogiorno$`Urban ecosystem`, y=Mezzogiorno$`Air quality`) #-0.6 good negative correlation
cor.test(Mezzogiorno$`Urban ecosystem`, Mezzogiorno$`Urban ecosystem`) # p-value, 2.2x10^(-16)
model_italy_5 <- lm(Mezzogiorno$`Urban ecosystem`~ Mezzogiorno$`Air quality`)
summary(model_italy_5)

# CATEGORIZATION
# Now we want to divide our indicator "pedestrian areas" in four main categories
# poor, medium, good and very good by looking at the squared metres per inhabitant.
# General table:
range(newdf$`Pedestrian areas`) # between 0 and 6.8
pedestrian_areas_cat <-cut(newdf$`Pedestrian areas`, 
                           breaks=c(0, 1.5, 3, 4.5, 7), 
                           labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat <-table(pedestrian_areas_cat)

# Northern Italy table
range(Nord$`Pedestrian areas`) # wide range, between 0.00 and 5
# 5.18
pedestrian_areas_cat_nord<-cut(Nord$`Pedestrian areas`, 
                               breaks=c(0, 1.5, 3, 4.5, 7), 
                               labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat_nord <-table(pedestrian_areas_cat_nord)

# Center Italy table
range(Centro$`Pedestrian areas`) # widest range, between 0.1 and 6.79 highest value)
# 6.68
pedestrian_areas_cat_centro <-cut(Centro$`Pedestrian areas`, 
                                  breaks=c(0, 1.5, 3, 4.5, 7), 
                                  labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat_centro <-table(pedestrian_areas_cat_centro)

# Southern Italy table
range(Mezzogiorno$`Pedestrian areas`) # extermely low values, maximum value at 1.66
# 1.66
pedestrian_areas_cat_mezz <- cut(Mezzogiorno$`Pedestrian areas`, 
                                 breaks=c(0, 1.5, 3, 4.5, 7), 
                                 labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7")) 
barcat_mezz <-table(pedestrian_areas_cat_mezz)

# A color palette is defined
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

par(mfrow=c(2,2))
barplot(barcat, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,110),
        main="PAs in Italy", col=coul, border="red")
barplot(barcat_nord, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50),
        main="PAs in Northern Italy", col=coul, border="red" )
barplot(barcat_centro, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,25), 
        main="PAs in Central Italy", col=coul, border="red")
barplot(barcat_mezz, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,40),
        main="PAs in Southern Italy", col=coul, border="red")
dev.off()

# From this we can see how the situation is pretty limited all around Italy, 
# with the worst situation in Southern Italy.

#####HISTOGRAM
par(mfrow=c(2,2))
hist(newdf$`Urban ecosystem`, freq=F, xlab="Synthetic UE Index", 
     ylab="% of province", ylim = c(0,6),
     main="Urban Ecosystem Italy", col=coul, border="red")

lines(density(newdf$`Urban ecosystem`), col= "orange", lwd=2, ) 
lines(density(Nord$`Urban ecosystem`), col= "blue", lwd=2, ) 
lines(density(Centro$`Urban ecosystem`), col= "darkgreen", lwd=2, ) 
lines(density(Mezzogiorno$`Urban ecosystem`), lwd=2, ) 
abline(v=mean(newdf$`Urban ecosystem`), col='red', lwd=3)

legend("topright", inset = c(-0.05, 0), legend= c("Italy","Northern Italy", "Central Italy", "Southern Italy"),
       fill= c("orange", "blue", "darkgreen", "black"), box.lwd= 0.1, cex=0.6, text.width= 0.2, title="Lines legend",
       y.intersp = 0.5, text.font = 2, text.col = "black")

hist(newdf$`Urban ecosystem`, freq=F, xlab="Synthetic Urban Ecosystem Index", 
     ylab="% of province", ylim = c(0,6),
     main="Urban Ecosystem Northern Italy", col=coul, border="red")
lines(density(Nord$`Urban ecosystem`), col= "blue", lwd=2, ) 
abline(v=mean(Nord$`Urban ecosystem`), col='red', lwd=3)

hist(newdf$`Urban ecosystem`, freq=F, xlab="Synthetic Urban Ecosystem Index", 
     ylab="% of province", ylim = c(0,6),
     main="Urban Ecosystem Central Italy", col=coul, border="red")
lines(density(Centro$`Urban ecosystem`), col= "darkgreen", lwd=2, ) 
abline(v=mean(Centro$`Urban ecosystem`), col='red', lwd=3)

hist(newdf$`Urban ecosystem`, freq=F, xlab="Synthetic Urban Ecosystem Index", 
     ylab="% of province", ylim = c(0,6),
     main="Urban Ecosystem Southern Italy", col=coul, border="red")
lines(density(Mezzogiorno$`Urban ecosystem`), lwd=2, ) 
abline(v=mean(Mezzogiorno$`Urban ecosystem`), col='red', lwd=3)
dev.off()

# General graphic data interpretation:
# Around 5% of provinces have extremely low values, between 0.2 and 0.3
# little more of 50% of the southern province have values between 0.3 and 0,6 with the maximum density around 0.45, which is a value below the national italian mean
# for urban ecosystem. Instead, only a small percentage, less than 5% has good values ranging between 0.7 and 0.8
# this confirm what we've stated before about our values: the situation among the southern province is bad almost everywhere, except some few exception.
# we compare the situation among the whole italian peninsula and then we create the cumulative plot


######INFERENCE
shapiro.test(newdf$`Pedestrian areas`) # data do not come from a normal distribution
shapiro.test(newdf$`Urban ecosystem`) # data come from a normal distribution
# The Shapiro test confirm what we have foreseen in the histograms results and that the pedestrian areas doesn't follow a standard normal distribution while the Urban Ecosystem does.

# Here we see the different distribution
# PEdestrian areas has an extremely negative skew graphic while Urban Ecosystem has the "bell-shaped"
# curve typical of a normal distribution. To confirm and graphically see our results on Shapiro Test
par(mfrow=c(1,2))
hist(newdf$`Pedestrian areas`, freq=F, xlab="m^ per inhabitant", 
     ylab="% of province", ylim = c(0,1),
     main="Pedestrian Areas in Italy", col=coul, border="red")
abline(v=mean(newdf$`Urban ecosystem`), col='red', lwd=3)

hist(newdf$`Urban ecosystem`, freq=F, xlab="Synthetic UE Index", 
     ylab="% of province", ylim = c(0,4),
     main="Urban Ecosystem Italy", col=coul, border="red")
abline(v=mean(newdf$`Urban ecosystem`), col='red', lwd=3)
dev.off()

# When it comes to pedestrian areas, since according to Shapiro test it has not a normal distribution,
# what we can do for large sample (>30) is to apply a z-test and calculate the SE(X) and SE(Y).
# This can be done for the Northern and the Southern provinces, but not for the Central one, since the number of sample 
# is 22 and so it can't be considered as a large sample.
install.packages("EnvStats")
install.packages("BSDA")
library(EnvStats)
library(BSDA)

# We need to calculate both sigma Y and sigma X, but to do that we need the unbiased sample variance
# so we will use the var function

Nord_var <- var(Nord$`Pedestrian areas`)
Centro_var <- var(Centro$`Pedestrian areas`)
Sud_var <- var(Mezzogiorno$`Pedestrian areas`)

# To calculate the unbiased sample variance we use sqrt:
Nord_usv <-sqrt(Nord_var)
Centro_usv <-sqrt(Centro_var)
Sud_usv <-sqrt(Sud_var)

# Now we can appyl the z.test on our variable "Pedestrian Areas"
z.test(x=Nord$`Pedestrian areas`, y = Mezzogiorno$`Pedestrian areas`, alternative = "two.sided",
       mu=0, sigma.x= Nord_usv, sigma.y= Sud_usv, conf.level = 0.99) # p-value 0.11
z.test(x=Nord$`Pedestrian areas`, y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, sigma.x= Nord_usv, sigma.y= Centro_usv, conf.level = 0.99) #p-value 0.52
z.test(x=Mezzogiorno$`Pedestrian areas`, y = Centro$`Pedestrian areas`, alternative = "two.sided",
       mu=0, sigma.x= Sud_usv, sigma.y= Centro_usv, conf.level = 0.99) #p-value 0.17

# Instead, when it comes to Urban Ecosystem, since a normal distribution is followed, 
# a t-test can be applied.
t.test(x=Nord$`Urban ecosystem` , y = Mezzogiorno$`Urban ecosystem`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99) #4x10^(-8) strong evidence against H0
t.test(x=Nord$`Urban ecosystem` , y = Centro$`Urban ecosystem`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99) #0.0166 strong evidence against H0
t.test(x=Mezzogiorno$`Urban ecosystem` , y = Centro$`Urban ecosystem`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99) #0.009 strong evidence against H0

#As expected, H0 is rejected, since our mean values for Urban Ecosystem for North, Centre and South are diffent within eachothers.




