#PEDESTRIAN AREAS
range(newdf$`Pedestrian areas`) #between 0 and 6.8
pedestrian_areas_cat <-cut(newdf$`Pedestrian areas`, 
                           breaks=c(0, 1.5, 3, 4.5, 7), 
                           labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat <-table(pedestrian_areas_cat)

# Northern Italy table
range(Nord$`Pedestrian areas`) #wide range, between 0.00 and 5
5.19-0.01#5.18
pedestrian_areas_cat_nord<-cut(Nord$`Pedestrian areas`, 
                               breaks=c(0, 1.5, 3, 4.5, 7), 
                               labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat_nord <-table(pedestrian_areas_cat_nord)

# Center Italy table
range(Centro$`Pedestrian areas`) #widest range, between 0.1 and 6.79 highest value)
6.79-0.11 #6.68
pedestrian_areas_cat_centro <-cut(Centro$`Pedestrian areas`, 
                                  breaks=c(0, 1.5, 3, 4.5, 7), 
                                  labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat_centro <-table(pedestrian_areas_cat_centro)

#Southern Italy table
range(Mezzogiorno$`Pedestrian areas`) #extermely low values, maximum value at 1.66
1.66-0.0 #1.66
pedestrian_areas_cat_mezz <- cut(Mezzogiorno$`Pedestrian areas`, 
                                 breaks=c(0, 1.5, 3, 4.5, 7), 
                                 labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7")) 
barcat_mezz <-table(pedestrian_areas_cat_mezz)

#we define a colour palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

par(mfrow=c(2,2))
barplot(barcat, xlab = "m^2 per inhabitant", ylab= "Number of province",
        main="PAs in Italy", col=coul, border="red")
barplot(barcat_nord, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50),
        main="PAs in Northern Italy", col=coul, border="red" )
barplot(barcat_centro, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50), 
        main="PAs in Central Italy", col=coul, border="red")
barplot(barcat_mezz, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50),
        main="PAs in Southern Italy", col=coul, border="red")

#MOTORIZATION RATE
range(newdf$`Motorization rate`) 
motorization_rate_cat <-cut(newdf$`Motorization rate`, 
                           breaks=c(40, 50, 60, 70, 80), 
                           labels=c("40-50", "50-60", "60-70", "70-80"))
barcat_mr <-table(motorization_rate_cat)

# Northern Italy table
range(Nord$`Motorization rate`) 
motorization_rate_nord<-cut(Nord$`Motorization rate`, 
                            breaks=c(40, 50, 60, 70, 80), 
                            labels=c("40-50", "50-60", "60-70", "70-80"))
barcat_mr_nord <-table(motorization_rate_nord)

# Center Italy table
range(Centro$`Motorization rate`) 
motorization_rate_centro<-cut(Centro$`Motorization rate`, 
                            breaks=c(40, 50, 60, 70, 80), 
                            labels=c("40-50", "50-60", "60-70", "70-80"))
barcat_mr_centro <-table(motorization_rate_centro)

#Southern Italy table
range(Mezzogiorno$`Motorization rate`) 
motorization_rate_sud<-cut(Mezzogiorno$`Motorization rate`, 
                            breaks=c(40, 50, 60, 70, 80), 
                            labels=c("40-50", "50-60", "60-70", "70-80"))
barcat_mr_sud <-table(motorization_rate_sud)

#we define a colour palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

par(mfrow=c(2,2))
barplot(barcat_mr, xlab = "N. of cars every 100 inhabitants", ylab= "Number of province",
        main="MR in Italy", col=coul, border="red")
barplot(barcat_mr_nord, xlab = "N. of cars every 100 inhabitants", ylab= "Number of province", ylim= c(0,40),
        main="MR in Northern Italy", col=coul, border="red" )
barplot(barcat_mr_centro, xlab = "N. of cars every 100 inhabitants", ylab= "Number of province", ylim= c(0,40), 
        main="MR in Central Italy", col=coul, border="red")
barplot(barcat_mr_sud, xlab = "N. of cars every 100 inhabitants", ylab= "Number of province", ylim= c(0,40),
        main="MR in Southern Italy", col=coul, border="red")

#AIR QUALITY
range(newdf$`Air quality`) 
air_quality_cat <-cut(newdf$`Air quality`, 
                            breaks=c(20, 38, 55, 73, 90), 
                            labels=c("20-38", "38-55", "55-73", "73-90"))
barcat_aq <-table(air_quality_cat)

# Northern Italy table
range(Nord$`Air quality`) 
air_quality_nord<-cut(Nord$`Air quality`, 
                            breaks=c(20, 38, 55, 73, 90), 
                            labels=c("20-38", "38-55", "55-73", "73-90"))
barcat_aq_nord <-table(air_quality_nord)

# Center Italy table
range(Centro$`Air quality`) 
air_quality_centro<-cut(Centro$`Air quality`, 
                              breaks=c(20, 38, 55, 73, 90), 
                              labels=c("20-38", "38-55", "55-73", "73-90"))
barcat_aq_centro <-table(air_quality_centro)

#Southern Italy table
range(Mezzogiorno$`Air quality`) 
air_quality_sud<-cut(Mezzogiorno$`Air quality`, 
                           breaks=c(20, 38, 55, 73, 90), 
                           labels=c("20-38", "38-55", "55-73", "73-90"))
barcat_aq_sud <-table(air_quality_sud)


par(mfrow=c(2,2))
barplot(barcat_aq, xlab = "PM10, NO2 and O3 index", ylab= "Number of province",
        main="AQ in Italy", col=coul, border="red")
barplot(barcat_aq_nord, xlab = "PM10, NO2 and O3 index", ylab= "Number of province", ylim= c(0,30),
        main="AQ in Northern Italy", col=coul, border="red" )
barplot(barcat_aq_centro, xlab = "PM10, NO2 and O3 index", ylab= "Number of province", ylim= c(0,30), 
        main="AQ in Central Italy", col=coul, border="red")
barplot(barcat_aq_sud, xlab = "PM10, NO2 and O3 index", ylab= "Number of province", ylim= c(0,30),
        main="AQ in Southern Italy", col=coul, border="red")





#FINAL PLOT
par(mfrow=c(3,3))

barplot(barcat_nord, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50),
        main="PAs in Northern Italy", col=coul, border="red" )
barplot(barcat_centro, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50), 
        main="PAs in Central Italy", col=coul, border="red")
barplot(barcat_mezz, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50),
        main="PAs in Southern Italy", col=coul, border="red")

barplot(barcat_mr_nord, xlab = "N. of cars every 100 inhabitants", ylab= "Number of province", ylim= c(0,40),
        main="MR in Northern Italy", col=coul, border="red" )
barplot(barcat_mr_centro, xlab = "N. of cars every 100 inhabitants", ylab= "Number of province", ylim= c(0,40), 
        main="MR in Central Italy", col=coul, border="red")
barplot(barcat_mr_sud, xlab = "N. of cars every 100 inhabitants", ylab= "Number of province", ylim= c(0,40),
        main="MR in Southern Italy", col=coul, border="red")

barplot(barcat_aq_nord, xlab = "PM10, NO2 and O3 index", ylab= "Number of province", ylim= c(0,30),
        main="AQ in Northern Italy", col=coul, border="red" )
barplot(barcat_aq_centro, xlab = "PM10, NO2 and O3 index", ylab= "Number of province", ylim= c(0,30), 
        main="AQ in Central Italy", col=coul, border="red")
barplot(barcat_aq_sud, xlab = "PM10, NO2 and O3 index", ylab= "Number of province", ylim= c(0,30),
        main="AQ in Southern Italy", col=coul, border="red")
