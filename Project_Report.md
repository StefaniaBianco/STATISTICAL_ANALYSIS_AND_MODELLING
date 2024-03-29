---
title: "Does the Italian quality of life depends on the geographical area?"
author: "Stefania Bianco, Mattia Fabris, Gioia Riolli"
date: "2024-02-27"
output:
  html_document: default
  word_document: default
  pdf_document: default
---
```{r, eval=TRUE, include=FALSE}
getwd()
df <- read.csv("/Users/mattiafabris/Desktop/unibo/statistical analysis/Progetto.def/20221213_QDV2022_001 .csv", dec=".", sep=",", stringsAsFactors = T)

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

newdf<-data.frame(Piste_ciclabili[,c(1,2,3)], Tasso_motorizzazione[,3], Verde_urbano[,3], Qualità_aria[,3], Isole_pedonali[,3], Ecosistema_urbano[,3])


names(newdf) <- c("Province", "Province code", "Cycling lanes", "Motorization rate", 
                  "Urban green", "Air quality", 
                  "Pedestrian areas", "Urban ecosystem")
Nord <- newdf[c(1:40, 93, 96:99, 103:104),]

Centro <- newdf[c(41:60, 100, 105),]

Mezzogiorno <- newdf[c(61:92, 94:95, 101:102, 106:107),]

pedestrian_areas_cat <-cut(newdf$`Pedestrian areas`, 
                           breaks=c(0, 1.5, 3, 4.5, 7), 
                           labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
```

# Introduction and Data Description 

Life quality in cities is a major discussion topic nowadays in the European countries, and the focus of the influence of the environmental aspect is getting more and more important. 
Italy, on its side, is struggling to reach some good environmental index and indicators: furthermore, the complex socio-economical situation throughout the peninsula may underlie some differences among the Nord, Centre and South. 
The present project aims both at:

- understanding the overall state of art of six environmental parameters in the whole Italy;
- analysing and understanding if the parameters are significantly different between the three macroareas of the peninsula.

Data have been collected from "Il Sole 24 Ore" website and GitHub account. The newspaper ranks every year all the 107 Italian provinces from the highest to the lowest for quality of life standards. 
The results are published in an user-friendly website in form of a general and specific ranking, and in the GitHub profile, from where the present data have been taken and imported on R by using the function:

```{r, eval=FALSE }
getwd()
df <- read.csv("C:/Users/Utente/Desktop/STATISTICAL ANALYSIS AND MODELLING/exam/project/20221213_QDV2022_001 (4).csv", dec=".", sep=",", stringsAsFactors = T)
```

Secondarily to the data import, the variables "Province", "Province code", "Cycling lanes", "Motorization rate", "Urban green", "Air quality", "Pedestrian areas", "Urban ecosystem" have been selected. 
These latter have been listed by using the R function "order" according to the province code. 
Finally, a new data frame has been created, called "newdf", and all the variables have been renamed in English. 

For the indicators selection the following code has been used:
```{r, eval=FALSE }
#Example of data cleaning and ordering for one indicator
Piste_ciclabili<-df[df$INDICATORE=="Piste ciclabili", c(1,3,5)]
Piste_ciclabili<-Piste_ciclabili[order(Piste_ciclabili$CODICE.PROVINCIA.ISTAT..STORICO.),]
```

Every indicator has a different unit of measurements: 

- "Cycling lanes" is expressed in equivalent metres every 100 inhabitant;
- "Motorization rate" in number of circulating cars every 100 inhabitants; 
- "Urban green" is squared metres per inhabitant;
- "Air quality" is an index based on PM10, NO2 and O3 data in the regional county seats; 
- "Pedestrian areas" is expressed in squared metres per inhabitant;
- "Urban ecosystem" is an indicator that merged 18 environmental parameters (such as waste recycling, water consumption and public local transportation).

In the present study, the 107 Italian provinces represent the observed statistical units while the environmental parameters are quantitative continuous variables.

The new data frame has been then further divided into three new subsets: 

- North, 
- Centre, 
- "Mezzogiorno" or South,

according to the ISTAT division of the Italian administrative territory.

A descriptive and inferential statistical analysis of all the chosen indicators has been made and it will be explained in the next paragraphs. 
For the descriptive part, a first study of all the indicators was made using:

- measures of central tendency;
- measures of variability;
- plots and graphics to visually appreaciate the data analysis. 

Note that different analysis have been made for the different variables in order to obtain a more dynamic and smooth report. 

Moreover, four correlations between variables have been made, willing to understand if any possible relationship among them could exist.

Finally, for the inferencial part, some tests have been made about the differences among population means, assuming that our data come from a random sample of cities and expanding our inference to the whole geographical area (North, Centre, South).

# Descriptive statistics

In order to make some univariate descriptive analysis, for each indicator the mean, the variance, the standard deviation and the coefficient of variation has been calculated. 
For some variables graphics, such as boxplots, barplots and histograms have been drawn.  

### Urban green
The first analyzed variable is "Urban green", expressed in squared metres per inhabitant. 
As first numerical measure, we computed the arithmetic mean for each indicator across various regions. 
Note that, as this is the first variable, almost the full code has been included, to be thorough.

For "Urban Green", the results are the following:
```{r, eval=TRUE }
mean_urban_green<-mean(newdf$`Urban green`)
mean_urban_green
mean_urban_green_nord<-mean(Nord$`Urban green`)
mean_urban_green_nord
mean_urban_green_centro<-mean(Centro$`Urban green`)
mean_urban_green_centro
mean_urban_green_mezzogiorno<-mean(Mezzogiorno$`Urban green`)
mean_urban_green_mezzogiorno
```
As the barplot shows, the values differ across the peninsula, with a higher mean in the North than the Centre and the South.
```{r, eval=TRUE}
mean_urban_green_values<-c(22.40, 29.79, 19.87, 14.74)
bp_mean_urban_green<-barplot(mean_urban_green_values, 
                                 names.arg = c("Italy", "North", "Centre", "South"), 
                                 col="light green", ylab="m^2 per inhabitant")
title("Barplot of Urban green")
abline(h=22.40)
```

To visualize the quantiles, median, and range of the Italian data, a boxplot has been generated. 
It revealed the presence of five outliers (Gorizia, Pordenone, Verbano-Cusio-Ossola, Monza e Brianza, Reggio Emilia). 
Consequently, we opted to display the boxplot excluding these outliers.
```{r, eval=TRUE}
par(mfrow=c(1,2))
boxplot(newdf$`Urban green`, main = "Urban green", ylab = "m^2 per inhabitant", outline = FALSE)
boxplot(newdf$`Urban green`, main = "Urban green", ylab = "m^2 per inhabitant")
```

The presence of these outliers skews the mean. In this case a more robust measure is the median.
```{r, eval=TRUE}
median_urban_green<-median(newdf$`Urban green`)
median_urban_green
```

Measures of variability have also been calculated: variance, standard deviation and coefficient of variation.
```{r, eval=TRUE}
# Variance of Urban green
variance_urban_green<-mean(newdf$`Urban green`^2)-(mean_urban_green)^2
variance_urban_green
variance_urban_green_nord<-mean(Nord$`Urban green`^2)- (mean_urban_green_nord)^2
variance_urban_green_nord
variance_urban_green_centro<-mean(Centro$`Urban green`^2)-(mean_urban_green_centro)^2
variance_urban_green_centro
variance_urban_green_mezzogiorno<-mean(Mezzogiorno$`Urban green`^2)- (mean_urban_green_mezzogiorno)^2
variance_urban_green_mezzogiorno
```

In the northern region, the variance is notably higher compared to other values, primarily because outlier provinces are concentrated in that area.

```{r, eval=TRUE}
# Standard deviation of Urban green 
sd_urban_green<-sqrt(variance_urban_green)
sd_urban_green
sd_urban_green_nord<-sqrt(variance_urban_green_nord)
sd_urban_green_nord
sd_urban_green_centro<-sqrt(variance_urban_green_centro)
sd_urban_green_centro
sd_urban_green_mezzogiorno<-sqrt(variance_urban_green_mezzogiorno)
sd_urban_green_mezzogiorno

# Coefficient of variation of Urban green 
cv_urban_green<-sd_urban_green/mean_urban_green
cv_urban_green
cv_urban_green_nord<-sd_urban_green_nord/mean_urban_green_nord
cv_urban_green_nord
cv_urban_green_centro<-sd_urban_green_centro/mean_urban_green_centro
cv_urban_green_centro
cv_urban_green_mezzogiorno<-sd_urban_green_mezzogiorno/mean_urban_green_mezzogiorno
cv_urban_green_mezzogiorno
```

### Air quality
The average values for the variable "Air quality," measured with an index based on PM10, NO2, and O3 data in the regional county seats, are as follows:

- Italy: 51.32
- North: 62.41
- Centre: 43.83
- South: 41.95

These values indicate that, on average, air quality is higher in the South, followed by the Centre, whereas the North has the lowest air quality.

```{r, eval=TRUE, echo=FALSE}
mean_air_quality_values<-c(51.32, 62.41, 43.83, 41.95)
bp_mean_air_quality<-barplot(mean_air_quality_values, 
                                 names.arg = c("Italy", "North", "Centre", "South"), 
                                 col="light green", ylab="Index based on PM10, NO2 and O3 data")
title("Barplot of Air quality")
abline(h=51.32)
```

```{r, eval=TRUE, echo=FALSE}
boxplot(newdf$`Air quality`, main = "Boxplot of Air quality", ylab = "Index based on PM10, NO2 and O3 data", ylim=c(10,90))

```

To gain a deeper understanding of the air quality across various regions of Italy, an analysis on the grouped frequency distribution has been done pertaining to different levels of the PM10, NO2, and O3 indices in the regional county seats. 
The data have been segmented by using the function "cut" into five classes: 

- "very poor" (20-34), 
- "poor" (34-48), 
- "medium" (48-62), 
- "good" (62-76), 
- "very good" (76-90).

```{r, eval=TRUE, echo=FALSE}
air_quality_rate_cat<-cut(newdf$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("very good", "good", "medium","poor", "very poor"))
table(air_quality_rate_cat)

air_quality_rate_cat_nord<-cut(Nord$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("very good", "good", "medium","poor", "very poor"))
table(air_quality_rate_cat_nord)

air_quality_rate_cat_centro<-cut(Centro$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("very good", "good", "medium","poor", "very poor"))
table(air_quality_rate_cat_centro)

air_quality_rate_cat_sud<-cut(Mezzogiorno$`Air quality`, breaks = c(20, 34, 48, 62, 76, 90), labels = c("very good", "good", "medium","poor", "very poor"))
table(air_quality_rate_cat_sud)

library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

barplot(table(air_quality_rate_cat), xlab="Index on Pm10, NO2 and O3", ylab="Number of cities", ylim = c(0,50), main="Air quality Italy", col=coul, border="black")

par(mfrow=c(1,3))
barplot(table(air_quality_rate_cat_nord), xlab="Index on Pm10, NO2 and O3", ylab="Number of cities", ylim = c(0,25), main="Air quality North Italy", col=coul, border="black")
barplot(table(air_quality_rate_cat_centro), xlab="Index on Pm10, NO2 and O3", ylab="Number of cities", ylim = c(0,25), main="Air quality Centre Italy", col=coul, border="black")
barplot(table(air_quality_rate_cat_sud), xlab="Index on Pm10, NO2 and O3", ylab="Number of cities", ylim = c(0,25), main="Air quality South Italy", col=coul, border="black")
```


As can be observed, a significant number of provinces fall within the first two classes, suggesting a low presence of harmful pollutants. 
However, a lot of provinces still exhibit poor air quality conditions.

An histogram was also created, in order to see more clearly the distribution of the data.
```{r, eval=TRUE}
hist(newdf$`Air quality`, freq=F, xlab="Index on Pm10, nitrogen dioxide and ozone data", ylab="Relative Frequency Density", main="Air quality Italy", col=coul, border="black")
lines(density(newdf$`Air quality`), lwd=2)
abline(v=mean(newdf$`Air quality`), col='red', lwd=3)

curve(dnorm(x, mean=mean(newdf$`Air quality`), 
            sd=sd(newdf$`Air quality`)), 
      add=T, col="orange", lwd=2)
```

The black line indicates the density curve, the orange one represents the shape of hypothetically normally distributed data and the red one is the mean.
The histogram is slightly positively skewed.

To understand if the data are normally distributed or not, Shapiro test can be performed.
```{r, eval=TRUE}
shapiro.test(newdf$`Air quality`)
```
The latter test states that: 

- H0 : sample data come from a normal distribution; 
- H1 : sample data do NOT come from a normal distribution.

Since the p-value is really low, strong evidence against H0 can be found, so the data do not come from a normal distribution.

Measures of variability followed a different pattern compared to the mean:

- North: var = 238.60, sd = 15.45, cv = 0.25;
- Centre: var = 50.46, sd = 7.10, cv = 0.16;
- South: var = 126.11, sd = 11.23, cv = 0.27.


### Cycling lanes

The variable "cycling lane" is expressed in equivalent metres every 100 inhabitants. 
The values obtained for the means are:

- Italy = 9.67 
- Nord = 14.90 
- Centre = 7.10 
- South = 4.69

```{r, eval=TRUE,echo=FALSE}
mean_cycling_lanes_values<-c(9.67, 14.90, 7.10, 4.70)
bp_mean_cycling_lanes<-barplot(mean_cycling_lanes_values, 
                               names.arg = c("Italy", "North", "Centre", "South"), 
                               col="GREEN", ylab="Equivalent metres every 100 inhabitant")
title("Barplot of cycling lanes ")
abline(h=9.67)
abline(h=0)

```

The measures of variability follows the same pattern, with:

- Higher variance and standard deviation in the North (var=99.74, sd=9.99);
- Smaller in the centre (var=36.19, sd=6.02)
- Even smaller in the south (var=35.70, sd=5.98). 

The overall variance for Italy is 86.08, while the standard deviation is 9.28.

To understand from a qualitative point of view if the cycling lane presence is consistent or not in Italy, the range has been calculated: it goes from 0 to 46.5 equivalent meters every 100 inhabitants. 5 categories have been created: 

- "Very poor" = 0-10
- "Poor" = 10-20
- "Medium" = 20-30
- "Good" = 30-40
- "Very good" = 40-50

Note that the above-made classification is based only on the collected data: it is not an arbitrary and universal classification. This means that probably, what has been classified as "very good" could not be good enough for the European standards, or, more generally, for a proper cyclable city.

```{r, eval=TRUE, echo=FALSE}
cycling_lanes_cat<-cut(newdf$`Cycling lanes`, breaks = c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat)


library(RColorBrewer) #choosing colours
coul <- brewer.pal(5, "Set2")

barplot(table(cycling_lanes_cat), ylab="Number of cities", main="Cycling lanes Italy", col=coul, border="red")
```

It is clear that the majority of Italian cities have a poor presence of cycling lanes, as more than 60 cities belong to the category "very poor", and the number of cities decreases when the categories get better in terms of presence of cycling lanes. 

To understand how the situation is in the macroregion of Italy, the same process has been applied, but within small subsets. 
```{r, eval=TRUE, echo=FALSE}
cycling_lanes_cat_nord<-cut(Nord$`Cycling lanes`, breaks =c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat_nord)

cycling_lanes_cat_centro<-cut(Centro$`Cycling lanes`, breaks = c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat_centro)

cycling_lanes_cat_sud<-cut(Mezzogiorno$`Cycling lanes`, breaks = c(0, 10, 20, 30, 40, 50), labels = c("Very poor", "Poor", "Medium","Good","Very good"))
table(cycling_lanes_cat_sud)

par(mfrow=c(1,3))
barplot(table(cycling_lanes_cat_nord), ylab="Number of cities", ylim=c(0,35), main="Cycling lanes Nord", col=coul, border="red")
barplot(table(cycling_lanes_cat_centro), ylab="Number of cities", ylim=c(0,35), main="Cycling lanes Centre", col=coul, border="red")
barplot(table(cycling_lanes_cat_sud), ylab="Number of cities", ylim=c(0,35), main="Cycling lanes South", col=coul, border="red")
```
From this barplots, it can be seen that in the North of Italy there are more cities belonging to better categories compared to the Centre and the South. 

The last analysis made on this variable was an histogram. 
Again, the distribution is strongly positively skewed.

```{r, eval=TRUE, echo=F}
hist(newdf$`Cycling lanes`, freq=F, xlab="Equivalent metres every 100 inhabitants", ylab="Number of cycling lanes", main="Cycling lanes Italy", col=coul, border="red")
lines(density(newdf$`Cycling lanes`), lwd=2) 
abline(v=mean(newdf$`Cycling lanes`), col='red', lwd=3)
curve(dnorm(x, mean=mean(newdf$`Cycling lanes`), 
            sd=sd(newdf$`Cycling lanes`)), 
      add=T, col="orange", lwd=2)
```

The red line indicates the mean, the black line is the actual density curve and the orange line indicates a normal distributed density curve. 
With the function
```{r, eval=FALSE}
shapiro.test(newdf$`Cycling lanes`) 
```
an extremely low p-value (1.976^-08) has been obtained, meaning strong evidence against H0: data do not come from a normal distribution, as foreseen. 

### Motorization rate 
The variable "Motorization rate" is expressed in circulating cars every 100 inhabitants. The trend this time showed higher values in the South, medium in the Centre and smaller in the North.
The obtained means:

- Italy = 65.54 
- Nord =  62.47
- Centre = 67.40
- South = 68.26

```{r, eval=TRUE, echo=FALSE}
mean_motorization_rate_values<-c(65.54, 62.47, 67.40, 68.26)
bp_mean_motorization_rate<-barplot(mean_motorization_rate_values, 
                                   names.arg = c("Italy", "North", "Centre", "South"), 
                                   col="GREEN", ylab="Number of cars per 100 inhabitant")
title("Barplot of motorization rate means")
abline(h=65.54)
abline(h=0)
```

The difference is not that consistent, but still the lower motorization rate in the North of Italy may be due to a better public transportation system, to more cycling lanes in the urban areas and to more pedestrian areas. 

The variance and the standard deviation follow the same pattern:

- Italy var = 45.66; Italy sd = 6.76
- North var = 37.04; North sd = 6.09
- Centre var = 37.37; Centre sd = 6.11
- South var = 40.06; South sd = 6.33

In this case as well a boxplot of the variable "Motorization rate" in Italy has been drawn:
```{r, eval=TRUE, echo=FALSE}
par(mfrow=c(1,2))
boxplot(newdf$`Motorization rate`,  main = "Motorization rate")
boxplot(newdf$`Motorization rate`,  main = "Motorization rate", outline = FALSE)
```
Once more, the presence of three outliers was noticed: Venice, Genova and Milan.
Overall, the motorization rate is very high and needs to be lowered in order to have more environmental-friendly and human-friendly cities. 

### Pedestrian areas

The index Pedestrian Areas measures the m^2 of pedestrian pavement for every inhabitant.
The results are the following:

- Italy: mean = 0.46, var = 0.72, sd = 0.85, cv = 1.82 
- North: mean = 0.49, var = 0.67, sd = 0.81, cv = 1.70
- Centre: mean = 0.70, var = 1.83, sd = 1.35, cv = 1.21
- South: mean = 0.29, var = 0.082, sd = 0.28, cv = 2.92

In this case, the values obtained from the analysis across the North, Central, and South regions differ significantly. 
While the values for provinces in the North are around the Italian average, those in the South are much lower, whereas those in the Central region are somewhat higher. 
There's also notable variability in the data, particularly in the Central region, indicating a considerable degree of difference among the various provinces.

To visually appreciate the difference in the means:

```{r, eval=TRUE, echo=FALSE}
mean_pedestrian_areas_values<-c(0.47, 0.50, 0.70, 0.29)
bp_mean_pedestrian_areas<-barplot(mean_pedestrian_areas_values, 
                                  names.arg = c("Italy", "North", "Centre", "South"), 
                                  col="light green", ylab="m^2 for inhabitant")
title("Barplot of Pedestrian Areas")
abline(h=0.47)
```

The "Pedestrian Areas" indicator has been divided in four main categories by using the "cat" function and by looking at the squared metres per inhabitant.

General table:
```{r, eval=FALSE, echo=FALSE}
range(newdf$`Pedestrian areas`) # between 0 and 6.8
pedestrian_areas_cat <-cut(newdf$`Pedestrian areas`, 
                           breaks=c(0, 1.5, 3, 4.5, 7), 
                           labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat <-table(pedestrian_areas_cat)
```
Northern Italy table
```{r, eval=TRUE, echo=FALSE}
range(Nord$`Pedestrian areas`) # wide range, between 0.00 and 5
5.19-0.01# 5.18
pedestrian_areas_cat_nord<-cut(Nord$`Pedestrian areas`, 
                               breaks=c(0, 1.5, 3, 4.5, 7), 
                               labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat_nord <-table(pedestrian_areas_cat_nord)
```
Center Italy table
```{r, eval=TRUE, echo=FALSE}
range(Centro$`Pedestrian areas`) # widest range, between 0.1 and 6.79 highest value)
6.79-0.11 # 6.68
pedestrian_areas_cat_centro <-cut(Centro$`Pedestrian areas`, 
                                  breaks=c(0, 1.5, 3, 4.5, 7), 
                                  labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7"))
barcat_centro <-table(pedestrian_areas_cat_centro)
```
Southern Italy table
```{r, eval=TRUE, echo=FALSE}
range(Mezzogiorno$`Pedestrian areas`) # extermely low values, maximum value at 1.66
1.66-0.0 # 1.66
pedestrian_areas_cat_mezz <- cut(Mezzogiorno$`Pedestrian areas`, 
                                 breaks=c(0, 1.5, 3, 4.5, 7), 
                                 labels=c("0-1.5", "1.5-3", "3-4.5", "4.5-7")) 
barcat_mezz <-table(pedestrian_areas_cat_mezz)
```

The situation is pretty limited all around Italy, with the worst situation again in the South.

```{r, eval=TRUE, echo=FALSE}
barcat <-table(pedestrian_areas_cat)
par(mfrow=c(2,2))
barplot(barcat, xlab = "m^2 per inhabitant", ylab= "Number of province",
        main="PAs in Italy", col=coul, border="red")
barplot(barcat_nord, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50),
        main="PAs in Northern Italy", col=coul, border="red" )
barplot(barcat_centro, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50), 
        main="PAs in Central Italy", col=coul, border="red")
barplot(barcat_mezz, xlab = "m^2 per inhabitant", ylab= "Number of province", ylim= c(0,50),
        main="PAs in Southern Italy", col=coul, border="red")
```

### Urban ecosystem

Urban Ecosystem is a synthetic index based on 18 parameters: air quality, waste, water networks, land consumption. 
Data have been collected from Legambiente - Environment Italy, 2022.

The obtained values show a shallow pattern across the Italian peninsula; rather, they remain relatively consistent throughout. 
This highlights the need for increased green spaces and urban redevelopment projects throughout the region to make urban areas more environmental-friendly.

The results are the following:

- Italy: mean = 0.53, var = 0.011, sd = 0.10, cv = 0.19 
- North: mean = 0.59, var = 0.008, sd = 0.09, cv = 0.16
- Centre: mean = 0.52, var = 0.007, sd = 0.08, cv = 0.16
- South: mean = 0.46, var = 0.008, sd = 0.09, cv = 0.19

To visually appreciate the results, the barplot of the mean of the index "Urban Ecosystem":

```{r, eval=TRUE, echo=FALSE}
mean_urban_ecosystem_values<-c(0.53, 0.59, 0.53, 0.46)
bp_mean_urban_ecosystem<-barplot(mean_urban_ecosystem_values, 
                               names.arg = c("Italy", "North", "Centre", "South"), 
                               col="dark green", ylab="Urban Ecosystem Synthetic Index",
                               ylim =c(0, 0.7) )
title("Barplot of Urban Ecosystem")
abline(h=0.53)
```

For Urban Ecosystem synthetic index, the aim has been to highlight the difference in data density distribution between the North, Center, and South compared to the national Italian average. Therefore, four histograms were created.

```{r, eval=TRUE, echo=F}
par(mfrow=c(2,2))

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
```

When it comes to the Northern table, the data density curve is better-positioned compared to the national mean, meaning that the best situation can be found there.
Instead, central region follows the general italian distribution with a good overlap.
Finally, around 5% of Southern provinces have extremely low values, between 0.2 and 0.3. Little more of 50% of them have values ranging between 0.3 and 0.6 with the maximum density around 0.45, which still falls below the national italian mean. Instead, only a small percentage, less than 5% has good values ranging between 0.7 and 0.8.
This confirm what has been previously stated: the situation among the Southern provinces is critical almost everywhere, excluding few cities.

# Correlation between variables
In this section, a study on the correlation between indicators within the dataset was conducted, aiming to identify any potential links between them.

- Air quality and Urban green 
- Pedestrian areas and Motorization rate
- Urban ecosystem and Air quality
- Motorization rate and Cycling lanes

### Air quality and Urban green
It is known that trees play a central role in improving air quality in cities, so it was expected to get a negative correlation between urban green and the concentration of pollutants and particular matter. 
However, by plotting the two variables together a slightly positive correlation was obtained instead (cor=0.13).
```{r, eval=TRUE}
model_italy <- lm(newdf$`Urban green`~ newdf$`Air quality`)
plot(x=newdf$`Urban green`, y=newdf$`Air quality`,    
     xlab = "Urban Green", ylab="PM10, NO2 and O3", 
     main="Urban Green and Air quality", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
abline(model_italy)
cor(newdf$`Urban green`, newdf$`Air quality`) #cor=0.13    
```
To understand if the correlation is stastically relevant:

```{r, eval=F}
cor.test(newdf$`Urban green`, newdf$`Air quality`)
```
The p-value obatained is 0.17. In this case: 

- H0 = true correlation is equal to 0;
- H1 = true correlation is not equal to 0.

The p-value does not imply any evidence against H0: the variables are not significantly correlated. 

Also with the functions
```{r}
model_italy <- lm(newdf$`Urban green`~ newdf$`Air quality`)
summary(model_italy)
```
It is observed that the R^2 is very low, so once more they do not seem to be correlated. 

Is this pattern of "no correlaton" the same throughout Italy? To understand that, a deeper analysis among subsets has been made.

The results are the following:

- North: very poor correlation (-0.05), that is once more not statistically relevant (p-value=0.71),
- Centre: no significant correlation, 
- South: good negative correlation of -0.44, stastically relevant (p-value=0.005). 

The strong evidence against the null hypotesis implies that for the Southern provinces the two variables are correlated. 
The correlation can be appreciated in the hereunder present scatterplot.
```{r, eval=TRUE, echo=F}
model_mezzogiorno <- lm(Mezzogiorno$`Urban green`~ Mezzogiorno$`Air quality`)
plot(x=Mezzogiorno$`Urban green`, y=Mezzogiorno$`Air quality`, 
     xlab = "Urban Green", ylab="PM10, NO2 and 03", 
     main="Urban Green and Air quality, South", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
abline(model_mezzogiorno)
```

The regression line obtained is:

- Urban Green=50.21-0.56 x [PM10, NO2 and O3 concentration]

However, the coefficient of determination R^2 obtained with the function "lm" was 0.20, so the regression line explains only the 20% of the variability of the pollutants' concentrations. 

Still, the hypotesis is that the more direct correlation between urban green and pollutants in the South is due to a lower presence of factories and industries. Probably, the main emissions in the southern cities are due to traffic, and the pollutants can be more easily absorbed by urban green (when there is a decent amount of trees in the city). 
In the North, probably there are more sources of pollutants that can not be absorbed by urban green, so the correlation is not that direct.

### Pedestrian areas and Motorization rate

As second correlation, the relationship between the variables "Pedestrian areas" and "Motorization rate" was studied. Does an area with more pedestrian areas have less motorization rate?

The scatterplot for the whole Italy is:

```{r, eval=TRUE, echo= FALSE}
model_italy_pa_mr<-lm (newdf$`Pedestrian areas`~newdf$`Motorization rate`)
plot(x=newdf$`Pedestrian areas`, y=newdf$`Motorization rate`, xlab = "Pedestrian areas", ylab="Motorization rate", 
     main="Pedestrian areas and Motorization rate", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
abline(model_italy_pa_mr)
```

From the function "cor" the value of linear correlation coefficient obtained is r=-0.18. The p-value the function "cor.test" provided is 0.07, that implies weak evidence against H0.
Once more, the null hypothesis states that the true correlation is equal to 0, so the two variable may be weakly correlated. The coefficient of determination, however, is pretty low (R^2=0.03), so the regression line would explain only the 3% of the total variability.

As before, the correlation within the different geographical areas was made. 
By correlating the data from the North of Italy, the r value obtained is -0.39, with a p-value of 0.007 (strong evidence against H0), so the link within this two variable in the North is consistent. 
Furthermore, also the coefficient of determination was higher (0.15). 

About the Centre and the South of the peninsula, such clear values were not found: the linear correlation coefficient is almost null for the centre (0.009) and also very low for the South (-0.07).

### Air quality and Urban Ecosystem
A third correlation between the two indicator Air Quality and Urban Ecosystem was made.
The aim was to understand if, whenever a good air quality is present, a better Urban Ecosystem value is obtained.

The results are the following:

- Italy: no general correlation (0.05), the cor test confirms it with the p-value.
- North: a poor negative correlation (-0.3). Though, this value is not statically relevant, because according to the p-value there's no evidene against H0.
- Centre: no correlation (0.06), the cor test confirms it with the p-value.
- South: good negative correlation (-0.6), the cor test confirms it with the p-value. 

These can be read as follow: while there's no general correlation throughtout the peninsula, which is confirmed by Northern and Central data analysis, when it comes to the Southern ones, the trend is different.
In fact, a good negative correlation is found, meaning that with the increasing of PM10, O3 and NO2 concentration, the Urban Ecosystem value decreases.
This is probably due to the fact that in the Southern provinces there is a less massive presence of industries and so the improvement of air quality bring to the improvement of the variable Urban Ecosystem.
However, since in this study a "Presence of industries" index is not taken into account, the previous can be seen only as a possible explanation. Further study should be done.

```{r, eval=TRUE, echo= FALSE}
plot(x=newdf$`Air quality`, y=newdf$`Urban ecosystem`,    
     xlab = "Air quality", ylab="Urban ecosystem", 
     main="Air quality and U.E.", 
     cex.main=1.4, font.main=2, 
     col.main="navyblue")

plot(x=Mezzogiorno$`Air quality`, y=Mezzogiorno$`Urban ecosystem`,    
     xlab = "Air quality", ylab="Urban ecosystem", 
     main="Air quality and U.E., South", 
     cex.main=1.4, font.main=2, 
     col.main="navyblue")
```

### Motorization rate and Cycling lanes
A final correlation analysis was conducted between two indicators: Motorization rate and the presence of cycling lanes. The objective of this study was to determine if there exists a correlation between the motorization rate and the availability of cycling lanes. The idea was that a higher presence of cycling lanes would correspond to a lower motorization rate, similar to the study conducted for pedestrian areas.

The correlation value across all Italy is -0.07, and the p-value obtained from the "cor.test" function is 0.45, suggesting that there is no evidence against the null hypothesis (H0), so the two variables do not seem to be correlated.

```{r, eval=TRUE, echo= FALSE}
plot(x=newdf$`Motorization rate`, y=newdf$`Cycling lanes`,    
     xlab = "Motorization rate", ylab="Cycling lanes", 
     main="Motorization rate and Cycling lanes", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
```
In the remaining regions of the peninsula:

- North: a poor positive correlation was found (0.32) with a p-value of 0.03, indicating moderate evidence against the null hypothesis (H0).
- Centre: there is no correlation (-0.048) with a p-value of 0.83, suggesting that it is not statistically significant.
- South: there is also no correlation (0.009) with a p-value of 0.96, indicating no statistical relevance.

```{r, eval=TRUE, echo= FALSE}   
# different areas of Italy
plot(x=Nord$`Motorization rate`, y=Nord$`Cycling lanes`,
     xlab = "Motorization rate", ylab="Cycling lanes", 
     main="Motorization rate and Cycling lanes Northern Italy", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Nord$`Motorization rate`, y=Nord$`Cycling lanes`) 


plot(x=Centro$`Motorization rate`, y=Centro$`Cycling lanes`,
     xlab = "Motorization rate", ylab="Cycling lanes", 
     main="Motorization rate and Cycling lanes Central Italy", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Centro$`Motorization rate`, y=Centro$`Cycling lanes`)  

plot(x=Mezzogiorno$`Motorization rate`, y=Mezzogiorno$`Cycling lanes`,
     xlab = "Motorization rate", ylab="Cycling lanes", 
     main="Motorization rate and Cycling lanes Southern Italy", 
     cex.main=1.4, font.main=2, 
     col.main="orange")
cor(x=Mezzogiorno$`Motorization rate`, y=Mezzogiorno$`Cycling lanes`)  

```
From the obtained results, it is evident that there is mostly no correlation, except for the case of the North where a poor positive correlation was observed. This suggests that with the increase of cycling lanes, the motorization rate is also increasing. This outcome is contrary to our expectations, indicating a need for further studies to understand the underlying reasons for this trend.

# Inference

For the inferencial analysis, the work was done assuming that the provences are stastical units coming from a random sample and assuming it is possible to broaden the results obtained thanks to the values of the main cities to the whole geographical areas.
Given the observed patterns of differences between provinces within the same indicators, the goal is to validate these results through hypothesis testing and subsequently generalize them to whole Italy.

The following packages were used:
```{r, eval=FALSE}
library(EnvStats)
library(BSDA)
```
and for every subset the Shapiro test was performed in order to understand the normality of the data, and subsequently performing a T test or a Z test. 
We performed the t.test every time we obtained normality in the population data, taking into account the fact that the variance is unknown.
We performed the z.test every time we did not obtain normality in the population data, knowing that it can assumed that the data follow a normal distribution in case of large samples (n>30). 
However, the subset of the central Italy has not a sample size large enough (n=22), so it was possible to perform the z test only for comparing the population of the North (n=47) and of the South (n=38) of Italy.

### Inference on the variable "Cycling lanes"

By looking at the data of the variable "Cycling lane", it is already possible to note some important differences among the peninsula. The mean values decreases going southern, but the aim in this part was to test if the differences are statistically relevant. 
The Shapiro test was performed to check whether the subset data are normally distributed. 
```{r, eval=FALSE}
shapiro.test(Nord$`Cycling lanes`)
shapiro.test(Mezzogiorno$`Cycling lanes`)
shapiro.test(Centro$`Cycling lanes`)
```
In all the three cases a low p-value was found, meaning that all the data come from a non-normal distribution. 
As already stated in the introduction, it is possible to perform the z test even if the variance is unknown if we have a sample size large enough. 
It is possible in this case to make inference on the difference between the population means between the North and the South. 
In order to perform the z test, it is necessary to have the standard deviation of the two samples, that can be calculated using the function var() that provides the unbiased sample variance and then it is possible to get the standard deviation by using the function sqrt().
```{r, eval=FALSE}
z.test(x=Nord$`Cycling lanes` , y = Mezzogiorno$`Cycling lanes`,
       alternative = "two.sided", sigma.x=10.09, sigma.y=6.06, mu=0, conf.level = 0.99)
```
With this test:

- H0=true difference between population means is equal to 0
- H1=true difference between population means is not equal to 0

An extremely low p-value (8.17^-09) was obtaind, so the difference between the amount of Cycling lanes in the North and the South is stastically significative with a 99% Confidence Interval. 

### Inference on the variable "Motorization rate" 

With the variable "Motorization rate" we found an opposite trend: higher values in the South and lower values in the North. 
This time, with the Shapiro test, we discovered the datas in the subset North follow a non-normal distribution, whereas in the Centre and in the South follow a normal distribution. 
Therefore, we could perform the T test within the difference of means of Centre and South
```{r, eval=FALSE}
t.test(x=Mezzogiorno$`Motorization rate` , y = Centro$`Motorization rate`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99)
```
but this time obtaining an high p-value (0.6167), implying no evidence against H0, so basically there is not significative difference among these two geographical areas.

### Inference on the variable "Urban green"

With the variable "Urban green," it was observed that the means decrease moving from the North to the Center and the South. Do the mean values significantly differ across these regions? To achieve this, by applying the Shapiro test it was assested if the data are normally distributed or not. 
```{r, eval=FALSE}
shapiro.test(Nord$`Urban green`) #not normal distribution
shapiro.test(Mezzogiorno$`Urban green`) #not normal distribution
shapiro.test(Centro$`Urban green`) #normal distribution
```

Since a normal distribution was identified only in the center, while the north and south exhibit non-normal distributions, a z-test was conducted between the latter two. The unbiased variance was calculated and used for the z-test to assess the difference between means in the north and south.
```{r, eval=FALSE}
var(Nord$`Urban green`)
sqrt(var(Nord$`Urban green`))
var(Mezzogiorno$`Urban green`)
sqrt(var(Mezzogiorno$`Urban green`))

z.test(x=Nord$`Urban green` , y = Mezzogiorno$`Urban green`, alternative = "two.sided",
       sigma.x=24.07, sigma.y=9.02, mu=0, conf.level = 0.99)
#p-value = 7,585e-05 

```

The obtained very small p-value (7.585e-05) provides strong evidence to reject the null hypothesis (H0). Consequently, it is possible conclude with a 99% confidence interval that the difference between the two means is not equal to 0.

### Inference on the variable "Pedestrian Areas"
Since the variable pedestrian areas has not a normal distribution, a z-test has been applied for large sample (>30).
This has be successfully done for the Northern and the Southern provinces, but not for the Central one, where n = 22.

In order to apply a z.test, the unbiased sample variance must be calculated. This is done as follow:
```{r, eval=FALSE}
Nord_var <- var(Nord$`Pedestrian areas`)
Centro_var <- var(Centro$`Pedestrian areas`)
Sud_var <- var(Mezzogiorno$`Pedestrian areas`)
```

```{r, eval=FALSE}
Nord_usv <-sqrt(Nord_var)
Centro_usv <-sqrt(Centro_var)
Sud_usv <-sqrt(Sud_var)
```
A z.test for the variable "Pedestrian Areas" can be applied:
```{r, eval=FALSE}
z.test(x=Nord$`Pedestrian areas`, y = Mezzogiorno$`Pedestrian areas`, alternative = "two.sided",
       mu=0, sigma.x= Nord_usv, sigma.y= Sud_usv, conf.level = 0.99) # p-value 0.11
```
According to the p-value obtained, no differences in population means are shown with a 99% confidence interval. 

### Inference on the variable "Urban Ecosystem"
When it comes to Urban Ecosystem, since a normal distribution is followed, a t-test can be applied.
```{r, eval=FALSE}
t.test(x=Nord$`Urban ecosystem` , y = Mezzogiorno$`Urban ecosystem`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99) #4x10^(-8) strong evidence against H0
t.test(x=Nord$`Urban ecosystem` , y = Centro$`Urban ecosystem`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99) #0.0166 strong evidence against H0
t.test(x=Mezzogiorno$`Urban ecosystem` , y = Centro$`Urban ecosystem`, alternative = "two.sided",
       mu=0, var.equal=T, conf.level = 0.99) #0.009 strong evidence against H0
```
As expected, H0 is rejected, since the mean values for Urban Ecosystem for North, Centre and South are diffent within eachothers.

# Conclusion

Our project's aim was to understand the Italian situation regarding some environmental parameters and to discover if there are patterns along the Peninsula. 

Overall, the environmental state of the Italian cities can be definitely improved: the indicators underline a critical situation for all the parameters. 
Furthermore, the gap between North-Centre-South is almost always visible and consistent in every aspect analysed. 

The need of creating sustainable cities, both for human life and for nature, is getting more and more urgent. 
Thanks to the indicators we can understand where is more important to act, both in terms of parameters and in terms of geographical area.
However, further researches and studies should be carried out to fully investigate how to improve the general situation and also meet SDGs standards.

# Sitography
- https://github.com/IlSole24ORE/QDV2022/blob/main/20221213_QDV2022_001.csv
- https://lab24.ilsole24ore.com/qualita-della-vita/tabelle/2022/classifica-finale 
- https://www.tuttitalia.it/statistiche/nord-centro-mezzogiorno-italia/
