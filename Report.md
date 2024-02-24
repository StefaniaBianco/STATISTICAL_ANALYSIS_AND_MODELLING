---
title: "Does the Italian quality of life depends on the geographical area?"
author: "Gioia Riolli, Stefania Bianco, Mattia Fabris"
date: "2024-02-24"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
getwd()
df <- read.table("/Users/riolli/Desktop/Progetto_province/progetto_province_stat/20221213_QDV2022_001.csv", header=T, sep=",", stringsAsFactors=T)
df
```

## Introduction and Data Clean

First of all, our data have been taken from the "Sole 24 ore" website, where every year all the Italian provinces get ranked from the highest to the lowest for quality of life standards. 
Initially our data were not completely cleaned, so we had to import them and select only the indicators that would have been significant for our study.

Out of 31 indicators, we selected the following 8:
"Province", "Province code", "Cycling lanes", 
"Motorization rate", "Urban green", "Air quality", 
"Pedestrian areas", "Urban ecosystem"

With the selected indicators a new data frame has been created, called "newdf", and all the variables renamed in english.

The new data frame has then been divided into three sections: North, Centre, and the so-called "mezzogiorno" or south, according to the STAT division per regions of the Italian administrative territory.


```{r Indicators Selection}
Nord
Centro
Mezzogiorno
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
