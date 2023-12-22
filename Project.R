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

mean(Nord$`Urban ecosystem`)
mean(Centro$`Urban ecosystem`)
mean(Mezzogiorno$`Urban ecosystem`)

