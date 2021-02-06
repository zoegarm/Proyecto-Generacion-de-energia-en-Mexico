##Curso Data Science Santander-BEDU | Módulo 2: R
## Equipo 2: Zoé Ariel García Martínez, Gerardo Miguel Pérez Solis, Atenea De La Cruz Brito
##Proyecto: Generación de energía en México: alternativas limpias

########################## I. Inicio ##########################
#Librerías
library(DescTools)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)


#Directorio de trabajo
getwd()
setwd("C:/Users/Familia Solis/Documents/Documentos de Gerardo/CURSO DATA SCIENCE/Proyecto") #Cambiar según usuario


#Realizo apertura de los datos .dbf convertidos a .csv
rank <- read.csv("solargis_pvpot_countryrank_2020.csv")
head(rank); tail(rank); names(rank); class(rank); str(rank); summary(rank)

gen <- read.csv("mx_inventory_gen_new.csv")
head(gen); tail(gen); names(gen); class(gen); str(gen); summary(gen)

pot <- read.csv("mx_inventory_pot_new.csv")
head(pot); tail(pot); names(pot); class(pot); str(pot); summary(pot)

dni <- read.csv("nsrdb_mx_dni_new.csv")
head(dni); tail(dni); names(dni); class(dni); str(dni); summary(dni)

#Seleccionar y renombrar columnas a emplear
rank <- select(rank, iso = ISO_A3, country = Country, region = WorldBankRegion,
               theoghi = Average_theoretical_potential_GHI_kWh_m2dayLongterm,
               pracpvout = Average_practical_potential_PVOUT_Level1_kWh_kWdayLongterm,
               avlcoe = Average_economic_potential_LCOE_USD_kWh2018,
               pvpc = AveragePVseasonality_index_longterm)
head(rank); tail(rank); length(rank)

#promedio  de generacion por planta
count(gen,plant_type)

(gen.mean.estado <- aggregate(gengwh~plant_type,FUN =mean,data = gen))
ggplot(gen.mean.estado,aes(x=gengwh,y=plant_type,fill=plant_type))+ 
    geom_bar(stat = "identity",position ="stack",col="black")+
    ggtitle("Generacion electrica (GWh) promedio por tipo de planta")+
    xlab("Generación en GWh")+ylab("Planta")+
    labs(fill = "Tipo Planta")

#promedio de generacion por estado y planta
count(gen,plant_type,ESTADO)

gen.mean.edo.plant <- aggregate(gengwh~plant_type+ESTADO,FUN =mean,data = gen)

ggplot(gen.mean.edo.plant,aes(x=gengwh,y=ESTADO,fill =plant_type))+
    geom_bar(stat = "identity",position ="stack",col="black")+
    ggtitle("Generación promedio por estado ")+
    xlab("Generación en GWh")+ylab("Estado")+
    labs(fill = "Tipo Planta")

#promedio de potencial por planta
count(pot,plant_type)

(pot.mean.plant <- aggregate(potencial~plant_type,FUN =mean,data = pot))
ggplot(pot.mean.plant,aes(x=potencial,y=plant_type,fill =plant_type))+
    geom_bar(stat = "identity",position ="stack",col="black")+
    ggtitle("Potencial promedio por tipo de planta")+
    xlab("Potencial")+ylab("Planta")+
    labs(fill = "Tipo Planta")

#promedio de potencial por estado y planta
count(pot,plant_type,ESTADO)

pot.mean.edo.plant <- aggregate(potencial~plant_type+ESTADO,FUN =mean,data = pot)
baja_california <- pot.mean.edo.plant[pot.mean.edo.plant$ESTADO=="Baja California",]
otros_edos <- pot.mean.edo.plant[pot.mean.edo.plant$ESTADO!="Baja California",]

ggplot(otros_edos,aes(x=potencial,y=ESTADO,fill =plant_type))+
    geom_bar(stat = "identity",position ="stack",col="black")+
    ggtitle("Potencial promedio por estado ")+
    xlab("Potencial en GWh")+ylab("Estado")+
    labs(fill = "Tipo Planta")

ggplot(baja_california,aes(x=potencial,y=ESTADO,fill =plant_type))+
    geom_bar(stat = "identity",position ="stack",col="black")+
    ggtitle("Potencial promedio de Baja California")+
    xlab("Potencial en GWh")+ylab("Estado")+
    labs(fill = "Tipo Planta")
