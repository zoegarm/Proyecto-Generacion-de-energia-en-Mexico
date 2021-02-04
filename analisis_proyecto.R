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
setwd("C:/Users/AteneaHP/Documents/2020 MOOCs/2021 BEDU Project") #Cambiar según usuario


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


########################## II. Análisis Exploratorio de Datos ##########################

#Medidas de dispersión: Rango Intercuartílico, Varianza y Desviación Estándar
IQR(rank$avlcoe)
var(rank$avlcoe)
sd(rank$avlcoe)

IQR(gen$gengwh)
var(gen$gengwh)
sd(gen$gengwh)

IQR(pot$potencial)
var(pot$potencial)
sd(pot$potencial)

IQR(dni$dni)
var(dni$dni)
sd(dni$dni)

hist(rank$avlcoe,
     main = "Histograma potencial económico LCOE",
     xlab = "avLCOE",
     ylab = "Frecuencia")

hist(gen$gengwh,
     main = "Histograma generación eléctrica GWh",
     xlab = "gengwh",
     ylab = "Frecuencia")

hist(pot$potencial,
     main = "Histograma potencial eléctrico GWh",
     xlab = "potencial",
     ylab = "Frecuencia")

hist(dni$dni,
     main = "Histograma irradiación normal diaria",
     xlab = "dni",
     ylab = "Frecuencia")

(scatplot_theo <- ggplot(rank, aes(x=rank$theoghi, y=rank$pracpvout)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T))
scatplot_theo + xlab('Potencial teórico medio') + ylab('Potencial práctico medio')

(scatplot_avpv <- ggplot(rank, aes(x=rank$pvpc, y=rank$pracpvout)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T))
scatplot_avpv + xlab('Indice PV estacional') + ylab('Potencial práctico medio')

##### NOTA ATENEA: AQUÍ ME QUEDÉ, ME FALTA AGREGAR BOXPLOTS Y MEJORAR PRESENTACIÓN HISTOGRAMAS ###
########################## III.  ##########################

#reviso qué tantas veces aparece un estado en gen

group_by(gen, ESTADO) # <- hay 27 estados listados

cuentaEstado <- count(gen, ESTADO) # <- cuento qué tantas veces se repite un estado

cuentaEstado[cuentaEstado$n == max(cuentaEstado$n), ] # <- el estado más repetido es Veracruz

cuentaEstado[cuentaEstado$n == min(cuentaEstado$n), ] # <- el estado que menos aparece

#Ahora hago un análogo con las plantas que se tienen listadas en gen: 

group_by(gen, plant_type) # <- hay 5 tipos de plantas

cuentaPlanta <- count(gen, plant_type) # <- enlisto los tipo de plantas

# tenemos 96 plantas de poder hídrico funcionando...

################################################################################
###
##
#

#Ahora voy a comparar estos datos con los de la BD pot

group_by(pot, ESTADO) # <- aquí se enlistan 32 estados potenciales para tener plantas generadoras

cuentaEstadoP <- count(pot, ESTADO)

cuentaEstadoP[cuentaEstadoP$n == max(cuentaEstadoP$n), ] # <- el EDO que aparece más veces es jalísco

cuentaEstadoP[cuentaEstadoP$n == min(cuentaEstadoP$n), ] #el EDO que aparece menos es Tlaxcala

#ahora analizo las plantas que se tienen listadas en pot: 

group_by(pot, plant_type) # <- hay 5 tipos de plantas

cuentaPlantaP <- count(pot, plant_type) # <- enlisto los tipo de plantas

#la planta en pot que se repite más veces es la geotérmica con 1089 veces.