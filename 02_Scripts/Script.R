library(ggplot2)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
source("01_Raw_Data/Raw_data.R")
source("03_Functions/Functions.R")

#############################################
## Base inicial
###

tot <- mutate(datos_covid_qro, rango_edad = rangos_edades(datos_covid_qro$EDAD))


## Gráfica apilada de casos positivos a covid por rangos de edades 
## en (18-, 18-29,30-39,40-49,50-59,60-70, 70+)

###############################################################################
##                                                                           ##
##                             G R A F I C A                                 ##
##                                                                           ##
###############################################################################

###### Base de datos #######

positivos <- filter(datos_covid_qro, CLASIFICACION_FINAL == 1 | 
                      CLASIFICACION_FINAL == 2 |
                      CLASIFICACION_FINAL == 3 )

re <- rangos_edades(positivos$EDAD)

positivos_re <- mutate(positivos, rango_edad = re)

######### Grafica ##########

plot_positivos_re <- ggplot(positivos_re, 
                            aes(x=FECHA_INGRESO, y=EDAD, fill = rango_edad)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Casos positivos a COVID por rangos de edades 
          para el estado de Queretaro") + 
  labs(x="Tiempo", y="Casos") +
  labs(fill="Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T)
plot_positivos_re

###############################################################################
##                                                                           ##
##                      P R O B A B I L I D A D E S                          ##
##                                                                           ##
###############################################################################

########## Suceptible a Infectado ##########

p_p <- probabilidades(positivos_re, tot)
p_p

######## Infectado a Leve (Ambulatorio) #########

leve <- filter(positivos_re, TIPO_PACIENTE == 1)

p_l <- probabilidades(leve, positivos_re)
p_l

######## Infectado a Grave (Hospitalizado) #########

hosp<- filter(positivos_re, TIPO_PACIENTE == 2)

p_h <- probabilidades(hosp, positivos_re)
p_h

######## Grave (Hospitalizado) a Intubado (ICU) #########

int <- filter(positivos_re, INTUBADO == 1)

p_i <- probabilidades(int, positivos_re)
p_i

######## Intubado (ICU) a Muerte #########

positivos_m <- mutate(positivos_re, muerte = c
                      ( ifelse( !is.na( positivos_re$FECHA_DEF ), 
                                "Muerte", "No muerte") ) )
muerte <- filter(positivos_m, muerte == "Muerte")

p_m<- probabilidades(muerte, positivos_re)
p_m
