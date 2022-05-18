library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
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


## TABLA CONJUNTA ===========================================
p_t <- cbind(p_p, p_l, p_h, p_i, p_m)
colnames(p_t) <- c("Suceptible --> Infectado",
                      "Infectado --> Ambulatorio",
                      "Infectado --> Grave",
                      "Grave --> ICU",
                      "ICU --> Muerte")

## Correlación de las probabilidades =======================

heatmap_p_t <- heatmap(cor(t(p_t)))

## Analisis de clasificación ===============================
d_p_t <- dist(p_t)
clu <- hclust(d_p_t, method = "complete", members = NULL)
clu_p <- plot ( as.phylo  (clu), type = "phylogram" )

### Robustez de la clasificacion

agroup1 <- hclust(d_p_t, method = "complete", members = NULL)
agroup2 <- hclust(d_p_t, method = "ward.D", members = NULL)
agroup3 <- hclust(d_p_t, method = "ward.D2", members = NULL)
agroup4 <- hclust(d_p_t, method = "single", members = NULL)
agroup5 <- hclust(d_p_t, method = "average", members = NULL)
agroup6 <- hclust(d_p_t, method = "mcquitty", members = NULL)
agroup7 <- hclust(d_p_t, method = "median", members = NULL)
agroup8 <- hclust(d_p_t, method = "median", members = NULL)
agroup9 <- hclust(d_p_t, method = "centroid", members = NULL)
#   3) Graficar usando la funcion: plot ()
#      Para visualizar llos analisis usando layout
layout ( matrix ( c( 1 : 9 ), 3, 3))
plot(agroup1)
plot(agroup2) 
plot(agroup3)
plot(agroup4)
plot(agroup5) 
plot(agroup6) 
plot(agroup7)
plot(agroup8)
plot(agroup9)
layout(matrix (c (1), 1, 1))

## ACTUALIZACION DE DATOS AL 8-04-2022=======================================++
# datos_covidmx #base de datos de Mexico
# datos_covid_qro_act_8_04_2022 <- filter(datos_covidmx, ENTIDAD_UM == 22) #datos de qro actualizados
# save(datos_covid_qro_act_8_04_2022, file = "01_Raw_Data/datos_covid_qro_actualizados.RData")
datos_covid_qro_act_8_04_2022

### filtra los datos actualizados a solamente casos positivos
# positivos_act <- filter(datos_covid_qro_act_8_04_2022, CLASIFICACION_FINAL == 1 | 
#                      CLASIFICACION_FINAL == 2 |
#                     CLASIFICACION_FINAL == 3 )
#####
# pos <- c() #crea un vector vacio
# for (i in 1:length(positivos_act$FECHA_SINTOMAS) ) {
#   pos <- c(pos,1)
# } # por cada uno de los positivos, coloca un 1 en el vector-
# positivos_act <- mutate(positivos_act, positivos = pos) # genera una nueva columna en la base de los positivos
# la nueva columna la rellena con el vector de 1's creado. Hay un 1 en todos los renglones
# Suma todos los positivos de un solo fía por fecha de inicio de sintomas
# positivos_conteo <- aggregate(positivos~FECHA_SINTOMAS, data = positivos_act, 
#                              FUN = sum)
# Genera otra columna en elobjeto
# positivos_conteo [,3] <- c(1:length(positivos_conteo$FECHA_SINTOMAS))
# colnames(positivos_conteo)[3] <- "num.dia" # agrega el numero de dia a la columna 3
positivos_conteo
# re2 <- rangos_edades(positivos_act$EDAD)
positivos_act <- mutate(positivos_act, rango_edad = re2)
positivos_act <- mutate(positivos_act, muerte = c
                                ( ifelse( !is.na( positivos_act$FECHA_DEF ), 
                                          "Muerte", "No muerte") ) )
positivos_act
# Esta grafica contiene EL TOTAL de positivos por fecha de inciio de síntomas separado
# por rango de edades
ggplot(positivos_act, aes(x = FECHA_SINTOMAS, y = rango_edad, fill = 0.5 - 
                   abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)
##==============================================
tot_jbr <- mutate(tot, individuo = 1)
str(tot_jbr)
tot_jbexf <- aggregate(tot_jbr$individuo, by = list(tot_jbr$rango_edad, tot_jbr$FECHA_SINTOMAS), FUN = sum)
tot_jbexf ### Se obtiene una tabla con las personas detectadas por dia y por rango de edad. No contempla 
# su estado (positivos, negativos, hospitalizados, intubados, etc)
##==============================================
## Conteo de suceptibles

## Conteo de Infectados

## Conteo de Hospitalizados

## Conteo de Intubados

## Conteo de Muertes









