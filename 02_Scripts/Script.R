library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)
source("01_Raw_Data/Raw_data.R")
source("03_Functions/Functions.R")

#############################################
## Base inicial
###

tot <- mutate(datos_covid_qro, rango_edad = rangos_edades(datos_covid_qro$EDAD))
# tot contiene todos los datos de covid incluido RANGO DE EDAD hasta diciembre 
# de 2021.

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
# positivos contiene todos los casos POSITIVOS de la base de datos 2021

re <- rangos_edades(positivos$EDAD)
# re es un vector que contiene los rangos de edades de todos los casos positivos

positivos_re <- mutate(positivos, rango_edad = re)
# positivos_re contiene los CASOS POSITIVOS a Covid incluido el rango de edad.

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
# leve contiene solamente los TIPO_PACIENTE = 1, que es caso ambulatorio
p_l <- probabilidades(leve, positivos_re)
p_l

######## Infectado a Grave (Hospitalizado) #########

hosp<- filter(positivos_re, TIPO_PACIENTE == 2)
# hosp contiene solamente los TIPO_PACIENTE = 2, que es caso hospitalizado
p_h <- probabilidades(hosp, positivos_re)
p_h

######## Grave (Hospitalizado) a Intubado (ICU) #########

int <- filter(positivos_re, INTUBADO == 1)
# int contiene solamente pacientes INTUBADOS
p_i <- probabilidades(int, positivos_re)
p_i

######## Intubado (ICU) a Muerte #########

positivos_m <- mutate(positivos_re, muerte = c
                      ( ifelse( !is.na( positivos_re$FECHA_DEF ), 
                                "Muerte", "No muerte") ) )
# positivos_m contiene una columna extra que indica el rango de edad +++++++
# si el paciente murió o no
muerte <- filter(positivos_m, muerte == "Muerte")
# muerte contiene solamente las personas registradas que fallecieron
p_m<- probabilidades(muerte, positivos_re)
p_m


## TABLA CONJUNTA ===========================================
p_t <- cbind(p_p, p_l, p_h, p_i, p_m)
colnames(p_t) <- c("Suceptible --> Infectado",
                      "Infectado --> Ambulatorio",
                      "Infectado --> Grave",
                      "Grave --> ICU",
                      "ICU --> Muerte")
p_t
# p_t es la tabla de probabilidades de transición de los distintos estados 
# definidos en el modelo

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

#==============================================================================


## ACTUALIZACION DE DATOS AL 8-04-2022=======================================++
# datos_covidmx #base de datos de Mexico
# datos_covid_qro_act_8_04_2022 <- filter(datos_covidmx, ENTIDAD_UM == 22) #datos de qro actualizados
# save(datos_covid_qro_act_8_04_2022, file = "01_Raw_Data/datos_covid_qro_actualizados.RData")
datos_covid_qro_act_8_04_2022
# datos_covid_qro_act_8_04_2022 contiene los datos crudos actualizados a la
# fecha indicada
re3 <- rangos_edades(datos_covid_qro_act_8_04_2022$EDAD)
datos_covid_qro_act_8_04_2022_re <- mutate(datos_covid_qro_act_8_04_2022, rango_edad = re3)
#datos_covid_qro_act_8_04_2022_re cotneiene los datos actualizados y se agrega
#la clasificación de los rangos de edad

### filtra los datos actualizados a solamente casos positivos
positivos_act <- filter(datos_covid_qro_act_8_04_2022, CLASIFICACION_FINAL == 1 | 
                      CLASIFICACION_FINAL == 2 |
                     CLASIFICACION_FINAL == 3 )
# positivos_act cotneien solamente los casos positivos actualziados + rangos 
# de edad

#####
pos <- c() #crea un vector vacio
for (i in 1:length(positivos_act$FECHA_SINTOMAS) ) {
   pos <- c(pos,1) } # por cada uno de los positivos, coloca un 1 en el vector-
positivos_act <- mutate(positivos_act, positivos = pos) # genera una nueva columna en la base de los positivos
# la nueva columna la rellena con el vector de 1's creado. Hay un 1 en todos los renglones
# Suma todos los positivos de un solo fía por fecha de inicio de sintomas
positivos_conteo <- aggregate(positivos~FECHA_SINTOMAS, data = positivos_act, 
                              FUN = sum)
# Genera otra columna en elobjeto
positivos_conteo [,3] <- c(1:length(positivos_conteo$FECHA_SINTOMAS))
colnames(positivos_conteo)[3] <- "num.dia" # agrega el numero de dia a la columna 3
positivos_conteo
# positivos_conteo es una tabla que contiene la FECHA DE INICIO DE SINTOMAS y
# los casos positivos de ese día, así como una seriación de días que lleva
# desde el primer caso positicvo

#####

re2 <- rangos_edades(positivos_act$EDAD)
positivos_act <- mutate(positivos_act, rango_edad = re2)
positivos_act <- mutate(positivos_act, muerte = c
                                ( ifelse( !is.na( positivos_act$FECHA_DEF ), 
                                          "Muerte", "No muerte") ) )
positivos_act
# positivos_act contiene los casos positivos actualizados + 
# rango de edad +
# una columna de 1's que indica que son 1 posiivo +
# una columna que indica si el paciente falleció o no

# Esta grafica contiene EL TOTAL de positivos por fecha de inciio de síntomas separado
# por rango de edades
ggplot(positivos_act, aes(x = FECHA_SINTOMAS, y = rango_edad, fill = 0.5 - 
                   abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)
##==============================================

tot_jbr <- mutate(tot, individuo = 1)
str(tot_jbr)
# tot_jbr contiene la base de datos COMPLETA + rangos de edad + 
# una columna de 1's para indicar que son 1 persona
tot_jbexf <- aggregate(tot_jbr$individuo, by = list(tot_jbr$rango_edad, tot_jbr$FECHA_SINTOMAS), FUN = sum)
colnames(tot_jbexf) <- c("Rango de Edad", "FECHA_SINTOMAS", "Casos totales")
tot_jbexf ### Se obtiene una tabla con las personas detectadas por dia y por rango de edad. No contempla 
# su estado (positivos, negativos, hospitalizados, intubados, etc)
###===================+++

###==================++++
## GRAFICA DE LOS DATOS ACTUALIZADOS CON CASOS POSITIVOS ACUMULADOS 
plot_positivos_act <- ggplot(positivos_act, 
                             aes(x=FECHA_SINTOMAS, y = EDAD, fill = rango_edad)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Casos positivos a COVID ACTUALIZADOS por rangos de edades 
          para el estado de Queretaro") + 
  labs(x="Tiempo", y="Casos") +
  labs(fill="Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T)
plot_positivos_act
## Se van a obtener las probabilidades de los casos recientes. De septiembre de 
#  2021 a la fecha de actualización de los datos
#-----------------------------
totales_act_recientes <- filter(datos_covid_qro_act_8_04_2022_re, FECHA_SINTOMAS > "2021-09-01")
# totales_act_recientes contiene los datos TOTALES de los datos actualizados
# pero solo desde el 1 de septiembre del 2021 al 7 de abril del 2022
positivos_act_recientes <- filter(positivos_act, FECHA_SINTOMAS > "2021-09-01")
# positivos_act_recientes contiene los datos de casos POSITIVOS de los datos actualizados
# pero solo desde el 1 de septiembre del 2021 al 7 de abril del 2022
#----------##
# Probabilidades recientes
######## Suceptible a Infectado ##########
p_p_rec <- probabilidades(positivos_act_recientes, totales_act_recientes)
p_p_rec
######## Infectado a Leve (Ambulatorio) #########
leve_rec <- filter(positivos_act_recientes, TIPO_PACIENTE == 1)
p_l_rec <- probabilidades(leve_rec, positivos_act_recientes)
p_l_rec
######## Infectado a Grave (Hospitalizado) #########
hosp_rec <- filter(positivos_act_recientes, TIPO_PACIENTE == 2)
p_h_rec <- probabilidades(hosp_rec, positivos_act_recientes)
p_h_rec
######## Grave (Hospitalizado) a Intubado (ICU) #########
int_rec <- filter(positivos_act_recientes, INTUBADO == 1)
p_i_rec <- probabilidades(int_rec, positivos_act_recientes)
p_i_rec
######## Intubado (ICU) a Muerte #########
positivos_m_rec <- mutate(positivos_act_recientes, muerte = c
                      ( ifelse( !is.na( positivos_act_recientes$FECHA_DEF ), 
                                "Muerte", "No muerte") ) )
muerte_rec <- filter(positivos_m_rec, muerte == "Muerte")
p_m_rec <- probabilidades(muerte_rec, positivos_act_recientes)
p_m_rec
##==============================================
# Tabla conjunta de datos RECIENTES del 01-sep-2021 a los datos actuales de las 
# probabilidades de transición
p_t_rec <- cbind(p_p_rec, p_l_rec, p_h_rec, p_i_rec, p_m_rec)
colnames(p_t_rec) <- c("Suceptible --> Infectado",
                   "Infectado --> Ambulatorio",
                   "Infectado --> Grave",
                   "Grave --> ICU",
                   "ICU --> Muerte")
p_t_rec
p_t


################=============================--------------------------------
##
##     GRAFICA DE LAS MUERTES
##
##
positivos_m_t <- mutate(datos_covid_qro_act_8_04_2022_re, muerte = c
                          ( ifelse( !is.na( datos_covid_qro_act_8_04_2022_re$FECHA_DEF ), 
                                    "Muerte", "No muerte") ) )
positivos_m_t
ggplot(positivos_m_t, 
       aes(x = FECHA_SINTOMAS,
           y = rango_edad,
           fill=muerte)) +
  geom_density_ridges2(alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

#### FECHAS DE VACUNACION
# Diciembre 2020 - Febrero 2021 : Personal de sail
# Febrero - Mayo 2021 : 60+
# Mayo - Junio 2021 : 50 - 59 
# Junio - Julio 2021 : 40 - 49 
# Julio 2021 - Marzo 2022 : resto 

vac <- fechas_vacunacion(positivos_m_t$FECHA_SINTOMAS)

positivos_m_t <- mutate(positivos_m_t, FECHAS_VACUNACION = vac)
## Ahora, positivos_m_t contiene una columna extra donde se indica la fase de 
## vacunación, de acuerdo a lo0s datos obtenidos de la página de la secretaría de
## salud.

pdf("04_Output/Plots/positivos_muerte_vacunacion.pdf")
plot_positivos_m_t <- ggplot(positivos_m_t, 
       aes(x = FECHA_SINTOMAS,
           y = rango_edad,
           col = muerte,
           fill = FECHAS_VACUNACION)) +
  geom_density_ridges2(alpha = 0.5, size = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(y = "Rangos de edades", x = "Tiempo") +
  ggtitle("Densidad de casos positivos que murieron y que no muerieron con las fechas de vacunación")
dev.off()

## la nueva gráfica nos muestra la densidad de individuos que murieron y los que
## no muerieron, resaltando las fases de vacunación en la que se encuentraba para
## la fecha del inicio de síntomas.