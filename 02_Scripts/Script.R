library(ggplot2)
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
datos_covidmx
datos_covid_qro_act_8_04_2022 <- filter(datos_covid_qro, ENTIDAD_UM == 22)
positivos_act <- filter(datos_covid_qro_act_8_04_2022, CLASIFICACION_FINAL == 1 | 
                      CLASIFICACION_FINAL == 2 |
                      CLASIFICACION_FINAL == 3 )
pos <- c()
for (i in 1:length(positivos_act$FECHA_SINTOMAS) ) {
  pos <- c(pos,1)
}
positivos_act <- mutate(positivos_act, positivos = pos)
p <- aggregate(positivos~FECHA_SINTOMAS, data = positivos_act,
               FUN = sum)
p[,3] <- c(1:length(p$FECHA_SINTOMAS))
colnames(p)[3] <- "num.dia"
print(p)
