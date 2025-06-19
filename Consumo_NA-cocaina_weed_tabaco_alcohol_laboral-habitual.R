# Variable dependiente: Consumo habitual de cocaína
# Variables independientes: Consumo habitual de marihuana, consumo habitual de
#                           bebidas alcohólica, consumo habitual de tabaco,
#                           situación laboral (pertenencia a PEA o PEI), edad,
#                           sexo, nivel educativo (bachillerato o menos, 
#                           educación superior completa o incompleta, posgrado)
#
# El valor 'No aplica' en el consumo habitual de las sustancias psicoactivas
# fue reemplazado por 'No', ya que las personas para quienes no aplica la 
# pregunta de consumo habitual no han consumido anteriormente la droga.

rm(list = ls())


#### Importación de librerías ####
# library(pacman)
pacman::p_load(questionr,tidyverse,mice,sandwich,magrittr,forcats)
pacman::p_load(DescTools,lmtest,graphics,car,psych,ggplot2,haven,stargazer)
pacman::p_load(dplyr,pscl,vcd,gridExtra, readxl)


#### Importación de datos ####
AllData <- read_excel("AllData.xlsx")



#### Selección de variables ####
vbles = c('P1_E','P3_S','P4_ES','P6_NE','P13_Marihuana_Habitual',
          'P13_Cocaina__perico_Habitual','P13_Bebidas_alcoholicas_Habitual',
          'P13_Tabaco_Habitual','P7_SL')

Datos <- select(AllData, all_of(vbles))


Datos[sapply(Datos, is.character)] <- lapply(Datos[sapply(Datos, is.character)], 
                                       as.factor)

attach(Datos)


#### Definición de categoría base ####
sexo <- P3_S
estrato <- P4_ES %>% as.factor
niveleduc <- P6_NE
laboral <- P7_SL

sexo <- relevel(sexo, ref = 'Mujer')
niveleduc <- relevel(niveleduc, ref = 'Media (10-13)')
laboral <- relevel(laboral, ref = 'Ninguna de las anteriores')

#### Nueva definición del consumo habitual de cocaína ####
cocaina <- P13_Cocaina__perico_Habitual %>% dplyr::recode('No aplica' = 'No')

#### Nueva definición del consumo habitual de marihuana ####
marihuana <- P13_Marihuana_Habitual %>% dplyr::recode('No aplica' = 'No')

#### Nueva definición del consumo habitual de bebidas alcohólicas ####
alcohol <- P13_Bebidas_alcoholicas_Habitual %>% 
  dplyr::recode('No aplica' = 'No')

#### Nueva definición del consumo habitual de tabaco ####
tabaco <- P13_Tabaco_Habitual %>% dplyr::recode('No aplica' = 'No')

#### Nueva definición de los niveles educativos ####
nuevo_niveleduc <- niveleduc %>% 
  dplyr::recode('Basica Primaria (1-5)' = 'Primaria o menos',
                'Basica Secundaria (6-9)' = 'Bachillerato',
                'Media (10-13)' = 'Bachillerato',
                'Posgrado (Especializacion Maestria Doctorado)' = 'Posgrado',
                'Prescolar' = 'Primaria o menos',
                'Tecnica' = 'Tecn., tecnol. o pregrado',
                'Tecnologica' = 'Tecn., tecnol. o pregrado',
                'Universitaria - pregrado' = 'Tecn., tecnol. o pregrado',
                'Ninguno' = 'Primaria o menos') %>% 
  dplyr::recode('Primaria o menos' = 'Bachillerato o menos',
                'Bachillerato' = 'Bachillerato o menos') 

# Reorganización de los niveles para que queden en orden
nuevo_niveleduc <- factor(nuevo_niveleduc,
                          levels = c('Bachillerato o menos',
                                     'Tecn., tecnol. o pregrado','Posgrado'))

table(nuevo_niveleduc)


#### Nueva definición de la situación laboral ####
nuevo_laboral <- laboral %>% 
  dplyr::recode('Estudia' = 'PEI', 'Ninguna de las anteriores' = 'PEI',
                'Se encuentra buscando empleo' = 'PEA', 'Trabaja' = 'PEA',
                'Trabaja y estudia' = 'PEA')

nuevo_laboral <- factor(nuevo_laboral, levels = c('PEA','PEI'))
table(nuevo_laboral)


#### Nueva definición de los niveles socioeconómicos ####
nuevo_estrato <- estrato %>% 
  dplyr::recode('1' = 'Bajo', '2' = 'Bajo', '3' = 'Medio', '4' = 'Medio',
                '5' = 'Alto', '6' = 'Alto') %>% 
  relevel('Alto')

table(nuevo_estrato)


#### Gráficos de variables ####

h <- hist(P1_E, plot = FALSE)
# h$density = h$counts/sum(h$counts)*100
plot(h, freq = FALSE, xlab = 'Edad', ylab = '%', col = 'skyblue3', 
     main = 'Distribución de edades')

plot1 <- spineplot(P1_E, cocaina, 
                   col = c('forestgreen','firebrick2'), 
          xlab = 'Edad', ylab = 'Consumo habitual de cocaína/perico',
          main = 'Consumo habitual de cocaína/perico por edades')

plot2 <- spineplot(P3_S, cocaina, 
                   col = c('forestgreen','firebrick2'), 
          xlab = 'Sexo', ylab = 'Consumo habitual de cocaína/perico',
          main = 'Consumo habitual de cocaína/perico por sexo')

plot3 <- spineplot(factor(nuevo_estrato, levels = c('Bajo','Medio','Alto')), 
          cocaina, col = c('forestgreen','firebrick2'),
          xlab = 'Estrato socioeconómico', 
          ylab = 'Consumo habitual de cocaína/perico',
          main = 'Consumo habitual de cocaína/perico por estrato')

plot4 <- spineplot(factor(nuevo_niveleduc, 
                          levels = c('Bachillerato o menos',
                                     'Tecn., tecnol. o pregrado','Posgrado')),
                   cocaina,
          col = c('forestgreen','firebrick2'), 
          xlab = 'Nivel educativo (completo o incompleto)',
          ylab = 'Consumo habitual de cocaína/perico habitual',
          main = 'Consumo habitual de cocaína/perico\npor nivel educativo')

plot5 <- spineplot(factor(marihuana, levels = c('Si','No')),
          cocaina, 
          col = c('forestgreen','firebrick2'),
          xlab = 'Consumo habitual de marihuana',
          ylab = 'Consumo habitual de cocaína/perico',
          main = 'Consumo habitual de cocaína/perico\npor consumo habitual de marihuana')

plot6 <- spineplot(factor(tabaco, levels = c('Si','No')), 
          cocaina, 
          col = c('forestgreen','firebrick2'), 
          xlab = 'Consumo habitual de tabaco', 
          ylab = 'Consumo habitual de cocaína/perico',
          main = 'Consumo habitual de cocaína/perico\npor consumo habitual de tabaco')

plot7 <- spineplot(factor(alcohol, 
                          levels = c('Si','No')), cocaina, 
          col = c('forestgreen','firebrick2'), 
          xlab = 'Consumo habitual de bebidas alcohólicas', 
          ylab = 'Consumo habitual de cocaína/perico',
          main = 'Consumo habitual de cocaína/perico\npor consumo habitual de alcohol')

plot8 <- spineplot(nuevo_laboral, cocaina, 
          col = c('forestgreen','firebrick2'), 
          xlab = 'Situación laboral', 
          ylab = 'Consumo habitual de cocaína/perico',
          main = 'Consumo habitual de cocaína/perico\npor situación laboral')


#### Modelos econométricos ####

nuevo_niveleduc <- relevel(nuevo_niveleduc, ref = 'Posgrado')

edad <- P1_E
estrato <- nuevo_estrato
niveleduc <- nuevo_niveleduc
laboral <- nuevo_laboral


Mprobit <- glm(cocaina ~ edad + sexo + estrato +
                 niveleduc + marihuana + tabaco +
                 alcohol,
                family = binomial('probit'))

Mlogit <- glm(cocaina ~ edad + sexo + estrato +
                 niveleduc + marihuana + tabaco +
                 alcohol,
               family = binomial('logit'))


summary(Mprobit)
summary(Mlogit)


#### Criterios de información ####
# Tomar el modelo con el valor más bajo de los criterios
bic<-(BIC(Mprobit, Mlogit))
bic


# A partir del criterio de información bayesiano, se escoge el modelo probit.


#### R^2 McFadden ####

PseudoR2(Mprobit, c('McKelveyZavoina')) # Tambien se puede usar 'McFadden'


#### Odds ratio ####
oddsratio <- odds.ratio(Mprobit) # Odds ratio probit
round(oddsratio, digits = 4)

# Odd ratio = 1: no permite decidir si la tenencia de la característica hace
# más o menos probable la tenencia del atributo

# Odd ratio < 1: la relación es inversa
# Odd ratio > 1: la relación es directa

# https://www.statology.org/interpret-odds-ratio-less-than-1


#### Matriz de confusión ####
predicciones <- ifelse(test = Mprobit$fitted.values >= 0.5, 
                       yes = 'Si', no = 'No')
matriz_confusion <- table(Mprobit$model$cocaina, predicciones,
                          dnn = c('Observaciones', 'Predicciones'))
matriz_confusion

N <- matriz_confusion %>% diag %>% sum # Se suma la diagonal
D <- Mprobit$model %>% nrow # Tamaño de la muestra con la que se estimó el modelo
Cap_Clasif <- (N/D)*100
paste('Predicciones correctas:', round(Cap_Clasif,2), '%')

mosaic(matriz_confusion, shade = TRUE, colorize = TRUE,
       gp = gpar(fill = matrix(c("forestgreen", "firebrick2", 
                                 "firebrick2", "forestgreen"), 2, 2)))


