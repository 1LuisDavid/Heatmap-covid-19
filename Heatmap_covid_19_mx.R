Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(viridis)
library(hrbrthemes)
library(ggthemes)
library(patchwork)
library(viridis)



#Pueden bajar los datos de: https://www.gob.mx/salud/documentos/datos-abiertos-152127

cv <- read_csv("200715COVID19MEXICO.csv")
cv <- clean_names(cv)


cv <-
  cv %>% select(1, 13, 16, 20:24, 26:29, 31)

cv <- 
  cv %>%
  mutate(fecha_def = as_date(fecha_def),
         fecha_actualizacion = as_date(fecha_actualizacion))


cv <-
  cv %>% filter(resultado == 1)  
mutate(resultado = as.character(resultado))

cv_contagios_totales <-
  cv %>% mutate(fecha_actualizacion_charac = as.character(fecha_actualizacion)) %>%
  group_by(fecha_actualizacion_charac, edad) %>% 
  mutate(contagiados_x_dia_x_edad = sum(resultado)) %>% ungroup()

###### graficas de contagiados por lustros   ------
contagios_por_lustros <-
  cv_contagios_totales %>% select(fecha_actualizacion, fecha_actualizacion_charac, edad, resultado) %>% 
  group_by(fecha_actualizacion_charac, edad) %>% arrange(edad) %>% ungroup() %>% 
  mutate(lustros = case_when(edad <= 4 ~ "00-04",
                             edad > 4 & edad <= 9 ~"05-09",
                             edad > 9 & edad <= 14 ~"10-14",
                             edad > 14 & edad <= 19 ~"15-19",
                             edad > 19 & edad <= 24 ~"20-24",
                             edad > 24 & edad <= 29 ~"25-29",
                             edad > 29 & edad <= 34 ~"30-34",
                             edad > 34 & edad <= 39 ~"35-39",
                             edad > 39 & edad <= 44 ~"40-44",
                             edad > 44 & edad <= 49 ~"45-49",
                             edad > 49 & edad <= 54 ~"50-54",
                             edad > 54 & edad <= 59 ~"55-59",
                             edad > 59 & edad <= 64 ~"60-64",
                             edad > 64 & edad <= 69 ~"65-69",
                             edad > 69 & edad <= 74 ~"70-74",
                             edad > 74 & edad <= 79 ~"75-79",
                             edad > 79 & edad <= 84 ~"80-84",
                             edad > 84 & edad <= 89 ~"85-89",
                             edad > 89 ~"90+")) %>% 
  
  group_by(fecha_actualizacion_charac, lustros) %>% 
  mutate(Número_de_contagios = sum(resultado)) %>% ungroup %>% 
  
  ggplot(aes(x = fecha_actualizacion, y = lustros, fill = Número_de_contagios))+
  geom_tile()+
  scale_fill_viridis(discrete = F, option = "B")+
  theme_bw()+
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(face = "italic", hjust = 0, size = 14),
        legend.text = element_text(size = 14))+
  
  labs(x = " ", y = " ",
       title="Contagios y muertes acumulados por COVID-19",
       subtitle = "Datos con corte al 15 de julio",
       caption = "  ",
       fill = "Número de\ncontagios")




####### Gráficas de muertes     --------  

cv <-
  na.omit(cv)

cv_heat <-
  cv %>% mutate(fecha_def_charac = as.character(fecha_def),
                fecha_actualizacion_charac = as.character(fecha_actualizacion)) %>%
  group_by(fecha_def_charac, edad) %>% 
  mutate(muertos_x_dia_x_edad = sum(resultado)) %>% ungroup()



Por_fecha_muerte <-
  cv_heat %>% select(fecha_def, fecha_def_charac, edad, muertos_x_dia_x_edad, resultado) %>% 
  group_by(fecha_def_charac, edad) %>% arrange(edad) %>% ungroup() %>% 
  mutate(lustros = case_when(edad <= 4 ~ "00-04",
                             edad > 4 & edad <= 9 ~"05-09",
                             edad > 9 & edad <= 14 ~"10-14",
                             edad > 14 & edad <= 19 ~"15-19",
                             edad > 19 & edad <= 24 ~"20-24",
                             edad > 24 & edad <= 29 ~"25-29",
                             edad > 29 & edad <= 34 ~"30-34",
                             edad > 34 & edad <= 39 ~"35-39",
                             edad > 39 & edad <= 44 ~"40-44",
                             edad > 44 & edad <= 49 ~"45-49",
                             edad > 49 & edad <= 54 ~"50-54",
                             edad > 54 & edad <= 59 ~"55-59",
                             edad > 59 & edad <= 64 ~"60-64",
                             edad > 64 & edad <= 69 ~"65-69",
                             edad > 69 & edad <= 74 ~"70-74",
                             edad > 74 & edad <= 79 ~"75-79",
                             edad > 79 & edad <= 84 ~"80-84",
                             edad > 84 & edad <= 89 ~"85-89",
                             edad > 89 ~"90+")) %>% 
  
  group_by(fecha_def_charac, lustros) %>% 
  mutate(muertos_x_lustro_x_dia = sum(resultado)) %>% ungroup %>% 
  
  ggplot(aes(x = fecha_def, y = lustros, fill = muertos_x_lustro_x_dia))+
  geom_tile()+
  scale_fill_viridis(discrete = F, option = "B")+
  theme_bw()+
  
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(face = "italic", hjust = 0, size = 10),
        legend.text = element_text(size = 12))+
  
  labs(x = "Fecha de defunción", y = " ",
       title="Heatmap de muertes por COVID-19 en México",
       subtitle = "Datos con corte al 15 de julio. \nCada pixel representa un día asociado a un rango de edad.",
       caption = "Elaborado por @1LuisDavid con datos de la Secretaría de Salud.",
       fill = "Número de\nmuertes")










Muerte_por_lustros <-
  
  cv_heat %>% select(fecha_actualizacion, fecha_actualizacion_charac, edad, resultado) %>% 
  group_by(fecha_actualizacion_charac, edad) %>% arrange(edad) %>% ungroup() %>% 
  mutate(lustros = case_when(edad <= 4 ~ "00-04",
                             edad > 4 & edad <= 9 ~"05-09",
                             edad > 9 & edad <= 14 ~"10-14",
                             edad > 14 & edad <= 19 ~"15-19",
                             edad > 19 & edad <= 24 ~"20-24",
                             edad > 24 & edad <= 29 ~"25-29",
                             edad > 29 & edad <= 34 ~"30-34",
                             edad > 34 & edad <= 39 ~"35-39",
                             edad > 39 & edad <= 44 ~"40-44",
                             edad > 44 & edad <= 49 ~"45-49",
                             edad > 49 & edad <= 54 ~"50-54",
                             edad > 54 & edad <= 59 ~"55-59",
                             edad > 59 & edad <= 64 ~"60-64",
                             edad > 64 & edad <= 69 ~"65-69",
                             edad > 69 & edad <= 74 ~"70-74",
                             edad > 74 & edad <= 79 ~"75-79",
                             edad > 79 & edad <= 84 ~"80-84",
                             edad > 84 & edad <= 89 ~"85-89",
                             edad > 89 ~"90+")) %>% 
  
  group_by(fecha_actualizacion_charac, lustros) %>% 
  mutate(Número_de_muertes = sum(resultado)) %>% ungroup %>% 
  
  ggplot(aes(x = fecha_actualizacion, y = lustros, fill = Número_de_muertes))+
  geom_tile()+
  scale_fill_viridis(discrete = F, option = "B")+
  theme_bw()+
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(face = "italic", hjust = 0, size = 11),
        legend.text = element_text(size = 14))+
  
  labs(x = " ", y = " ",
       title=" ",
       caption = "Elaborado por @1LuisDavid con datos de la Secretaría de Salud.",
       fill = "Número de\nmuertes")



#Patchwork

contagios_por_lustros + Muerte_por_lustros

Por_fecha_muerte



