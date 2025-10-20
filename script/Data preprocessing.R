
######
# PS 2 BDML Equipo 10
#####

Packages <- c("tidyverse",  "ggplot2", "pacman", "dplyr", "haven",
              "boot", "broom", "lmtest", "fixest", "gridExtra", 
              "writexl", "readxl", "glmnet", "VIM", "caret", 
              "MLmetrics", "Metrics", "pROC","naivebayes")
library(readr)
library(dplyr)
library(tidyr)

invisible(lapply(Packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)){ 
    install.packages(pkg)}
  library(pkg, character.only = TRUE)}))

setwd("C:/Users/natalia buitrago/OneDrive - Pontificia Universidad Javeriana/Nat Personal/BD & ML/Taller 2")  

#Carga de los datos:

train_hogares<-read.csv("train_hogares.csv")
train_personas<-read.csv("train_personas.csv")
test_hogares<-read.csv("test_hogares.csv")
test_personas<-read.csv("test_personas.csv")

#Organización y Pre procesamiento:

#Calculo de pobreza:

table(train_hogares$Pobre)

#Podemos generar variables a nivel de hogar a partir de la base de datos de personas:

#Realizamos el preprocesamiento para las bases de personas

preprocess_personas <- function(data, ...) {
  data <- data %>%
    dplyr::mutate(
      mujer             = ifelse(P6020 == 2, 1, 0),
      menor_de_edad     = ifelse(P6040 <= 16, 1, 0),
      adulto_mayor      = ifelse(P6040 >= 60, 1, 0),
      regimen_salud     = ifelse(P6100 == 4, 3, P6100),
      regimen_salud     = ifelse(is.na(regimen_salud), 3, regimen_salud),
      educLevel         = ifelse(P6210 == 9, 0, P6210),
      jefe_H            = ifelse(P6050 == 1, 1, 0),
      ocupado           = ifelse(is.na(Oc),  0, 1),
      desocupado        = ifelse(is.na(Des), 0, 1),
      inactivo          = ifelse(is.na(Ina), 0, 1),
      tipo_primer_empleo= ifelse(P6430 == 9, 0, P6430),
      tipo_primer_empleo= ifelse(is.na(tipo_primer_empleo), 1, tipo_primer_empleo),
      segundo_empleo    = ifelse(P7040 == 1, 1, 0),
      segundo_empleo    = ifelse(is.na(segundo_empleo), 0, segundo_empleo),
      recibio_horasextra= ifelse(P6510 == 1, 1, 0),
      recibio_horasextra= ifelse(is.na(recibio_horasextra), 0, recibio_horasextra)
    ) %>%
    dplyr::rename(edad = P6040) %>%
    dplyr::select(
      id, mujer, edad, menor_de_edad, adulto_mayor, regimen_salud, educLevel, 
      jefe_H, ocupado, desocupado, inactivo, tipo_primer_empleo, segundo_empleo,
      recibio_horasextra
    )
  return(data)
}

train_personas <- preprocess_personas(train_personas)
test_personas  <- preprocess_personas(test_personas)

safe_max <- function(x) {
  if (all(is.na(x))) return(NA)
  max(x, na.rm = TRUE)
}

#Agregación a nivel hogar 
preprocess_personas_agregacion <- function(data, ...) {
  
  agregacion <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      num_mujeres             = sum(mujer, na.rm = TRUE),
      num_menores             = sum(menor_de_edad, na.rm = TRUE),
      num_adulto_mayor        = sum(adulto_mayor, na.rm = TRUE),
      maxEducLevel            = safe_max(educLevel),     
      num_ocupados            = sum(ocupado, na.rm = TRUE),
      num_desocupados         = sum(desocupado, na.rm = TRUE),
      num_inactivos           = sum(inactivo, na.rm = TRUE), 
      num_con_segundo_empleo  = sum(segundo_empleo, na.rm = TRUE),
      num_recibieron_hrextra  = sum(recibio_horasextra, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Una sola fila por hogar con info del jefe (jefe_H == 1)
  por_hogares_jefeh <- data %>%
    dplyr::filter(jefe_H == 1) %>%                        
    dplyr::arrange(id) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%             
    dplyr::select(
      id, mujer, regimen_salud, educLevel, jefe_H,        
      ocupado, inactivo, desocupado,                       
      tipo_primer_empleo, segundo_empleo, edad,           
      recibio_horasextra                                   
    ) %>%
    dplyr::rename(
      Jefe_H_mujer            = mujer,
      Jefe_regimen_salud      = regimen_salud,
      Jefe_EducLevel          = educLevel,
      Jefe_edad               = edad,
      Jefe_ocupado            = ocupado,
      Jefe_Inactivo           = inactivo,
      Jefe_desocupado         = desocupado,
      Jefe_Tipo_primer_empleo = tipo_primer_empleo,
      Jefe_segundo_empleo     = segundo_empleo,
      Jefe_Recibio_horasextra = recibio_horasextra
    )
  
  resultado_final <- dplyr::left_join(agregacion, por_hogares_jefeh, by = "id")
  return(resultado_final)
}

#APLICAR
train_personas <- preprocess_personas_agregacion(train_personas)
test_personas  <- preprocess_personas_agregacion(test_personas)


#Generamos variables a nivel hogar con base hogar

preproces_hogares <- function(data, ...) {
  data <- data %>% 
    dplyr::mutate(
      arrienda = ifelse(P5090 == 3, 1, 0)
    ) %>%
    dplyr::select(
      id, Clase, Dominio, arrienda, Nper,
      dplyr::any_of("Pobre") 
    )
  return(data)
}

train_hogares <- preproces_hogares(train_hogares)
test_hogares  <- preproces_hogares(test_hogares)

#Unimos las bases de datos correspondientes a nivel hogar

TRAIN <-merge(train_hogares,train_personas)
TEST <- merge(test_hogares,test_personas)


#convertimos las variables a formatos adecuados

TRAIN<- TRAIN %>% 
  mutate(Pobre=factor(Pobre,
                      levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         Clase=factor(Clase),
         arrienda=factor(arrienda,
                         levels=c(0,1),
                         labels = c("No","Yes")),
         Jefe_H_mujer=factor(Jefe_H_mujer,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_regimen_salud=factor(Jefe_regimen_salud,
                                   levels=c(1,2,3),
                                   labels=c("Contributivo (eps)","Especial","Subsidiado")),
         Jefe_EducLevel=factor(Jefe_EducLevel,
                               levels=c(0:6), 
                               labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                        'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                      'Secundaria','Media', 'Universitaria')),
         Jefe_ocupado=factor(Jefe_ocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Inactivo=factor(Jefe_Inactivo,
                              levels=c(0,1),
                              labels=c("No","Yes")),
         Jefe_desocupado=factor(Jefe_desocupado,
                                levels=c(0,1),
                                labels=c("No","Yes")),
         Jefe_Tipo_primer_empleo=factor(Jefe_Tipo_primer_empleo,
                                        levels=c(0,1,2,3,4,5,6,7,8),
                                        labels=c("Ns","Obrero o empleado de empresa particular","Obrero o empleado del gobierno",
                                                 "Empleado doméstico","Trabajador por cuenta propia","Patrón o empleador",
                                                 "Trabajador familiar sin remuneración",
                                                 "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                                 "Jornalero o peón ")),
         Jefe_segundo_empleo=factor(Jefe_segundo_empleo,
                                    levels=c(0,1),
                                    labels=c("No","Yes")),
         Jefe_Recibio_horasextra=factor(Jefe_Recibio_horasextra,
                                        levels=c(0,1),
                                        labels=c("No","Yes"))
  )

TEST<- TEST %>% 
  mutate(Dominio=factor(Dominio),
         Clase=factor(Clase),
         arrienda=factor(arrienda,
                         levels=c(0,1),
                         labels = c("No","Yes")),
         Jefe_H_mujer=factor(Jefe_H_mujer,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_regimen_salud=factor(Jefe_regimen_salud,
                                   levels=c(1,2,3),
                                   labels=c("Contributivo (eps)","Especial","Subsidiado")),
         Jefe_EducLevel=factor(Jefe_EducLevel,
                               levels=c(0:6), 
                               labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                        'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                      'Secundaria','Media', 'Universitaria')),
         Jefe_ocupado=factor(Jefe_ocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Inactivo=factor(Jefe_Inactivo,
                              levels=c(0,1),
                              labels=c("No","Yes")),
         Jefe_desocupado=factor(Jefe_desocupado,
                                levels=c(0,1),
                                labels=c("No","Yes")),
         Jefe_Tipo_primer_empleo=factor(Jefe_Tipo_primer_empleo,
                                        levels=c(0,1,2,3,4,5,6,7,8),
                                        labels=c("Ns","Obrero o empleado de empresa particular","Obrero o empleado del gobierno",
                                                 "Empleado doméstico","Trabajador por cuenta propia","Patrón o empleador",
                                                 "Trabajador familiar sin remuneración",
                                                 "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                                 "Jornalero o peón ")),
         Jefe_segundo_empleo=factor(Jefe_segundo_empleo,
                                    levels=c(0,1),
                                    labels=c("No","Yes")),
         Jefe_Recibio_horasextra=factor(Jefe_Recibio_horasextra,
                                        levels=c(0,1),
                                        labels=c("No","Yes"))
  )


corrijo_na <- function(data, ...) {
  data <- data %>%
    mutate(Jefe_regimen_salud= ifelse(is.na(Jefe_regimen_salud), 3, Jefe_regimen_salud))
  return(data)
}

train_ready <- corrijo_na(TRAIN)
test_ready <- corrijo_na(TEST)

write_csv(train_ready, "train_ready.csv")
write_csv(test_ready,  "test_ready.csv")




