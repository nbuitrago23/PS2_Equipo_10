######## 01_data_clean.R  ############################################
# Limpieza y unión de hogares + personas (train/test)
# Autor: TU NOMBRE
# Fecha: YYYY-MM-DD
######################################################################

## Paquetes -----------------------------------------------------------
# install.packages(c("readr","dplyr","tidyr","ggplot2","skimr")) # solo primera vez
library(readr)
library(dplyr)
library(tidyr)

## Rutas (usa el proyecto como raíz) ---------------------------------
# Si trabajas dentro del repo, ideal usar rutas relativas:
# data/  o  DATAS2/
# Si quieres dejar tu setwd temporalmente, puedes:
# setwd("C:/Users/juanf/OneDrive/Documentos/DATAS2")

## Carga de datos -----------------------------------------------------
hog_tr <- read_csv("train_hogares.csv", show_col_types = FALSE)
per_tr <- read_csv("train_personas.csv", show_col_types = FALSE)
hog_te <- read_csv("test_hogares.csv",  show_col_types = FALSE)
per_te <- read_csv("test_personas.csv", show_col_types = FALSE)

sample_sub <- read_csv("sample_submission.csv", show_col_types = FALSE) # no borrar

## Función util: winsorizar numéricos (recorta extremos) -------------
winsor <- function(x, p = 0.01) {
  if(!is.numeric(x)) return(x)
  ql <- quantile(x, p, na.rm = TRUE)
  qu <- quantile(x, 1 - p, na.rm = TRUE)
  pmin(pmax(x, ql), qu)
}

## Limpieza de HOGARES (train y test) --------------------------------
clean_hogares <- function(df){
  df %>%
    select(-any_of("P5100")) %>%  # quitar cuota amortización (muchos NA)
    mutate(
      across(any_of(c("P5130","P5140")),
             ~ if_else(is.na(.), median(., na.rm = TRUE), .) ),
      id      = as.character(id),
      Dominio = if ("Dominio" %in% names(.)) as.factor(Dominio) else Dominio,
      Depto   = if ("Depto"   %in% names(.)) as.factor(Depto)   else Depto,
      Clase   = if ("Clase"   %in% names(.)) as.factor(Clase)   else Clase
    ) %>%
    mutate(
      across(intersect(c("P5130","P5140","Ingtotug","Ingtotugarr","Ingpcug"), names(.)),
             winsor)
    )
}

hog_tr_clean <- clean_hogares(hog_tr)
hog_te_clean <- clean_hogares(hog_te)

## Limpieza de PERSONAS + agregado por hogar -------------------------
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sum  <- function(x) if (all(is.na(x))) NA_integer_ else sum(x, na.rm = TRUE)
has_col   <- function(df, nm) nm %in% names(df)

clean_personas <- function(df_personas,
                           id_col = "id",
                           edad_col = "P6040",
                           sexo_col = "P6020",
                           ocup_col = "Oc",
                           edu_col  = "P6210s1",
                           persona_key = "Orden"  # en tus datos existe Orden
){
  df <- df_personas
  
  # llave hogar
  stopifnot(has_col(df, id_col))
  df[[id_col]] <- as.character(df[[id_col]])
  
  # quitar columnas 100% NA
  df <- df %>% select(where(~ !all(is.na(.))))
  
  # duplicados por persona (id + Orden)
  if (has_col(df, persona_key)) {
    df <- df %>% distinct(.data[[id_col]], .data[[persona_key]], .keep_all = TRUE)
  } else {
    df <- df %>% distinct(across(everything()), .keep_all = TRUE)
    message("⚠️ Sin llave individual: distinct por todas las columnas.")
  }
  
  # Edad
  df$edad <- if (has_col(df, edad_col)) suppressWarnings(as.numeric(df[[edad_col]])) else NA_real_
  df$edad[df$edad < 0 | df$edad > 110] <- NA
  
  # Sexo (1 hombre, 2 mujer) -> fem
  if (has_col(df, sexo_col)) {
    sex_num <- suppressWarnings(as.numeric(df[[sexo_col]]))
    df$fem <- case_when(
      is.na(sex_num) ~ NA_real_,
      sex_num == 2   ~ 1,
      sex_num == 1   ~ 0,
      TRUE           ~ NA_real_
    )
  } else df$fem <- NA_real_
  
  # Ocupado: 1 sí, todo lo demás 0 (para que la tasa no quede siempre 1/NA)
  if (has_col(df, ocup_col)) {
    oc_num <- suppressWarnings(as.numeric(df[[ocup_col]]))
    df$ocup <- ifelse(oc_num == 1, 1, 0)  # NA/otros => 0
  } else df$ocup <- NA_real_
  
  # Educación (grados): valores >20 suelen ser códigos (ej. 99=NS/NR)
  df$aedu <- if (has_col(df, edu_col)) suppressWarnings(as.numeric(df[[edu_col]])) else NA_real_
  df$aedu[df$aedu > 20] <- NA
  
  # Banderas por edad
  df <- df %>%
    mutate(
      nino   = if_else(!is.na(edad) & edad < 15, 1L, 0L),
      joven  = if_else(!is.na(edad) & edad >= 15 & edad < 25, 1L, 0L),
      adulto = if_else(!is.na(edad) & edad >= 25 & edad < 65, 1L, 0L),
      mayor  = if_else(!is.na(edad) & edad >= 65, 1L, 0L)
    )
  
  # Agregado por hogar
  per_agg <- df %>%
    group_by(.data[[id_col]]) %>%
    summarise(
      n_personas = n(),
      edad_prom  = safe_mean(edad),
      edad_max   = if (all(is.na(edad))) NA_real_ else max(edad, na.rm = TRUE),
      n_ninos    = safe_sum(nino),
      n_jovenes  = safe_sum(joven),
      n_adultos  = safe_sum(adulto),
      n_mayores  = safe_sum(mayor),
      prop_mujer = safe_mean(fem),             # 0..1
      tasa_ocup  = sum(ocup, na.rm = TRUE) / n_personas,  # 0..1
      educ_prom  = safe_mean(aedu),
      .groups = "drop"
    ) %>%
    rename(id = !!id_col)
  
  list(personas_clean = df, personas_agg = per_agg)
}

# aplicar
res_tr <- clean_personas(per_tr)
per_tr_clean <- res_tr$personas_clean
per_tr_agg   <- res_tr$personas_agg

res_te <- clean_personas(per_te)
per_te_clean <- res_te$personas_clean
per_te_agg   <- res_te$personas_agg

## Uniones finales (una fila por hogar) ------------------------------
train_ready <- hog_tr_clean %>% left_join(per_tr_agg, by = "id")
test_ready  <- hog_te_clean %>% left_join(per_te_agg, by = "id")

## Checks rápidos -----------------------------------------------------
stopifnot(!any(duplicated(train_ready$id)))
stopifnot(!any(duplicated(test_ready$id)))

## Guardar salidas ----------------------------------------------------
write_csv(train_ready, "train_ready.csv")
write_csv(test_ready,  "test_ready.csv")

# (NO borres) sample_sub te da el formato de envío
# write_csv(sample_sub, "sample_submission_copy.csv")
######################################################################

