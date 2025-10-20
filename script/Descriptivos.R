
if(!require(skimr)) install.packages("skimr")
if(!require(psych)) install.packages("psych")
if(!require(GGally)) install.packages("GGally")
if(!require(stargazer)) install.packages("stargazer")

library(dplyr)
library(skimr)
library(writexl)

#Variables que quieres forzar como categóricas

binarias_o_categoricas <- c(
  "num_mujeres","num_menores","num_adulto_mayor","num_ocupados",
  "num_desocupados","num_inactivos","num_con_segundo_empleo",
  "num_recibieron_hrextra","Jefe_regimen_salud","jefe_H"
)

to_cats <- function(df, vars){
  vars <- intersect(vars, names(df))
  if (length(vars) == 0) return(df)
  df %>%
    mutate(across(all_of(vars), \(x){
      if (is.numeric(x) && all(na.omit(unique(x)) %in% c(0,1))) {
        factor(x, levels = c(0,1), labels = c("No","Yes"))
      } else {
        as.factor(x)
      }
    }))
}

vars_modelo1 <- setdiff(all.vars(modelo1), "Pobre")

train_m1 <- train_ready %>%
  select(any_of(c("Pobre", vars_modelo1))) %>%
  to_cats(binarias_o_categoricas)

test_m1  <- test_ready %>%
  select(any_of(vars_modelo1)) %>%
  to_cats(binarias_o_categoricas)

skim_num <- function(df){
  df %>% 
    select(where(is.numeric)) %>% 
    skim() %>% 
    filter(skim_type == "numeric") %>% 
    rename_with(~ sub("^numeric\\.", "", .x)) %>%   # quita "numeric."
    select(any_of(c("skim_variable","n","mean","sd","p0","p25","p50","p75","p100")))
}

# --- Tablas categóricas: elimina prefijo "factor." si existe y usa nombres estándar
skim_cat <- function(df){
  df %>% 
    select(where(is.factor)) %>% 
    skim() %>% 
    filter(skim_type == "factor") %>% 
    rename_with(~ sub("^factor\\.", "", .x)) %>%    # quita "factor."
    select(any_of(c("skim_variable","n_missing","complete_rate","ordered","n_unique","top_counts")))
}

# Vuelve a correr:
train_num <- skim_num(train_m1)
train_cat <- skim_cat(train_m1)

test_num  <- skim_num(test_m1)
test_cat  <- skim_cat(test_m1)

# --- 6) Exportar a un solo Excel (en tu working directory actual)
out_path <- file.path(getwd(), "Descriptivas_Modelo1_train_test.xlsx")
write_xlsx(
  list(
    "Train_Numerical" = train_num,
    "Train_Categorical" = train_cat,
    "Test_Numerical" = test_num,
    "Test_Categorical" = test_cat
  ),
  path = out_path
)
