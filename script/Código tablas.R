

library(dplyr) 
library(ggplot2)

# 1) Balance de clases
tabla_clases <- train_ready %>%
  count(Pobre) %>%
  mutate(prop = round(n / sum(n), 4))
print(tabla_clases)

#Resumen de predictores numéricos usados en 'modelo1'
vars_num <- train_ready %>%
  select(any_of(c("Nper","num_ocupados","num_menores","num_adulto_mayor"))) %>%
  mutate(across(everything(), as.numeric))

resumen_num <- vars_num %>%
  summarise(across(everything(),
                   list(N = ~sum(!is.na(.)),
                        mean = ~mean(., na.rm=TRUE),
                        sd = ~sd(., na.rm=TRUE),
                        p25 = ~quantile(., .25, na.rm=TRUE),
                        p50 = ~quantile(., .50, na.rm=TRUE),
                        p75 = ~quantile(., .75, na.rm=TRUE))))
print(resumen_num)


#HELPERS PARA MÉTRICAS Y PROBABILIDADES

#F1 in-sample desde predicción de clases
f1_insample <- function(modelo, data, positive="Yes"){
  pred <- predict(modelo, newdata = data, type = "raw")
  pred <- factor(pred, levels = c("No","Yes"))
  obs  <- factor(data$Pobre, levels = c("No","Yes"))
  MLmetrics::F1_Score(y_pred = pred, y_true = obs, positive = positive)
}

#F1 out-of-fold (si model$pred existe y tiene columnas 'obs','pred','Resample')

f1_oof <- function(modelo, positive="Yes"){
  if (is.null(modelo$pred) || !all(c("obs","pred") %in% names(modelo$pred))) return(NA_real_)
  df <- modelo$pred
  df$obs  = factor(df$obs,  levels = c("No","Yes"))
  df$pred = factor(df$pred, levels = c("No","Yes"))
  s <- df %>%
    group_by(Resample) %>%
    summarise(F1 = MLmetrics::F1_Score(y_pred = pred, y_true = obs, positive = positive), .groups="drop")
  mean(s$F1, na.rm=TRUE)
}

#Densidades de prob(Y=Yes)

plot_prob <- function(modelo, data, titulo){
  if (!"Pobre" %in% names(data)) return(invisible(NULL))
  if (!"prob" %in% names(methods::formalArgs(predict.default))) { # protección mínima
    return(invisible(NULL))
  }
  p <- tryCatch({
    prob <- predict(modelo, newdata = data, type = "prob")[,"Yes"]
    ggplot(data.frame(prob=prob, Pobre=data$Pobre),
           aes(x=prob, fill=Pobre)) +
      geom_density(alpha=.5) +
      labs(title=titulo, x="P(Y=Yes)", y="Densidad") +
      theme_minimal()
  }, error=function(e) NULL)
  if (!is.null(p)) print(p)
}


#F1 DE CADA MODELO (TRAIN y CV si aplica)

resultados <- tibble::tibble(
  Modelo = character(), Tipo = character(), F1 = numeric()
)

#Elastic Net 1
resultados <- dplyr::bind_rows(resultados,
                               tibble::tibble(Modelo="EN_1", Tipo="Train", F1 = f1_insample(model1, train_ready)),
                               tibble::tibble(Modelo="EN_1", Tipo="CV-OOF", F1 = f1_oof(model1))
)

#Elastic Net 2
resultados <- dplyr::bind_rows(resultados,
                               tibble::tibble(Modelo="EN_2", Tipo="Train", F1 = f1_insample(model1_EN2, train_ready)),
                               tibble::tibble(Modelo="EN_2", Tipo="CV-OOF", F1 = f1_oof(model1_EN2))
)

#Logit (regla estándar de clase mayor prob)
resultados <- dplyr::bind_rows(resultados,
                               tibble::tibble(Modelo="LOGIT_default", Tipo="Train", F1 = f1_insample(logit_1, train_ready)),
                               tibble::tibble(Modelo="LOGIT_default", Tipo="CV-OOF", F1 = f1_oof(logit_1))
)

#Logit con mejor umbral 

if (exists("logit_best_threshold")){
  p_logit_train <- predict(logit_1, newdata = train_ready, type="prob")[,"Yes"]
  pred_best <- factor(ifelse(p_logit_train >= logit_best_threshold$threshold, "Yes","No"),
                      levels=c("No","Yes"))
  obs <- factor(train_ready$Pobre, levels=c("No","Yes"))
  f1_logit_best <- MLmetrics::F1_Score(y_pred = pred_best, y_true = obs, positive="Yes")
  resultados <- dplyr::bind_rows(resultados,
                                 tibble::tibble(Modelo="LOGIT_thr_top1", Tipo="Train", F1 = f1_logit_best)
  )
}

#Logit con segundo mejor umbral 

if (exists("logit_second_threshold")){
  p_logit_train2 <- predict(logit_1, newdata = train_ready, type="prob")[,"Yes"]
  pred_top2 <- factor(ifelse(p_logit_train2 >= logit_second_threshold, "Yes","No"),
                      levels=c("No","Yes"))
  obs <- factor(train_ready$Pobre, levels=c("No","Yes"))
  f1_logit_top2 <- MLmetrics::F1_Score(y_pred = pred_top2, y_true = obs, positive="Yes")
  resultados <- dplyr::bind_rows(resultados,
                                 tibble::tibble(Modelo="LOGIT_thr_top2", Tipo="Train", F1 = f1_logit_top2)
  )
}

#CART

resultados <- dplyr::bind_rows(resultados,
                               tibble::tibble(Modelo="CART", Tipo="Train", F1 = f1_insample(cv_tree_c, train_ready)),
                               tibble::tibble(Modelo="CART", Tipo="CV-OOF", F1 = f1_oof(cv_tree_c))
)

#Random Forest 1

resultados <- dplyr::bind_rows(resultados,
                               tibble::tibble(Modelo="RF_1", Tipo="Train", F1 = f1_insample(cv_RForest_c, train_ready)),
                               tibble::tibble(Modelo="RF_1", Tipo="CV-OOF", F1 = f1_oof(cv_RForest_c))
)

#Random Forest 2

resultados <- dplyr::bind_rows(resultados,
                               tibble::tibble(Modelo="RF_2", Tipo="Train", F1 = f1_insample(cv_RForest_c2, train_ready)),
                               tibble::tibble(Modelo="RF_2", Tipo="CV-OOF", F1 = f1_oof(cv_RForest_c2))
)

#Naive Bayes

if (exists("bestTH")){
  p_nb_train <- predict(modelo_nb, newdata = train_ready, type = "prob")[,"Yes"]
  pred_nb_best <- factor(ifelse(p_nb_train >= bestTH, "Yes","No"), levels=c("No","Yes"))
  obs <- factor(train_ready$Pobre, levels=c("No","Yes"))
  f1_nb_best <- MLmetrics::F1_Score(y_pred = pred_nb_best, y_true = obs, positive="Yes")
  f1_nb_oof <- f1_oof(modelo_nb)
  resultados <- dplyr::bind_rows(resultados,
                                 tibble::tibble(Modelo="NB_thr_opt", Tipo="Train", F1 = f1_nb_best),
                                 tibble::tibble(Modelo="NB",          Tipo="CV-OOF", F1 = f1_nb_oof)
  )
}

# Tabla final ordenada

resultados <- resultados %>%
  arrange(desc(F1), Modelo, Tipo)
print(resultados)


#GRÁFICO F1 POR MODELO/TIPO

ggplot(resultados %>% filter(!is.na(F1)),
       aes(x=reorder(paste(Modelo, Tipo, sep=" | "), F1), y=F1, fill=Tipo)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Train" = "#0072B2", "CV-OOF" = "#E69F00")) +
  labs(title="F1 por modelo y tipo de evaluación",
       x=NULL, y="F1") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

library(ggplot2)

plot_prob <- function(model, data, title_label) {
  df <- data.frame(
    Pobre = data$Pobre,
    Prob = predict(model, newdata = data, type = "prob")[, "Yes"]
  )
  
  ggplot(df, aes(x = Prob, fill = Pobre)) +
    geom_histogram(position = "identity", alpha = 0.6, bins = 35, color = "white") +
    scale_fill_manual(values = c("No" = "#E69F00", "Yes" = "#0072B2")) +
    labs(
      x = "Predicted Probability P(Y = Yes)",
      y = "Count",
      title = title_label,
      subtitle = "Positive class: 'Yes'"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}



#  E) DENSIDADES DE PROBABILIDADES (P(Y=Yes))

plot_prob(model1,        train_ready, "P(Y = Yes) - EN_1 (TRAIN)")
plot_prob(model1_EN2,    train_ready, "P(Y = Yes) - EN_2 (TRAIN)")
plot_prob(logit_1,       train_ready, "P(Y = Yes) - LOGIT (TRAIN)")
plot_prob(cv_RForest_c,  train_ready, "P(Y = Yes) - RF_1 (TRAIN)")
plot_prob(cv_tree_c,     train_ready, "P(Y = Yes) - CART (TRAIN)")
plot_prob(modelo_nb,     train_ready, "P(Y = Yes) - NB (TRAIN)")


#IMPORTANCIA DE VARIABLES 

# RF
try({
  vi_rf1 <- caret::varImp(cv_RForest_c)
  plot(vi_rf1, top = 12, main = "Importancia de variables - RF_1")
}, silent = TRUE)

# EN 
try({
  vi_en1 <- caret::varImp(model1)
  plot(vi_en1, top = 12, main = "Importancia (|coef|) - EN_1")
}, silent = TRUE)

# CART
try({
  vi_cart <- caret::varImp(cv_tree_c)
  plot(vi_cart, top = 12, main = "Importancia - CART")
}, silent = TRUE)


#Exportar tablas a CSV

readr::write_csv(tabla_clases, "tabla_balance_clases.csv")
readr::write_csv(resultados,   "tabla_F1_modelos.csv")
