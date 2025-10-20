
######
# PS 2 BDML Equipo 10
#####

##########
# MODELOS 
##########

train_ready$Pobre <- stats::relevel(train_ready$Pobre, ref = "Yes")
fiveStats <- function(...) c(caret::prSummary(...))

#####
#[1] PREDICCIÓN 1 - ELASTIC NET 

ctrl <- caret::trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = fiveStats,
  savePredictions = TRUE
)

modelo1 <- Pobre ~ Nper + num_ocupados + arrienda +
  maxEducLevel + Jefe_H_mujer + Jefe_desocupado +
  Jefe_regimen_salud + Jefe_Tipo_primer_empleo +
  Jefe_segundo_empleo

grid_EN <- expand.grid(
  alpha  = seq(0, 1, by = 0.5),        
  lambda = 10^seq(-3, -1, length = 10)  
)


set.seed(2025)
model1 <- caret::train(
  modelo1,
  data      = train_ready,
  method    = "glmnet",
  family    = "binomial",
  metric    = "F",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid  = grid_EN
)

model1
model1$bestTune

# Creación de la matriz de confusión:
predicciones <- predict(model1, newdata = train_ready, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
train_ready$Pobre <- factor(train_ready$Pobre, levels = c("No", "Yes"))

cm_model1 <- confusionMatrix(predicciones, train_ready$Pobre, positive = "Yes")
cm_model1

df_EN <- data.frame(
  Model = "model1",
  F1_Score = cm_model1$byClass["F1"]
)

rownames(df_EN)<-NULL
df_EN

# Envío a Kaggle:

predictSample <- test_ready   %>% 
  mutate(pobre_lab = predict(model1, newdata = test_ready, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model1$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model1$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)

#####
#[2] PREDICCIÓN 2 - ELASTIC NET 

# Grid 

grid_EN2 <- expand.grid(
  alpha  = c(0.15, 0.35, 0.65, 0.85, 1.00),  
  lambda = 10^seq(-4, -2, length = 12)       
)

set.seed(2025)
model1_EN2 <- caret::train(
  modelo1,
  data       = train_ready,
  method     = "glmnet",
  family     = "binomial",
  metric     = "F",
  trControl  = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid   = grid_EN2
)

model1_EN2
model1_EN2$bestTune


## Matriz confusión)

pred_EN2_train <- predict(model1_EN2, newdata = train_ready, type = "raw")
pred_EN2_train <- factor(pred_EN2_train, levels = c("No", "Yes"))
Pobre_train    <- factor(train_ready$Pobre, levels = c("No", "Yes"))

cm_model1_EN2 <- caret::confusionMatrix(pred_EN2_train, Pobre_train, positive = "Yes")
cm_model1_EN2

df_EN2 <- data.frame(
  Model    = "ElasticNet_2",
  F1_Score = cm_model1_EN2$byClass["F1"]
)
rownames(df_EN2) <- NULL
df_EN2

## Predicción TEST

predictSample_EN2 <- test_ready %>% 
  dplyr::mutate(pobre_lab = predict(model1_EN2, newdata = test_ready, type = "raw")) %>% 
  dplyr::transmute(id, pobre = ifelse(pobre_lab == "Yes", 1, 0))

head(predictSample_EN2)

## Nombre del archivo para Kaggle

lambda_str2 <- gsub("\\.", "_", as.character(round(model1_EN2$bestTune$lambda, 6)))
alpha_str2  <- gsub("\\.", "_", as.character(model1_EN2$bestTune$alpha))

name2 <- paste0("EN_lambda_", lambda_str2, "_alpha_", alpha_str2, ".csv")

# Guardar envío
write.csv(predictSample_EN2, name2, row.names = FALSE)

#####
# [3] PREDICCIÓN 3 - LOGIT

#Logit parámetros default

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

set.seed(2025)
logit_1 <- train(form_modelo_logit1,
                 data = train_ready, 
                 method = "glm",
                 trControl = ctrl,
                 family = "binomial")
logit_1

predicciones <- predict(logit_1, newdata = train_ready, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
train_ready$Pobre <- factor(train_ready$Pobre, levels = c("No", "Yes"))

cm_model1 <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
print(cm_model1)

df_logit_1 <- data.frame(
  logit_1 = "logit_1",
  F1_Score = cm_model1$byClass["F1"])

rownames(df_logit_1)<-NULL
df_logit_1

#Logit mejor threshold 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

set.seed(2025)
logit_1 <- train(form_modelo_logit1,
                 data = train_ready, 
                 method = "glm",
                 trControl = ctrl,
                 family = "binomial")
logit_1

predicciones <- predict(logit_1, newdata = train_ready, type = "prob")[, "Yes"]

library(pROC)

roc_obj_logit <- roc(response = train_ready$Pobre,  
                     predictor = predicciones,  
                     levels = c("No", "Yes"),  
                     direction = "<")
logit_best_threshold <- coords(roc_obj_logit, x = "best", best.method = "closest.topleft")
logit_best_threshold$threshold
logit_best_threshold

pred_clase <- ifelse(predicciones >= logit_best_threshold[1], "Yes", "No")

#Evaluando en el train set con mejor threshold

Logit_nuevo_umbral <- train_ready %>%
  mutate(pobre_prob_logit_en_sens = predict(logit_1, newdata = train_ready, type = "prob")[, "Yes"],
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_prob_logit_en_sens >= logit_best_threshold$threshold, "Yes", "No"),
           levels = c("No", "Yes")))

cm_model1_Nuevo_umbral <- confusionMatrix(Logit_nuevo_umbral$clasificacion_nuevo_umbral, train_ready$Pobre, positive = "Yes",  mode = "prec_recall")
print(cm_model1_Nuevo_umbral)

df_logit_1_best_threshold <- data.frame(
  logit_1_best_threshold = "logit_1_best_threshold",
  F1_Score = cm_model1_Nuevo_umbral$byClass["F1"]
)

rownames(df_logit_1_best_threshold)<-NULL
df_logit_1_best_threshold

#Envío a Kaggle:

predictSample <- test_ready %>%
  mutate(pobre_lab = predict(logit_1, newdata = test_ready, type = "prob")[, "Yes"],
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_lab >= logit_best_threshold$threshold, "Yes", "No"),
           levels = c("No", "Yes")))%>% select(id,clasificacion_nuevo_umbral)
head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(clasificacion_nuevo_umbral=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

thr_str <- gsub("\\.", "_", as.character(round(0.2095521, 4)))

logit_1 <- paste0("LOGIT_threshold_", thr_str, ".csv")
write.csv(predictSample, logit_1, row.names = FALSE)

#####
# [4] Predicción 4 - Logit threshold top 2


set.seed(2025)
logit_1 <- train(
  form_modelo_logit1,
  data = train_ready, 
  method = "glm",
  trControl = ctrl,
  family = "binomial"
)

predicciones <- predict(logit_1, newdata = train_ready, type = "prob")[, "Yes"]

# Curva ROC (pROC), igual que antes
library(pROC)
roc_obj_logit <- roc(response = train_ready$Pobre,
                     predictor = predicciones,
                     levels = c("No", "Yes"),
                     direction = "<")

#buscamos el 2º mejor umbral por F1 

roc_tab <- as.data.frame(coords(
  roc_obj_logit,
  x   = roc_obj_logit$thresholds,   # evaluar en todos los cortes
  input = "threshold",
  ret = c("threshold", "sensitivity", "specificity", "ppv"),
  transpose = FALSE
))

# Calculamos F1 
roc_tab$F1 <- with(roc_tab, (2 * ppv * sensitivity) / pmax(ppv + sensitivity, 1e-12))

# Ordenamos por F1 desc y tomamos el segundo mejor
roc_tab <- roc_tab[order(-roc_tab$F1), ]
logit_second_threshold <- roc_tab$threshold[2]  
logit_second_threshold

# Evaluación en TRAIN usando el nuevo umbral 

Logit_nuevo_umbral_2 <- train_ready %>%
  mutate(
    pobre_prob_logit_en_sens = predict(logit_1, newdata = train_ready, type = "prob")[, "Yes"],
    clasificacion_nuevo_umbral_2 = factor(
      ifelse(pobre_prob_logit_en_sens >= logit_second_threshold, "Yes", "No"),
      levels = c("No", "Yes"))
  )

cm_model1_Nuevo_umbral_2 <- confusionMatrix(
  Logit_nuevo_umbral_2$clasificacion_nuevo_umbral_2,
  train_ready$Pobre,
  positive = "Yes",
  mode = "prec_recall"
)
print(cm_model1_Nuevo_umbral_2)

df_logit_2_best_threshold <- data.frame(
  logit_2_best_threshold = "logit_2_best_threshold",
  F1_Score = cm_model1_Nuevo_umbral_2$byClass["F1"]
)
rownames(df_logit_2_best_threshold) <- NULL
df_logit_2_best_threshold

# Envío a Kaggle 

predictSample <- test_ready %>%
  mutate(
    pobre_lab = predict(logit_1, newdata = test_ready, type = "prob")[, "Yes"],
    clasificacion_nuevo_umbral_2 = factor(
      ifelse(pobre_lab >= logit_second_threshold, "Yes", "No"),
      levels = c("No", "Yes"))
  ) %>%
  select(id, clasificacion_nuevo_umbral_2)

predictSample <- predictSample %>%
  mutate(pobre = ifelse(clasificacion_nuevo_umbral_2 == "Yes", 1, 0)) %>%
  select(id, pobre)

#Nombre del archivo con el umbral 

thr2_str <- gsub("\\.", "_", as.character(round(logit_second_threshold, 4)))
logit_2 <- paste0("LOGIT_threshold_", thr2_str, ".csv")

write.csv(predictSample, logit_2, row.names = FALSE)

#####
# [5] Predicción 5 - CARTs

fiveStats <- function(...) c(prSummary(...))
set.seed(2025)
ctrl_cart <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,   # Precision, Recall y F1
  classProbs = TRUE,
  verbose = FALSE,
  savePredictions = TRUE
)

#Grilla de hiperparámetros

grid_cart_fast <- expand.grid(cp = c(0, 0.0001, 0.0005, 0.001, 0.003, 0.005))

#Entrenamiento CART con TU fórmula y TU data
cv_tree_c <- train(
  modelo1,
  data      = train_ready,
  method    = "rpart",
  trControl = ctrl_cart,
  tuneGrid  = grid_cart,
  metric    = "F"              
)
print(cv_tree_c)
print(cv_tree_c$bestTune)

#Matriz de confusión
pred_cart_train <- predict(cv_tree_c, newdata = train_ready, type = "raw")
pred_cart_train <- factor(pred_cart_train, levels = c("No", "Yes"))
cm_cart <- confusionMatrix(pred_cart_train, train_ready$Pobre, positive = "Yes", mode = "prec_recall")
print(cm_cart)

#Predicción test

pred_cart_test <- predict(cv_tree_c, newdata = test_ready, type = "raw")
submission_cart <- data.frame(
  id    = test_ready$id,
  pobre = ifelse(pred_cart_test == "Yes", 1, 0)
)

#Envío Kaggle

cp_str <- gsub("\\.", "_", as.character(round(cv_tree_c$bestTune$cp, 4)))
file_cart <- paste0("CART_cp_", cp_str, ".csv")
write.csv(submission_cart, file_cart, row.names = FALSE)


#####
# [6] Predicción 6 - Random Forest 

library(ranger)

# 1) Control CV con F1 
set.seed(2025)
ctrl_rf <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  verbose = FALSE,
  savePredictions = TRUE
)

#Grilla RF 

grid_rf <- expand.grid(
  mtry          = c(8, 11, 15),
  splitrule     = "gini",          
  min.node.size = c(50, 60, 70, 80)
)

#Entrenamiento RF

cv_RForest_c <- train(
  modelo1,
  data      = train_ready,
  method    = "ranger",
  trControl = ctrl_rf,
  metric    = "F",                   
  tuneGrid  = grid_rf,
  num.trees = 300,                  
  importance = "impurity"
)
print(cv_RForest_c)
print(cv_RForest_c$bestTune)       

#Matriz de confusión
pred_rf_train <- predict(cv_RForest_c, newdata = train_ready, type = "raw")
pred_rf_train <- factor(pred_rf_train, levels = c("No", "Yes"))
cm_rf <- confusionMatrix(pred_rf_train, train_ready$Pobre, positive = "Yes", mode = "prec_recall")
print(cm_rf)

#Predicción TEST y archivo Kaggle

predictSample <- test_ready %>%
  mutate(pobre_lab = predict(cv_RForest_c, newdata = test_ready, type = "raw")) %>%
  select(id, pobre_lab)

predictSample <- predictSample %>%
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) %>%
  select(id, pobre)

# Nombre con hiperparámetros óptimos

best_mtry <- cv_RForest_c$bestTune$mtry
best_min_node <- cv_RForest_c$bestTune$min.node.size
file_name <- paste0("RF_mtry_", best_mtry,
                    "_minNode_", best_min_node,
                    "_ntree_300.csv")

write.csv(predictSample, file_name, row.names = FALSE)

#####
# [7] Predicción 7 - Random Forest 

library(ranger)

#Control CV con F1 
set.seed(2025)
ctrl_rf <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  verbose = FALSE,
  savePredictions = TRUE
)

#Hiperparámetros 

grid_rf2 <- expand.grid(
  mtry          = c(6, 9),  
  splitrule     = "gini",
  min.node.size = c(40, 55, 75)
)

# Entrenamiento RF 
cv_RForest_c2 <- train(
  modelo1,
  data       = train_ready,
  method     = "ranger",
  trControl  = ctrl_rf,
  metric     = "F",
  tuneGrid   = grid_rf2,
  num.trees  = 200,             
  importance = "impurity"
)

print(cv_RForest_c2)
print(cv_RForest_c2$bestTune)


# Matriz de confusión 

pred_rf_train2 <- predict(cv_RForest_c2, newdata = train_ready, type = "raw")
pred_rf_train2 <- factor(pred_rf_train2, levels = c("No", "Yes"))

pred_rf_train2 <- factor(pred_rf_train2, levels = c("No", "Yes"))
train_ready$Pobre <- factor(train_ready$Pobre, levels = c("No", "Yes"))

cm_rf2 <- confusionMatrix(pred_rf_train2, train_ready$Pobre, positive = "Yes", mode = "prec_recall")
print(cm_rf2)

# Predicción test y archivo Kaggle 

predictSample2 <- test_ready %>%
  mutate(pobre_lab = predict(cv_RForest_c2, newdata = test_ready, type = "raw")) %>%
  select(id, pobre_lab)

predictSample2 <- predictSample2 %>%
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) %>%
  select(id, pobre)

#Envío Kaggle

best_mtry <- cv_RForest_c$bestTune$mtry
best_min_node <- cv_RForest_c$bestTune$min.node.size
file_name <- paste0("RF_mtry_", best_mtry,
                    "_minNode_", best_min_node,
                    "_ntree_300.csv")

write.csv(predictSample2, file_name, row.names = FALSE)

#####
# [8] Predicción 8 - Naive Bayes

train_ready$Pobre <- factor(train_ready$Pobre, levels = c("Yes","No"))


#Control con CV

fiveStats_nb <- function(...) c(caret::prSummary(...))
set.seed(2025)
ctrl_nb <- caret::trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats_nb,  # Precision, Recall, F
  classProbs = TRUE,
  savePredictions = "final",
  verbose = FALSE
)

grid_nb <- expand.grid(
  laplace   = c(0, 1, 2),    
  usekernel = c(TRUE, FALSE), 
  adjust    = c(1, 1.5)   
)

set.seed(2025)
modelo_nb <- caret::train(
  modelo1,
  data      = train_ready,
  method    = "naive_bayes",
  metric    = "F",
  trControl = ctrl_nb,
  tuneGrid  = grid_nb,
  preProcess = c("nzv") 
)
print(modelo_nb)
print(modelo_nb$bestTune)

# 2) Buscar UMBRAL que maximiza F1 en TRAIN

# Probabilidades de "Yes" en TRAIN
p_train <- predict(modelo_nb, newdata = train_ready, type = "prob")[,"Yes"]

# Buscamos umbral sobre una malla fina (rápida)
th_grid <- seq(0.05, 0.5, by = 0.01)

bestF1 <- -Inf; bestTH <- 0.5
for (th in th_grid) {
  pred_lab <- factor(ifelse(p_train >= th, "Yes", "No"), levels = c("No","Yes"))
  obs_lab  <- factor(train_ready$Pobre, levels = c("No","Yes"))
  f1 <- MLmetrics::F1_Score(y_pred = pred_lab, y_true = obs_lab, positive = "Yes")
  if (is.na(f1)) f1 <- -Inf
  if (f1 > bestF1) { bestF1 <- f1; bestTH <- th }
}
cat("Umbral óptimo (max F1) =", round(bestTH, 4), " | F1 =", round(bestF1, 4), "\n")

# Matriz de confusión en TRAIN con el umbral óptimo
pred_train_lab <- factor(ifelse(p_train >= bestTH, "Yes", "No"), levels = c("No","Yes"))
cm_nb <- caret::confusionMatrix(pred_train_lab, train_ready$Pobre, positive = "Yes", mode = "prec_recall")
print(cm_nb)

#Predicción TEST

p_test <- predict(modelo_nb, newdata = test_ready, type = "prob")[,"Yes"]
pred_test_lab <- ifelse(p_test >= bestTH, 1, 0)

submission_nb <- data.frame(
  id    = test_ready$id,
  pobre = pred_test_lab
)

#Envío Kaggle

bt <- modelo_nb$bestTune
lap_str <- sprintf("%d", bt$laplace)
ker_str <- ifelse(bt$usekernel, "1", "0")
adj_str <- gsub("\\.", "_", as.character(bt$adjust))
thr_str <- gsub("\\.", "_", as.character(round(bestTH, 4)))

file_nb <- paste0("NB_laplace_", lap_str,
                  "_usekernel_", ker_str,
                  "_adjust_", adj_str,
                  "_thr_", thr_str, ".csv")
write.csv(submission_nb, file_nb, row.names = FALSE)

