## Modelo XGBoost modelando independientemente PT y PC
## Se cambia la base de datos para incluir a todos los predios habitacionales con construcciones
## Se incluye información de NDVI, estrato y otros.
## Correr con versión 4.2.1
## Se mejora el proceso de selección de variables

gc()
rm(list = ls())

options(scipen = 999)

# library(brms)
# library(tidyverse)
# library(posterior)
# library(bayesplot)
library(caret)
library(stringr)
library(sf)
library(car)
library(dplyr)
# library(tidymodels)

path.out = "F:/OneDrive/Escritorio/IGAC/02_PrecioVivienda/out/results/xgboost_4/"

source("00_Funciones.R", encoding = "UTF-8")

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#===============================================#
#                1. Datos                  ######
#===============================================#
##========= Shape de predios de aracataca
df.shp = read_sf(dsn = "F:/OneDrive/Escritorio/IGAC/01_Distances/data/PrediosAracataca/Predios")
df.shp = df.shp %>% 
  distinct(NUMERO_PRE,.keep_all = T)

df.shp = df.shp[
  ,c("NUMERO_PRE",
     "HABITACION", "BANOS_1", "LOCALES_1", "PISOS_1", "TIPIFICACI", 
     "USO_1", "PUNTAJE_1", "HABITACI_1", "BANOS_2", 
     "LOCALES_2", "PISOS_2", "TIPIFICA_1", "USO_2", "PUNTAJE_2",
     "HABITACI_2", "BANOS_3", "LOCALES_3", "PISOS_3", "TIPIFICA_2", 
     "USO_3", "PUNTAJE_3",
     "Vivienda", "COORD_X", "COORD_Y")] %>% 
  as.data.frame()

##======== Datos de predios información de áreas y precios
df = readxl::read_excel("./datos/predios_valores_completos.xlsx") %>% as.data.frame()
df = df %>% 
  distinct(NUMERO_PRE,.keep_all = T)

df = df %>% 
  left_join(df.shp, by = "NUMERO_PRE")

# Dejar solo predios habitables, con área construido y viviendas
df = df %>% 
  filter(DESTINO_EC == "A" & AREA_CONST != 0 & Vivienda == "Si" & VALOR_CONS != 0 & VALOR_TERR != 0)

##====== Datos distancias
df.dist1 = foreign::read.dbf("F:/OneDrive/Escritorio/IGAC/01_Distances/data/PrediosAracataca/PrediosDistColegiosPlaza.dbf") %>% 
  as.data.frame()

df.dist1 = df.dist1 %>% dplyr::select(NUMERO_PRE, D_SP_PLAZA, D_SP_CL_MN, D_SP_CL_MX, D_SP_CL_MU)

df.dist2 = foreign::read.dbf("F:/OneDrive/Escritorio/IGAC/01_Distances/data/PrediosAracataca/PrediosDistCSalud.dbf") %>% 
  as.data.frame()

df.dist2 = df.dist2 %>% dplyr::select(NUMERO_PRE, D_SP_CS_MN, D_SP_CS_MX, D_SP_CS_MU)

##========== Datos estrato
df.otros = read_sf(dsn = "F:/OneDrive/Escritorio/IGAC/02_PrecioVivienda/datos/ShapeAracatacaNDVI/")
df.otros = df.otros %>% 
  as.data.frame()

df.otros = df.otros %>% 
  dplyr::select(NUMERO_PRE, NDVI_MEAN, Energia, Estrato, Acueducto, Alcantaril, Basura, Internet) %>% 
  distinct(NUMERO_PRE, .keep_all = T)

df.otros$NDVI_MEAN[df.otros$NDVI_MEAN == -9999] = NA
df.otros$Estrato = str_remove_all(df.otros$Estrato, "Estrato ") %>% as.numeric

#============================#
##= Arreglo de los datos  ####
#============================#
## Combinar para obtener distancias
df = df %>% left_join(df.dist1 %>% distinct(NUMERO_PRE, .keep_all = T), by = "NUMERO_PRE")
df = df %>% left_join(df.dist2 %>% distinct(NUMERO_PRE, .keep_all = T), by = "NUMERO_PRE")
df = df %>% left_join(df.otros, by = "NUMERO_PRE")

# Arreglar datos
df = df %>% 
  dplyr::rename(c("PC" = "VALOR_CONS","PV" = "AV_COMERCI", "PT" = "VALOR_TERR", "AT" = "AREA_TERRE", "AC" = "AREA_CONST"))

colnames(df) = toupper(stringi::stri_trans_general(str = colnames(df), id = "Latin-ASCII"))
colnames(df) = stringr::str_remove_all(colnames(df), "_")

## Crear indicador de si el predio tiene una, 2 o 3 construcciones. 
## Por los filtros aplicados, todos tienen al menos la primera edificación
df$EDIF1 = apply(df[,c("HABITACION","BANOS1", "LOCALES1", "PISOS1")] %>% mutate_all(as.numeric), 1, sum) 
df$EDIF2 = apply(df[,c("HABITACI1","BANOS2", "LOCALES2", "PISOS2")] %>% mutate_all(as.numeric), 1, sum) 
df$EDIF3 = apply(df[,c("HABITACI2","BANOS3", "LOCALES3", "PISOS3")] %>% mutate_all(as.numeric), 1, sum) 

df = df %>% 
  mutate(
    EDIF1 = ifelse(EDIF1 > 0, 1, 0),
    EDIF2 = ifelse(EDIF2 > 0, 1, 0),
    EDIF3 = ifelse(EDIF3 > 0, 1, 0),
    NEDIF = EDIF1 + EDIF2 + EDIF3)

## Crear indicadoras para servicios
df = df %>% 
  mutate(
    ACUEDUCTO = ifelse(ACUEDUCTO == "Si", 1, 0),
    ALCANTARIL = ifelse(ALCANTARIL == "Si", 1, 0),
    BASURA = ifelse(BASURA == "Si", 1, 0),
    INTERNET = ifelse(INTERNET == "Si", 1, 0) )

#===========================================================#
#     2. Obtener estrato usando distancias cercanas      ####
#===========================================================#
## Calcular distancias
MM = dist(df[,c("COORDX","COORDY")], diag = T, upper = T) %>% 
  as.matrix()

rownames(MM) = df$NUMEROPRE
colnames(MM) = df$NUMEROPRE

## Imputar variables para los 5 predios más cercanos que tienen esa info
v.est = NULL
v.dist.f = NULL
v.dist.fsd = NULL

var.to.impute = c("ESTRATO","ACUEDUCTO", "ALCANTARIL", "BASURA", "INTERNET")

df.tmp = df %>% filter(!is.na(INTERNET))

for(ii in (df %>% filter(is.na(INTERNET)) %>% select(NUMEROPRE) %>% pull) ){
  v.dist = MM[ii,] %>% sort
  v.dist = v.dist[names(v.dist) %in% df.tmp$NUMEROPRE]
  v.dist = v.dist[names(v.dist) != ii]
  
  v.est = rbind(
    v.est, 
    df %>% 
      filter(NUMEROPRE %in% names(v.dist)[1:5] ) %>%
      select(all_of(var.to.impute)) %>% 
      summarise_all(get_mode) %>% 
      as.data.frame() %>% 
      mutate(NUMEROPRE = ii) )
  
  v.dist.f = c(v.dist.f, mean(v.dist[1:5]))
  v.dist.fsd = c(v.dist.fsd, sd(v.dist[1:5]))
  
}

v.est$DISTIMPU = v.dist.f
v.est$DISTIMPUSD = v.dist.fsd

colnames(v.est) = paste0(colnames(v.est), "_IMPU")

df = df %>% left_join(v.est, by = c("NUMEROPRE" = "NUMEROPRE_IMPU"))
df = df %>% 
  mutate(
    INTERNET = ifelse(is.na(INTERNET), INTERNET_IMPU, INTERNET),
    ACUEDUCTO = ifelse(is.na(ACUEDUCTO), ACUEDUCTO_IMPU, ACUEDUCTO),
    ALCANTARIL = ifelse(is.na(ALCANTARIL), ALCANTARIL_IMPU, ALCANTARIL),
    BASURA = ifelse(is.na(BASURA), BASURA_IMPU, BASURA),
    ESTRATO = ifelse(is.na(ESTRATO), ESTRATO_IMPU, ESTRATO) ) %>% 
  select(-INTERNET_IMPU, -ACUEDUCTO_IMPU, -ALCANTARIL_IMPU, -BASURA_IMPU)

rm(v.est, v.dist.f, v.dist.fsd)

dir.create(path.out)
saveRDS(df, paste0(path.out, "/datos_completos_modelo_xgb4.rds"))

#=====================================================#
#          2. Selección de variables            #######
#=====================================================#
## Ver frecuencias de las variables para saber si incluirlas o no
table(df$HABITACION)
table(df$BANOS1)
table(df$LOCALES1) # Muy poca frecuencia
table(df$PISOS1)
table(df$TIPIFICACI)
table(df$USO1) # Muy poca frecuencia
table(df$PUNTAJE1)

## Calcular matriz de correlaciones para ver multicolinealidad
cor(df[,c("HABITACION","BANOS1","PISOS1","TIPIFICACI","PUNTAJE1","NEDIF","PC")] %>% 
      mutate_all(as.numeric))

cor(df[,c("ESTRATO","ANCHOVIA","DSPPLAZA", "DSPCLMN", "DSPCLMX", "DSPCLMU", 
          "DSPCSMN", "DSPCSMX", "DSPCSMU","PT")] %>% 
      mutate_all(as.numeric))

## Hacer regresión para seleccionar variables para PT
var.pt.tmp = c("ESTRATO", "DSPCLMN", "DSPCLMX", "DSPCLMU", 
               "DSPCSMN", "DSPCSMX", "DSPCSMU",
               "NDVIMEAN","ACUEDUCTO","BASURA","ALCANTARIL","INTERNET")

mod_tmp = lm( paste0("log(PT/AT) ~ ", paste0(var.pt.tmp, collapse = " + ")) %>% as.formula, df)

seleccion_por_vif(
  datos = df %>% mutate(y = PT/AT) %>% .[,c("y", var.pt.tmp)], 
  var_respuesta = "y", umbral_vif = 10)

# Selección stepwise
mod_tmp = lm( paste0("log(PT/AT) ~ ", paste0(c("ESTRATO", "DSPCLMN","DSPCLMX", "DSPCSMN", "NDVIMEAN","ACUEDUCTO", "BASURA", "ALCANTARIL", "INTERNET"), collapse = " + ")) %>% as.formula, df)
MASS::stepAIC(mod_tmp, direction = "both") %>% summary

rm(var.pt.tmp, mod_tmp)
## Hacer regresión para seleccionar variables para PC
var.pc.tmp = c("HABITACION","BANOS1","PISOS1","TIPIFICACI","PUNTAJE1","NEDIF","ESTRATO","ACUEDUCTO","ALCANTARIL","BASURA","INTERNET")

mod_tmp = lm( paste0("log(PC/AC) ~ ", paste0(var.pc.tmp, collapse = " + ")) %>% as.formula, df %>% mutate_at(all_of(var.pc.tmp), as.numeric) )

seleccion_por_vif(
  datos = df %>% mutate(y = log(PC/AC)) %>% .[,c("y", var.pc.tmp)] %>% mutate_at(var.pc.tmp, as.numeric), 
  var_respuesta = "y", umbral_vif = 10)

# Selección stepwise
mod_tmp = lm( paste0("log(PC/AC) ~ ", paste0(var.pc.tmp, collapse = " + ")) %>% as.formula, df %>% mutate_at(all_of(var.pc.tmp), as.numeric))
MASS::stepAIC(mod_tmp, direction = "both") %>% summary
summary(mod_tmp)

rm(var.pc.tmp, mod_tmp)

#===================#
# Variables finales #
#===================#
# predictores para PT
# x_names <- c("ESTRATO","DSPCLMN","DSPCLMX","DSPCSMN","NDVIMEAN","ACUEDUCTO","ALCANTARIL","BASURA","INTERNET")
x_names = c("ESTRATO", "DSPCLMN", "DSPCLMX", "DSPCSMN", "ACUEDUCTO", "BASURA")

# predictores para PC
# z_names <- c("HABITACION","BANOS1","PISOS1","TIPIFICACI","PUNTAJE1","NEDIF","ESTRATO","ACUEDUCTO","ALCANTARIL","BASURA","INTERNET") 
z_names = c("TIPIFICACI","PUNTAJE1","NEDIF","ESTRATO","ACUEDUCTO","ALCANTARIL")

## Convertir en numéricas
df = df %>% 
  mutate_at(c(x_names, z_names), as.numeric) # Validar que sí todas sean numéricas

# Comprobar columnas mínimas
required_cols <- c("PV","PT","PC", x_names, z_names)
if(!all(required_cols %in% names(df))) stop("Faltan columnas en df. Ajusta x_names/z_names o los nombres en df.")

# Construir strings con los predictores estandarizados
PC_x = paste0(z_names, "std") # Variables finales para PC
PT_x = paste0(x_names, "std") # Variables finales para PT

cols_to_scale_x = x_names
cols_to_scale_z = z_names

#======================================#
#        Validación cruzada      #######
#======================================#
k = 5 
seed = 20
data_cv = df

# Create folds
set.seed(45)
folds <- create_cv_folds(data_cv, k = k, seed = seed)

# Initialize storage for predictions and metrics
cv_models <- list()
cv_metrics <- NULL
res = NULL

# Perform cross-validation
for (i in 1:k) {
  cat("Processing fold", i, "of", k, "...\n")
  
  # Split data
  test_idx <- folds[[i]]
  
  train_data <- data_cv[-test_idx, ]
  test_data <- data_cv[test_idx, ]
  
  # Standardize
  for(nm in unique(c(cols_to_scale_x, cols_to_scale_z))){
    mu <- mean(train_data[[nm]], na.rm = TRUE)
    sdv <- sd(train_data[[nm]], na.rm = TRUE)
    
    train_data[[paste0(nm, "std")]] <- (train_data[[nm]] - mu) / sdv
    test_data[[paste0(nm, "std")]] <- (test_data[[nm]] - mu) / sdv
  }
  
  # Fit model on training data
  # grid <- expand.grid(
  #   n.trees = c(10,20,50), #c(100, 300, 500, 800),
  #   interaction.depth = c(1, 3),
  #   shrinkage = seq(0.1,0.2, 0.02),
  #   n.minobsinnode = c(3, 10, 20)
  # )
  
  grid <- expand.grid(
    nrounds = c(150, 200),
    max_depth = 12, #c(9, 12),
    eta = seq(0.01, 0.05, 0.01),
    gamma = 0, # c(0, 1),
    colsample_bytree = 0.8, #c(0.6, 0.8),
    min_child_weight = 3, #c(3,5), ##c(3, 5),
    subsample = c(0.8)
  )
  
  cat("=======> Fitting PT")
  fit_PT <- train(
    paste0("PT/AT ~ 1 + ", paste0(PT_x, collapse = " + ")) %>% as.formula,
    # PT/AT ~ 1 + ESTRATOstd + ANCHOVIAstd + DSPCLMNstd + DSPCLMXstd + DSPCSMNstd,
    data = train_data,
    method = "xgbTree", #"xgbTree", #"gbm",
    tuneGrid = grid, verbosity = 0,
    verbose = F
  )
  
  cat("=======> Fitting PC\n")
  fit_PC <- train(
    paste0("PC/AC ~ 1 + ", paste0(PC_x, collapse = " + ")) %>% as.formula,
    data = train_data,
    method = "xgbTree", #"xgbTree", #"gbm",
    tuneGrid = grid,
    verbose = F
  )
  
  # plot(fit) # Mirar tuneo de parámetros
  # fit$bestTune # Mejor modelo
  
  cv_models[[i]] = list(PT = fit_PT, PC = fit_PC)
  
  ## Predicciones test y train
  est_PT_train = predict(fit_PT, train_data)
  est_PC_train = predict(fit_PC, train_data)
  est_PT_test = predict(fit_PT, test_data)
  est_PC_test = predict(fit_PC, test_data)
  
  est_PT_train = ifelse(est_PT_train < 0, 0, est_PT_train)
  est_PC_train = ifelse(est_PC_train < 0, 0, est_PC_train)
  est_PT_test = ifelse(est_PT_test < 0, 0, est_PT_test)
  est_PC_test = ifelse(est_PC_test < 0, 0, est_PC_test)
  
  ## Residuales
  res = rbind(data.frame(NUMEROPRE = test_data$NUMEROPRE,
                         PV_est = (est_PT_test*test_data$AT) + (est_PC_test*test_data$AC), PV_obs = test_data$PV,
                         PC_est = (est_PC_test*test_data$AC), PC_obs = test_data$PC,
                         PT_est = (est_PT_test*test_data$AT), PT_obs = test_data$PT),
              res)
  
  ## Métricas
  metrics_PV_train <- calculate_metrics(train_data$PV, (est_PT_train*train_data$AT) + (est_PC_train*train_data$AC), df.int = NULL) %>% mutate(set = "Train", Variable = "PV")
  metrics_PC_train <- calculate_metrics(train_data$PC, (est_PC_train*train_data$AC), df.int = NULL) %>% mutate(set = "Train", Variable = "PC")
  metrics_PT_train <- calculate_metrics(train_data$PT, (est_PT_train*train_data$AT), df.int = NULL) %>% mutate(set = "Train", Variable = "PT")
  
  metrics_PV_test <- calculate_metrics(test_data$PV, (est_PT_test*test_data$AT) + (est_PC_test*test_data$AC), df.int = NULL) %>% mutate(set = "Test", Variable = "PV")
  metrics_PC_test <- calculate_metrics(test_data$PC, (est_PC_test*test_data$AC), df.int = NULL) %>% mutate(set = "Test", Variable = "PC")
  metrics_PT_test <- calculate_metrics(test_data$PT, (est_PT_test*test_data$AT), df.int = NULL) %>% mutate(set = "Test", Variable = "PT")
  
  cv_metrics = rbind(
    cv_metrics, 
    rbind(metrics_PV_train, metrics_PC_train, metrics_PT_train,
          metrics_PV_test, metrics_PC_test, metrics_PT_test) %>% mutate(Fold = i) ) 
  
  rm(fit_PC, fit_PT)
}

cv_metrics_global = rbind(
  cv_metrics %>% 
    group_by(set, Variable) %>% 
    summarise_all(.,mean) %>% 
    as.data.frame() %>% 
    select(-Fold) %>% 
    mutate(Metric = "Mean"),
  cv_metrics %>% 
    group_by(set, Variable) %>% 
    summarise_all(.,sd) %>% 
    as.data.frame() %>% 
    select(-Fold) %>% 
    mutate(Metric = "Sd"),
  cv_metrics %>% 
    group_by(set, Variable) %>% 
    summarise_all(.,min) %>% 
    as.data.frame() %>% 
    select(-Fold) %>% 
    mutate(Metric = "Min"),
  cv_metrics %>% 
    group_by(set, Variable) %>% 
    summarise_all(.,max) %>% 
    as.data.frame() %>% 
    select(-Fold) %>% 
    mutate(Metric = "Max")
)

res = res %>% 
  mutate(
    PV_res = PV_est - PV_obs,
    PC_res = PC_est - PC_obs,
    PT_res = PT_est - PT_obs,
    PV_res_porc = PV_res/PV_obs,
    PC_res_porc = PC_res/PC_obs,
    PT_res_porc = PT_res/PT_obs
  )

## Exportar resultados
dir.create(path.out)
saveRDS(cv_models, "out/mod_xgboost_4.rds")

writexl::write_xlsx(
  list(DataTest = res, Metrics = cv_metrics_global, Metrics_Folds = cv_metrics),
  paste0(path.out, "data_results.xlsx"))
