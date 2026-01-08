cat("\014")
rm(list = ls())


library(dplyr)
library(caret)
library(sf)

path_out = "F:/OneDrive/Escritorio/IGAC/02_PrecioVivienda/simulacion_hospitales/01_iteration/"

source("00_Funciones.R", encoding = "UTF-8")

#================================================#
#               Lectura de datos              ####
#================================================#
## Datos modelo
df = readRDS("./out/results/xgboost_4/datos_completos_modelo_xgb4.rds")
df$GEOMETRY = NULL

## Datos simulación
# Lectura distancais nuevas de hospitales
df.dist = foreign::read.dbf("F:/OneDrive/Escritorio/IGAC/01_Distances/data/PrediosAracataca/PrediosDistCSalud_Simulacion.dbf") %>% 
  as.data.frame()

df.dist = df.dist %>% 
  transmute(NUMEROPRE = as.character(NUMERO_PRE), DSPCSMN = as.numeric(D_SP_CS_MN)) %>% 
  distinct(NUMEROPRE, .keep_all = T) 

# Obtener distancia nueva a centros de salud
df.sim = df %>% 
  dplyr::select(-DSPCSMN) %>% 
  dplyr::left_join(df.dist, by = "NUMEROPRE")

#===================#
# Variables finales #
#===================#
# predictores para PT
x_names <- c("ESTRATO", "DSPCLMN", "DSPCLMX", "DSPCSMN", "ACUEDUCTO", "BASURA")

# predictores para PC
z_names <- c("TIPIFICACI","PUNTAJE1","NEDIF","ESTRATO","ACUEDUCTO","ALCANTARIL")

## Convertir en numéricas
df = df %>% 
  mutate_at(c(x_names, z_names), as.numeric) # Validar que sí todas sean numéricas

df.sim = df.sim %>% 
  mutate_at(c(x_names, z_names), as.numeric) # Validar que sí todas sean numéricas

# Comprobar columnas mínimas
required_cols <- c("PV","PT","PC", x_names, z_names)
if(!all(required_cols %in% names(df))) stop("Faltan columnas en df. Ajusta x_names/z_names o los nombres en df.")

# Construir strings con los predictores estandarizados
PC_x = paste0(z_names, "std") # Variables finales para PC
PT_x = paste0(x_names, "std") # Variables finales para PT

cols_to_scale_x = x_names
cols_to_scale_z = z_names

#=======================================================#
#  2. Entrenamiento del modelo con todos los datos   ####
#=======================================================#
## Cargar modelos para ver parámetros óptimos
cv_models = readRDS("out/mod_xgboost_4.rds")

## Parámetros óptimos para PT
rbind(
cv_models[[1]]$PT$finalModel$tuneValue %>% as.data.frame,
cv_models[[2]]$PT$finalModel$tuneValue %>% as.data.frame,
cv_models[[3]]$PT$finalModel$tuneValue %>% as.data.frame,
cv_models[[4]]$PT$finalModel$tuneValue %>% as.data.frame,
cv_models[[5]]$PT$finalModel$tuneValue %>% as.data.frame)

## Parámetros óptimos para PC
rbind(
  cv_models[[1]]$PC$finalModel$tuneValue %>% as.data.frame,
  cv_models[[2]]$PC$finalModel$tuneValue %>% as.data.frame,
  cv_models[[3]]$PC$finalModel$tuneValue %>% as.data.frame,
  cv_models[[4]]$PC$finalModel$tuneValue %>% as.data.frame,
  cv_models[[5]]$PC$finalModel$tuneValue %>% as.data.frame)

## Entrenar modelo con todos los datos
train_data <- df
simu_data = df.sim

# Standardize
for(nm in unique(c(cols_to_scale_x, cols_to_scale_z))){
  mu <- mean(train_data[[nm]], na.rm = TRUE)
  sdv <- sd(train_data[[nm]], na.rm = TRUE)
  
  train_data[[paste0(nm, "std")]] <- (train_data[[nm]] - mu) / sdv
  simu_data[[paste0(nm, "std")]] <- (simu_data[[nm]] - mu) / sdv
}


cat("=======> Fitting PT")
fit_PT <- train(
  paste0("PT/AT ~ 1 + ", paste0(PT_x, collapse = " + ")) %>% as.formula,
  # PT/AT ~ 1 + ESTRATOstd + ANCHOVIAstd + DSPCLMNstd + DSPCLMXstd + DSPCSMNstd,
  data = train_data,
  method = "xgbTree", #"xgbTree", #"gbm",
  tuneGrid = expand.grid(
    nrounds = 150,
    max_depth = 12,
    eta = 0.03,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 3,
    subsample = 0.8
  ), 
  verbosity = 0,
  verbose = F
)

cat("=======> Fitting PC\n")
fit_PC <- train(
  paste0("PC/AC ~ 1 + ", paste0(PC_x, collapse = " + ")) %>% as.formula,
  data = train_data,
  method = "xgbTree", #"xgbTree", #"gbm",
  tuneGrid = expand.grid(
    nrounds = 150,
    max_depth = 12,
    eta = 0.03,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 3,
    subsample = 0.8
  ),
  verbose = F
)

# plot(fit) # Mirar tuneo de parámetros
# fit$bestTune # Mejor modelo

cv_models_final = list(PT = fit_PT, PC = fit_PC)

## Predicciones test y train
est_PT_train = predict(fit_PT, train_data)
est_PC_train = predict(fit_PC, train_data)
est_PT_test = predict(fit_PT, simu_data)
est_PC_test = predict(fit_PC, simu_data)

est_PT_train = ifelse(est_PT_train < 0, 0, est_PT_train)
est_PC_train = ifelse(est_PC_train < 0, 0, est_PC_train)
est_PT_test = ifelse(est_PT_test < 0, 0, est_PT_test)
est_PC_test = ifelse(est_PC_test < 0, 0, est_PC_test)


#=====================================================#
#        3. Arreglar datos con la simulación      #####
#=====================================================#
df.finales = train_data %>% 
  left_join(simu_data %>% select(NUMEROPRE, DSPCSMNSIM = DSPCSMN), by = "NUMEROPRE") %>% 
  mutate(
    PTATest = est_PT_train,
    PCACest = est_PC_train,
    PTATsim = est_PT_test,
    PCACsim = est_PC_test) %>% 
  mutate(
    PTAT = PT/AT, PCAC = PC/AC,
    PTATchg = round(100*(PTATest - PTATsim)/PTATest,3),
    PCACchg = round(100*(PCACest - PCACsim)/PCACest,3)) %>% 
  mutate(
    CATDSCS = case_when(
      DSPCSMN*1000 <= 100 ~ "01_100m o menos",
      DSPCSMN*1000 <= 200 ~ "02_200m o menos",
      DSPCSMN*1000 <= 300 ~ "03_300m o menos",
      DSPCSMN*1000 <= 400 ~ "04_400m o menos",
      DSPCSMN*1000 <= 500 ~ "05_500m o menos",
      DSPCSMN*1000 <= 750 ~ "06_750m o menos",
      DSPCSMN*1000 <= 1000 ~ "07_1000m o menos",
      DSPCSMN*1000 <= 1500 ~ "08_1500m o menos",
      DSPCSMN*1000 > 1500 ~ "09_Más de 1500m"),
    CATDSCSSIM = case_when(
      DSPCSMNSIM*1000 <= 100 ~ "01_100m o menos",
      DSPCSMNSIM*1000 <= 200 ~ "02_200m o menos",
      DSPCSMNSIM*1000 <= 300 ~ "03_300m o menos",
      DSPCSMNSIM*1000 <= 400 ~ "04_400m o menos",
      DSPCSMNSIM*1000 <= 500 ~ "05_500m o menos",
      DSPCSMNSIM*1000 <= 750 ~ "06_750m o menos",
      DSPCSMNSIM*1000 <= 1000 ~ "07_1000m o menos",
      DSPCSMNSIM*1000 <= 1500 ~ "08_1500m o menos",
      DSPCSMNSIM*1000 > 1500 ~ "09_Más de 1500m")  ) %>% 
  relocate(
    NUMEROPRE, 
    PTAT, PTATest, PTATsim, PTATchg, AT,
    PCAC, PCACest, PCACsim, PCACchg, AC, 
    DSPCSMN, CATDSCS, DSPCSMNSIM, CATDSCSSIM)


## Cargar shape
df.shp = read_sf(dsn = "F:/OneDrive/Escritorio/IGAC/01_Distances/data/PrediosAracataca/Predios")

df.shp = df.shp %>% 
  rename(NUMEROPRE = NUMERO_PRE) %>% 
  inner_join(
    df.finales %>% 
      select(
        NUMEROPRE, 
        PTAT, PTATest, PTATsim, PTATchg, AT,
        PCAC, PCACest, PCACsim, PCACchg, AC, 
        DSPCSMN, CATDSCS, DSPCSMNSIM, CATDSCSSIM),
    by = "NUMEROPRE")

## Exportar shape con principales variables
dir.create(paste0(path_out, "ShapeHospitales/"))
sf::st_write(df.shp, paste0(path_out, "ShapeHospitales/ShapeHospitales_01.shp"))

## Guardar datos
writexl::write_xlsx(df.finales, paste0(path_out, "datos_simulacion_hospitales_20251214.xlsx"))

  


#=#=#=#=#=#=#=# 
df.shp = sf::read_sf(paste0(path_out, "ShapeHospitales/ShapeHospitales_01.shp"))


classInt::classIntervals(df.shp$PTAT, style = "fisher", n = 6)
classInt::classIntervals(df.shp$PTATest, style = "fisher", n = 8)
classInt::classIntervals(df.shp$PTATest[df.shp$CATDSCS != df.shp$CATDSCSSIM], style = "kmeans", n = 4)
classInt::classIntervals(df.shp$PTATest, style = "", n = 8)

df.shp1 = df.shp %>% 
  mutate(
    CATobs = case_when(
      PTAT < 27000 ~ "01 <27k",
      PTAT < 46000 ~ "02 <46k",
      PTAT < 67000 ~ "03 <67k",
      PTAT < 100000 ~ "04 <100k",
      PTAT >= 100000 ~ "06 >100k",
      TRUE ~ "10 Other") %>% factor(., levels = c("01 <27k", "02 <46k", "03 <67k", "04 <100k", "06 >100k")),
    CATest = case_when(
      PTATest < 27000 ~ "01 <27k",
      PTATest < 46000 ~ "02 <46k",
      PTATest < 67000 ~ "03 <67k",
      PTATest < 100000 ~ "04 <100k",
      PTATest >= 100000 ~ "06 >100k",
      TRUE ~ "10 Other") %>% factor(., levels = c("01 <27k", "02 <46k", "03 <67k", "04 <100k", "06 >100k")),
    CATsim = case_when(
      PTATsim < 27000 ~ "01 <27k",
      PTATsim < 46000 ~ "02 <46k",
      PTATsim < 67000 ~ "03 <67k",
      PTATsim < 100000 ~ "04 <100k",
      PTATsim >= 100000 ~ "06 >100k",
      TRUE ~ "10 Other") %>% factor(., levels = c("01 <27k", "02 <46k", "03 <67k", "04 <100k", "06 >100k")))

Diverging
BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral

Qualitative
Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

Sequential
Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd

colors_manual = c("#0000FF", "#8000FF", "#FF00FF", "#FF0080", "#FF0000")
names(colors_manual) = levels(df.shp1$CATest)

par(mfrow = c(1,2))
gg0 = df.shp1 %>% 
  filter(DSPCSMN != DSPCSMNSIM) %>%
  ggplot(.) +
  geom_sf(aes(fill = CATobs), color= "grey") +
  scale_fill_brewer(palette = "RdBu", direction = -1, drop = F) +
  # scale_fill_manual(values = colors_manual, drop = F) +
  # scale_fill_manual()
  theme_minimal() +
  labs(title = "Observado")
gg1 = df.shp1 %>% 
  filter(DSPCSMN != DSPCSMNSIM) %>%
  ggplot(.) +
  geom_sf(aes(fill = CATest), color= "grey") +
  scale_fill_brewer(palette = "RdBu", direction = -1, drop = F) +
  # scale_fill_manual(values = colors_manual, drop = F) +
  # scale_fill_manual()
  theme_minimal() +
  labs(title = "Estimado")
gg2 = df.shp1 %>% 
  filter(DSPCSMN != DSPCSMNSIM) %>%
  ggplot(.) +
  geom_sf(aes(fill = CATsim), color= "grey") +
  scale_fill_brewer(palette = "RdBu", direction = -1, drop = FALSE) +
  # scale_fill_manual(values = colors_manual, drop =F) + 
  # scale_fill_manual()
  theme_minimal() +
  labs(title = "Simulado")
par(mfrow = c(1,1))

ggpubr::ggarrange(gg0, gg1, gg2, ncol = 3)


## Impacto
df.shp1 %>% 
  mutate(FLAG = ifelse(PTATest != PTATsim, "Impacted", "No change")) %>% 
  ggplot(.) +
  geom_sf(aes(fill = FLAG), color= "grey") +
  # scale_fill_brewer(palette = "RdBu", direction = -1, drop = F) +
  # scale_fill_manual(values = colors_manual, drop = F) +
  # scale_fill_manual()
  theme_minimal() 


## Cambio porcentual y absolito
PTATchg
classInt::classIntervals(
  df.shp1$DIFestsim[df.shp1$DIFestsim != 0] ,
  style = "kmeans", n = 5)


df.shp1 = df.shp1 %>% 
  mutate(
    DIFestsim = PTATsim - PTATest,
    PTATchg = DIFestsim/PTATest) 


df.shp1 %>% 
  filter(DSPCSMN != DSPCSMNSIM) %>%
  mutate(FLAG = ifelse(PTATest != PTATsim, "Impacted", "No change")) %>% 
  ggplot(aes(x = CATDSCSSIM, y = DIFestsim, fill = CATobs)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_fill_brewer(palette = "RdBu", direction = -1, drop = FALSE) +
  theme_minimal() + 
  theme(legend.position = "bottom")





# Grafica de Dif con intervalos de confianza
# Mapa de la diferencia
  


# Efecto total: Algoritmo + Hospital (Después)
# Efecto algoritmo: Primera estimación sin hospital
# Efecto total: Estimación con Hospital (Después)


# Total: Después con hospital - Catastro
# Algoritmo: Antes sin hospital - Catastro
# Hospital: Después con hospital - Antes sin hospital (anomalias)


# Después: Algoritmo + Hospital
# Antes: Algoritmo
# Hospital => Después - Algoritmo
# Hospital => Después - Antes

