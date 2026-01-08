# En este código se exploran las metodologías para identificar outliers con los 
# resultados del XGBoost con todas las viviendas de Aracataca.
# Se definen las metodologías aplicadas para identificar outliers y se 
# ejecuta con la versión 4 del modelo XGBoost.

rm(list = ls(all = T))

options(scipen = 999)

library(stringr)
library(ggplot2)
library(sf)
library(cluster) 
library(dplyr)

path_out = "F:/OneDrive/Escritorio/IGAC/02_PrecioVivienda/outliers/results/03_iteration/"
dir.create(path_out, showWarnings = F)

qqoutliers = function(x){
  return_x = c(
    quantile(x, probs = 0.25) - 1.5*IQR(x),
    quantile(x, probs = 0.75) + 1.5*IQR(x)
  )
  
  return(return_x)
}

#=============================# 
#      1.Cargar resultados ####
#=============================#
{
  ## Cargar datos
  df.xg = readxl::read_excel("./out/results/xgboost_4/data_results.xlsx", sheet = "DataTest") %>% 
    as.data.frame()
  
  ## Arreglar datos para unir
  colnames(df.xg)[colnames(df.xg) != "NUMEROPRE"] = paste0(colnames(df.xg)[colnames(df.xg) != "NUMEROPRE"], "_X")
  
  ## Eliminar datos observados ya que estan en base completa
  df.xg = df.xg[,str_subset(colnames(df.xg), "\\_obs\\_", negate = T)]
  
  colnames(df.xg) = str_remove_all(colnames(df.xg), "\\_")
}

# Datos modelo
df = readRDS("./out/results/xgboost_4/datos_completos_modelo_xgb4.rds")

## Combinar datos
df.comb = df %>% 
  left_join(df.xg, by = "NUMEROPRE")

#======================================#
#      2. Calcular otras métricas ######
#======================================#
df.comb = df.comb %>% 
  mutate(
    # Porcentajes terreno y construido sobre el total
    PTestPVX = PTestX/PVestX,
    # PTestPVBY = PTestBY/PVestBY,
    PTobsPV = PT/PV,
    # Razón terreno y construido  por área
    RPTestPCX = (PTestX/AT)/(PCestX/AC),
    # RPTestPCBY = (PTestBY/AT)/(PCestBY/AC),
    RPTobsPC = (PT/AT)/(PC/AC)
  ) %>% # Residuales
  mutate(
    PTestX_AT = PTestX/AT,
    PCestX_AC = PCestX/AC,
    PTresX_AT = PTestX_AT - (PT/AT),
    PCresX_AC = PCestX_AC - (PC/AC) )

##========== Porcentaje terreno/total ####
# Dispersión
png(paste0(path_out, "porc_pt.png"), width = 8, height = 6, res = 400, units = "in")
print(
  df.comb %>% 
    ggplot(aes(x = PTobsPV, y = PTestPVX)) + 
    geom_point() + 
    geom_abline(slope = 1, linetype = "dashed") +
    labs(
      title = "Percentage Terrain/Total",
      x = "Observed", y = "Predicted") +
    theme_minimal(base_size = 13)
)
dev.off()

# Distribución
df_long <- df.comb %>% 
  tidyr::pivot_longer(
    cols = c(PTobsPV, PTestPVX),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  mutate(Tipo = case_when(
    Tipo == "PTobsPV" ~ "Observed",
    Tipo == "PTestPVX" ~ "XGBoost",
    Tipo == "PTestPVBY" ~ "Bayesian"
  ))

estadisticas <- df_long %>%
  group_by(Tipo) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE)
  )

png(paste0(path_out, "porc_pt_distribucion.png"), width = 10, height = 6, res = 400, units = "in")
print(
  df_long %>%
    ggplot(aes(x = Valor, fill = Tipo)) + 
    geom_histogram(alpha = 0.7, bins = 20) + 
    geom_vline(data = estadisticas, aes(xintercept = Media, color = "Mean"), 
               linetype = "dashed", linewidth = 1) +
    geom_vline(data = estadisticas, aes(xintercept = Mediana, color = "Median"), 
               linetype = "solid", linewidth = 1) +
    facet_wrap(~Tipo) +
    scale_color_manual(name = "", values = c("Mean" = "grey50", "Median" = "grey50")) +
    labs(x = "Percentage Terrain/Total", y = "Frequency", fill = "") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
)
dev.off()


rm(df_long, estadisticas)

##========== Razón (PT/AT)/(PC/AC) ####
gg1 = df.comb %>% 
  ggplot(aes(x = RPTobsPC, y = RPTestPCX)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = "dashed") +
  labs(
    title = "Ratio PT/AT over PC/AC",
    x = "Observed", y = "Predicted") +
  theme_minimal(base_size = 13)

gg2 = df.comb %>% 
  filter(RPTobsPC < 2) %>% 
  ggplot(aes(x = RPTobsPC, y = RPTestPCX)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = "dashed") +
  labs(
    title = "Ratio PT/AT over PC/AC",
    subtitle = "Ratio lower than 2",
    x = "Observed", y = "Predicted") +
  theme_minimal(base_size = 13)

png(paste0(path_out, "raz_pt_pc.png"), width = 12, height = 6, res = 400, units = "in")
print(ggpubr::ggarrange(gg1, gg2, nrow = 1, ncol = 2))
dev.off()

# Distribución
df_long <- df.comb %>% 
  tidyr::pivot_longer(
    cols = c(RPTobsPC, RPTestPCX),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  mutate(Tipo = case_when(
    Tipo == "RPTobsPC" ~ "Observed",
    Tipo == "RPTestPCX" ~ "XGBoost",
    Tipo == "RPTestPCBY" ~ "Bayesian"
  ))

estadisticas <- df_long %>%
  group_by(Tipo) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE)
  )

png(paste0(path_out, "raz_pt_pc_distribucion.png"), width = 12, height = 6, res = 400, units = "in")
print(
  df_long %>%
    filter(Valor < 2) %>% 
    ggplot(aes(x = Valor, fill = Tipo)) + 
    geom_histogram(alpha = 0.7, bins = 30) + 
    geom_vline(data = estadisticas, aes(xintercept = Media, color = "Mean"), 
               linetype = "dashed", linewidth = 1) +
    geom_vline(data = estadisticas, aes(xintercept = Mediana, color = "Median"), 
               linetype = "solid", linewidth = 1) +
    facet_wrap(~Tipo) +
    scale_color_manual(name = "", values = c("Mean" = "grey50", "Median" = "grey50")) +
    labs(
      x = "Ratio PT/AT over PC/AC", y = "Frequency", fill = "",
      title = "Distribution ratio PT/AT over PC/AC",
      subtitle = "Ratio lower than 2") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
)
dev.off()

rm(df_long, estadisticas)


##=============== Residuales ####

## Comparación contra valores observados
gg1 = df.comb %>% 
  ggplot(aes(x = PT/AT, y = 100*PTresporcX)) + 
  geom_point() + 
  labs(
    y = "% Residuals PT/AT",
    x = "Price terrain per square meter (PT/AT)")

gg2 = df.comb %>% 
  ggplot(aes(x = PC/AC, y = 100*PCresporcX)) + 
  geom_point() + 
  labs(
    y = "% Residuals PC/AC",
    x = "Price terrain per square meter (PC/AC)")


png(paste0(path_out, "res_porc_vs_obs.png"), width = 12, height = 6, res = 400, units = "in")
print(ggpubr::ggarrange(gg1, gg2, nrow = 1, ncol = 2))
dev.off()

## Distribución residuales
df_long <- df.comb %>% 
  tidyr::pivot_longer(
    cols = c(PVresporcX, PTresporcX, PCresporcX),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  mutate(Tipo = case_when(
    Tipo == "PVresporcX" ~ "Total price",
    Tipo == "PTresporcX" ~ "Terrain price",
    Tipo == "PCresporcX" ~ "Construction price"
  ))

estadisticas <- df_long %>%
  group_by(Tipo) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE)
  )

png(paste0(path_out, "res_porc_distribucion.png"), width = 12, height = 6, res = 400, units = "in")
print(
  df_long %>%
    filter(Valor*100 < 100) %>% 
    ggplot(aes(x = Valor*100, fill = Tipo)) + 
    geom_histogram(alpha = 0.7, bins = 30) + 
    geom_vline(data = estadisticas, aes(xintercept = Media, color = "Mean"), 
               linetype = "dashed", linewidth = 1) +
    geom_vline(data = estadisticas, aes(xintercept = Mediana, color = "Median"), 
               linetype = "solid", linewidth = 1) +
    facet_wrap(~Tipo) +
    scale_color_manual(name = "", values = c("Mean" = "grey50", "Median" = "grey50")) +
    labs(
      x = "% Residuals", y = "Frequency", fill = "",
      title = "Percentage Residuals",
      subtitle = "Lower than 100%") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
)
dev.off()


##========== Observados vs Estimados

gg1 = df.comb %>% 
  ggplot(aes(x = PV, y = PVestX)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  scale_x_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  geom_abline(slope = 1, linetype = "dashed", color = "grey") + 
  labs(
    title = "Total price",
    x = "Observed",
    y = "Predicted") + 
  theme_minimal(base_size = 13)

gg2 = df.comb %>% 
  ggplot(aes(x = PT/AT, y = PTestX/AT)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) + 
  scale_x_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) + 
  geom_abline(slope = 1, linetype = "dashed", color = "grey") + 
  labs(
    title = "Terrain price per square meter (PT/AT)",
    x = "Observed",
    y = "Predicted") +
  theme_minimal(base_size = 13)

gg3 = df.comb %>% 
  ggplot(aes(x = PC/AC, y = PCestX/AC)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = "dashed", color = "grey") + 
  scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) + 
  scale_x_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) + 
  labs(
    title = "Construction price per square meter (PC/AC)",
    x = "Observed",
    y = "Predicted") +
  theme_minimal(base_size = 13)

png(paste0(path_out, "obs_vs_pred.png"), width = 16, height = 6, res = 400, units = "in")
print(ggpubr::ggarrange(gg1, gg2, gg3, nrow = 1))
dev.off()

##======= Residuales PC vs Residuales PT
png(paste0(path_out, "residuals_porc_pt_pc.png"), width = 8, height = 6, res = 400, units = "in")
print(
  df.comb %>% 
    ggplot(aes(x = 100*PTresporcX, y = 100*PCresporcX)) + 
    geom_point() + 
    ylim(c(-100,200)) + 
    xlim(c(-100,200)) +
    labs(
      x = "% Residuals terrain price",
      y = "% Residuals construction price")
)
dev.off()

png(paste0(path_out, "residuals_pt_pc.png"), width = 8, height = 6, res = 400, units = "in")
print(
  df.comb %>% 
    ggplot(aes(x = PTresX/AT, y = PCresX/AC)) + 
    geom_point() +
    labs(
      x = "Residuals terrain price per square meter\n(est - obs)",
      y = "Residuals construction price per square meter\n(est - obs)") + 
    scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) + 
    scale_x_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) +
    theme_minimal(base_size = 13)
)
dev.off()

#================================================#
#          3. Identificar outliers         #######
#================================================#
# library(isotree)
library(parameters)

df.out = df.comb

var.out = c("PTresX_AT","PCresX_AC")

## Estandarizar variables
df.out[,paste0(var.out, "_std")] = sweep(df.out[,var.out], MARGIN = 2, STATS = df.out[,var.out] %>% apply(.,2,mean), FUN = "-") %>% 
  sweep(., MARGIN = 2, STATS = df.out[,var.out] %>% apply(.,2,var) %>% sqrt, FUN = "/")

plot(df.out[,paste0(var.out[1:2], "_std")])

#========================#
##    Outliers PC    #####
#========================#
df.out$X = 1

## Selección eps óptimo
var.clusters = c("PCresX_AC_std", "X")

res_dbscan_knn <- n_clusters_dbscan(df.out[,var.clusters], 
                                    standardize = F,
                                    eps_range = c(0.001, 50),
                                    min_size = 5,
                                    distance_method = "euclidean",
                                    method = "SS")
res_dbscan_knn
plot(res_dbscan_knn)

## Selección eps dependiendo del porcentaje de outliers deseados
eps_opt <- attributes(res_dbscan_knn)$eps;eps_opt
eps_opt = 0.3 # Se escoge un epsilon para encontrar el ~1% de datos más atípicos
{
  res_DBSCAN <- dbscan::dbscan(x = df.out[,var.clusters], eps = eps_opt, minPts = 5)
  df.out$OUTPC = ifelse(res_DBSCAN$cluster != 1, "Outlier", "Inlier" )
  
  color.c = rep("blue", length(df.out$OUTPC %>% unique))
  names(color.c) = sort(unique(df.out$OUTPC))
  color.c["Outlier"] = "red"
  color.c["Inlier"] = rgb(0,0.8,0.3, alpha = 0.35)
  
  png(paste0(path_out, "outliers_pc_map.png"), width = 8, height = 6, units = "in", res = 400)
  print(
    df.out %>% 
      ggplot(aes(x = COORDX, y = COORDY, color = OUTPC )) + 
      geom_point() +
      scale_color_manual(values = color.c ) +
      labs(x = "Coordinates X", y = "Coordinates Y", color = "Outlier PC") +
      theme_minimal(base_size = 13)  
  )
  dev.off()
  
  png(paste0(path_out, "outliers_pc_vars.png"), width = 8, height = 6, units = "in", res = 400)
  print(
    df.out %>% 
      ggplot(aes(x = PCresX_AC, y = PTresX_AT, color = OUTPC )) + 
      geom_point() +
      scale_color_manual(values = color.c ) +
      # geom_vline(xintercept = qqoutliers(df.out$PCresX_AC)) + # Outliers boxplot
      # geom_hline(yintercept = qqoutliers(df.out$PTresX_AT)) + # Outliers boxplot
      labs(
        x = "Residual construction price per square meter\n(est - obs)", 
        y = "Residual terrain price per square meter\n(est - obs)",
        color = "Outlier PC") +
      scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) + 
      scale_x_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) +
      theme_minimal(base_size = 13)    
  )
  dev.off()
}

#==============================#
##       Outliers PT        ####
#==============================#

## Selección eps óptimo
var.clusters = c("PTresX_AT_std", "X")

res_dbscan_knn <- n_clusters_dbscan(df.out[,var.clusters], 
                                    standardize = F,
                                    eps_range = c(0.001, 50),
                                    min_size = 5,
                                    distance_method = "euclidean",
                                    method = "SS")
res_dbscan_knn
plot(res_dbscan_knn)

eps_opt <- attributes(res_dbscan_knn)$eps;eps_opt
eps_opt = 0.33# Se escoge un epsilon para encontrar el ~1% de datos más atípicos

{
  res_DBSCAN <- dbscan::dbscan(x = df.out[,var.clusters], eps = eps_opt, minPts = 5)
  df.out$OUTPT = ifelse(res_DBSCAN$cluster != 1, "Outlier", "Inlier" )
  
  color.c = rep("blue", length(df.out$OUTPT %>% unique))
  names(color.c) = sort(unique(df.out$OUTPT))
  color.c["Outlier"] = "red"
  color.c["Inlier"] = rgb(0,0.8,0.3, alpha = 0.35)
  
  png(paste0(path_out, "outliers_pt_map.png"), width = 8, height = 6, units = "in", res = 400)
  print(
    df.out %>% 
      ggplot(aes(x = COORDX, y = COORDY, color = OUTPT )) + 
      geom_point() +
      scale_color_manual(values = color.c ) +
      labs(x = "Coordinates X", y = "Coordinates Y", color = "Outlier PT") +
      theme_minimal(base_size = 13)  
  )
  dev.off()
  
  png(paste0(path_out, "outliers_pt_vars.png"), width = 8, height = 6, units = "in", res = 400)
  print(
    df.out %>% 
      ggplot(aes(x = PCresX_AC, y = PTresX_AT, color = OUTPT )) + 
      geom_point() +
      scale_color_manual(values = color.c ) +
      # geom_vline(xintercept = qqoutliers(df.out$PCresX_AC)) + # Outliers boxplot
      # geom_hline(yintercept = qqoutliers(df.out$PTresX_AT)) + # Outliers boxplot
      labs(
        x = "Residual construction price per square meter\n(est - obs)", 
        y = "Residual terrain price per square meter\n(est - obs)",
        color = "Outlier PT") +
      scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) + 
      scale_x_continuous(labels = scales::unit_format(unit = "K", scale = 0.001)) +
      theme_minimal(base_size = 13)    
  )
  dev.off()
}

#=====================================================#
#         4. Exportar shape con resultados         ####
#=====================================================#
table(df.out$OUTPT, df.out$OUTPC)

## Cargar shape con resultados 
df.shp = read_sf(dsn = "F:/OneDrive/Escritorio/IGAC/01_Distances/data/PrediosAracataca/Predios")

df.shp = df.shp %>% 
  inner_join(
    df.out %>% 
      transmute(
        NUMERO_PRE = NUMEROPRE, 
        OUTPC, OUTPT, 
        PTAT = round(PT/AT,2), PTATest = round(PTestX_AT,2), AT, PTATres = round(PTresX_AT,2),
        PCAC = round(PC/AC,2), PCACest = round(PCestX_AC,2), AC, PCACres = round(PCresX_AC,2) ), 
    by = "NUMERO_PRE")

## Exportar shape con principales variables
dir.create(paste0(path_out, "ShapeOutliers/"))
sf::st_write(df.shp, paste0(path_out, "ShapeOutliers/ShapeOutliers_20251213.shp"))

## Guardar datos
saveRDS(df.out, paste0(path_out, "data_outliers.rds"))
