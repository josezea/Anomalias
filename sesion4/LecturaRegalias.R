rm(list = ls())
options(scipen=999)
library(readxl)
library(data.table)
library(skimr)
library(dplyr)
library(Rlof)
ruta <- "C:/Users/Home/Documents/Laboral 2021/UNAL Curso Anomalías/Sesión4"
setwd(ruta)
# df <- read_xlsx(path = "Base_Gesproy_15022021.xlsx", 
#        sheet = 'Proyectos', trim_ws = TRUE)
df_temp <- fread("Base_Gesproy_15022021.csv", nrows = 1)
names(df_temp) <- tolower(names(df_temp))
names(df_temp) <- gsub("\r\n", "", names(df_temp))
names(df_temp) <- gsub("\n", "", names(df_temp))
names(df_temp) <- gsub(" ", "_", names(df_temp))
names(df_temp) <- gsub("-", "_", names(df_temp))

var_sel <- c("bpin", "nombre_ocad", 
  "región", "departamento", "sector_suifp", "programa", "subprograma", "id_proyecto", 
  "nombre_del_proyecto", 
  "avance_físico", "avance_financiero", "estado_detalle", 
  "estado_general", "fecha_aprobación", "año", "valor_sgr", "valor_nación", "valor_otros", 
"total_proyecto")

table(names(df_temp) %in% var_sel)
which(names(df_temp) %in% var_sel)
which(names(df_temp) %in% "fecha_aprobación")



sel <- rep("NULL", 122)
sel[c(1, 2, 5, 6, 18, 19, 20, 21, 22, 25)] <- "character"
sel[c(23, 24, 26, 28, 34, 35, 36, 37)] <- "numeric"


df <- fread("Base_Gesproy_15022021.csv", colClasses = sel)
names(df) <- tolower(names(df))
names(df) <- gsub("\r\n", "", names(df))
names(df) <- gsub("\n", "", names(df))
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub("-", "_", names(df))

df$valor_sgr <- round(as.numeric(df$valor_sgr) / 3000)
df$valor_nación <- round(as.numeric(df$valor_nación) / 3000)
df$valor_otros <- round(as.numeric(df$valor_otros) / 3000)
df$total_proyecto <- round(as.numeric(df$total_proyecto) / 3000)
df$pctg_sgr <- round(df$valor_sgr/df$total_proyecto * 100, 1) 
df <- na.omit(df)
sort(table(df$programa), decreasing = T) %>% as.data.frame() -> prueba


dfsub <- df[df$programa == 'Infraestructura Red Vial Regional' & df$subprograma == "Red Vial Terciaria" , ]
dfsub <- dfsub[dfsub$estado_detalle %in% c("Cerrado", "Terminado "), ]
dfsub <- as.data.frame(dfsub)

dfsub <- dfsub[c("id_proyecto", "nombre_del_proyecto", "nombre_ocad", "región", "departamento", "sector_suifp", 
                  "programa", "subprograma",  
                  "avance_físico", "avance_financiero", "estado_detalle", "estado_general", 
                  "año", "valor_sgr", "valor_nación", "valor_otros", "total_proyecto", 
                  "pctg_sgr")]
dfsub$id_proyecto <- paste0("id_", dfsub$id_proyecto)

names(dfsub) <- c("id_proyecto", "nombre_del_proyecto", "nombre_ocad", "region", 
        "departamento", "sector_suifp", "programa", "subprograma", "avance_fisico", 
        "avance_financiero", "estado_detalle", "estado_general", "anno", 
        "valor_sgr", "valor_nación", "valor_otros", "total_proyecto", 
        "pctg_sgr")

dfsub$avance_fisico <- as.numeric(gsub(",", ".", dfsub$avance_fisico))
dfsub$avance_financiero <- as.numeric(gsub(",", ".", dfsub$avance_financiero))

write.csv(dfsub, "regalias_viasterciarias.csv")

df_revisar <- dfsub[c("valor_sgr", "valor_nación", "valor_otros", "total_proyecto", 
"pctg_sgr")]



hist(log(df_revisar$total_proyecto))
df_revisar$lof <- lof(df_revisar, k = 3, cores = 3)
hist(df_revisar$lof)

