



rm(list=ls())
library(janitor)

load("~/data_share/rs1/2023/training.grandesciudades/datos.Rda")
datos1 <- datos
names(datos1)
datos1$localidad = NULL

datos1 = rename(datos1, localidad = localidad_def)

load("~/data_share/rs1/2023/grandes_ciudades/Prediccion/datos.Rda")
datos2 <- datos

datos1=datos1[,c("localidad","d_ruta","d_viasprin","d_viassec",
"d_alta","d_baja","d_lineadiv","d_depre","d_rio","prom_lote",
"perc_edif","perc_baldm","perc_bald","porc_uec","porc_ued",
"porc_re","porc_eau","porc_bu","porc_ear","porc_agua","bci",
"rndsi","ui","ndbi","ndvi","ind_con","osm_iibb","vut_2019","vut_2018","vut_2020","vut_2021","vut_2022",
"fragment0","fragment1","fragment2","fragment3","dens_osm","oferta_inm","barr_priv")]



datos2=datos2[,c("localidad","d_ruta","d_viasprin","d_viassec",
                 "d_alta","d_baja","d_lineadiv","d_depre","d_rio","prom_lote",
                 "perc_edif","perc_baldm","perc_bald","porc_uec","porc_ued",
                 "porc_re","porc_eau","porc_bu","porc_ear","porc_agua","bci",
                 "rndsi","ui","ndbi","ndvi","ind_con","osm_iibb","vut_2019","vut_2018","vut_2020","vut_2021","vut_2022",
                 "fragment0","fragment1","fragment2","fragment3","dens_osm","oferta_inm","barr_priv")]


mapview::mapview(datos1, zcol = "localidad")
mapview::mapview(datos2, zcol = "localidad")


compare_df_cols(datos1, datos)
summary(datos1)
summary(datos2)


library(arsenal)
summary(comparedf(st_drop_geometry(datos1), st_drop_geometry(datos2)))




rm(list=ls())
library(janitor)

load("~/data_share/rs1/2023/training.grandesciudades/nombre1.Rda")
nombre <- nombre1
names(nombre1)
load("~/data_share/rs1/2023/grandes_ciudades/Prediccion/nombre1.Rda")


compare_df_cols(nombre, nombre1)
summary(datos1)
summary(datos2)


##library(arsenal)
#summary(comparedf(st_drop_geometry(datos1), st_drop_geometry(datos2)))








