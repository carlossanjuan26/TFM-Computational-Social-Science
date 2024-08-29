
# 1. LIBRERIES ====================================================================

library(sas7bdat)
library(tidyverse)
library(haven)
library(corrplot)
library(grDevices)
library(Hmisc)
library(factoextra)
library(sf)
library(rnaturalearth)
library(giscoR)
library(dplyr)
library(ggplot2)
library(grid)
library(caret)
library(glmnet)
library(cowplot)
library(gridExtra)
library(gtable)
library(patchwork)
library(mice)
library(RColorBrewer)
library(mice)
library(scales)
library(patchwork)
library(ineq)
library(miceadds)
library(sandwich)
library(lmtest)
library(regclass)
library(car)
library(lmtest)
library(skedastic)
library(skedastic)
library(haven)
library(foreign)
library(car)
library(estimatr)
library(knitr)
library(olsrr)
library(caret)
library(randomForest)
library(pdp)

# 2. DATABASES ====================================================================

# You must change the path to select where you have saved the databases on your computer.

# AÑO 2018
datos_pisa_18 <- read_sav("BASE DE DATOS copia/2018/CY07_MSU_STU_QQQ.sav")|>
  filter ( CNT == "ESP") # write the correct path, where you save the database
datos_colegio_18 <- read_sav("BASE DE DATOS copia/2018/CY07_MSU_SCH_QQQ.sav") # write the correct path, where you save the database
merged_data_18 <- merge(datos_pisa_18, datos_colegio_18, by = "CNTSCHID", all.x = TRUE)

# AÑO 2015
datos_pisa_15 <- read_sav("~/Desktop/TFM/BASE DE DATOS copia/2015/CY6_MS_CMB_STU_QQQ.sav") |>
  filter ( CNT == "QES") # write the correct path, where you save the database
datos_colegio_15 <-read_sav("BASE DE DATOS copia/2015/CY6_MS_CMB_SCH_QQQ.sav") |>
  filter ( CNT == "QES") # write the correct path, where you save the database
merged_data_15 <- merge(datos_pisa_15, datos_colegio_15, by = "CNTSCHID", all.x = TRUE)

#AÑO 2022
datos_pisa_22 <-read_sav("BASE DE DATOS copia/2023/CY08MSP_STU_QQQ.sav") |>
  filter ( CNT == "ESP") # write the correct path, where you save the database
datos_colegio_22 <- read_sav("BASE DE DATOS copia/2023/CY08MSP_SCH_QQQ.sav")|>
  filter ( CNT == "ESP") # write the correct path, where you save the database
merged_data_22 <- merge(datos_pisa_22, datos_colegio_22, by = "CNTSCHID", all.x = TRUE)

df_22 <- merged_data_22 %>%
  select(ST001D01T, ST004D01T, ST255Q01JA, ST005Q01JA, ST007Q01JA, ST022Q01TA, ST034Q02TA, ST034Q05TA, ST034Q06TA, ST059Q01TA, ST254Q02JA, WB166Q01HA, PA042Q01TA, OCOD1, OCOD2, AGE, REPEAT, LANGN, BSMJ, BULLIED, MISCED, FISCED, EXPECEDU, WB031Q01NA, CURSUPP, ST062Q02TA, IMMIG, WB166Q01HA, ST297Q01JA, PV1MATH,PV2MATH,PV3MATH,PV4MATH,PV5MATH,PV6MATH,PV7MATH,PV8MATH,PV9MATH,PV10MATH, ST348Q02JA,ST348Q01JA,ST348Q04JA,ST348Q05JA,ST350Q01JA,ST352Q01JA,ST352Q02JA,ST352Q03JA,ST352Q04JA,ST354Q01JA,ST354Q05JA,SC001Q01TA, SC013Q01TA, SC002Q01TA, SC002Q02TA, TOTAT, RATCMP1, RATCMP2, STRATIO, SCHSIZE, ST250Q05JA, REGION.y, ST062Q01TA, ESCS, HOMEPOS 
         )

df_18 <- merged_data_18 %>%
  select(
    LANGTEST_QQQ, ST001D01T,IC001Q04TA,IC001Q01TA, ST004D01T, ST013Q01TA, ST005Q01TA, ST007Q01TA,
    ST022Q01TA, ST034Q02TA, ST034Q05TA, ST034Q06TA, ST059Q02TA, IC001Q01TA,
    WB166Q01HA, PA042Q01TA, OCOD1, OCOD2, AGE, REPEAT, LANGN, BSMJ, BEINGBULLIED,
    MISCED, FISCED, EC153Q01HA, PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH,
    PV6MATH, PV7MATH, PV8MATH, PV9MATH, PV10MATH, EMOSUPP, ST062Q02TA, IMMIG,
    EC154Q02IA, ST061Q01NA, SC001Q01TA, SC013Q01TA, SC002Q01TA, SC002Q02TA, TOTAT, RATCMP1, RATCMP2, STRATIO, SCHSIZE, Region, ST062Q01TA, ESCS, HOMEPOS 
  )

df_15 <- merged_data_15 %>%
  select(
    LANGTEST_QQQ, ST001D01T, ST004D01T, ST013Q01TA, ST005Q01TA, ST007Q01TA,
    ST022Q01TA, ST034Q02TA, ST034Q05TA, ST034Q06TA, ST059Q02TA, IC001Q01TA,
    EC020Q09NA, PA042Q01TA, OCOD1, OCOD2, AGE, REPEAT, LANGN, BSMJ, ST039Q05NA,
    MISCED, FISCED, PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH, PV6MATH, PV7MATH,
    PV8MATH, PV9MATH, PV10MATH, ST061Q01NA, EMOSUPS, ST062Q02TA, IMMIG, SC001Q01TA, SC013Q01TA, SC002Q01TA, SC002Q02TA, TOTAT, RATCMP1, RATCMP2, STRATIO, SCHSIZE, IC001Q04TA, PA042Q01TA, EC015Q01NA, Region.y,ST059Q01TA, ST062Q01TA, ESCS, HOMEPOS
  )

## 2.1 Cleaning 2018 =====================================================================

## Schools

 df_18 <- df_18 |>
  mutate (village = ifelse(SC001Q01TA ==  1, 1, 0),
          town = ifelse ( SC001Q01TA == 3 , 1, 0),
          city = ifelse ( SC001Q01TA == 4 , 1 , 0),
          large_city = ifelse ( SC001Q01TA == 5, 1, 0))

df_18 <- df_18 |>
  mutate(
    public = car::recode(SC013Q01TA, "2=0; 5=NA; 7=NA; 8=NA; 9=NA")
  )

df_18 <- df_18 |>
  mutate( number_boys = case_when(
    SC002Q01TA %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ SC002Q01TA
  ))

df_18 <- df_18 |>
  mutate( number_girls = case_when(
    SC002Q02TA %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ SC002Q02TA
  ))

df_18 <- df_18 |>
  mutate( number_teachers = case_when(
    TOTAT %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ TOTAT
  ))

df_18 <- df_18 |>
  mutate( number_computers = case_when(
    RATCMP1 %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ RATCMP1
  ))
df_18 <- df_18 |>
  mutate( computers_internet = case_when(
    RATCMP2 %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ RATCMP2
  ))

df_18 <- df_18 |>
  mutate( Student_teacher_ratio = case_when(
    STRATIO %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ STRATIO
  ))

df_18 <- df_18 |>
  mutate( SCHSIZE = ifelse(
    SCHSIZE > 1e+07, NA, SCHSIZE ))

## Caracteristics of Individuals 

df_18 <- df_18 |>
  mutate( Female = ifelse( ST004D01T== 1, 1,0))



df_18 <- df_18 |>
  mutate(
    Repeat = car::recode(REPEAT, "5=NA; 7=NA; 8=NA; 9=NA")
  )


df_18 <- df_18 %>%
  mutate(
    IMMIG = case_when(
      IMMIG %in% c(5, 7, 8, 9) ~ NA_real_,
      TRUE ~ IMMIG
    )
  )

df_18 <- df_18 |>
  mutate (Native = ifelse(IMMIG ==  1, 1, 0),
          Second_generation = ifelse ( IMMIG == 3 , 1, 0))

df_18 <- df_18 |>
  mutate(
    Language_test = car::recode(ST022Q01TA, "2=0 ; 95=NA; 97=NA; 98=NA; 99=NA")
  )

df_18 <- df_18 |>
  mutate(
    Easily  = case_when(
      ST034Q02TA == 1 | ST034Q02TA == 2 ~ 1, 
      ST034Q02TA == 3 | ST034Q02TA == 4 ~ 0,  
      ST034Q02TA == 5 | ST034Q02TA == 7 | ST034Q02TA == 8 | ST034Q02TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_18 <- df_18 |>
  mutate(
      Seem = case_when(
      ST034Q05TA == 1 | ST034Q05TA == 2 ~ 1,  
      ST034Q05TA == 3 | ST034Q05TA == 4 ~ 0,  
      ST034Q05TA == 5 | ST034Q05TA == 7 | ST034Q05TA == 8 | ST034Q05TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_18 <- df_18 |>
  mutate(
    Lonely  = case_when(
      ST034Q06TA == 1 | ST034Q06TA == 2 ~ 1,  
      ST034Q06TA == 3 | ST034Q06TA == 4 ~ 0,  
      ST034Q06TA == 5 | ST034Q06TA == 7 | ST034Q06TA == 8 | ST034Q06TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


df_18 <- df_18 |>
  mutate(
    Internet  = case_when(
      IC001Q04TA == 1 | IC001Q04TA == 2 ~ 1,  
      IC001Q04TA == 3  ~ 0,  
      IC001Q04TA == 5 | IC001Q04TA == 7 | IC001Q04TA == 8 | IC001Q04TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_18 <- df_18 |>
  mutate(
    Computers = case_when(
      IC001Q01TA == 1 | IC001Q01TA == 2 ~ 1,  
      IC001Q01TA == 3  ~ 0,  
      IC001Q01TA == 5 | IC001Q01TA == 7 | IC001Q01TA == 8 | IC001Q01TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


## Parents 

df_18 <- df_18 |>
  mutate(
    ST013Q01TA = car::recode(ST013Q01TA, " 95=NA; 97=NA; 98=NA; 99=NA")
  ) |>
  mutate( `0-10 books` = ifelse( ST013Q01TA == 1, 1, 0),
          `11-25 books` = ifelse(ST013Q01TA == 2, 1, 0),
          `26-100 books` = ifelse(ST013Q01TA == 3, 1, 0),
          `101-200 books` = ifelse(ST013Q01TA == 2, 1, 0),
          `201-500 books` = ifelse(ST013Q01TA == 5, 1, 0),
          `>500 books` = ifelse (ST013Q01TA == 6, 1, 0 )
          )

df_18$MISCED[df_18$MISCED %in% c(95, 97, 98, 99)] <- NA

df_18 <- df_18 %>%
  mutate(M_None = ifelse(MISCED == 0, 1, 0),
         M_Primaria = ifelse(MISCED == 1, 1, 0),
         M_ESO = ifelse(MISCED == 2, 1, 0),
         M_Grado_medio_bachillerato = ifelse(MISCED == 3, 1, 0),
         M_Educaion_postsecundaria = ifelse(MISCED == 4, 1, 0),
         M_Educacion_superior = ifelse(MISCED == 5, 1, 0),
         M_Educacion_avanzada = ifelse(MISCED == 6, 1, 0))

df_18$FISCED[df_18$FISCED %in% c(95, 97, 98, 99)] <- NA

df_18 <- df_18 %>%
  mutate(F_None = ifelse(FISCED == 0, 1, 0),
         F_Primaria = ifelse(FISCED == 1, 1, 0),
         F_ESO = ifelse(FISCED == 2, 1, 0),
         F_Grado_medio_bachillerato = ifelse(FISCED == 3, 1, 0),
         F_Educaion_postsecundaria = ifelse(FISCED == 4, 1, 0),
         F_Educacion_superior = ifelse(FISCED == 5, 1, 0),
         F_Educacion_avanzada = ifelse(FISCED == 6, 1, 0))

### Father 

df_18$OCOD2[df_18$OCOD2 %in% c("9997", "9998", "9999")] <- NA
df_18 <- df_18 %>%
  mutate(
    Directores_y_Gerentes = ifelse(OCOD2 %in% c("1000", "1100", "1110", "1111", "1112", "1113", "1114", "1120", "1200", "1210", "1211", "1212", "1213", "1219", "1220", "1221", "1222", "1223", "1300", "1310", "1311", "1312", "1320", "1321", "1322", "1323", "1324", "1330", "1340", "1341", "1342", "1343", "1344", "1345", "1346", "1349", "1400", "1410", "1411", "1412", "1420", "1430", "1431", "1439"), 1, 0),
    Tecnicos_y_profesionales_cientificos = ifelse(OCOD2 %in% c("2000", "2100", "2110", "2111", "2112", "2113", "2114", "2120", "2130", "2131", "2132", "2133", "2140", "2141", "2142", "2143", "2144", "2145", "2146", "2149", "2150", "2151", "2152", "2153", "2160", "2161", "2162", "2163", "2164", "2165", "2166"), 1, 0),
    Otros_tecnicos_cientificos = ifelse(OCOD2 %in% c("2600", "2610", "2611", "2612", "2619", "2620", "2621", "2622", "2630", "2631", "2632", "2633", "2634", "2635", "2636", "2640", "2641", "2642", "2643", "2650", "2651", "2652", "2653", "2654", "2655", "2656", "2659"), 1, 0),
    Tecnicos_y_profesionales_de_apoyo = ifelse(OCOD2 %in% c("3000", "3100", "3110", "3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119", "3120", "3121", "3122", "3123", "3130", "3131", "3132", "3133", "3134", "3135", "3139", "3140", "3141", "3142", "3143", "3150", "3151", "3152", "3153", "3154", "3155"), 1, 0),
    Empleados_administrativos_y_de_oficina = ifelse(OCOD2 %in% c("4000", "4100", "4110", "4120", "4130", "4131", "4132"), 1, 0),
    Otros_empleados_de_oficina = ifelse(OCOD2 %in% c("4200", "4210", "4211", "4212", "4213", "4214", "4220", "4221", "4222", "4223", "4224", "4225", "4226", "4227", "4229", "4300", "4310", "4311", "4312", "4313", "4320", "4321", "4322", "4323", "4400", "4410", "4411", "4412", "4413", "4414", "4415", "4416", "4419"), 1, 0),
    Servicios_de_restauracion_y_vendedores = ifelse(OCOD2 %in% c("5000", "5100", "5110", "5111", "5112", "5113", "5120", "5130", "5131", "5132", "5140", "5141", "5142", "5150", "5151", "5152", "5153", "5160", "5161", "5162", "5163", "5164", "5165", "5169", "5200", "5210", "5211", "5212", "5220", "5221", "5222", "5223", "5230", "5240", "5241", "5242", "5243", "5244", "5245", "5246", "5249"), 1, 0),
    Servicios_de_Salud = ifelse(OCOD2 %in% c("2200", "2210", "2211", "2212", "2220", "2221", "2222", "2230", "2240", "2250", "2260", "2261", "2262", "2263", "2264", "2265", "2266", "2267", "2269"), 1, 0),
    Servicios_de_Proteccion = ifelse(OCOD2 %in% c("5400", "5410", "5411", "5412", "5413", "5414", "5419"), 1, 0),
    Trabajadores_cualificados_en_agricultura = ifelse(OCOD2 %in% c("6000", "6100", "6110", "6111", "6112", "6113", "6114", "6120", "6121", "6122", "6123", "6129", "6130"), 1, 0),
    Trab_cual_de_manufacturas_y_construccion = ifelse(OCOD2 %in% c("7000", "7100", "7110", "7111", "7112", "7113", "7114", "7115", "7119", "7120", "7121", "7122", "7123", "7124", "7125", "7126", "7127", "7130", "7131", "7132", "7133"), 1, 0),
    Trab_cual_de_industrias = ifelse(OCOD2 %in% c("7200", "7210", "7211", "7212", "7213", "7214", "7215", "7220", "7221", "7222", "7223", "7224", "7230", "7231", "7232", "7233", "7234", "7300", "7310", "7311", "7312", "7313", "7314", "7315", "7316", "7317", "7318", "7319", "7320", "7321", "7322", "7323", "7400", "7410", "7411", "7412", "7413", "7420", "7421", "7422"), 1, 0),
    Operadores_de_instalaciones_y_maquinaria = ifelse(OCOD2 %in% c("8000", "8100", "8110", "8111", "8112", "8113", "8114", "8120", "8121", "8122", "8130", "8131", "8132", "8140", "8141", "8142", "8143", "8150", "8151", "8152", "8153", "8154", "8155", "8156", "8157", "8159", "8160"), 1, 0),
    Conductores_y_Operadores_de_Maquinaria = ifelse(OCOD2 %in% c("8300", "8310", "8311", "8320", "8321", "8322", "8330", "8331", "8332", "8340", "8341", "8342", "8343", "8344", "8350"), 1, 0),
    Trabajadores_no_cualificados_en_Servicios = ifelse(OCOD2 %in% c("9000", "9100", "9110", "9111", "9112", "9120", "9121", "9122", "9123", "9129", "9200", "9210", "9211", "9212", "9213", "9214", "9215", "9216", "9300", "9310", "9311", "9312", "9313", "9320", "9321", "9329", "9330", "9331", "9332", "9333", "9334", "9400", "9410", "9411", "9412", "9500", "9510", "9520", "9600", "9610", "9611", "9612", "9613", "9620", "9621", "9622", "9623", "9624", "9629"), 1, 0),
    Ocupaciones_militares = ifelse(OCOD2 %in% c("0000", "0100", "0110", "0200", "0210", "0300", "0310"), 1, 0)
  )



### Mother

df_18$OCOD1[df_18$OCOD1 %in% c("9997", "9998", "9999")] <- NA
df_18 <- df_18 %>%
  mutate(
    M_Directores_y_Gerentes = ifelse(OCOD1 %in% c("1000", "1100", "1110", "1111", "1112", "1113", "1114", "1120", "1200", "1210", "1211", "1212", "1213", "1219", "1220", "1221", "1222", "1223", "1300", "1310", "1311", "1312", "1320", "1321", "1322", "1323", "1324", "1330", "1340", "1341", "1342", "1343", "1344", "1345", "1346", "1349", "1400", "1410", "1411", "1412", "1420", "1430", "1431", "1439"), 1, 0),
    M_Tecnicos_y_profesionales_cientificos = ifelse(OCOD1 %in% c("2000", "2100", "2110", "2111", "2112", "2113", "2114", "2120", "2130", "2131", "2132", "2133", "2140", "2141", "2142", "2143", "2144", "2145", "2146", "2149", "2150", "2151", "2152", "2153", "2160", "2161", "2162", "2163", "2164", "2165", "2166"), 1, 0),
    M_Otros_tecnicos_cientificos = ifelse(OCOD1 %in% c("2600", "2610", "2611", "2612", "2619", "2620", "2621", "2622", "2630", "2631", "2632", "2633", "2634", "2635", "2636", "2640", "2641", "2642", "2643", "2650", "2651", "2652", "2653", "2654", "2655", "2656", "2659"), 1, 0),
    M_Tecnicos_y_profesionales_de_apoyo = ifelse(OCOD1 %in% c("3000", "3100", "3110", "3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119", "3120", "3121", "3122", "3123", "3130", "3131", "3132", "3133", "3134", "3135", "3139", "3140", "3141", "3142", "3143", "3150", "3151", "3152", "3153", "3154", "3155"), 1, 0),
    M_Empleados_administrativos_y_de_oficina = ifelse(OCOD1 %in% c("4000", "4100", "4110", "4120", "4130", "4131", "4132"), 1, 0),
    M_Otros_empleados_de_oficina = ifelse(OCOD1 %in% c("4200", "4210", "4211", "4212", "4213", "4214", "4220", "4221", "4222", "4223", "4224", "4225", "4226", "4227", "4229", "4300", "4310", "4311", "4312", "4313", "4320", "4321", "4322", "4323", "4400", "4410", "4411", "4412", "4413", "4414", "4415", "4416", "4419"), 1, 0),
    M_Servicios_de_restauracion_y_vendedores = ifelse(OCOD1 %in% c("5000", "5100", "5110", "5111", "5112", "5113", "5120", "5130", "5131", "5132", "5140", "5141", "5142", "5150", "5151", "5152", "5153", "5160", "5161", "5162", "5163", "5164", "5165", "5169", "5200", "5210", "5211", "5212", "5220", "5221", "5222", "5223", "5230", "5240", "5241", "5242", "5243", "5244", "5245", "5246", "5249"), 1, 0),
    M_Servicios_de_Salud = ifelse(OCOD1 %in% c("2200", "2210", "2211", "2212", "2220", "2221", "2222", "2230", "2240", "2250", "2260", "2261", "2262", "2263", "2264", "2265", "2266", "2267", "2269"), 1, 0),
    M_Servicios_de_Proteccion = ifelse(OCOD1 %in% c("5400", "5410", "5411", "5412", "5413", "5414", "5419"), 1, 0),
    M_Trabajadores_cualificados_en_agricultura = ifelse(OCOD1 %in% c("6000", "6100", "6110", "6111", "6112", "6113", "6114", "6120", "6121", "6122", "6123", "6129", "6130"), 1, 0),
    M_Trab_cual_de_manufacturas_y_construccion = ifelse(OCOD1 %in% c("7000", "7100", "7110", "7111", "7112", "7113", "7114", "7115", "7119", "7120", "7121", "7122", "7123", "7124", "7125", "7126", "7127", "7130", "7131", "7132", "7133"), 1, 0),
    M_Trab_cual_de_industrias = ifelse(OCOD1 %in% c("7200", "7210", "7211", "7212", "7213", "7214", "7215", "7220", "7221", "7222", "7223", "7224", "7230", "7231", "7232", "7233", "7234", "7300", "7310", "7311", "7312", "7313", "7314", "7315", "7316", "7317", "7318", "7319", "7320", "7321", "7322", "7323", "7400", "7410", "7411", "7412", "7413", "7420", "7421", "7422"), 1, 0),
    M_Operadores_de_instalaciones_y_maquinaria = ifelse(OCOD1 %in% c("8000", "8100", "8110", "8111", "8112", "8113", "8114", "8120", "8121", "8122", "8130", "8131", "8132", "8140", "8141", "8142", "8143", "8150", "8151", "8152", "8153", "8154", "8155", "8156", "8157", "8159", "8160"), 1, 0),
    M_Conductores_y_Operadores_de_Maquinaria = ifelse(OCOD1 %in% c("8300", "8310", "8311", "8320", "8321", "8322", "8330", "8331", "8332", "8340", "8341", "8342", "8343", "8344", "8350"), 1, 0),
    M_Trabajadores_no_cualificados_en_Servicios = ifelse(OCOD1 %in% c("9000", "9100", "9110", "9111", "9112", "9120", "9121", "9122", "9123", "9129", "9200", "9210", "9211", "9212", "9213", "9214", "9215", "9216", "9300", "9310", "9311", "9312", "9313", "9320", "9321", "9329", "9330", "9331", "9332", "9333", "9334", "9400", "9410", "9411", "9412", "9500", "9510", "9520", "9600", "9610", "9611", "9612", "9613", "9620", "9621", "9622", "9623", "9624", "9629"), 1, 0),
    M_Ocupaciones_militares = ifelse(OCOD1 %in% c("0000", "0100", "0110", "0200", "0210", "0300", "0310"), 1, 0)
  )


### Others 

df_18 <- df_18 %>%
  mutate(
    Math_class = ifelse(ST059Q02TA %in% c(995, 997, 998, 999), NA, ST059Q02TA)
  )

df_18 <- df_18 %>%
  mutate(
    Andalusia = ifelse(Region == 72401, 1, 0),
    Aragon = ifelse(Region == 72402, 1, 0),
    Asturias = ifelse(Region == 72403, 1, 0),
    `Balearic Islands` = ifelse(Region == 72404, 1, 0),
    `Canary Islands` = ifelse(Region == 72405, 1, 0),
    Cantabria = ifelse(Region == 72406, 1, 0),
    `Castile and Leon` = ifelse(Region == 72407, 1, 0),
    `Castile-La Mancha` = ifelse(Region == 72408, 1, 0),
    Catalonia = ifelse(Region == 72409, 1, 0),
    Extremadura = ifelse(Region == 72410, 1, 0),
    Galicia = ifelse(Region == 72411, 1, 0),
    `La Rioja` = ifelse(Region == 72412, 1, 0),
    Madrid = ifelse(Region == 72413, 1, 0),
    Murcia = ifelse(Region == 72414, 1, 0),
    Navarre = ifelse(Region == 72415, 1, 0),
    `Basque Country` = ifelse(Region == 72416, 1, 0),
    `Comunidad Valenciana` = ifelse(Region == 72417, 1, 0),
    Ceuta = ifelse(Region == 72418, 1, 0),
    Melilla = ifelse(Region == 72419, 1, 0)
  )

df_18 <- df_18 %>%
  mutate(
    region_final = case_when(
      Region == 72401 ~ "Andalucía",
      Region == 72402 ~ "Aragón",
      Region == 72403 ~ "Principado de Asturias",
      Region == 72404 ~ "Illes Balears",
      Region == 72405 ~ "Canarias",
      Region == 72406 ~ "Cantabria",
      Region == 72407 ~ "Castilla y León",
      Region == 72408 ~ "Castilla-La Mancha",
      Region == 72409 ~ "Cataluña",
      Region == 72410 ~ "Extremadura",
      Region == 72411 ~ "Galicia",
      Region == 72412 ~ "La Rioja",
      Region == 72413 ~ "Comunidad de Madrid",
      Region == 72414 ~ "Región de Murcia",
      Region == 72415 ~ "Comunidad Foral de Navarra",
      Region == 72416 ~ "País Vasco",
      Region == 72417 ~ "Comunitat Valenciana",
      Region == 72418 ~ "Ciudad de Ceuta ",
      Region == 72419 ~ "Ciudad de Melilla",
      TRUE ~ "Unknown"  
    )
  )

df_18 <- df_18 |> 
  rename(skipped_class=ST062Q01TA)

## Var Dependiente 18

df_18 <- df_18 %>%
  mutate(
    Mean_MATH = rowMeans(select(., PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH,
                                PV6MATH, PV7MATH, PV8MATH, PV9MATH, PV10MATH),
                         na.rm = TRUE)
  )


## 2.2 Cleaning 2022 ================================================================

## Colegio 22

df_22 <- df_22 |>
  mutate (village = ifelse(SC001Q01TA ==  1, 1, 0),
          town = ifelse ( SC001Q01TA == 3 , 1, 0),
          city = ifelse ( SC001Q01TA == 4 , 1 , 0),
          large_city = ifelse ( SC001Q01TA == 5, 1, 0))

df_22 <- df_22 |>
  mutate(
    public = car::recode(SC013Q01TA, "2=0; 5=NA; 7=NA; 8=NA; 9=NA")
  )

df_22 <- df_22 |>
  mutate( number_boys = case_when(
    SC002Q01TA %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ SC002Q01TA
  ))

df_22 <- df_22 |>
  mutate( number_girls = case_when(
    SC002Q02TA %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ SC002Q02TA
  ))

df_22 <- df_22 |>
  mutate( number_teachers = case_when(
    TOTAT %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ TOTAT
  ))

df_22 <- df_22 |>
  mutate( number_computers = case_when(
    RATCMP1 %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ RATCMP1
  ))
df_22 <- df_22 |>
  mutate( computers_internet = case_when(
    RATCMP2 %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ RATCMP2
  ))

df_22 <- df_22 |>
  mutate( Student_teacher_ratio = case_when(
    STRATIO %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ STRATIO
  ))

df_22 <- df_22 |>
  mutate( SCHSIZE = ifelse(
    SCHSIZE > 1e+07, NA, SCHSIZE ))

## Caracteristics of Individuals 22

df_22 <- df_22 |>
  mutate( Female = ifelse( ST004D01T== 1, 1,0))


df_22 <- df_22 |>
  mutate(
    Repeat = car::recode(REPEAT, "5=NA; 7=NA; 8=NA; 9=NA")
  )

df_22 <- df_22 %>%
  mutate(
    IMMIG = case_when(
      IMMIG %in% c(5, 7, 8, 9) ~ NA_real_,
      TRUE ~ IMMIG
    )
  )

df_22 <- df_22 |>
  mutate (Native = ifelse(IMMIG ==  1, 1, 0),
          Second_generation = ifelse ( IMMIG == 3 , 1, 0))

df_22 <- df_22 |>
  mutate(
    Language_test = car::recode(ST022Q01TA, "2=0 ; 95=NA; 97=NA; 98=NA; 99=NA")
  )


df_22 <- df_22 |>
  mutate(
    Easily  = case_when(
      ST034Q02TA == 1 | ST034Q02TA == 2 ~ 1,  
      ST034Q02TA == 3 | ST034Q02TA == 4 ~ 0,  
      ST034Q02TA == 5 | ST034Q02TA == 7 | ST034Q02TA == 8 | ST034Q02TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_22 <- df_22 |>
  mutate(
    Seem = case_when(
      ST034Q05TA == 1 | ST034Q05TA == 2 ~ 1,  
      ST034Q05TA == 3 | ST034Q05TA == 4 ~ 0,  
      ST034Q05TA == 5 | ST034Q05TA == 7 | ST034Q05TA == 8 | ST034Q05TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_22 <- df_22 |>
  mutate(
    Lonely  = case_when(
      ST034Q06TA == 1 | ST034Q06TA == 2 ~ 1,  
      ST034Q06TA == 3 | ST034Q06TA == 4 ~ 0,  
      ST034Q06TA == 5 | ST034Q06TA == 7 | ST034Q06TA == 8 | ST034Q06TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


df_22 <- df_22 |>
  mutate(
    Internet  = case_when(
      ST250Q05JA == 2  ~ 0,  
      ST250Q05JA == 95 | ST250Q05JA == 97 | ST250Q05JA == 98 | ST250Q05JA == 99 ~ NA_real_,
      TRUE ~ ST250Q05JA
    )
  )

df_22 <- df_22 |>
  mutate(
    Computers = case_when(
      ST254Q02JA == 1  ~ 0,
      ST254Q02JA == 2| ST254Q02JA == 3| ST254Q02JA == 4 ~ 1, 
      ST254Q02JA == 5 | ST254Q02JA== 95| ST254Q02JA == 97 | ST254Q02JA == 98 | ST254Q02JA == 99 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# Parents 22 

df_22 <- df_22 |>
  mutate(
    ST255Q01JA = car::recode(ST255Q01JA, " 95=NA; 97=NA; 98=NA; 99=NA")
  ) |>
  mutate( `0-10 books` = ifelse( ST255Q01JA == 1, 1, 0),
          `11-25 books` = ifelse(ST255Q01JA == 2, 1, 0),
          `26-100 books` = ifelse(ST255Q01JA == 3, 1, 0),
          `101-200 books` = ifelse(ST255Q01JA == 2, 1, 0),
          `201-500 books` = ifelse(ST255Q01JA == 5, 1, 0),
          `>500 books` = ifelse (ST255Q01JA == 6, 1, 0 )
  )

df_22$MISCED[df_22$MISCED %in% c(95, 97, 98, 99)] <- NA

df_22 <- df_22 %>%
  mutate(M_None = ifelse(MISCED == 0, 1, 0),
         M_Primaria = ifelse(MISCED == 1, 1, 0),
         M_ESO = ifelse(MISCED == 2, 1, 0),
         M_Grado_medio_bachillerato = ifelse(MISCED == 3, 1, 0),
         M_Educaion_postsecundaria = ifelse(MISCED == 4, 1, 0),
         M_Educacion_superior = ifelse(MISCED == 5, 1, 0),
         M_Educacion_avanzada = ifelse(MISCED == 6, 1, 0))

df_22$FISCED[df_22$FISCED %in% c(95, 97, 98, 99)] <- NA

df_22 <- df_22 %>%
  mutate(F_None = ifelse(FISCED == 0, 1, 0),
         F_Primaria = ifelse(FISCED == 1, 1, 0),
         F_ESO = ifelse(FISCED == 2, 1, 0),
         F_Grado_medio_bachillerato = ifelse(FISCED == 3, 1, 0),
         F_Educaion_postsecundaria = ifelse(FISCED == 4, 1, 0),
         F_Educacion_superior = ifelse(FISCED == 5, 1, 0),
         F_Educacion_avanzada = ifelse(FISCED == 6, 1, 0))

### Father

df_22$OCOD2[df_22$OCOD2 %in% c("9997", "9998", "9999")] <- NA
df_22 <- df_22 %>%
  mutate(
    Directores_y_Gerentes = ifelse(OCOD2 %in% c("1000", "1100", "1110", "1111", "1112", "1113", "1114", "1120", "1200", "1210", "1211", "1212", "1213", "1219", "1220", "1221", "1222", "1223", "1300", "1310", "1311", "1312", "1320", "1321", "1322", "1323", "1324", "1330", "1340", "1341", "1342", "1343", "1344", "1345", "1346", "1349", "1400", "1410", "1411", "1412", "1420", "1430", "1431", "1439"), 1, 0),
    Tecnicos_y_profesionales_cientificos = ifelse(OCOD2 %in% c("2000", "2100", "2110", "2111", "2112", "2113", "2114", "2120", "2130", "2131", "2132", "2133", "2140", "2141", "2142", "2143", "2144", "2145", "2146", "2149", "2150", "2151", "2152", "2153", "2160", "2161", "2162", "2163", "2164", "2165", "2166"), 1, 0),
    Otros_tecnicos_cientificos = ifelse(OCOD2 %in% c("2600", "2610", "2611", "2612", "2619", "2620", "2621", "2622", "2630", "2631", "2632", "2633", "2634", "2635", "2636", "2640", "2641", "2642", "2643", "2650", "2651", "2652", "2653", "2654", "2655", "2656", "2659"), 1, 0),
    Tecnicos_y_profesionales_de_apoyo = ifelse(OCOD2 %in% c("3000", "3100", "3110", "3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119", "3120", "3121", "3122", "3123", "3130", "3131", "3132", "3133", "3134", "3135", "3139", "3140", "3141", "3142", "3143", "3150", "3151", "3152", "3153", "3154", "3155"), 1, 0),
    Empleados_administrativos_y_de_oficina = ifelse(OCOD2 %in% c("4000", "4100", "4110", "4120", "4130", "4131", "4132"), 1, 0),
    Otros_empleados_de_oficina = ifelse(OCOD2 %in% c("4200", "4210", "4211", "4212", "4213", "4214", "4220", "4221", "4222", "4223", "4224", "4225", "4226", "4227", "4229", "4300", "4310", "4311", "4312", "4313", "4320", "4321", "4322", "4323", "4400", "4410", "4411", "4412", "4413", "4414", "4415", "4416", "4419"), 1, 0),
    Servicios_de_restauracion_y_vendedores = ifelse(OCOD2 %in% c("5000", "5100", "5110", "5111", "5112", "5113", "5120", "5130", "5131", "5132", "5140", "5141", "5142", "5150", "5151", "5152", "5153", "5160", "5161", "5162", "5163", "5164", "5165", "5169", "5200", "5210", "5211", "5212", "5220", "5221", "5222", "5223", "5230", "5240", "5241", "5242", "5243", "5244", "5245", "5246", "5249"), 1, 0),
    Servicios_de_Salud = ifelse(OCOD2 %in% c("2200", "2210", "2211", "2212", "2220", "2221", "2222", "2230", "2240", "2250", "2260", "2261", "2262", "2263", "2264", "2265", "2266", "2267", "2269"), 1, 0),
    Servicios_de_Proteccion = ifelse(OCOD2 %in% c("5400", "5410", "5411", "5412", "5413", "5414", "5419"), 1, 0),
    Trabajadores_cualificados_en_agricultura = ifelse(OCOD2 %in% c("6000", "6100", "6110", "6111", "6112", "6113", "6114", "6120", "6121", "6122", "6123", "6129", "6130"), 1, 0),
    Trab_cual_de_manufacturas_y_construccion = ifelse(OCOD2 %in% c("7000", "7100", "7110", "7111", "7112", "7113", "7114", "7115", "7119", "7120", "7121", "7122", "7123", "7124", "7125", "7126", "7127", "7130", "7131", "7132", "7133"), 1, 0),
    Trab_cual_de_industrias = ifelse(OCOD2 %in% c("7200", "7210", "7211", "7212", "7213", "7214", "7215", "7220", "7221", "7222", "7223", "7224", "7230", "7231", "7232", "7233", "7234", "7300", "7310", "7311", "7312", "7313", "7314", "7315", "7316", "7317", "7318", "7319", "7320", "7321", "7322", "7323", "7400", "7410", "7411", "7412", "7413", "7420", "7421", "7422"), 1, 0),
    Operadores_de_instalaciones_y_maquinaria = ifelse(OCOD2 %in% c("8000", "8100", "8110", "8111", "8112", "8113", "8114", "8120", "8121", "8122", "8130", "8131", "8132", "8140", "8141", "8142", "8143", "8150", "8151", "8152", "8153", "8154", "8155", "8156", "8157", "8159", "8160"), 1, 0),
    Conductores_y_Operadores_de_Maquinaria = ifelse(OCOD2 %in% c("8300", "8310", "8311", "8320", "8321", "8322", "8330", "8331", "8332", "8340", "8341", "8342", "8343", "8344", "8350"), 1, 0),
    Trabajadores_no_cualificados_en_Servicios = ifelse(OCOD2 %in% c("9000", "9100", "9110", "9111", "9112", "9120", "9121", "9122", "9123", "9129", "9200", "9210", "9211", "9212", "9213", "9214", "9215", "9216", "9300", "9310", "9311", "9312", "9313", "9320", "9321", "9329", "9330", "9331", "9332", "9333", "9334", "9400", "9410", "9411", "9412", "9500", "9510", "9520", "9600", "9610", "9611", "9612", "9613", "9620", "9621", "9622", "9623", "9624", "9629"), 1, 0),
    Ocupaciones_militares = ifelse(OCOD2 %in% c("0000", "0100", "0110", "0200", "0210", "0300", "0310"), 1, 0)
  )


### Mother

df_22$OCOD1[df_22$OCOD1 %in% c("9997", "9998", "9999")] <- NA
df_22 <- df_22 %>%
  mutate(
    M_Directores_y_Gerentes = ifelse(OCOD1 %in% c("1000", "1100", "1110", "1111", "1112", "1113", "1114", "1120", "1200", "1210", "1211", "1212", "1213", "1219", "1220", "1221", "1222", "1223", "1300", "1310", "1311", "1312", "1320", "1321", "1322", "1323", "1324", "1330", "1340", "1341", "1342", "1343", "1344", "1345", "1346", "1349", "1400", "1410", "1411", "1412", "1420", "1430", "1431", "1439"), 1, 0),
    M_Tecnicos_y_profesionales_cientificos = ifelse(OCOD1 %in% c("2000", "2100", "2110", "2111", "2112", "2113", "2114", "2120", "2130", "2131", "2132", "2133", "2140", "2141", "2142", "2143", "2144", "2145", "2146", "2149", "2150", "2151", "2152", "2153", "2160", "2161", "2162", "2163", "2164", "2165", "2166"), 1, 0),
    M_Otros_tecnicos_cientificos = ifelse(OCOD1 %in% c("2600", "2610", "2611", "2612", "2619", "2620", "2621", "2622", "2630", "2631", "2632", "2633", "2634", "2635", "2636", "2640", "2641", "2642", "2643", "2650", "2651", "2652", "2653", "2654", "2655", "2656", "2659"), 1, 0),
    M_Tecnicos_y_profesionales_de_apoyo = ifelse(OCOD1 %in% c("3000", "3100", "3110", "3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119", "3120", "3121", "3122", "3123", "3130", "3131", "3132", "3133", "3134", "3135", "3139", "3140", "3141", "3142", "3143", "3150", "3151", "3152", "3153", "3154", "3155"), 1, 0),
    M_Empleados_administrativos_y_de_oficina = ifelse(OCOD1 %in% c("4000", "4100", "4110", "4120", "4130", "4131", "4132"), 1, 0),
    M_Otros_empleados_de_oficina = ifelse(OCOD1 %in% c("4200", "4210", "4211", "4212", "4213", "4214", "4220", "4221", "4222", "4223", "4224", "4225", "4226", "4227", "4229", "4300", "4310", "4311", "4312", "4313", "4320", "4321", "4322", "4323", "4400", "4410", "4411", "4412", "4413", "4414", "4415", "4416", "4419"), 1, 0),
    M_Servicios_de_restauracion_y_vendedores = ifelse(OCOD1 %in% c("5000", "5100", "5110", "5111", "5112", "5113", "5120", "5130", "5131", "5132", "5140", "5141", "5142", "5150", "5151", "5152", "5153", "5160", "5161", "5162", "5163", "5164", "5165", "5169", "5200", "5210", "5211", "5212", "5220", "5221", "5222", "5223", "5230", "5240", "5241", "5242", "5243", "5244", "5245", "5246", "5249"), 1, 0),
    M_Servicios_de_Salud = ifelse(OCOD1 %in% c("2200", "2210", "2211", "2212", "2220", "2221", "2222", "2230", "2240", "2250", "2260", "2261", "2262", "2263", "2264", "2265", "2266", "2267", "2269"), 1, 0),
    M_Servicios_de_Proteccion = ifelse(OCOD1 %in% c("5400", "5410", "5411", "5412", "5413", "5414", "5419"), 1, 0),
    M_Trabajadores_cualificados_en_agricultura = ifelse(OCOD1 %in% c("6000", "6100", "6110", "6111", "6112", "6113", "6114", "6120", "6121", "6122", "6123", "6129", "6130"), 1, 0),
    M_Trab_cual_de_manufacturas_y_construccion = ifelse(OCOD1 %in% c("7000", "7100", "7110", "7111", "7112", "7113", "7114", "7115", "7119", "7120", "7121", "7122", "7123", "7124", "7125", "7126", "7127", "7130", "7131", "7132", "7133"), 1, 0),
    M_Trab_cual_de_industrias = ifelse(OCOD1 %in% c("7200", "7210", "7211", "7212", "7213", "7214", "7215", "7220", "7221", "7222", "7223", "7224", "7230", "7231", "7232", "7233", "7234", "7300", "7310", "7311", "7312", "7313", "7314", "7315", "7316", "7317", "7318", "7319", "7320", "7321", "7322", "7323", "7400", "7410", "7411", "7412", "7413", "7420", "7421", "7422"), 1, 0),
    M_Operadores_de_instalaciones_y_maquinaria = ifelse(OCOD1 %in% c("8000", "8100", "8110", "8111", "8112", "8113", "8114", "8120", "8121", "8122", "8130", "8131", "8132", "8140", "8141", "8142", "8143", "8150", "8151", "8152", "8153", "8154", "8155", "8156", "8157", "8159", "8160"), 1, 0),
    M_Conductores_y_Operadores_de_Maquinaria = ifelse(OCOD1 %in% c("8300", "8310", "8311", "8320", "8321", "8322", "8330", "8331", "8332", "8340", "8341", "8342", "8343", "8344", "8350"), 1, 0),
    M_Trabajadores_no_cualificados_en_Servicios = ifelse(OCOD1 %in% c("9000", "9100", "9110", "9111", "9112", "9120", "9121", "9122", "9123", "9129", "9200", "9210", "9211", "9212", "9213", "9214", "9215", "9216", "9300", "9310", "9311", "9312", "9313", "9320", "9321", "9329", "9330", "9331", "9332", "9333", "9334", "9400", "9410", "9411", "9412", "9500", "9510", "9520", "9600", "9610", "9611", "9612", "9613", "9620", "9621", "9622", "9623", "9624", "9629"), 1, 0),
    M_Ocupaciones_militares = ifelse(OCOD1 %in% c("0000", "0100", "0110", "0200", "0210", "0300", "0310"), 1, 0)
  )

### Others 22

df_22 <- df_22 %>%
  mutate(
    Math_class = ifelse(ST059Q01TA %in% c(995, 997, 998, 999), NA, ST059Q01TA)
  )

df_22 <- df_22 %>%
  mutate(
    Andalusia = ifelse(REGION.y == 72401, 1, 0),
    Aragon = ifelse(REGION.y == 72402, 1, 0),
    Asturias = ifelse(REGION.y == 72403, 1, 0),
    `Balearic Islands` = ifelse(REGION.y == 72404, 1, 0),
    `Canary Islands` = ifelse(REGION.y == 72405, 1, 0),
    Cantabria = ifelse(REGION.y == 72406, 1, 0),
    `Castile and Leon` = ifelse(REGION.y == 72407, 1, 0),
    `Castile-La Mancha` = ifelse(REGION.y == 72408, 1, 0),
    Catalonia = ifelse(REGION.y == 72409, 1, 0),
    Extremadura = ifelse(REGION.y == 72410, 1, 0),
    Galicia = ifelse(REGION.y == 72411, 1, 0),
    `La Rioja` = ifelse(REGION.y == 72412, 1, 0),
    Madrid = ifelse(REGION.y == 72413, 1, 0),
    Murcia = ifelse(REGION.y == 72414, 1, 0),
    Navarre = ifelse(REGION.y == 72415, 1, 0),
    `Basque Country` = ifelse(REGION.y == 72416, 1, 0),
    `Comunidad Valenciana` = ifelse(REGION.y == 72417, 1, 0),
    Ceuta = ifelse(REGION.y == 72418, 1, 0),
    Melilla = ifelse(REGION.y == 72419, 1, 0)
  )

#We will be used in te following map
df_22 <- df_22 %>%
  mutate(
    region_final = case_when(
      REGION.y == 72401 ~ "Andalucía",
      REGION.y == 72402 ~ "Aragón",
      REGION.y == 72403 ~ "Principado de Asturias",
      REGION.y == 72404 ~ "Illes Balears",
      REGION.y == 72405 ~ "Canarias",
      REGION.y == 72406 ~ "Cantabria",
      REGION.y == 72407 ~ "Castilla y León",
      REGION.y == 72408 ~ "Castilla-La Mancha",
      REGION.y == 72409 ~ "Cataluña",
      REGION.y == 72410 ~ "Extremadura",
      REGION.y == 72411 ~ "Galicia",
      REGION.y == 72412 ~ "La Rioja",
      REGION.y == 72413 ~ "Comunidad de Madrid",
      REGION.y == 72414 ~ "Región de Murcia",
      REGION.y == 72415 ~ "Comunidad Foral de Navarra",
      REGION.y == 72416 ~ "País Vasco",
      REGION.y == 72417 ~ "Comunitat Valenciana",
      REGION.y == 72418 ~ "Ciudad de Ceuta ",
      REGION.y == 72419 ~ "Ciudad de Melilla",
      TRUE ~ "Unknown"  
    )
  )

barplot(table(df_22$Native), main = "Diagrama de barras de la variable", xlab = "Valor", ylab = "Frecuencia")

## COVID

df_22 <- df_22 |>
  mutate( Covid_homework = ifelse(ST348Q02JA == 1, 0, 1 ) )

df_22 <- df_22 |>
  mutate( Covid_Zoom = ifelse ( ST348Q05JA == 1, 0, 1))

df_22 <- df_22 |>
  mutate(Covid_learned_comparison = case_when(
    ST350Q01JA == 1 ~ "Learnt Less",
    ST350Q01JA == 2 ~ "Learnt the Same",
    ST350Q01JA == 3 ~ "Learnt More",
    TRUE ~ NA
  ))

df_22 <- df_22 |> 
  rename(skipped_class=ST062Q01TA)

# Dependent Var 22 

df_22 <- df_22 %>%
  mutate(
    Mean_MATH = rowMeans(select(., PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH,
                                PV6MATH, PV7MATH, PV8MATH, PV9MATH, PV10MATH),
                         na.rm = TRUE)
  )

## 2.3 Cleaning 2015 =================================================================

# Schools 15

df_15 <- df_15 |>
  mutate (village = ifelse(SC001Q01TA ==  1, 1, 0),
          town = ifelse ( SC001Q01TA == 3 , 1, 0),
          city = ifelse ( SC001Q01TA == 4 , 1 , 0),
          large_city = ifelse ( SC001Q01TA == 5, 1, 0))

df_15 <- df_15 |>
  mutate(
    public = car::recode(SC013Q01TA, "2=0; 5=NA; 7=NA; 8=NA; 9=NA")
  )

df_15 <- df_15 |>
  mutate( number_boys = case_when(
    SC002Q01TA %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ SC002Q01TA
  ))

df_15 <- df_15 |>
  mutate( number_girls = case_when(
    SC002Q02TA %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ SC002Q02TA
  ))

df_15 <- df_15 |>
  mutate( number_teachers = case_when(
    TOTAT %in% c(99995, 99997, 99998, 99999) ~ NA,
    TRUE~ TOTAT
  ))

df_15 <- df_15 |>
  mutate( number_computers = case_when(
    RATCMP1 %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ RATCMP1
  ))
df_15 <- df_15 |>
  mutate( computers_internet = case_when(
    RATCMP2 %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ RATCMP2
  ))

df_15 <- df_15 |>
  mutate( Student_teacher_ratio = case_when(
    STRATIO %in% c(995, 997, 998, 999) ~ NA,
    TRUE~ STRATIO
  ))

df_15 <- df_15 |>
  mutate( SCHSIZE = ifelse(
    SCHSIZE > 1e+07, NA, SCHSIZE ))

## Caracteristics of Individuals 15 

df_15 <- df_15 |>
  mutate( Female = ifelse( ST004D01T== 1, 1,0))


df_15 <- df_15 |>
  mutate(
    Repeat = car::recode(REPEAT, "5=NA; 7=NA; 8=NA; 9=NA")
  )

df_15 <- df_15 %>%
  mutate(
    IMMIG = case_when(
      IMMIG %in% c(5, 7, 8, 9) ~ NA_real_,
      TRUE ~ IMMIG
    )
  )

df_15 <- df_15 |>
  mutate (Native = ifelse(IMMIG ==  1, 1, 0),
          Second_generation = ifelse ( IMMIG == 3 , 1, 0))

df_15 <- df_15 |>
  mutate(
    Language_test = car::recode(ST022Q01TA, "2=0 ; 95=NA; 97=NA; 98=NA; 99=NA")
  )

df_15 <- df_15 |>
  mutate(
    Easily  = case_when(
      ST034Q02TA == 1 | ST034Q02TA == 2 ~ 1,  
      ST034Q02TA == 3 | ST034Q02TA == 4 ~ 0,  
      ST034Q02TA == 5 | ST034Q02TA == 7 | ST034Q02TA == 8 | ST034Q02TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_15 <- df_15 |>
  mutate(
    Seem = case_when(
      ST034Q05TA == 1 | ST034Q05TA == 2 ~ 1,  
      ST034Q05TA == 3 | ST034Q05TA == 4 ~ 0,  
      ST034Q05TA == 5 | ST034Q05TA == 7 | ST034Q05TA == 8 | ST034Q05TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_15 <- df_15 |>
  mutate(
    Lonely  = case_when(
      ST034Q06TA == 1 | ST034Q06TA == 2 ~ 1,  
      ST034Q06TA == 3 | ST034Q06TA == 4 ~ 0,  
      ST034Q06TA == 5 | ST034Q06TA == 7 | ST034Q06TA == 8 | ST034Q06TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_15 <- df_15 |>
  mutate(
    Internet  = case_when(
      IC001Q04TA == 1 | IC001Q04TA == 2 ~ 1,  
      IC001Q04TA == 3  ~ 0,  # Disagree
      IC001Q04TA == 5 | IC001Q04TA == 7 | IC001Q04TA == 8 | IC001Q04TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_15 <- df_15 |>
  mutate(
    Computers = case_when(
      IC001Q01TA == 1 | IC001Q01TA == 2 ~ 1,  
      IC001Q01TA == 3  ~ 0,  
      IC001Q01TA == 5 | IC001Q01TA == 7 | IC001Q01TA == 8 | IC001Q01TA == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


## Parents 15 

df_15 <- df_15 |>
  mutate(
    ST013Q01TA = car::recode(ST013Q01TA, " 95=NA; 97=NA; 98=NA; 99=NA")
  ) |>
  mutate( `0-10 books` = ifelse( ST013Q01TA == 1, 1, 0),
          `11-25 books` = ifelse(ST013Q01TA == 2, 1, 0),
          `26-100 books` = ifelse(ST013Q01TA == 3, 1, 0),
          `101-200 books` = ifelse(ST013Q01TA == 2, 1, 0),
          `201-500 books` = ifelse(ST013Q01TA == 5, 1, 0),
          `>500 books` = ifelse (ST013Q01TA == 6, 1, 0 )
  )

df_15$MISCED[df_15$MISCED %in% c(95, 97, 98, 99)] <- NA

df_15 <- df_15 %>%
  mutate(M_None = ifelse(MISCED == 0, 1, 0),
         M_Primaria = ifelse(MISCED == 1, 1, 0),
         M_ESO = ifelse(MISCED == 2, 1, 0),
         M_Grado_medio_bachillerato = ifelse(MISCED == 3, 1, 0),
         M_Educaion_postsecundaria = ifelse(MISCED == 4, 1, 0),
         M_Educacion_superior = ifelse(MISCED == 5, 1, 0),
         M_Educacion_avanzada = ifelse(MISCED == 6, 1, 0))

df_15$FISCED[df_15$FISCED %in% c(95, 97, 98, 99)] <- NA

df_15 <- df_15 %>%
  mutate(F_None = ifelse(FISCED == 0, 1, 0),
         F_Primaria = ifelse(FISCED == 1, 1, 0),
         F_ESO = ifelse(FISCED == 2, 1, 0),
         F_Grado_medio_bachillerato = ifelse(FISCED == 3, 1, 0),
         F_Educaion_postsecundaria = ifelse(FISCED == 4, 1, 0),
         F_Educacion_superior = ifelse(FISCED == 5, 1, 0),
         F_Educacion_avanzada = ifelse(FISCED == 6, 1, 0))

### Father

df_15$OCOD2[df_15$OCOD2 %in% c("9997", "9998", "9999")] <- NA
df_15 <- df_15 %>%
  mutate(
    Directores_y_Gerentes = ifelse(OCOD2 %in% c("1000", "1100", "1110", "1111", "1112", "1113", "1114", "1120", "1200", "1210", "1211", "1212", "1213", "1219", "1220", "1221", "1222", "1223", "1300", "1310", "1311", "1312", "1320", "1321", "1322", "1323", "1324", "1330", "1340", "1341", "1342", "1343", "1344", "1345", "1346", "1349", "1400", "1410", "1411", "1412", "1420", "1430", "1431", "1439"), 1, 0),
    Tecnicos_y_profesionales_cientificos = ifelse(OCOD2 %in% c("2000", "2100", "2110", "2111", "2112", "2113", "2114", "2120", "2130", "2131", "2132", "2133", "2140", "2141", "2142", "2143", "2144", "2145", "2146", "2149", "2150", "2151", "2152", "2153", "2160", "2161", "2162", "2163", "2164", "2165", "2166"), 1, 0),
    Otros_tecnicos_cientificos = ifelse(OCOD2 %in% c("2600", "2610", "2611", "2612", "2619", "2620", "2621", "2622", "2630", "2631", "2632", "2633", "2634", "2635", "2636", "2640", "2641", "2642", "2643", "2650", "2651", "2652", "2653", "2654", "2655", "2656", "2659"), 1, 0),
    Tecnicos_y_profesionales_de_apoyo = ifelse(OCOD2 %in% c("3000", "3100", "3110", "3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119", "3120", "3121", "3122", "3123", "3130", "3131", "3132", "3133", "3134", "3135", "3139", "3140", "3141", "3142", "3143", "3150", "3151", "3152", "3153", "3154", "3155"), 1, 0),
    Empleados_administrativos_y_de_oficina = ifelse(OCOD2 %in% c("4000", "4100", "4110", "4120", "4130", "4131", "4132"), 1, 0),
    Otros_empleados_de_oficina = ifelse(OCOD2 %in% c("4200", "4210", "4211", "4212", "4213", "4214", "4220", "4221", "4222", "4223", "4224", "4225", "4226", "4227", "4229", "4300", "4310", "4311", "4312", "4313", "4320", "4321", "4322", "4323", "4400", "4410", "4411", "4412", "4413", "4414", "4415", "4416", "4419"), 1, 0),
    Servicios_de_restauracion_y_vendedores = ifelse(OCOD2 %in% c("5000", "5100", "5110", "5111", "5112", "5113", "5120", "5130", "5131", "5132", "5140", "5141", "5142", "5150", "5151", "5152", "5153", "5160", "5161", "5162", "5163", "5164", "5165", "5169", "5200", "5210", "5211", "5212", "5220", "5221", "5222", "5223", "5230", "5240", "5241", "5242", "5243", "5244", "5245", "5246", "5249"), 1, 0),
    Servicios_de_Salud = ifelse(OCOD2 %in% c("2200", "2210", "2211", "2212", "2220", "2221", "2222", "2230", "2240", "2250", "2260", "2261", "2262", "2263", "2264", "2265", "2266", "2267", "2269"), 1, 0),
    Servicios_de_Proteccion = ifelse(OCOD2 %in% c("5400", "5410", "5411", "5412", "5413", "5414", "5419"), 1, 0),
    Trabajadores_cualificados_en_agricultura = ifelse(OCOD2 %in% c("6000", "6100", "6110", "6111", "6112", "6113", "6114", "6120", "6121", "6122", "6123", "6129", "6130"), 1, 0),
    Trab_cual_de_manufacturas_y_construccion = ifelse(OCOD2 %in% c("7000", "7100", "7110", "7111", "7112", "7113", "7114", "7115", "7119", "7120", "7121", "7122", "7123", "7124", "7125", "7126", "7127", "7130", "7131", "7132", "7133"), 1, 0),
    Trab_cual_de_industrias = ifelse(OCOD2 %in% c("7200", "7210", "7211", "7212", "7213", "7214", "7215", "7220", "7221", "7222", "7223", "7224", "7230", "7231", "7232", "7233", "7234", "7300", "7310", "7311", "7312", "7313", "7314", "7315", "7316", "7317", "7318", "7319", "7320", "7321", "7322", "7323", "7400", "7410", "7411", "7412", "7413", "7420", "7421", "7422"), 1, 0),
    Operadores_de_instalaciones_y_maquinaria = ifelse(OCOD2 %in% c("8000", "8100", "8110", "8111", "8112", "8113", "8114", "8120", "8121", "8122", "8130", "8131", "8132", "8140", "8141", "8142", "8143", "8150", "8151", "8152", "8153", "8154", "8155", "8156", "8157", "8159", "8160"), 1, 0),
    Conductores_y_Operadores_de_Maquinaria = ifelse(OCOD2 %in% c("8300", "8310", "8311", "8320", "8321", "8322", "8330", "8331", "8332", "8340", "8341", "8342", "8343", "8344", "8350"), 1, 0),
    Trabajadores_no_cualificados_en_Servicios = ifelse(OCOD2 %in% c("9000", "9100", "9110", "9111", "9112", "9120", "9121", "9122", "9123", "9129", "9200", "9210", "9211", "9212", "9213", "9214", "9215", "9216", "9300", "9310", "9311", "9312", "9313", "9320", "9321", "9329", "9330", "9331", "9332", "9333", "9334", "9400", "9410", "9411", "9412", "9500", "9510", "9520", "9600", "9610", "9611", "9612", "9613", "9620", "9621", "9622", "9623", "9624", "9629"), 1, 0),
    Ocupaciones_militares = ifelse(OCOD2 %in% c("0000", "0100", "0110", "0200", "0210", "0300", "0310"), 1, 0)
  )


### Mother

df_15$OCOD1[df_15$OCOD1 %in% c("9997", "9998", "9999")] <- NA
df_15 <- df_15 %>%
  mutate(
    M_Directores_y_Gerentes = ifelse(OCOD1 %in% c("1000", "1100", "1110", "1111", "1112", "1113", "1114", "1120", "1200", "1210", "1211", "1212", "1213", "1219", "1220", "1221", "1222", "1223", "1300", "1310", "1311", "1312", "1320", "1321", "1322", "1323", "1324", "1330", "1340", "1341", "1342", "1343", "1344", "1345", "1346", "1349", "1400", "1410", "1411", "1412", "1420", "1430", "1431", "1439"), 1, 0),
   M_Tecnicos_y_profesionales_cientificos = ifelse(OCOD1 %in% c("2000", "2100", "2110", "2111", "2112", "2113", "2114", "2120", "2130", "2131", "2132", "2133", "2140", "2141", "2142", "2143", "2144", "2145", "2146", "2149", "2150", "2151", "2152", "2153", "2160", "2161", "2162", "2163", "2164", "2165", "2166"), 1, 0),
    M_Otros_tecnicos_cientificos = ifelse(OCOD1 %in% c("2600", "2610", "2611", "2612", "2619", "2620", "2621", "2622", "2630", "2631", "2632", "2633", "2634", "2635", "2636", "2640", "2641", "2642", "2643", "2650", "2651", "2652", "2653", "2654", "2655", "2656", "2659"), 1, 0),
    M_Tecnicos_y_profesionales_de_apoyo = ifelse(OCOD1 %in% c("3000", "3100", "3110", "3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119", "3120", "3121", "3122", "3123", "3130", "3131", "3132", "3133", "3134", "3135", "3139", "3140", "3141", "3142", "3143", "3150", "3151", "3152", "3153", "3154", "3155"), 1, 0),
    M_Empleados_administrativos_y_de_oficina = ifelse(OCOD1 %in% c("4000", "4100", "4110", "4120", "4130", "4131", "4132"), 1, 0),
    M_Otros_empleados_de_oficina = ifelse(OCOD1 %in% c("4200", "4210", "4211", "4212", "4213", "4214", "4220", "4221", "4222", "4223", "4224", "4225", "4226", "4227", "4229", "4300", "4310", "4311", "4312", "4313", "4320", "4321", "4322", "4323", "4400", "4410", "4411", "4412", "4413", "4414", "4415", "4416", "4419"), 1, 0),
    M_Servicios_de_restauracion_y_vendedores = ifelse(OCOD1 %in% c("5000", "5100", "5110", "5111", "5112", "5113", "5120", "5130", "5131", "5132", "5140", "5141", "5142", "5150", "5151", "5152", "5153", "5160", "5161", "5162", "5163", "5164", "5165", "5169", "5200", "5210", "5211", "5212", "5220", "5221", "5222", "5223", "5230", "5240", "5241", "5242", "5243", "5244", "5245", "5246", "5249"), 1, 0),
    M_Servicios_de_Salud = ifelse(OCOD1 %in% c("2200", "2210", "2211", "2212", "2220", "2221", "2222", "2230", "2240", "2250", "2260", "2261", "2262", "2263", "2264", "2265", "2266", "2267", "2269"), 1, 0),
    M_Servicios_de_Proteccion = ifelse(OCOD1 %in% c("5400", "5410", "5411", "5412", "5413", "5414", "5419"), 1, 0),
    M_Trabajadores_cualificados_en_agricultura = ifelse(OCOD1 %in% c("6000", "6100", "6110", "6111", "6112", "6113", "6114", "6120", "6121", "6122", "6123", "6129", "6130"), 1, 0),
    M_Trab_cual_de_manufacturas_y_construccion = ifelse(OCOD1 %in% c("7000", "7100", "7110", "7111", "7112", "7113", "7114", "7115", "7119", "7120", "7121", "7122", "7123", "7124", "7125", "7126", "7127", "7130", "7131", "7132", "7133"), 1, 0),
    M_Trab_cual_de_industrias = ifelse(OCOD1 %in% c("7200", "7210", "7211", "7212", "7213", "7214", "7215", "7220", "7221", "7222", "7223", "7224", "7230", "7231", "7232", "7233", "7234", "7300", "7310", "7311", "7312", "7313", "7314", "7315", "7316", "7317", "7318", "7319", "7320", "7321", "7322", "7323", "7400", "7410", "7411", "7412", "7413", "7420", "7421", "7422"), 1, 0),
    M_Operadores_de_instalaciones_y_maquinaria = ifelse(OCOD1 %in% c("8000", "8100", "8110", "8111", "8112", "8113", "8114", "8120", "8121", "8122", "8130", "8131", "8132", "8140", "8141", "8142", "8143", "8150", "8151", "8152", "8153", "8154", "8155", "8156", "8157", "8159", "8160"), 1, 0),
    M_Conductores_y_Operadores_de_Maquinaria = ifelse(OCOD1 %in% c("8300", "8310", "8311", "8320", "8321", "8322", "8330", "8331", "8332", "8340", "8341", "8342", "8343", "8344", "8350"), 1, 0),
    M_Trabajadores_no_cualificados_en_Servicios = ifelse(OCOD1 %in% c("9000", "9100", "9110", "9111", "9112", "9120", "9121", "9122", "9123", "9129", "9200", "9210", "9211", "9212", "9213", "9214", "9215", "9216", "9300", "9310", "9311", "9312", "9313", "9320", "9321", "9329", "9330", "9331", "9332", "9333", "9334", "9400", "9410", "9411", "9412", "9500", "9510", "9520", "9600", "9610", "9611", "9612", "9613", "9620", "9621", "9622", "9623", "9624", "9629"), 1, 0),
    M_Ocupaciones_militares = ifelse(OCOD1 %in% c("0000", "0100", "0110", "0200", "0210", "0300", "0310"), 1, 0)
  )

### Others 15 

df_15 <- df_15 %>%
  mutate(
    Math_class = ifelse(ST059Q01TA %in% c(995, 997, 998, 999), NA, ST059Q01TA)
  )

df_15 <- df_15 %>%
  mutate(
    Andalusia = ifelse(Region.y == 72401, 1, 0),
    Aragon = ifelse(Region.y == 72402, 1, 0),
    Asturias = ifelse(Region.y == 72403, 1, 0),
    `Balearic Islands` = ifelse(Region.y == 72404, 1, 0),
    `Canary Islands` = ifelse(Region.y == 72405, 1, 0),
    Cantabria = ifelse(Region.y == 72406, 1, 0),
    `Castile and Leon` = ifelse(Region.y == 72407, 1, 0),
    `Castile-La Mancha` = ifelse(Region.y == 72408, 1, 0),
    Catalonia = ifelse(Region.y == 72409, 1, 0),
    Extremadura = ifelse(Region.y == 72410, 1, 0),
    Galicia = ifelse(Region.y == 72411, 1, 0),
    `La Rioja` = ifelse(Region.y == 72412, 1, 0),
    Madrid = ifelse(Region.y == 72413, 1, 0),
    Murcia = ifelse(Region.y == 72414, 1, 0),
    Navarre = ifelse(Region.y == 72415, 1, 0),
    `Basque Country` = ifelse(Region.y == 72416, 1, 0),
    `Comunidad Valenciana` = ifelse(Region.y == 72417, 1, 0),
    Ceuta = ifelse(Region.y == 72418, 1, 0),
    Melilla = ifelse(Region.y == 72419, 1, 0)
  )

barplot(table(df_15$Native), main = "Diagrama de barras de la variable", xlab = "Valor", ylab = "Frecuencia")

df_15 <- df_15 |> 
  rename(skipped_class = ST062Q01TA)

# Var Dependiente 15 

df_15 <- df_15 %>%
  mutate(
    Mean_MATH = rowMeans(select(., PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH,
                                PV6MATH, PV7MATH, PV8MATH, PV9MATH, PV10MATH),
                         na.rm = TRUE)
  )

# 3. FINAL DATABASES =======================================================

df_18 <- df_18 |>
  select (51: 148)

df_15 <- df_15 |>
  select(51:147)

df_22 <- df_22 |>
  select(61:161)

columnas_no_en_df_15 <- setdiff(names(df_22), names(df_15))
print("Columnas de df_22 que no están en df_15:")
print(columnas_no_en_df_15)

# Encontrar las columnas en df_15 que no están en df_22
columnas_no_en_df_22 <- setdiff(names(df_15), names(df_22))
print("Columnas de df_15 que no están en df_22:")
print(columnas_no_en_df_22)

setdiff(names(df_18), names(df_15))


# 4. IMPUTATION ================================================================

## 4.1 Imputation 2015 =============================================================

set.seed(123)
percentage_missing <- colMeans(is.na(df_15)) * 100
print(percentage_missing)

missing_data <-data.frame(
  Variable = names(percentage_missing),
  Percentage = percentage_missing
) %>%
  filter(Percentage > 0) %>%  
  arrange(desc(Percentage)) %>%
  top_n(10, Percentage)

ggplot(missing_data, aes(x = reorder(Variable, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Hacer que el gráfico sea horizontal
  labs(title = "Top 10 Variables by Percentage of Missing Data",
       x = "Variables",
       y = "Percentage of Missing Data (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(size = 14,  hjust = 0.3)
  )

# impute

set.seed(123)

df_15_clean <- zap_labels(df_15)
names(df_15_clean) <- make.names(names(df_15_clean), unique = TRUE)
df_15_clean <- data.frame(lapply(df_15_clean, function(x) as.numeric(as.character(x))))

imputed_lasso <- complete(mice(df_15_clean, method = 'lasso.norm', m = 1, seed = 123, printFlag = TRUE))
imputed_pmm <- complete(mice(df_15_clean, m=1, method = "pmm", seed=123))
imputed_rf <- complete(mice(df_15_clean, m=1, method = "rf", seed=123))

## Extracting the variable of interest
original_data <- df_15_clean$Student_teacher_ratio
lasso_data <- imputed_lasso$Student_teacher_ratio
pmm_data <- imputed_pmm$Student_teacher_ratio
rf_data <- imputed_rf$Student_teacher_ratio

# Create a dataframe for visualisation
data_plot <- data.frame(
  Ratio = c(original_data, lasso_data, pmm_data, rf_data),
  Method = factor(rep(c("Original", "LASSO", "PMM", "RF"), each = length(original_data)))
)
data_plot$Method <- factor(data_plot$Method, levels = c("Original", "LASSO", "PMM", "RF"))

plot1 <- ggplot(data_plot, aes(x = Ratio, fill = Method)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ Method, ncol = 2, scales = "free") +
  theme_minimal() +
  labs(title = "",
       x = "
       Student-Teacher ratio",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none")


plot1

#######  Extracting the variable of interest
original_data <- df_15_clean$number_computers
lasso_data <- imputed_lasso$number_computers
pmm_data <- imputed_pmm$number_computers
rf_data <- imputed_rf$number_computers

data_plot <- data.frame(
  Ratio = c(original_data, lasso_data, pmm_data, rf_data),
  Method = factor(rep(c("Original", "LASSO", "PMM", "RF"), each = length(original_data)))
)
data_plot$Method <- factor(data_plot$Method, levels = c("Original", "LASSO", "PMM", "RF"))

# Density graph

plot2 <- ggplot(data_plot, aes(x = Ratio, fill = Method)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ Method, ncol = 2, scales = "free") +
  theme_minimal() +
  labs(title = "",
       x = "
       Number Computers",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none")
plot2

combined_plot <- (plot1 + plot2) + 
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom", legend.direction = "horizontal")

combined_plot <- combined_plot + 
  plot_annotation(
    title = "Distribution of Student-Teacher Ratio and Number of Computers"
  ) & 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

combined_plot

imputed_rf$id_school <- merged_data_15$CNTSCHID
df_15_clean <- imputed_rf |> na.omit()


  ## 4.2 Imputation 2018 ===========================================================

# Same proccess than 2015

set.seed(123)
percentage_missing_18 <- colMeans(is.na(df_18)) * 100
print(percentage_missing_18)

missing_data_18 <-data.frame(
  Variable = names(percentage_missing_18),
  Percentage = percentage_missing_18
) %>%
  filter(Percentage > 0) %>%  
  arrange(desc(Percentage)) %>%
  top_n(10, Percentage)

ggplot(missing_data_18, aes(x = reorder(Variable, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Top 10 Variables por Porcentaje de Datos Faltantes",
       x = "Variable",
       y = "Porcentaje de Datos Faltantes (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# impute

set.seed(123)
df_18_clean <- df_18 |> zap_labels() %>% select(-region_final)
names(df_18_clean) <- make.names(names(df_18_clean), unique = TRUE)
df_18_clean <- data.frame(lapply(df_18_clean, function(x) as.numeric(as.character(x))))

imputed_lasso_18 <- complete(mice(df_18_clean, m = 1, method = 'lasso.norm', seed = 123, printFlag = TRUE))
imputed_pmm_18 <- complete(mice(df_18_clean, m=1, method = "pmm", seed=123))
imputed_rf_18 <- complete(mice(df_18_clean, m=1, method = "rf", seed=123))


original_data_18 <- df_18_clean$Math_class
lasso_data_18 <- imputed_lasso_18$Math_class
pmm_data_18 <- imputed_pmm_18$Math_class
rf_data_18 <- imputed_rf_18$Math_class


data_plot <- data.frame(
  Ratio = c(original_data_18, lasso_data_18, pmm_data_18, rf_data_18),
  Method = factor(rep(c("Original", "LASSO", "PMM", "RF"), each = length(original_data_18)))
)
data_plot$Method <- factor(data_plot$Method, levels = c("Original", "LASSO", "PMM", "RF"))


plot1 <- ggplot(data_plot, aes(x = Ratio, fill = Method)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ Method, ncol = 2, scales = "free") +
  theme_minimal() +
  labs(title = "",
       x = "
       Math Class",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none")
plot1


original_data_18 <- df_18_clean$skipped_class
lasso_data_18 <- imputed_lasso_18$skipped_class
pmm_data_18 <- imputed_pmm_18$skipped_class
rf_data_18 <- imputed_rf_18$skipped_class


data_plot <- data.frame(
  Ratio = c(original_data_18, lasso_data_18, pmm_data_18, rf_data_18),
  Method = factor(rep(c("Original", "LASSO", "PMM", "RF"), each = length(original_data_18)))
)
data_plot$Method <- factor(data_plot$Method, levels = c("Original", "LASSO", "PMM", "RF"))

# Density Graph

plot2 <- ggplot(data_plot, aes(x = Ratio, fill = Method)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ Method, ncol = 2, scales = "free") +
  theme_minimal() +
  labs(title = "",
       x = "
       Skipped Class",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none")
plot2

#combined
combined_plot <- (plot1 + plot2) + 
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom", legend.direction = "horizontal")

combined_plot <- combined_plot + 
  plot_annotation(
    title = "Distribution of Math class and skipped class"
  ) & 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
combined_plot



imputed_rf_18$id_school <- merged_data_18$CNTSCHID
df_18_clean <- imputed_rf_18 |> na.omit()

## 4.3 Imputation 2022 =============================================================

# The same proccess tha previous years

set.seed(123)
percentage_missing_22 <- colMeans(is.na(df_22)) * 100
print(percentage_missing_22)

missing_data_22 <-data.frame(
  Variable = names(percentage_missing_22),
  Percentage = percentage_missing_22
) %>%
  filter(Percentage > 0) %>% 
  arrange(desc(Percentage)) %>%
  top_n(10, Percentage)

ggplot(missing_data_22, aes(x = reorder(Variable, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Top 10 Variables por Porcentaje de Datos Faltantes",
       x = "Variable",
       y = "Porcentaje de Datos Faltantes (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
#imputo
set.seed(123)

df_22_clean <- df_22 |> zap_labels() %>% select(-region_final, -Covid_Zoom, - Covid_homework, - Covid_learned_comparison )
names(df_22_clean) <- make.names(names(df_22_clean), unique = TRUE)
df_22_clean <- data.frame(lapply(df_22_clean, function(x) as.numeric(as.character(x))))

imputed_lasso_22 <- complete(mice(df_22_clean, m = 1, method = 'lasso.norm', seed = 123, printFlag = TRUE))
imputed_pmm_22 <- complete(mice(df_22_clean, m=1, method = "pmm", seed=123))
imputed_rf_22 <- complete(mice(df_22_clean, m=1, method = "rf", seed=123))


## Extracting the variable of interest
original_data <- df_22_clean$Student_teacher_ratio
lasso_data <- imputed_lasso_22$Student_teacher_ratio
pmm_data <- imputed_pmm_22$Student_teacher_ratio
rf_data <- imputed_rf_22$Student_teacher_ratio


data_plot <- data.frame(
  Ratio = c(original_data, lasso_data, pmm_data, rf_data),
  Method = factor(rep(c("Original", "LASSO", "PMM", "RF"), each = length(original_data)))
)
data_plot$Method <- factor(data_plot$Method, levels = c("Original", "LASSO", "PMM", "RF"))

# Density graph
plot1 <-ggplot(data_plot, aes(x = Ratio, fill = Method)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ Method, ncol = 2, scales = "free") +
  theme_minimal() +
  labs(title = "",
       x = "
       Student-Teacher Ratio",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none")

plot1

####### Extracting the variable of interest
original_data <- df_22_clean$number_computers
lasso_data <- imputed_lasso_22$number_computers
pmm_data <- imputed_pmm_22$number_computers
rf_data <- imputed_rf_22$number_computers


data_plot <- data.frame(
  Ratio = c(original_data, lasso_data, pmm_data, rf_data),
  Method = factor(rep(c("Original", "LASSO", "PMM", "RF"), each = length(original_data)))
)
data_plot$Method <- factor(data_plot$Method, levels = c("Original", "LASSO", "PMM", "RF"))

#Density Graph

plot2 <- ggplot(data_plot, aes(x = Ratio, fill = Method)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ Method, ncol = 2, scales = "free") +
  theme_minimal() +
  labs(title = "",
       x = "
       Number Computer",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none")
plot2

#combined
combined_plot <- (plot1 + plot2) + 
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom", legend.direction = "horizontal")
# Agregar el título global con la función plot_annotation correctamente
combined_plot <- combined_plot + 
  plot_annotation(
    title = "Distribution of Student-Teacher ratio and number of computers"
  ) & 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
combined_plot
imputed_rf_22$id_school <- merged_data_22$CNTSCHID
df_22_clean <- imputed_rf_22 |> na.omit()

# 5. Synthetic indicators ===========================================================================

## 5.1 PCA 2018 ================================================================

# PROCESS FOR MEN

### Correlations 2018 

CORR_18 <- df_18_clean[c("Directores_y_Gerentes",
                         "Tecnicos_y_profesionales_cientificos",
                         "Otros_tecnicos_cientificos",
                         "Empleados_administrativos_y_de_oficina",
                         "Otros_empleados_de_oficina",
                         "Servicios_de_restauracion_y_vendedores",
                         "Servicios_de_Proteccion",
                         "Trabajadores_cualificados_en_agricultura",
                         "Trab_cual_de_manufacturas_y_construccion",
                         "Trab_cual_de_industrias",
                         "Operadores_de_instalaciones_y_maquinaria",
                         "Conductores_y_Operadores_de_Maquinaria",
                         "Trabajadores_no_cualificados_en_Servicios",
                         "Ocupaciones_militares",
                         "Tecnicos_y_profesionales_de_apoyo",
                         "Servicios_de_Salud",
                         "Mean_MATH")]

## Correlations:

CORR <- rcorr(as.matrix(CORR_18))


corrplot(CORR$r,
         type="upper",  
         tl.col="black",  
         tl.srt=90,  
         pch.col="blue",  
         insig="p-value",  
         sig.level=0.05,  
         col=terrain.colors(100),  
         tl.cex=0.7, 
         number.cex=0.6,  
         addCoef.col="black",  
         addCoefasPercent=FALSE,  
         number.digits=2  
)

mean_math_correlations <- CORR$r[,"Mean_MATH", drop = FALSE]  


l <- lm(Mean_MATH ~., data =CORR_18 )
summary(l)

# PCA FOR MEN

PCA <- CORR_18 |>
  select(Directores_y_Gerentes, Tecnicos_y_profesionales_cientificos, Otros_tecnicos_cientificos, Empleados_administrativos_y_de_oficina, Otros_empleados_de_oficina, Servicios_de_restauracion_y_vendedores, Trabajadores_cualificados_en_agricultura, Trab_cual_de_manufacturas_y_construccion, Servicios_de_Proteccion, Trab_cual_de_industrias,Operadores_de_instalaciones_y_maquinaria, Trabajadores_no_cualificados_en_Servicios, Ocupaciones_militares, Conductores_y_Operadores_de_Maquinaria, Tecnicos_y_profesionales_de_apoyo, Servicios_de_Salud) |>
  select(-where(~ all(. == 0 | is.na(.)))) |>
  na.omit()

PCA <- CORR_18 |>
  select(-c(Mean_MATH, Servicios_de_Proteccion, Trab_cual_de_industrias, Operadores_de_instalaciones_y_maquinaria, Ocupaciones_militares))

set.seed(123)
# Apply PCA after removal of problematic columns
pca <- prcomp(PCA, scale = TRUE)
summary(pca)

fviz_screeplot(pca, addlabels = TRUE)

loadings <- as.data.frame(pca$rotation)
print(loadings)

df_pca_scores <- pca$x

Fathers_work <- rowMeans(df_pca_scores[,1:4])
df_18_clean$Fathers_work = Fathers_work

# PROCESS FOR WOMEN

## Employment Women.

PCAM <- df_18_clean[c("M_Directores_y_Gerentes",
                      "M_Tecnicos_y_profesionales_cientificos",
                      "M_Otros_tecnicos_cientificos",
                      "M_Empleados_administrativos_y_de_oficina",
                      "M_Otros_empleados_de_oficina",
                      "M_Servicios_de_restauracion_y_vendedores",
                      "M_Servicios_de_Proteccion",
                      "M_Trabajadores_cualificados_en_agricultura",
                      "M_Trab_cual_de_manufacturas_y_construccion",
                      "M_Trab_cual_de_industrias",
                      "M_Operadores_de_instalaciones_y_maquinaria",
                      "M_Conductores_y_Operadores_de_Maquinaria",
                      "M_Trabajadores_no_cualificados_en_Servicios",
                      "M_Ocupaciones_militares",
                      "M_Tecnicos_y_profesionales_de_apoyo",
                      "M_Servicios_de_Salud",
                      "Mean_MATH")]

## Correlations:

CORR <- rcorr(as.matrix(CORR_18))


corrplot(CORR$r,
         type="upper",  
         tl.col="black",  
         tl.srt=90,  
         pch.col="blue",  
         insig="p-value",  
         sig.level=0.05,  
         col=terrain.colors(100),  
         tl.cex=0.7,  
         number.cex=0.6,  
         addCoef.col="black",  
         addCoefasPercent=FALSE,  
         number.digits=2  
)

mean_math_correlations <- CORR$r[,"Mean_MATH", drop = FALSE]  


l <- lm(Mean_MATH ~., data =PCAM )
summary(l)

#PCA FOR WOMAN

PCAM <- PCAM |> select(-c(Mean_MATH,M_Servicios_de_Proteccion,M_Trab_cual_de_manufacturas_y_construccion, M_Trab_cual_de_industrias, M_Operadores_de_instalaciones_y_maquinaria, M_Conductores_y_Operadores_de_Maquinaria, M_Ocupaciones_militares ))
PCAM_scaled <- scale(PCAM)

# Aply PCA
pca <- prcomp(PCAM_scaled, scale = TRUE)
summary(pca)

fviz_screeplot(pca, addlabels = TRUE)

loadings <- as.data.frame(pca$rotation)
print(loadings)

df_pca_scores <- pca$x 

Mothers_work <- rowMeans(df_pca_scores[,1:5])

df_18_clean$Mothers_work = Mothers_work

#Cleaning

df_18_clean <-  df_18_clean |> select(-c(Directores_y_Gerentes, Tecnicos_y_profesionales_cientificos, Otros_tecnicos_cientificos, Empleados_administrativos_y_de_oficina, Otros_empleados_de_oficina,Servicios_de_Proteccion, Servicios_de_restauracion_y_vendedores, Trabajadores_cualificados_en_agricultura, Trab_cual_de_manufacturas_y_construccion,Trab_cual_de_industrias, Operadores_de_instalaciones_y_maquinaria, Trabajadores_no_cualificados_en_Servicios, Ocupaciones_militares, Conductores_y_Operadores_de_Maquinaria, Tecnicos_y_profesionales_de_apoyo, Servicios_de_Salud))

df_18_clean <-  df_18_clean |> select(-c(M_Directores_y_Gerentes, M_Tecnicos_y_profesionales_cientificos, M_Otros_tecnicos_cientificos, M_Empleados_administrativos_y_de_oficina, M_Otros_empleados_de_oficina,M_Servicios_de_Proteccion, M_Servicios_de_restauracion_y_vendedores, M_Trabajadores_cualificados_en_agricultura, M_Trab_cual_de_manufacturas_y_construccion, M_Trab_cual_de_industrias, M_Operadores_de_instalaciones_y_maquinaria, M_Trabajadores_no_cualificados_en_Servicios, M_Ocupaciones_militares, M_Conductores_y_Operadores_de_Maquinaria, M_Tecnicos_y_profesionales_de_apoyo, M_Servicios_de_Salud))


## 5.2 PCA 2015 ================================================================

# We repeat the same process
# PROCESS FOR MEN

### Correlations 2015 


CORR_15 <- df_15_clean[c("Directores_y_Gerentes",
                         "Tecnicos_y_profesionales_cientificos",
                         "Otros_tecnicos_cientificos",
                         "Empleados_administrativos_y_de_oficina",
                         "Otros_empleados_de_oficina",
                         "Servicios_de_restauracion_y_vendedores",
                         "Servicios_de_Proteccion",
                         "Trabajadores_cualificados_en_agricultura",
                         "Trab_cual_de_manufacturas_y_construccion",
                         "Trab_cual_de_industrias",
                         "Operadores_de_instalaciones_y_maquinaria",
                         "Conductores_y_Operadores_de_Maquinaria",
                         "Trabajadores_no_cualificados_en_Servicios",
                         "Ocupaciones_militares",
                         "Tecnicos_y_profesionales_de_apoyo",
                         "Servicios_de_Salud",
                         "Mean_MATH")]


CORR <- rcorr(as.matrix(CORR_15))


corrplot(CORR$r,
         type="upper",  
         tl.col="black",  
         tl.srt=90, 
         pch.col="blue",  
         insig="p-value",  
         sig.level=0.05,  
         col=terrain.colors(100),  
         tl.cex=0.7,  
         number.cex=0.6,  
         addCoef.col="black",  
         addCoefasPercent=FALSE,  
         number.digits=2  
)


mean_math_correlations <- CORR$r[,"Mean_MATH", drop = FALSE]  


l <- lm(Mean_MATH ~., data =CORR_15 )
summary(l)

# PCA MEN 2015 

#Same process


PCA <- CORR_15 |>
  select(-c(Mean_MATH, Trab_cual_de_industrias, Operadores_de_instalaciones_y_maquinaria))

PCAM_scaled <- scale(PCA)

set.seed(123)
pca <- prcomp(PCA, scale = TRUE)
summary(pca)

fviz_screeplot(pca, addlabels = TRUE)

loadings <- as.data.frame(pca$rotation)
print(loadings)

df_pca_scores <- pca$x 

Fathers_work <- rowMeans(df_pca_scores[,1:4])
df_15_clean$Fathers_work = Fathers_work


# PROCESS FOR WOMEN

PCAM <- df_15_clean[c("M_Directores_y_Gerentes",
                      "M_Tecnicos_y_profesionales_cientificos",
                      "M_Otros_tecnicos_cientificos",
                      "M_Empleados_administrativos_y_de_oficina",
                      "M_Otros_empleados_de_oficina",
                      "M_Servicios_de_restauracion_y_vendedores",
                      "M_Servicios_de_Proteccion",
                      "M_Trabajadores_cualificados_en_agricultura",
                      "M_Trab_cual_de_manufacturas_y_construccion",
                      "M_Trab_cual_de_industrias",
                      "M_Operadores_de_instalaciones_y_maquinaria",
                      "M_Conductores_y_Operadores_de_Maquinaria",
                      "M_Trabajadores_no_cualificados_en_Servicios",
                      "M_Ocupaciones_militares",
                      "M_Tecnicos_y_profesionales_de_apoyo",
                      "M_Servicios_de_Salud",
                      "Mean_MATH")]



CORR <- rcorr(as.matrix(PCAM))


corrplot(CORR$r,
         type="upper",  
         tl.col="black",  
         tl.srt=90, 
         pch.col="blue",  
         insig="p-value",  
         sig.level=0.05,  
         col=terrain.colors(100),  
         tl.cex=0.7,  
         number.cex=0.6,  
         addCoef.col="black", 
         addCoefasPercent=FALSE,  
         number.digits=2  
)


mean_math_correlations <- CORR$r[,"Mean_MATH", drop = FALSE]  

l <- lm(Mean_MATH ~., data =PCAM )
summary(l)

# PCA WOMEN
PCAM <- PCAM |> select(-c(Mean_MATH,M_Servicios_de_Proteccion,M_Trab_cual_de_manufacturas_y_construccion, M_Trab_cual_de_industrias, M_Operadores_de_instalaciones_y_maquinaria, M_Conductores_y_Operadores_de_Maquinaria, M_Ocupaciones_militares ))

PCAM_scaled <- scale(PCAM)


pca <- prcomp(PCAM_scaled, scale = TRUE)
summary(pca)

fviz_screeplot(pca, addlabels = TRUE)

loadings <- as.data.frame(pca$rotation)
print(loadings)

df_pca_scores <- pca$x 
Mothers_work <- rowMeans(df_pca_scores[,1:6])

df_15_clean$Mothers_work = Mothers_work

#limpieza 

df_15_clean <-  df_15_clean |> select(-c(Directores_y_Gerentes, Tecnicos_y_profesionales_cientificos, Otros_tecnicos_cientificos, Empleados_administrativos_y_de_oficina, Otros_empleados_de_oficina,Servicios_de_Proteccion, Servicios_de_restauracion_y_vendedores, Trabajadores_cualificados_en_agricultura, Trab_cual_de_manufacturas_y_construccion,Trab_cual_de_industrias, Operadores_de_instalaciones_y_maquinaria, Trabajadores_no_cualificados_en_Servicios, Ocupaciones_militares, Conductores_y_Operadores_de_Maquinaria, Tecnicos_y_profesionales_de_apoyo, Servicios_de_Salud))

df_15_clean <-  df_15_clean |> select(-c(M_Directores_y_Gerentes, M_Tecnicos_y_profesionales_cientificos, M_Otros_tecnicos_cientificos, M_Empleados_administrativos_y_de_oficina, M_Otros_empleados_de_oficina,M_Servicios_de_Proteccion, M_Servicios_de_restauracion_y_vendedores, M_Trabajadores_cualificados_en_agricultura, M_Trab_cual_de_manufacturas_y_construccion, M_Trab_cual_de_industrias, M_Operadores_de_instalaciones_y_maquinaria, M_Trabajadores_no_cualificados_en_Servicios, M_Ocupaciones_militares, M_Conductores_y_Operadores_de_Maquinaria, M_Tecnicos_y_profesionales_de_apoyo, M_Servicios_de_Salud))


## 5.3 PCA 2022 ================================================================

# PROCESS FOR MEN

### Correlations 2022 


CORR_22 <- df_22_clean[c("Directores_y_Gerentes",
                         "Tecnicos_y_profesionales_cientificos",
                         "Otros_tecnicos_cientificos",
                         "Empleados_administrativos_y_de_oficina",
                         "Otros_empleados_de_oficina",
                         "Servicios_de_restauracion_y_vendedores",
                         "Servicios_de_Proteccion",
                         "Trabajadores_cualificados_en_agricultura",
                         "Trab_cual_de_manufacturas_y_construccion",
                         "Trab_cual_de_industrias",
                         "Operadores_de_instalaciones_y_maquinaria",
                         "Conductores_y_Operadores_de_Maquinaria",
                         "Trabajadores_no_cualificados_en_Servicios",
                         "Ocupaciones_militares",
                         "Tecnicos_y_profesionales_de_apoyo",
                         "Servicios_de_Salud",
                         "Mean_MATH")]



CORR <- rcorr(as.matrix(CORR_22))


corrplot(CORR$r,
         type="upper", 
         tl.col="black",  
         tl.srt=90,  
         pch.col="blue",  
         insig="p-value",  
         sig.level=0.05,  
         col=terrain.colors(100),  
         tl.cex=0.7,  
         number.cex=0.6,  
         addCoef.col="black",  
         addCoefasPercent=FALSE,  
         number.digits=2  
)


mean_math_correlations <- CORR$r[,"Mean_MATH", drop = FALSE]  


l <- lm(Mean_MATH ~., data =CORR_22 )
summary(l)


# PCA MEN

PCA <- CORR_22 |>
  select(-c(Mean_MATH, Servicios_de_Proteccion, Trab_cual_de_industrias, Operadores_de_instalaciones_y_maquinaria, Ocupaciones_militares, Tecnicos_y_profesionales_de_apoyo))


set.seed(123)

pca <- prcomp(PCA, scale = TRUE)
summary(pca)

fviz_screeplot(pca, addlabels = TRUE)

loadings <- as.data.frame(pca$rotation)
print(loadings)

df_pca_scores <- pca$x 

Fathers_work <- rowMeans(df_pca_scores[,1:5])
df_22_clean$Fathers_work = Fathers_work


# PROCESS FOR WOMEN

PCAM <- df_22_clean[c("M_Directores_y_Gerentes",
                      "M_Tecnicos_y_profesionales_cientificos",
                      "M_Otros_tecnicos_cientificos",
                      "M_Empleados_administrativos_y_de_oficina",
                      "M_Otros_empleados_de_oficina",
                      "M_Servicios_de_restauracion_y_vendedores",
                      "M_Servicios_de_Proteccion",
                      "M_Trabajadores_cualificados_en_agricultura",
                      "M_Trab_cual_de_manufacturas_y_construccion",
                      "M_Trab_cual_de_industrias",
                      "M_Operadores_de_instalaciones_y_maquinaria",
                      "M_Conductores_y_Operadores_de_Maquinaria",
                      "M_Trabajadores_no_cualificados_en_Servicios",
                      "M_Ocupaciones_militares",
                      "M_Tecnicos_y_profesionales_de_apoyo",
                      "M_Servicios_de_Salud",
                      "Mean_MATH")]

## Correlaciones:

CORR <- rcorr(as.matrix(PCAM))


corrplot(CORR$r,
         type="upper",  
         tl.col="black",  
         tl.srt=90,  
         pch.col="blue",  
         insig="p-value",  
         sig.level=0.05,  
         col=terrain.colors(100),  
         tl.cex=0.7,  
         number.cex=0.6,  
         addCoef.col="black",  
         addCoefasPercent=FALSE,  
         number.digits=2  
)


mean_math_correlations <- CORR$r[,"Mean_MATH", drop = FALSE]  


l <- lm(Mean_MATH ~., data =PCAM )
summary(l)

#PCA WOMEN

PCAM <- PCAM |> select(-c(Mean_MATH,M_Servicios_de_Proteccion,M_Trab_cual_de_manufacturas_y_construccion, M_Trab_cual_de_industrias, M_Operadores_de_instalaciones_y_maquinaria, M_Conductores_y_Operadores_de_Maquinaria, M_Ocupaciones_militares ))
PCAM_scaled <- scale(PCAM)


pca <- prcomp(PCAM_scaled, scale = TRUE)
summary(pca)

fviz_screeplot(pca, addlabels = TRUE)

loadings <- as.data.frame(pca$rotation)
print(loadings)

df_pca_scores <- pca$x 
Mothers_work <- rowMeans(df_pca_scores[,1:6])

df_22_clean$Mothers_work = Mothers_work

# CLeaning 

df_22_clean <-  df_22_clean |> select(-c(Directores_y_Gerentes, Tecnicos_y_profesionales_cientificos, Otros_tecnicos_cientificos, Empleados_administrativos_y_de_oficina, Otros_empleados_de_oficina,Servicios_de_Proteccion, Servicios_de_restauracion_y_vendedores, Trabajadores_cualificados_en_agricultura, Trab_cual_de_manufacturas_y_construccion,Trab_cual_de_industrias, Operadores_de_instalaciones_y_maquinaria, Trabajadores_no_cualificados_en_Servicios, Ocupaciones_militares, Conductores_y_Operadores_de_Maquinaria, Tecnicos_y_profesionales_de_apoyo, Servicios_de_Salud))

df_22_clean <-  df_22_clean |> select(-c(M_Directores_y_Gerentes, M_Tecnicos_y_profesionales_cientificos, M_Otros_tecnicos_cientificos, M_Empleados_administrativos_y_de_oficina, M_Otros_empleados_de_oficina,M_Servicios_de_Proteccion, M_Servicios_de_restauracion_y_vendedores, M_Trabajadores_cualificados_en_agricultura, M_Trab_cual_de_manufacturas_y_construccion, M_Trab_cual_de_industrias, M_Operadores_de_instalaciones_y_maquinaria, M_Trabajadores_no_cualificados_en_Servicios, M_Ocupaciones_militares, M_Conductores_y_Operadores_de_Maquinaria, M_Tecnicos_y_profesionales_de_apoyo, M_Servicios_de_Salud))


# 6. Descriptive Analysis ======================================================

### Resumen General 

summary(df_15)

summary(df_18)

summary(df_22)


## 6.1 Dependent Variable ======================================================

summary(df_15$Mean_MATH)

summary(df_18$Mean_MATH)

summary(df_22$Mean_MATH)

##6.2 Dependent variable different percentiles =================================


percentiles_15 <- quantile(df_15$Mean_MATH, probs = seq(0, 1, 0.1), na.rm = TRUE)
percentiles_18 <- quantile(df_18$Mean_MATH, probs = seq(0, 1, 0.1), na.rm = TRUE)
percentiles_22 <- quantile(df_22$Mean_MATH, probs = seq(0, 1, 0.1), na.rm = TRUE)
print(percentiles_15)
print(percentiles_18)
print(percentiles_22)

# Creating data frames for each year
percentile_df_15 <- data.frame(Year = "2015", Percentile = seq(0, 1, 0.1), Mean_MATH = percentiles_15)
percentile_df_18 <- data.frame(Year = "2018", Percentile = seq(0, 1, 0.1), Mean_MATH = percentiles_18)
percentile_df_22 <- data.frame(Year = "2022", Percentile = seq(0, 1, 0.1), Mean_MATH = percentiles_22)

# Combining df
combined_df <- rbind(percentile_df_15, percentile_df_18, percentile_df_22)


ggplot(data = combined_df, aes(x = Percentile, y = Mean_MATH, color = Year, group = Year)) +
  geom_line(size = 1, lineend = "round", linejoin = "bevel") +  
  geom_point( size = 2, aes(shape = Year, fill = Year)) +
  scale_shape_manual(values = c("2015" = 24, "2018" = 23, "2022" = 22))+
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = comma) +  
  scale_color_manual(values = c("2015" = "#E69F00", "2018" = "#56B4E9", "2022" = "#009E73")) +
  scale_fill_manual(values = c("2015" = "#E69F00", "2018" = "#56B4E9", "2022" = "#009E73")) +  
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 1.5),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray"), 
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Distribution of Mathematics Grades by percentiles",
       x = "Percentiles (%)",
       y = "Math Notes")

#### Dependent variable density graphs


ggplot(df_22, aes(x = Mean_MATH)) +
  geom_density(fill = "#69b3a2", alpha = 0.8) +  
  labs(title = "Distribución de Resultados Matematicas 2018",
       x = "Resultados MATH",
       y = "Densidad") +
  theme_minimal() +  
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
        axis.title = element_text(size = 12))  


ggplot(df_18, aes(x = Mean_MATH)) +
  geom_density(fill = "#89CFF0", alpha = 0.8) + 
  labs(title = "Distribución de Resultados Matematicas 2018",
       x = "Resultados MATH",
       y = "Densidad") +
  theme_minimal() +  # Tema minimalista
  theme(text = element_text(size = 12),  
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
        axis.title = element_text(size = 12))  # Títulos de ejes


ggplot(df_15, aes(x = Mean_MATH)) +
  geom_density(fill = "#B0BEC5", alpha = 0.8) +  # Color y transparencia del relleno
  labs(title = "Distribución de Resultados Matematicas 2015",
       x = "Resultados MATH",
       y = "Densidad") +
  theme_minimal() +  # Tema minimalista
  theme(text = element_text(size = 12),  # Tamaño de texto general
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Título
        axis.title = element_text(size = 12))  # Títulos de ejes


ggplot() +
  geom_density(data = df_22, aes(x = Mean_MATH, color = "2022"), size = 1, fill = NA, alpha = 0.5) +  # Densidad para df_22
  geom_density(data = df_18, aes(x = Mean_MATH, color = "2018"), size = 1, fill = NA, alpha = 0.5) +  # Densidad para df_18
  geom_density(data = df_15, aes(x = Mean_MATH, color = "2015"), size = 1, fill = NA, alpha = 0.5) +  # Densidad para df_15
  labs(title = "Mathematics Grade Distribution",
       x = "Math Grade",
       y = "Densidad",
       color = "Year") +  
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "right") +
  scale_color_manual(values = c("2022" = "#69b3a2", "2018" = "#89CFF0", "2015" = "#B0BEC5"))  # Asignando colores específicos


### graficos de cajas

ggplot() +
  geom_boxplot(data = df_22, aes(y = Mean_MATH, x = factor(1), fill = "df_22"),
               outlier.color = "red", outlier.shape = 1) +
  geom_boxplot(data = df_18, aes(y = Mean_MATH, x = factor(2), fill = "df_18"),
               outlier.color = "red", outlier.shape = 1) +
  geom_boxplot(data = df_15, aes(y = Mean_MATH, x = factor(3), fill = "df_15"),
               outlier.color = "red", outlier.shape = 1) +
  labs(title = "Comparación de Math Performance entre datasets",
       x = "Evolución",
       y = "Resultados MATH") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "right") +
  scale_x_discrete(labels = c("df_22", "df_18", "df_15")) +
  scale_fill_manual(values = c("2023" = "#69b3a2", "2018" = "#89CFF0", "2015" = "#B0BEC5"),
                    name = "Years")
#violin plots
ggplot() +
  geom_violin(data = df_22, aes(x = factor(1), y = Mean_MATH, fill = "df_22"), adjust = 1.5) +
  geom_violin(data = df_18, aes(x = factor(2), y = Mean_MATH, fill = "df_18"), adjust = 1.5) +
  geom_violin(data = df_15, aes(x = factor(3), y = Mean_MATH, fill = "df_15"), adjust = 1.5) +
  labs(title = "Violin Plots de Mean_MATH por Dataset", x = "Dataset", y = "Mean_MATH") +
  scale_fill_manual(values = c("df_22" = "#69b3a2", "df_18" = "#89CFF0", "df_15" = "#B0BEC5")) +
  scale_x_discrete(labels = c("df_22", "df_18", "df_15")) +
  theme_minimal()

# frecuencia acumulada

ggplot() +
  stat_ecdf(data = df_22, aes(x = Mean_MATH, color = "df_22")) +
  stat_ecdf(data = df_18, aes(x = Mean_MATH, color = "df_18")) +
  stat_ecdf(data = df_15, aes(x = Mean_MATH, color = "df_15")) +
  labs(title = "Frecuencia Acumulativa de Mean_MATH por Dataset", x = "Mean_MATH", y = "ECDF") +
  scale_color_manual(values = c("df_22" = "#69b3a2", "df_18" = "#89CFF0", "df_15" = "#B0BEC5")) +
  theme_minimal()

## 6.3  MAP ====================================================================

# Obtain Regions 
spain_regions <- gisco_get_nuts(country = "ES", year = "2021", resolution = "01", nuts_level = "2")

region_means <- df_18 %>%
  group_by(region_final) %>%
  summarise(mean_math = mean(Mean_MATH, na.rm = TRUE))
spain_regions <- spain_regions %>%
  left_join(region_means, by = c("NUTS_NAME" = "region_final"))


peninsula <- spain_regions %>% filter(!str_detect(NUTS_NAME, "Canarias"))
canarias <- spain_regions %>% filter(str_detect(NUTS_NAME, "Canarias"))


value_range <- c(460, 510)


p_main <- ggplot(data = peninsula) +
  geom_sf(aes(fill = mean_math), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "Mean Math Score",
                       limits = value_range,  
                       option = "C", direction = -1,
                       labels = scales::comma, na.value = "lightgrey") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 5), ylim = c(34, 45), expand = FALSE) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_blank(),        
        axis.text = element_blank(),         
        axis.ticks = element_blank(),      
        axis.line = element_blank()) +
  labs(title = "Mean_MATH Scores by Region in Spain\n(Math performance by NUTS 2 regions)")


# Map for Canarias 
p_canarias <-ggplot(data = canarias) +
  geom_sf(aes(fill = mean_math), color = "white", size = 0.5) +
  scale_fill_viridis_c(
    option = "inferno",  
    begin = 0.85,        
    end = 0.95,         
    limits = c(460, 510), 
    guide = FALSE        
  ) +
  theme_void() +
  coord_sf(expand = FALSE)


canarias_plot <- ggplotGrob(p_canarias)


p_final18 <- p_main +
  annotation_custom(grob = canarias_plot, xmin = -9.5, xmax = -5, ymin = 32, ymax = 38)

print(p_final18)

## MAPA 2022 

spain_regions <- gisco_get_nuts(country = "ES", year = "2021", resolution = "01", nuts_level = "2")
region_means_22 <- df_22 %>%
  group_by(region_final) %>%
  summarise(mean_math = mean(Mean_MATH, na.rm = TRUE))
spain_regions_22 <- spain_regions %>%
  left_join(region_means_22, by = c("NUTS_NAME" = "region_final"))


peninsula <- spain_regions_22 %>% filter(!str_detect(NUTS_NAME, "Canarias"))
canarias <- spain_regions_22 %>% filter(str_detect(NUTS_NAME, "Canarias"))


value_range <- c(460, 510)


p_main <- ggplot(data = peninsula) +
  geom_sf(aes(fill = mean_math), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "Mean Math Score",
                       limits = value_range,  
                       option = "C", direction = -1,
                       labels = scales::comma, na.value = "lightgrey") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 5), ylim = c(34, 45), expand = FALSE) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_blank(),        # Eliminar títulos de los ejes
        axis.text = element_blank(),         # Eliminar etiquetas de los ejes
        axis.ticks = element_blank(),        # Eliminar las marcas de los ejes
        axis.line = element_blank()) +
  labs(title = "Mean_MATH Scores by Region in Spain\n(Math performance by NUTS 2 regions)")



p_canarias <- ggplot(data = canarias) +
  geom_sf(aes(fill = mean_math), color = "white", size = 0.5) +
  scale_fill_viridis_c(
    option = "inferno",  
    begin = 0.85,        
    end = 0.95,          
    limits = c(450, 510), 
    guide = FALSE        
  ) +
  theme_void() +
  coord_sf(expand = FALSE)


canarias_plot <- ggplotGrob(p_canarias)


p_final_22 <- p_main +
  annotation_custom(grob = canarias_plot, xmin = -9.5, xmax = -5, ymin = 32, ymax = 38)


print(p_final_22)

## 6.4 FINAL MAP ============

# Agregar títulos individuales
p_final18 <- p_final18 + ggtitle("2018")
p_final_22 <- p_final_22 + ggtitle("2022")

combined_plot <- p_final18 + p_final_22 +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") #


combined_plot <- combined_plot + plot_annotation(
  title = "Comparative Math Scores by Region in Spain for 2018 and 2022",
  theme = theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    legend.position = "bottom" 
  )
)
print(combined_plot)


## 6.5 Conditionals ===============================================================

# Gender

common_ylim <- range(c(df_15$Mean_MATH, df_18$Mean_MATH, df_22$Mean_MATH), na.rm = TRUE)

p_18 <- ggplot(df_18, aes(x = as.factor(Female), y = Mean_MATH, fill = as.factor(Female))) +
  geom_boxplot() +
  labs(title = "2018",
       x = "",
       y = "") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3"), labels = c("Men", "Female"), name = "Gender") +
  ylim(common_ylim) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

p_15 <- ggplot(df_15, aes(x = as.factor(Female), y = Mean_MATH, fill = as.factor(Female))) +
  geom_boxplot() +
  labs(title = "2015",
       x = "",
       y = "Average Grade in Math") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3"), labels = c("Men", "Female"), name = "Gender") +
  ylim(common_ylim) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

p_22 <- ggplot(df_22, aes(x = as.factor(Female), y = Mean_MATH, fill = as.factor(Female))) +
  geom_boxplot() +
  labs(title = "2022",
       x = "",
       y = "") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3"), labels = c("Men", "Female"), name = "Gender") +
  ylim(common_ylim) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

#COMBINATION
combined_plot <- (p_15 + p_18 + p_22) +
  plot_layout(guides = "collect") +  # Colectar y compartir leyenda
  plot_annotation(title = "Differences in the average score according to gender",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

print(combined_plot)

##  Gini INDEX

gini_18 <- Gini(df_18$Mean_MATH, na.rm = TRUE)
gini_15 <- Gini(df_15$Mean_MATH, na.rm = TRUE)
gini_22 <- Gini(df_22$Mean_MATH, na.rm = TRUE)

gini_data <- data.frame(
  Year = c("2018", "2015", "2022"),
  Gini_Index = c(gini_18, gini_15, gini_22)
)

ggplot(gini_data, aes(x = Year, y = Gini_Index, group = 1)) +
  geom_line(aes(color = "Gini Index"), size = 1.5) +  
  geom_point(aes(color = "Gini Index"), size = 4, shape = 21, fill = "white") +  
  scale_color_manual(values = c("Gini Index" = "steelblue"), guide = "none") +  
  labs(title = "Evolution of the Gini Index between 2015, 2018 and 2022",
       x = "Year",
       y = "Gini Index") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.position = "none"  
  )

## Native 

common_ylim <- range(c(df_15_clean$Mean_MATH, df_18_clean$Mean_MATH, df_22_clean$Mean_MATH), na.rm = TRUE)

p_18 <- ggplot(df_18_clean, aes(x = as.factor(Native), y = Mean_MATH, fill = as.factor(Native))) +
  geom_boxplot() +
  labs(title = "2018",
       x = "",
       y = "") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3"), labels = c("Foreigner", "Native"), name = "Origin") +
  ylim(common_ylim) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

p_15 <- ggplot(df_15_clean, aes(x = as.factor(Native), y = Mean_MATH, fill = as.factor(Native))) +
  geom_boxplot() +
  labs(title = "2015",
       x = "",
       y = "Average Grade in Math") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3"), labels = c("Foreigner", "Native"), name = "Origin") +
  ylim(common_ylim) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))

p_22 <- ggplot(df_22_clean, aes(x = as.factor(Native), y = Mean_MATH, fill = as.factor(Native))) +
  geom_boxplot() +
  labs(title = "2022",
       x = "",
       y = "") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3"), labels = c("Foreigner", "Native"), name = "Origin") +
  ylim(common_ylim) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12))


combined_plot <- (p_15 + p_18 + p_22) +
  plot_layout(guides = "collect") +  
  plot_annotation(title = "Differences in the average score according to their origin",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))


print(combined_plot)


## 6.6 COVID-19 ===================================================================

df_22_clean_covid <- na.omit(df_22)
df_22_clean_covid <- df_22_clean_covid %>%
  mutate(Covid_homework = factor(Covid_homework),  Covid_Zoom = factor(Covid_Zoom),
         Covid_learned_comparison = factor(Covid_learned_comparison))

ggplot(df_22_clean_covid, aes(x = Covid_homework, y = Mean_MATH, fill = Covid_homework)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFC0CB", "#6A5ACD")) +
  labs(title = "Distribution of grades in mathematics as a function of recieving homework during COVID-19",
       x = "Received homework during COVID-19",
       y = "Average grade in mathematics") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.position = "none"  
  )


## Covid and Gender

boxplot_gender <- ggplot(df_22_clean_covid, aes(x = Covid_homework, y = Mean_MATH, fill = as.factor(Covid_homework))) +
  geom_boxplot(outlier.color = "black",  width = 0.7) +  # Ajustar los outliers para que sean más visibles
  facet_wrap(~ Female, scales = "fixed", labeller = labeller(Female = c("0" = "Male", "1" = "Female"))) +
  scale_fill_manual(
    name = "Covid Homework",
    labels = c("Did not receive homework", "Received homework"),
    values = c("#FFC0CB", "#6A5ACD")  
  ) +
  scale_x_discrete(labels = c("0", "1")) +  
  labs(title = "Distribution of grades in mathematics according to whether homework was received during COVID and gender",
       x = "Received homework during the Covid",
       y = "Average grade in mathematics") +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.25),  
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    strip.text.x = element_text(size = 12, face = "bold") 
  )


# Mostrar el gráfico
print(boxplot_gender)


## COVID y Native

ggplot(df_22_clean_covid, aes(x = Covid_homework, y = Mean_MATH, fill = as.factor(Covid_homework))) +
  geom_boxplot(outlier.color = "black", width = 0.7) +  
  facet_wrap(~ Native, scales = "fixed", labeller = labeller(Native = c("0" = "Foreigner", "1" = "Native"))) +
  scale_fill_manual(
    name = "Covid Homework",
    labels = c("Did not receive homework", "Received homework"),
    values = c("#FFC0CB", "#6A5ACD")  
  ) +
  labs(title = "Distribution of grades in mathematics according to whether homework was received during COVID",
       x = "Received homework during the Covid",
       y = "Average grade in mathematics") +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.25),  
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    strip.text.x = element_text(size = 12, face = "bold")  
  )



# 7. Lasso Regression =======================================================================

#2015
set.seed(123)
in_train <- createDataPartition(df_15_clean$Mean_MATH, p = 0.8, list = FALSE)
# Verificar si in_train es una matriz y convertir a vector si es necesario
if (is.matrix(in_train)) {
  in_train <- in_train[, 1]
}

training <- df_15_clean[ in_train,]
testing <- df_15_clean[-in_train,]
nrow(training)

modellm<-lm(Mean_MATH ~ .,data=df_15_clean)


X<-df_15_clean |>  dplyr::select(-Mean_MATH)
Y<-df_15_clean |> dplyr::select(Mean_MATH)
grid = 10^seq(10, -2, length = 100)


# LASSO REGRESSION
model2<-cv.glmnet(x=as.matrix(X), y=Y[,1], lambda=grid, alpha=1)

# Example to extract predictions

predict(model2, newx=as.matrix(X)[1:10,], s="lambda.min")
# Example to extract coefficients
as.data.frame(as.matrix(predict(model2, type="coefficients", s="lambda.min"))) %>%
  mutate(across(c(lambda.min), round, 2))

df_15_final <- df_15_clean |> 
  select( - c(number_boys,number_girls,number_teachers,number_computers, Student_teacher_ratio, M_ESO,M_Grado_medio_bachillerato,M_Educacion_avanzada, F_ESO, Ceuta, Melilla,X101.200.books, M_None,F_Educacion_avanzada, F_None, X101.200.books  ))


#2018


in_train <- createDataPartition(df_18_clean$Mean_MATH, p = 0.8, list = FALSE)
# Verificar si in_train es una matriz y convertir a vector si es necesario
if (is.matrix(in_train)) {
  in_train <- in_train[, 1]
}
training <- df_18_clean[ in_train,]
testing <- df_18_clean[-in_train,]
nrow(training)

modellm<-lm(Mean_MATH ~ .,data=df_18_clean)

X<-df_18_clean |>  dplyr::select(-Mean_MATH)
Y<-df_18_clean |> dplyr::select(Mean_MATH)
grid = 10^seq(10, -2, length = 100)


# LASSO REGRESSION
model2<-cv.glmnet(x=as.matrix(X), y=Y[,1], lambda=grid, alpha=1)

# Example to extract predictions

predict(model2, newx=as.matrix(X)[1:10,], s="lambda.min")
# Example to extract coefficients
as.data.frame(as.matrix(predict(model2, type="coefficients", s="lambda.min"))) %>%
  mutate(across(c(lambda.min), round, 2))

df_18_final <- df_18_clean |> 
  select(  - c(number_boys,number_girls,number_teachers,number_computers, Student_teacher_ratio, M_ESO,M_Grado_medio_bachillerato,M_Educacion_avanzada, F_ESO, Ceuta, Melilla,X101.200.books, M_None,F_Educacion_avanzada, F_None, X101.200.books  ))


# 2022

in_train <- createDataPartition(df_22_clean$Mean_MATH, p = 0.8, list = FALSE)
# Verificar si in_train es una matriz y convertir a vector si es necesario
if (is.matrix(in_train)) {
  in_train <- in_train[, 1]
}
training <- df_22_clean[ in_train,]
testing <- df_22_clean[-in_train,]
nrow(training)

modellm<-lm(Mean_MATH ~ .,data=df_22_clean)

df_22_clean <- na.omit(df_22_clean)
X<-df_22_clean |>  dplyr::select(-Mean_MATH)
Y<-df_22_clean |> dplyr::select(Mean_MATH)
grid = 10^seq(10, -2, length = 100)


# LASSO REGRESSION
model2<-cv.glmnet(x=as.matrix(X), y=Y[,1], lambda=grid, alpha=1)

# Example to extract predictions

predict(model2, newx=as.matrix(X)[1:6,], s="lambda.min")
# Example to extract coefficients
as.data.frame(as.matrix(predict(model2, type="coefficients", s="lambda.min"))) %>%
  mutate(across(c(lambda.min), round, 2))

df_22_final <- df_22_clean |> 
  select(- c(number_boys,number_girls,number_teachers,number_computers, Student_teacher_ratio, M_ESO,M_Grado_medio_bachillerato,M_Educacion_avanzada, F_ESO, Ceuta, Melilla, M_None,F_Educacion_avanzada, F_None, X101.200.books  ))

setdiff(names(df_15_final), names(df_18_final))
setdiff(names(df_22_final), names(df_18_final))


# 8. OLS ======================================================================

modelobase_15 <- lm(Mean_MATH ~.-id_school- Comunidad.Valenciana, data = df_15_final)
modelobase_18 <- lm(Mean_MATH ~.-id_school , data = df_18_final)
modelobase_22 <- lm(Mean_MATH ~.-id_school , data = df_22_final)

summary(modelobase_15)
summary(modelobase_18)
summary(modelobase_22)

## 8.1 Multicolinearity ====================================
vif(modelobase_15)
vif(modelobase_18)
vif(modelobase_22)

## 8.2 Heterocedasticity ===================================

#modelo15
bptest(modelobase_15)
ols_test_score(modelobase_15)

#modelo18
bptest(modelobase_18)
ols_test_score(modelobase_18)

#modelo22
bptest(modelobase_22)
ols_test_score(modelobase_22)

## 8.3 Resiuduals ========================================================

residuos <- residuals(modelobase_15)
valores_ajustados <- fitted(modelobase_15)
plot(valores_ajustados, residuos, 
     xlab = "Adjusted Values", 
     ylab = "Residuals", 
     main = "Graph of Residuals vs Adjusted Values in 2015")

abline(h = 0, col = "red") # Eje 0
lines(lowess(valores_ajustados, residuos), col = "blue") 

##2018

residuos <- residuals(modelobase_18)
valores_ajustados <- fitted(modelobase_18)
plot(valores_ajustados, residuos, 
     xlab = "Adjusted Values", 
     ylab = "Residuals", 
     main = "Graph of Residuals vs Adjusted Values in 2018")

abline(h = 0, col = "red") 
lines(lowess(valores_ajustados, residuos), col = "blue") 

## 2022

residuos <- residuals(modelobase_22)
valores_ajustados <- fitted(modelobase_22)
plot(valores_ajustados, residuos, 
     xlab = "Adjusted Values", 
     ylab = "Residuals", 
     main = "Graph of Residuals vs Adjusted Values in 2022")

abline(h = 0, col = "red") 
lines(lowess(valores_ajustados, residuos), col = "blue") 



## 8.4 Clusters Models ====================

modelo15 <- miceadds::lm.cluster(Mean_MATH ~. , data = df_15_final, cluster = df_15_final$id_school)
modelo18 <- miceadds::lm.cluster(Mean_MATH ~. , data = df_18_final, cluster = df_18_final$id_school)
modelo22 <- miceadds::lm.cluster(Mean_MATH ~. , data = df_22_final, cluster = df_22_final$id_school)

summary(modelo15)
summary(modelo18)
summary(modelo22)

cluster_vcov_15 <- vcovCL(modelobase_15, cluster = ~ id_school)
coeftest(modelo15, cluster_vcov_15)

cluster_vcov_18 <- vcovCL(modelobase_18, cluster = ~ id_school)
coeftest(modelo18, cluster_vcov_18)

cluster_vcov_22 <- vcovCL(modelobase_22, cluster = ~ id_school)
coeftest(modelo22, cluster_vcov_22)


## 8.5 Significant Differences ===================================================

df_15_final$Year <- 2015
df_18_final$Year <- 2018
df_22_final$Year <- 2022

#2015 y 2018
df_combined <- rbind(df_15_final, df_18_final)
df_combined <- df_combined |> 
  mutate(interaction_factor= ifelse(df_combined$Year == 2018, 1, 0) ) |> 
  select(-Year)

modelo_interaccion <- lm(Mean_MATH ~ interaction_factor * .-interaction_factor-id_school - Comunidad.Valenciana , data = df_combined)
summary(modelo_interaccion)

cluster_vcov_comp <- vcovCL(modelo_interaccion, cluster = ~ id_school)
coeftest(modelo_interaccion, cluster_vcov_comp)

# 2022 y 2018

# df_18_final <- df_18_final |> 
#   select(-M_Educacion_avanzada)

setdiff(names(df_22_final), names(df_18_final))
df_combined <- rbind(df_18_final, df_22_final)

df_combined <- df_combined |> 
  mutate(interaction_factor= ifelse(df_combined$Year == 2022, 1, 0) ) |> 
  select(-Year)

modelo_interaccion <- lm(Mean_MATH ~ interaction_factor * .-interaction_factor, data = df_combined)
summary(modelo_interaccion)

cluster_vcov_comp <- vcovCL(modelo_interaccion, cluster = ~ id_school)
coeftest(modelo_interaccion, cluster_vcov_comp)


# 9. MACHINE LEARNING =============================================================

df_final <- df_combined %>%
  dplyr::select(-where(~ all(. == .[1] | is.na(.)) || var(., na.rm = TRUE) == 0)) |> 
  dplyr::select( -id_school, -ESCS)

in_train <- createDataPartition(df_final$Mean_MATH, p = 0.75, list = FALSE)  
training <- df_final[ in_train,]
testing <- df_final[-in_train,]
nrow(training)
nrow(testing)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, repeats = 1)
test_results <- data.frame(Mean_Math = testing$Mean_MATH)

ModelFF = Mean_MATH ~ . -interaction_factor


rf_MATH <- train(ModelFF, 
                    data = training,
                    method = "rf",
                    preProc=c('scale','center'),
                    trControl = ctrl,
                    ntree = 100,
                    tuneGrid = data.frame(mtry=c(1,3,5,7)),
                    importance = TRUE)

plot(rf_MATH)

test_results$rf <- predict(rf_MATH, testing)

postResample(pred = test_results$rf,  obs = test_results$Mean_Math)


plot(varImp(rf_MATH, scale = F), scales = list(y = list(cex = .80)))

importancia <- varImp(rf_MATH, scale = F)
importancia_df <- importancia$importance
top_indices <- order(importancia_df$Overall, decreasing = TRUE)[1:20]
top20_importancia <- importancia_df[top_indices, , drop = FALSE]  


rownames(top20_importancia) <- rownames(importancia_df)[top_indices]

ggplot(top20_importancia, aes(x = reorder(row.names(top20_importancia), Overall))) +
  geom_segment(aes(xend = reorder(row.names(top20_importancia), -Overall), y = 0, yend = Overall), color = "#4682B4", size = 1.5) +  # Líneas en color azul más sólido
  geom_point(aes(y = Overall), color = "#FF6347", size = 3) +  
  coord_flip() +  
  theme_minimal() +  
  theme(
    text = element_text(size = 12),  
    axis.title.y = element_text(color = "#333333"),  
    axis.title.x = element_text(color = "#333333"),  
    plot.title = element_text(hjust = 0.5)  
  ) +
  xlab("Variables") +  
  ylab("Importance") +  
  ggtitle("Top 20 Most Important Variables for Average Math Score") 


## 9.1 Machine Learnign Plots ============

# Generate partial dependence for multiple variables
partial_repeat <- partial(rf_MATH, pred.var = "Repeat", plot = FALSE, rug = TRUE)
partial_HOMEPOS <- partial(rf_MATH, pred.var = "HOMEPOS", plot = FALSE, rug = TRUE)
partial_female <- partial(rf_MATH, pred.var = "Female", plot = FALSE, rug = TRUE)

plot_repeat <- autoplot(partial_repeat) +
  geom_line(aes(y = yhat)) +
  theme_minimal() +
  labs(title = "How does the Repeat variable affect the Math Score?",
       y = "Math Score")

plot_HOMEPOS <- autoplot(partial_HOMEPOS) +
  geom_line(aes(y = yhat)) +
  theme_minimal() +
  labs(title = "How does the HOMEPOS variable affect the Math Score?",
       y = "Math Score")

plot_female <- autoplot(partial_female) +
  geom_line(aes(y = yhat)) +
  theme_minimal() +
  labs(title = "How does the Female variable affect the Math Score?",
       y = "Math Score")


grid.arrange(plot_repeat, plot_HOMEPOS, plot_female, nrow = 3)


# Generate partial dependence for multiple variables

partial_Native <- partial(rf_MATH, pred.var = "Native", plot = FALSE, rug = TRUE)
partial_public <- partial(rf_MATH, pred.var = "public", plot = FALSE, rug = TRUE)


plot_Native <- autoplot(partial_Native) +
  geom_line(aes(y = yhat)) +
  theme_minimal() +
  labs(title = "How does the Native variable affect the Math Score?",
       y = "Math Score")

plot_public <- autoplot(partial_public) +
  geom_line(aes(y = yhat)) +
  theme_minimal() +
  labs(title = "How does the Public variable affect the Math Score?",
       y = "Math Score")

# Combinar los gráficos en un solo gráfico
grid.arrange( plot_Native, plot_public, nrow = 2)
