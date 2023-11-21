#Problem Set 2
#Big Data y Machine Learning para Economía Aplicada

# Merit Tejeda: 202210104
# Celin Hernández: 202210067

install.packages("readxl")
install.packages("readr")
install.packages("pacman")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("writexl")
install.packages("stargazer")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("rvest")
install.packages("glmnet")
install.packages("caret")
install.packages(c("psych", "summarytools"))
remotes::install_github("r-lib/remotes")
library(ggplot2)
library(openxlsx)
library(pacman)
library(readxl)
library(dplyr)
library(writexl)
library(stargazer)
library(gridExtra)
library(tidyverse)
library(rvest)
library(readr)
library(glmnet)
library(caret)
library(psych)
library(summarytools)


# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(rio, # Import data easily
       plotly, # Gráficos 
       tmaptools, # geocode_OSM()
       tidymodels) #para modelos de ML


sample_submission <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/sample_submission.xlsx")
train_personas1 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_1.xlsx")
train_personas2 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_2.xlsx")
train_personas3 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_3.xlsx")
train_personas4 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/train_personas_4.xlsx")
train_hogares <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/train_hogares.xlsx")
test_personas1 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_personas_1.xlsx")
test_personas2 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_personas_2.xlsx")
test_hogares <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_hogares.xlsx")


# -----------------------UNIÓN DE DATOS------------------------------------# 


train_personas <- rbind(train_personas1, train_personas2,train_personas3,train_personas4)
test_personas <- rbind(test_personas1, test_personas2)

train_personas %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

train_personas <- train_personas %>%
  mutate(Des = ifelse(Oc == 1 & is.na(Des), 0, Des))
train_personas$Oc <- ifelse(train_personas$Des == 1 & is.na(train_personas$Oc), 0, train_personas$Oc)

# Renombrar las variables para una mayor comprensión de las variables que estamos trabajando
train_personas <- train_personas %>% rename(Sexo=P6020) # 1:Hombre y 2: Mujer
train_personas <- train_personas %>% rename(Edad=P6040) # ¿Cuantos años tiene cumplido?
train_personas <- train_personas %>% rename(Parent_jh=P6050) # Parentesco con el jefe de hogar
train_personas <- train_personas %>% rename(SS=P6090) # 1:Si, 2: No y 9: No sabe
train_personas <- train_personas %>% rename(R_SS=P6100) # 1:Si, 2: No y 9: No sabe
train_personas <- train_personas %>% rename(Educ=P6210) 
train_personas <- train_personas %>% rename(G_Educ=P6210s1)
train_personas <- train_personas %>% rename(Act_Ocup=P6240)
train_personas <- train_personas %>% rename(Exp_Emp=P6426)
train_personas <- train_personas %>% rename(Cat_Ocup=P6430)
train_personas <- train_personas %>% rename(Ing_MesP=P6500)
train_personas <- train_personas %>% rename(Ing_Extras=P6510)
train_personas <- train_personas %>% rename(Hrs_Extras=P6510s1)
train_personas <- train_personas %>% rename(Primas=P6545)
train_personas <- train_personas %>% rename(Ing_Primas=P6545s1)
train_personas <- train_personas %>% rename(Bonif=P6580)
train_personas <- train_personas %>% rename(Ing_Bonif=P6580s1)
train_personas <- train_personas %>% rename(Sub_Alim=P6585s1)
train_personas <- train_personas %>% rename(Ing_Sub_Alim=P6585s1a1)
train_personas <- train_personas %>% rename(Sub_Transp=P6585s2)
train_personas <- train_personas %>% rename(Ing_Sub_Transp=P6585s2a1)
train_personas <- train_personas %>% rename(Sub_Fam=P6585s3)
train_personas <- train_personas %>% rename(Ing_Sub_Fam=P6585s3a1)
train_personas <- train_personas %>% rename(Sub_Educ=P6585s4)
train_personas <- train_personas %>% rename(Ing_Sub_Educ=P6585s4a1)
train_personas <- train_personas %>% rename(Pag_Alim=P6590)
train_personas <- train_personas %>% rename(Ing_Pag_Alim=P6590s1)
train_personas <- train_personas %>% rename(Pag_Viv=P6600)
train_personas <- train_personas %>% rename(Ing_Pag_Viv=P6600s1)
train_personas <- train_personas %>% rename(Pag_Transp=P6610)
train_personas <- train_personas %>% rename(Ing_Pag_Transp=P6610s1)
train_personas <- train_personas %>% rename(Otros_Ing=P6620)
train_personas <- train_personas %>% rename(Vl_Otros_Ing=P6620s1)
train_personas <- train_personas %>% rename(Prima_Serv=P6630s1)
train_personas <- train_personas %>% rename(Ing_Prima_Serv=P6630s1a1)
train_personas <- train_personas %>% rename(Prima_Nav=P6630s2)
train_personas <- train_personas %>% rename(Ing_Prima_Nav=P6630s2a1)
train_personas <- train_personas %>% rename(Prima_Vac=P6630s3)
train_personas <- train_personas %>% rename(Ing_Prima_Vac=P6630s3a1)
train_personas <- train_personas %>% rename(Viats=P6630s4)
train_personas <- train_personas %>% rename(Ing_Viats=P6630s4a1)
train_personas <- train_personas %>% rename(Bons_Anuales=P6630s6)
train_personas <- train_personas %>% rename(Ing_Bons_Anuales=P6630s6a1)
train_personas <- train_personas %>% rename(Gan_Neta=P6750)
train_personas <- train_personas %>% rename(Gan_Neta_Meses=P6760)
train_personas <- train_personas %>% rename(Gan_Neta_Neg=P550)
train_personas <- train_personas %>% rename(Hrs_Trab=P6800)
train_personas <- train_personas %>% rename(Num_Trab=P6870)
train_personas <- train_personas %>% rename(Fond_Pens=P6920)
train_personas <- train_personas %>% rename(Act_Sec=P7040)
train_personas <- train_personas %>% rename(Hrs_Act_Sec=P7045)
train_personas <- train_personas %>% rename(Ocup_Act_Sec=P7050)
train_personas <- train_personas %>% rename(Ing_Act_Sec=P7070)
train_personas <- train_personas %>% rename(Qtrab_Hrs=P7090)
train_personas <- train_personas %>% rename(DC_trab=P7140s1)
train_personas <- train_personas %>% rename(Mot_DC_trab=P7140s2)
train_personas <- train_personas %>% rename(Rec_Rem=P7510s2)
train_personas <- train_personas %>% rename(Ing_Rec_Rem=P7510s2a1)
train_personas <- train_personas %>% rename(Ayuda_Gub=P7510s3)
train_personas <- train_personas %>% rename(Ing_Ayuda_Gub=P7510s3a1)
train_personas <- train_personas %>% rename(Intrs=P7510s5)
train_personas <- train_personas %>% rename(Ing_Intrs=P7510s5a1)
train_personas <- train_personas %>% rename(Cesant=P7510s6)
train_personas <- train_personas %>% rename(Ing_Cesant=P7510s6a1)
train_personas <- train_personas %>% rename(Otrs_Ftes=P7510s7)
train_personas <- train_personas %>% rename(Ing_Otrs_Ftes=P7510s7a1)
train_personas <- train_personas %>% rename(Ocupac=Oc)
train_personas <- train_personas %>% rename(Ing_Total_Obs=Ingtotob)
train_personas <- train_personas %>% rename(Ing_Total=Ingtot)


# Renombrar las variables de train_hogares

train_hogares <- train_hogares %>% rename(Habit_por_Hogar=P5000)
train_hogares <- train_hogares %>% rename(Dormit=P5010)
train_hogares <- train_hogares %>% rename(Prop_Vivienda=P5090)
train_hogares <- train_hogares %>% rename(Pag_Arriendo_Est=P5130)
train_hogares <- train_hogares %>% rename(Pag_Arriendo=P5140)
train_hogares <- train_hogares %>% rename(Per_por_Hogar=Nper)
train_hogares <- train_hogares %>% rename(Per_Uni_Gasto=Npersug)
train_hogares <- train_hogares %>% rename(Linea_Indigencia=Li)
train_hogares <- train_hogares %>% rename(Linea_Pobreza=Lp)
train_hogares <- train_hogares %>% rename(Ing_perc_ug=Ingpcug)



# Analisis descriptivo 

train_personas %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

train_personas <- train_personas %>%
  mutate(
    edadjefe = ifelse(Parent_jh == 1, Edad, NA_real_),
    edadconyugue = ifelse(Parent_jh == 2, Edad, NA_real_),
    edadhijos = ifelse(Parent_jh == 3, Edad, NA_real_),
    edadnietos = ifelse(Parent_jh == 4, Edad, NA_real_),
    sexojefe = ifelse(Parent_jh == 1, Sexo, NA_character_),
    sexoconyugue = ifelse(Parent_jh == 2, Sexo, NA_character_),
    Educjefe = ifelse(Parent_jh == 1, Educ, NA_character_),
    Educjefe1 = ifelse(Parent_jh == 1, G_Educ, NA_real_),
    Educconyugue = ifelse(Parent_jh == 2, Educ, NA_real_),
    Educhijos = ifelse(Parent_jh == 3, Educ, NA_real_),
    SS_Jefe = ifelse(Parent_jh == 1, SS, NA_character_),
    SS_Conyugue = ifelse(Parent_jh == 2, SS, NA_character_),
    tiempotrabajojefe_meses = ifelse(Parent_jh == 1, Exp_Emp, NA_real_),
    tiempotrabajoconyugue = ifelse(Parent_jh == 2, Exp_Emp, NA_real_),
    posicionocupacionjefe = ifelse(Parent_jh == 1, Cat_Ocup, NA_character_),
    posicionocupacionconyugue = ifelse(Parent_jh == 2, Cat_Ocup, NA_character_),
    categocupjefe = ifelse(Parent_jh == 1, Act_Ocup, NA_character_),
    categocupconyugue = ifelse(Parent_jh == 2, Act_Ocup, NA_character_),
    categocuphijos = ifelse(Parent_jh == 3, Act_Ocup, NA_character_),
    horastrabajadasjefe = ifelse(Parent_jh == 1, Hrs_Trab, NA_real_),
    horastrabajadasconyugue = ifelse(Parent_jh == 2, Hrs_Trab, NA_real_),
    especiejefe = ifelse(Parent_jh == 1, Otros_Ing, NA_character_),
    especieconyugue = ifelse(Parent_jh == 2, Otros_Ing, NA_character_),
    estratojefe = ifelse(Parent_jh == 2, Estrato1, NA_real_),
    otronegociojefe = ifelse(Parent_jh == 1, Act_Sec, NA_real_),
    otronegocioconyugue = ifelse(Parent_jh == 2 ,Act_Sec , NA_real_),
    otrashorasjefe = ifelse(Parent_jh == 1 ,Hrs_Act_Sec , NA_real_),
    otrashorasconyugue = ifelse(Parent_jh == 2 ,Hrs_Act_Sec , NA_real_),
    Jefe_Hogar_Mujer = ifelse(Parent_jh == 1 & Sexo == 2, 1, 0))


train_personas <- train_personas %>%
  group_by(id) %>%
  mutate(
    edadhijos = ifelse(Parent_jh == 3, Edad, NA_real_),
    total_ocupados = sum(Ocupac == 1, na.rm = TRUE),
    total_desocupados = sum(Des == 1, na.rm = TRUE),
    total_inactivos = sum(Ina == 1, na.rm = TRUE),
    htrabaocupados = sum(Hrs_Trab, na.rm = TRUE),
    niños6 = sum(edadhijos < 6, na.rm = TRUE),
    niños6a12 = sum(edadhijos >= 6 & edadhijos < 12, na.rm = TRUE),
    niños12a18 = sum(edadhijos >= 12 & edadhijos < 18, na.rm = TRUE),
    niños18 = sum(edadhijos < 18, na.rm = TRUE),
    edad_promediohijos = mean(edadhijos ,na.rm = TRUE) ,
    anos_educ_promedio_hijos = mean(Educhijos, na.rm = TRUE),
    Ingreso_Total_Por_Hogar = sum(Ing_Total, na.rm = TRUE),
    Subsidio = ifelse(Sub_Alim == 1 | Sub_Transp == 1 | Sub_Educ == 1 | Sub_Fam == 1, 1, 0),
    Subsidio_Familia = ifelse(any(Subsidio == 1), 1, 0),
    htrabaocupados_prop = htrabaocupados / total_ocupados)



variables_Jefe <- train_personas %>% 
  filter(Parent_jh == 1) %>%  # Filtra solo el jefe de hogar
  select(id, edadjefe, sexojefe, Educjefe, Educjefe1, SS_Jefe,categocupjefe,tiempotrabajojefe_meses,
         posicionocupacionjefe,horastrabajadasjefe,especiejefe ,Estrato1,otronegociojefe,
          otrashorasjefe,total_ocupados,total_desocupados,total_inactivos,htrabaocupados,niños6,niños6a12,niños12a18,niños18,
         Ingreso_Total_Por_Hogar, htrabaocupados_prop,Jefe_Hogar_Mujer, anos_educ_promedio_hijos,edad_promediohijos,Subsidio , Subsidio_Familia )
train_hogares <- left_join(train_hogares, variables_Jefe, by = "id")

variables_conyugue <- train_personas %>% 
  filter(Parent_jh == 2) %>%  # Filtra solo el jefe de hogar
  select(id,edadconyugue,sexoconyugue,Educconyugue,SS_Conyugue,categocupconyugue,tiempotrabajoconyugue
         ,posicionocupacionconyugue,horastrabajadasconyugue,especieconyugue,
         otronegocioconyugue,otrashorasconyugue )

variables_conyugue<- distinct(variables_conyugue, id, .keep_all = TRUE)

train_hogares <- left_join(train_hogares, variables_conyugue, by = "id")

#### Corregir las Variables para la base de datos train_hogares


train_hogares <- train_hogares %>% mutate(sexojefe= case_when(sexojefe==1 ~"Male",
                                   sexojefe==2 ~"Female"),
                                   Educjefe= case_when(Educjefe==1 ~"Ninguno",
                                                       Educjefe==2 ~"Preescolar",
                                                       Educjefe==3 ~"Educación básica en el ciclo de primaria",
                                                       Educjefe==4 ~"Educación básica en el ciclo de secundaria",
                                                       Educjefe==5 ~"Educación media",
                                                       Educjefe==6 ~"Superior o universitaria",
                                                       Educjefe==9 ~"No sabe"),
                                  SS_Jefe= case_when(SS_Jefe==1 ~"Cotiza a un Seguro",
                                                     SS_Jefe==2 ~"No Cotiza",
                                                     SS_Jefe==9 ~"Otro"),
                                  categocupjefe= case_when(categocupjefe==1 ~"Trabajando",
                                                           categocupjefe==2 ~"Buscando trabajo",
                                                           categocupjefe==3 ~"Estudiando",
                                                           categocupjefe==4 ~"Oficios del hogar",
                                                           categocupjefe==5 ~"Incapacitado permanente para trabajar f",
                                                           categocupjefe==6 ~"Otra"),
                                  posicionocupacionjefe= case_when(posicionocupacionjefe==1 ~"Obrero",
                                                                   posicionocupacionjefe==2 ~"empleado del gobierno",
                                                                   posicionocupacionjefe==3 ~"Empleado doméstico",
                                                                   posicionocupacionjefe==4 ~"Trabajador por cuenta propia",
                                                                   posicionocupacionjefe==5 ~"Patrón o empleador",
                                                                   posicionocupacionjefe==6 ~"Trabajador familiar sin remuneración",
                                                                   posicionocupacionjefe==7 ~"Trabajador sinremuneración en empresas o negocios de otros hogares",
                                                                   posicionocupacionjefe==8 ~ "Jornalero o peón",
                                                                   posicionocupacionjefe==9 ~ "Otro"),
                                especiejefe= case_when( especiejefe==1 ~"si",                     
                                                        especiejefe==2~"no",
                                                        especiejefe==3~"No Sabe"),
                                Subsidio_Familia= case_when( Subsidio_Familia==1 ~"si",                     
                                                             Subsidio_Familia==0~"no"),
                                sexoconyugue = case_when (sexoconyugue==1 ~"Male",
                                                          sexoconyugue==2 ~"Female"),
                                SS_Conyugue= case_when(SS_Conyugue==1 ~"Cotiza a un Seguro",
                                                       SS_Conyugue==2 ~"No Cotiza",
                                                       SS_Conyugue==9 ~"Otro"),
                                categocupconyugue= case_when( categocupconyugue==1 ~"Trabajando",
                                                              categocupconyugue==2 ~"Buscando trabajo",
                                                              categocupconyugue==3 ~"Estudiando",
                                                              categocupconyugue==4 ~"Oficios del hogar",
                                                              categocupconyugue==5 ~"Incapacitado permanente para trabajar f",
                                                              categocupconyugue==6 ~"Otra"),   
                                otronegociojefe= case_when( otronegociojefe==1 ~"si",
                                                            otronegociojefe==2 ~"no"),
                                otronegocioconyugue= case_when( otronegocioconyugue==1 ~"si",
                                                                otronegocioconyugue==2 ~"no"),
                               especieconyugue= case_when( especieconyugue==1 ~"si",                     
                                                           especieconyugue==2~"no",
                                                          especieconyugue==3~"No Sabe"),
                               Pobre= case_when(Pobre==1 ~"si",                     
                                                Pobre==0~"no"),
                          Indigente= case_when(Indigente==1 ~"si",                     
                                                    Indigente==0~"no"))
                    
# Definimos las variables categóricas
variables_categoricas <- c("sexojefe",
                           "Educjefe",
                           "SS_Jefe",
                           "categocupjefe",
                           "posicionocupacionjefe",
                           "especiejefe",
                           "Subsidio_Familia",
                           "sexoconyugue",
                           "SS_Conyugue",
                           "categocupconyugue",
                           "otronegociojefe",
                           "otronegocioconyugue",
                           "especieconyugue",
                           "Pobre",
                           "Indigente")
                
train_hogares <- train_hogares %>% mutate_at(variables_categoricas, as.factor)


# Convierte a variable binaria
train_hogares$Pobre <- ifelse(train_hogares$Pobre == "si", 1, 0)
train_hogares$sexojefe <- ifelse(train_hogares$sexojefe == "Male", 1, 0)
train_hogares <- train_hogares %>%
  mutate(Arriendo = ifelse(is.na(Pag_Arriendo_Est), Pag_Arriendo, Pag_Arriendo_Est))


##--------------------------- Bases de Datos Variables Seleccionadas------------------##


train_hogares1 <- train_hogares[ c("id","Dominio", "Habit_por_Hogar", "Dormit", "Arriendo","Estrato1", "Ing_perc_ug", "Ingreso_Total_Por_Hogar", 
                                                   "Linea_Indigencia", "Linea_Pobreza", "Pobre", "edadjefe","sexojefe","Educjefe", "Educjefe1", 
                                                   "categocupjefe", "tiempotrabajojefe_meses", "posicionocupacionjefe","horastrabajadasjefe","total_ocupados", 
                                                  "htrabaocupados","niños18", "anos_educ_promedio_hijos", "edad_promediohijos", "Subsidio", 
                                                   "Subsidio_Familia", "SS_Jefe", "Educconyugue")]

train_hogares1 %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# Revisión de los datos de habitaciones por hogar
promedio_habit_hogar <- train_hogares1 %>%
  group_by(Dominio) %>%
  summarize(Media_hh = mean(Habit_por_Hogar, na.rm = TRUE),
            Min_hh = min(Habit_por_Hogar, na.rm = TRUE),
            Max_hh = max(Habit_por_Hogar, na.rm = TRUE),
            sd_hh = sd(Habit_por_Hogar, na.rm = TRUE))
promedio_habit_hogar

# Imputar en los valores extremos para las habitaciones por hogar de las manizales y barranquilla el promedio global de país
media_habit_hogar <- mean(train_hogares1$Habit_por_Hogar, na.rm = TRUE)
train_hogares1$Habit_por_Hogar <- ifelse(train_hogares1$Habit_por_Hogar >= 43, media_habit_hogar, train_hogares1$Habit_por_Hogar)


# Promedio Ing_perc_ug por Departamento
promedio_Ing_perc_ug <- train_hogares1 %>%
  group_by(Dominio) %>%
  summarize(media_Ing_perc_ug = mean(Ing_perc_ug, na.rm = TRUE))
promedio_Ing_perc_ug
train_hogares1 <- left_join(train_hogares1, promedio_Ing_perc_ug %>% select(Dominio, media_Ing_perc_ug), by = "Dominio")

train_hogares1 <- train_hogares1 %>%
  mutate(Ing_perc_ug = ifelse(Ing_perc_ug == 0, media_Ing_perc_ug, Ing_perc_ug))
train_hogares1$media_Ing_perc_ug <- NULL


train_hogares1$R_Ingreso <- ifelse(train_hogares1$Ingreso_Total_Por_Hogar != 0 & train_hogares1$Ing_perc_ug != 0, 
                                   train_hogares1$Ingreso_Total_Por_Hogar / train_hogares1$Ing_perc_ug, NA)

# Imputar en el valor minimo para la relación ingreso
promedio_R_Ingreso <- train_hogares1 %>%
  group_by(Dominio) %>%
  summarize(media_R_Ingreso = mean(R_Ingreso, na.rm = TRUE))
promedio_R_Ingreso
 

train_hogares1 <- left_join(train_hogares1, promedio_R_Ingreso %>% select(Dominio, media_R_Ingreso), by = "Dominio")  

train_hogares1$Ingreso_Total_Por_Hogar <- ifelse(train_hogares1$Ingreso_Total_Por_Hogar == 0, 
                                   train_hogares1$media_R_Ingreso*train_hogares1$Ing_perc_ug, train_hogares1$Ingreso_Total_Por_Hogar)


train_hogares1$R_Arriendo <- round(train_hogares1$Arriendo / train_hogares1$Ingreso_Total_Por_Hogar, 2)

# Imputar en el valor minimo para la relación ingreso
promedio_R_Arriendo <- train_hogares1 %>%
  group_by(Dominio) %>%
  summarize(media_R_Arriendo = mean(R_Arriendo, na.rm = TRUE))
promedio_R_Arriendo

train_hogares1 <- left_join(train_hogares1, promedio_R_Arriendo %>% select(Dominio, media_R_Arriendo), by = "Dominio")

train_hogares1 <- train_hogares1 %>%
  mutate(R_Arriendo = ifelse(R_Arriendo >= 1 | R_Arriendo == 0, media_R_Arriendo, R_Arriendo))

train_hogares1$Arriendo <- train_hogares1$Ingreso_Total_Por_Hogar*train_hogares1$R_Arriendo


# Revisión de stadística descriptivas de variables para el Modelo
Tabla_Stat <- train_hogares1  %>% select(Habit_por_Hogar, 
                                         Dormit, 
                                         Arriendo,
                                         Ing_perc_ug,
                                         Ingreso_Total_Por_Hogar,
                                         edadjefe,
                                         Educjefe,
                                         tiempotrabajojefe_meses,
                                         horastrabajadasjefe,
                                         htrabaocupados,
                                         niños18,
                                         anos_educ_promedio_hijos,
                                         edad_promediohijos,
                                         Educconyugue,
                                         Estrato1)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas")

#Tabla_Ra_ <- "C:/Output R/Problem_Set3/Taller_3/Tabla_Ra.xlsx"
#write_xlsx(train_hogares1, path = Tabla_Ra_)


