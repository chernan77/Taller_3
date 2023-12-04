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
install.packages("nnet")
install.packages("rsample")
install.packages("openpyxl")


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
library(nnet)
library(rsample)
library(openpyxl)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(rio, # Import data easily
       plotly, # Gráficos 
       tmaptools, # geocode_OSM()
       yardstick) #para modelos de ML


sample_submission <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/sample_submission.xlsx")
train_personas1 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_1.xlsx")
train_personas2 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_2.xlsx")
train_personas3 <- read.xlsx("https://github.com/chernan77/Data_3/raw//main/train_personas_3.xlsx")
train_personas4 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/train_personas_4.xlsx")
train_hogares <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/train_hogares.xlsx")



# -----------------------UNIÓN DE DATOS------------------------------------# 


train_personas <- rbind(train_personas1, train_personas2,train_personas3,train_personas4)

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
    posicionocupacionjefe = ifelse(Parent_jh == 1, Act_Ocup, NA_character_),
    posicionocupacionconyugue = ifelse(Parent_jh == 2, Act_Ocup, NA_character_),
    categocupjefe = ifelse(Parent_jh == 1, Cat_Ocup, NA_character_),
    categocupconyugue = ifelse(Parent_jh == 2, Cat_Ocup, NA_character_),
    categocuphijos = ifelse(Parent_jh == 3, Cat_Ocup, NA_character_),
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
         ,posicionocupacionconyugue,horastrabajadasconyugue,especieconyugue,categocuphijos,
         otronegocioconyugue,otrashorasconyugue)

variables_conyugue<- distinct(variables_conyugue, id, .keep_all = TRUE)

train_hogares <- left_join(train_hogares, variables_conyugue, by = "id")

train_hogares <- train_hogares %>%
  mutate(edadconyugue = ifelse(edadconyugue <= 15, mean(edadconyugue, na.rm = TRUE), edadconyugue))

train_hogares$SS_Jefe <- ifelse(is.na(train_hogares$SS_Jefe), 1, train_hogares$SS_Jefe)



train_hogares$posicionocupacionjefe <- ifelse(is.na(train_hogares$posicionocupacionjefe), 1, train_hogares$posicionocupacionjefe)

train_hogares <- train_hogares %>%
  mutate(categocupjefe = ifelse(is.na(categocupjefe)  & train_hogares$posicionocupacionjefe == 1, 
                                          1, categocupjefe))
train_hogares <- train_hogares %>%
  mutate(categocupjefe = ifelse(is.na(categocupjefe)  & train_hogares$posicionocupacionjefe == 4, 
                                            4, categocupjefe))

train_hogares <- train_hogares %>%
  mutate(categocupjefe = ifelse(is.na(categocupjefe)  & train_hogares$posicionocupacionjefe == 3, 
                                4, categocupjefe))

train_hogares <- train_hogares %>%
  mutate(categocupjefe = ifelse(is.na(categocupjefe)  & train_hogares$posicionocupacionjefe == 2, 
                                4, categocupjefe))

train_hogares <- train_hogares %>%
  mutate(categocupjefe = ifelse(is.na(categocupjefe)  & train_hogares$posicionocupacionjefe == 6, 
                                9, categocupjefe))

train_hogares <- train_hogares %>%
  mutate(categocupjefe = ifelse(is.na(categocupjefe)  & train_hogares$posicionocupacionjefe == 5, 
                                9, categocupjefe))


train_hogares$Subsidio <- ifelse(is.na(train_hogares$Subsidio), 
                                  0, train_hogares$Subsidio)

train_hogares$SS_Jefe <- factor(train_hogares$SS_Jefe, levels = c(1, 2, 9), labels = c("Cotiza a un Seguro", "No Cotiza", "Otro"))

train_hogares$posicionocupacionjefe <- factor(train_hogares$posicionocupacionjefe, levels = c(1, 2, 3, 4, 5, 6), labels = c("Trabajando", "Buscando trabajo", 
                                                                                                              "Estudiando", "Oficios del hogar", 
                                                                                                              "Incapacitado permanente para trabajar", 
                                                                                                            "Otra Actividad"))


train_hogares$categocupjefe <- factor(train_hogares$categocupjefe, levels = c(1, 2, 3, 4, 5, 6,7,8, 9), labels = c("Obrero o empleado de empresa particular", "Obrero o empleado del gobierno",
                                                                                                                     "Empleado doméstico","Trabajador por cuenta propia",
                                                                                                                     "Patrón o empleador", "Trabajador familiar sin remuneración",
                                                                                                                     "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                                                                                                     "Jornalero o peón","Otro"))

train_hogares$Subsidio <- factor(train_hogares$Subsidio, levels = c(1, 0), labels = c("si","no"))

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
                                   Educconyugue= case_when(Educconyugue==1 ~"Ninguno",
                                                           Educconyugue==2 ~"Preescolar",
                                                           Educconyugue==3 ~"Educación básica en el ciclo de primaria",
                                                           Educconyugue==4 ~"Educación básica en el ciclo de secundaria",
                                                           Educconyugue==5 ~"Educación media",
                                                           Educconyugue==6 ~"Superior o universitaria",
                                                           Educconyugue==9 ~"No sabe"),
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
                                categocupconyugue= case_when( categocupconyugue==1 ~"Obrero",
                                                              categocupconyugue==2 ~"empleado del gobierno",
                                                              categocupconyugue==3 ~"Empleado doméstico",
                                                              categocupconyugue==4 ~"Trabajador por cuenta propia",
                                                              categocupconyugue==5 ~"Patrón o empleador",
                                                              categocupconyugue==6 ~"Trabajador familiar sin remuneración",
                                                              categocupconyugue==7 ~"Trabajador sinremuneración en empresas o negocios de otros hogares",
                                                              categocupconyugue==8 ~ "Jornalero o peón",
                                                              categocupconyugue==9 ~ "Otro"),  
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
                                               Indigente==0 ~"no"),
                          Prop_Vivienda = case_when(Prop_Vivienda==1 ~"Propia Pagada",
                                                    Prop_Vivienda==2 ~"Propia por Pagar",
                                                    Prop_Vivienda==3 ~"En Arriendo",
                                                    Prop_Vivienda==4 ~"En Usufructo",
                                                    Prop_Vivienda==5 ~"Ocupante",
                                                    Prop_Vivienda==6 ~"Otra"))
           
                                    
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
train_hogares <- train_hogares %>%
mutate(Arriendo = ifelse(is.na(Pag_Arriendo_Est), Pag_Arriendo, Pag_Arriendo_Est))


##--------------------------- Bases de Datos Variables Seleccionadas------------------##


train_hogares1 <- train_hogares[ c("id","Dominio","Per_por_Hogar","Per_Uni_Gasto", "Habit_por_Hogar", "Dormit", "Arriendo","Estrato1", "Ing_perc_ug", "Ingreso_Total_Por_Hogar", 
                                                   "Linea_Indigencia","Linea_Pobreza","edadconyugue", "Pobre", "edadjefe","sexojefe","Educjefe", "Educjefe1", "Educconyugue",
                                                   "categocupjefe","categocupconyugue","categocuphijos", "tiempotrabajojefe_meses", "posicionocupacionjefe","horastrabajadasjefe","total_ocupados", 
                                                  "htrabaocupados","niños18", "anos_educ_promedio_hijos", "edad_promediohijos", "Subsidio", 
                                                   "Subsidio_Familia","Prop_Vivienda", "SS_Jefe","htrabaocupados_prop")]

tabla_sexo <- table(train_hogares1$sexojefe)
print(tabla_sexo)
conteo_male <- sum(train_hogares1$sexojefe == "Male")
conteo_female <- sum(train_hogares1$sexojefe == "Female")

print(paste("Male:", conteo_male))
print(paste("Female:", conteo_female))


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
promedio_Ing_perc_ug <- as.data.frame(promedio_Ing_perc_ug)

library(ggplot2)

library(ggplot2)

# Gráfico de barras del promedio Ing_perc_ug por Departamento
ggplot(promedio_Ing_perc_ug, aes(x = Dominio, y = media_Ing_perc_ug)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Promedio Ingreso per cápita por Departamento",
       x = "Departamento",
       y = "Ingreso per cápita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))  



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


# Analisis y depuración de las Horas trabajadas
ht_jornada_oficial <- 48
desv_htj <-  round(sd(train_hogares1$horastrabajadasjefe, na.rm = TRUE))
desv_ht_op <-  round(sd(train_hogares1$htrabaocupados_prop, na.rm = TRUE))
umbral <- round(ht_jornada_oficial + 2*desv_htj,0)
umbral2 <- round(ht_jornada_oficial + 2.5*desv_ht_op,0)
umbral2_min <- round(ht_jornada_oficial - 2.5*desv_ht_op,0)

train_hogares1$horastrabajadasjefe[train_hogares1$horastrabajadasjefe > umbral] <- umbral
train_hogares1$htrabaocupados_prop[train_hogares1$htrabaocupados_prop > umbral2] <- umbral2
train_hogares1$htrabaocupados_prop[train_hogares1$htrabaocupados_prop < umbral2_min] <- umbral2_min

train_hogares1$htrabaocupados <- train_hogares1$htrabaocupados_prop*train_hogares1$total_ocupados

train_hogares1 <- train_hogares1 %>%
  mutate(htrabaocupados = ifelse(is.na(htrabaocupados)  & train_hogares1$categocupjefe == "Obrero o empleado de empresa particular", 
                                          mean(htrabaocupados, na.rm = TRUE), htrabaocupados))

# Revisión de stadística descriptivas de variables para el Modelo
Tabla_Stat <- train_hogares1  %>% select(Habit_por_Hogar, 
                                         Dormit, 
                                         Arriendo,
                                         Ing_perc_ug,
                                         Ingreso_Total_Por_Hogar,
                                         edadjefe,
                                         Educjefe,
                                         edadconyugue,
                                         Educconyugue,
                                         tiempotrabajojefe_meses,
                                         horastrabajadasjefe,
                                         htrabaocupados_prop,
                                         htrabaocupados,
                                         niños18,
                                         anos_educ_promedio_hijos,
                                         edad_promediohijos,
                                         Estrato1)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas")


promedio_exp_emp <- train_hogares1 %>%
  filter(tiempotrabajojefe_meses != 0) %>%  # Excluir ceros
  group_by(Dominio) %>%
  summarize(
    media_exp_emp = round(mean(tiempotrabajojefe_meses, na.rm = TRUE)),
    mediana_exp_emp = median(tiempotrabajojefe_meses, na.rm = TRUE),
    max_exp_emp = max(tiempotrabajojefe_meses, na.rm = TRUE),
    min_exp_emp = min(tiempotrabajojefe_meses, na.rm = TRUE),
    desv_exp_emp = sd(tiempotrabajojefe_meses, na.rm = TRUE),
    lim_sup_exp_emp = round(media_exp_emp + 2.5*desv_exp_emp),
    moda_exp_emp = as.numeric(names(sort(table(tiempotrabajojefe_meses), decreasing = TRUE)[1]))
  )
print(promedio_exp_emp)


train_hogares1 <- left_join(train_hogares1, promedio_exp_emp %>% select(Dominio, lim_sup_exp_emp), by = "Dominio")
train_hogares1 <- left_join(train_hogares1, promedio_exp_emp %>% select(Dominio, media_exp_emp), by = "Dominio")

train_hogares1 <- train_hogares1 %>%
  mutate(tiempotrabajojefe_meses = ifelse(tiempotrabajojefe_meses >= lim_sup_exp_emp, lim_sup_exp_emp, tiempotrabajojefe_meses))

train_hogares1 <- train_hogares1 %>%
  mutate(tiempotrabajojefe_meses = ifelse(tiempotrabajojefe_meses == 0  & train_hogares1$categocupjefe == "Obrero o empleado de empresa particular", 
                                          media_exp_emp, tiempotrabajojefe_meses))
train_hogares1 <- train_hogares1 %>%
  mutate(tiempotrabajojefe_meses = ifelse(tiempotrabajojefe_meses == 0  & train_hogares1$categocupconyugue == "Obrero o empleado de empresa particular", 
                                          media_exp_emp, tiempotrabajojefe_meses))
train_hogares1 <- train_hogares1 %>%
  mutate(tiempotrabajojefe_meses = ifelse(is.na(tiempotrabajojefe_meses) & categocupconyugue == "Obrero o empleado de empresa particular",
                                          media_exp_emp, tiempotrabajojefe_meses))
train_hogares1 <- train_hogares1 %>%
  mutate(tiempotrabajojefe_meses = ifelse(is.na(tiempotrabajojefe_meses) & train_hogares1$SS_Jefe == "Cotiza a un Seguro", 
                                          media_exp_emp, tiempotrabajojefe_meses))

# Analisis de Educación promedio hijos
media_años_educ_hijos <-  round(mean(train_hogares1$anos_educ_promedio_hijos, na.rm = TRUE))
train_hogares1$anos_educ_promedio_hijos <- ifelse(is.na(train_hogares1$anos_educ_promedio_hijos) & train_hogares1$niños18 >= 1, 
                                                  media_años_educ_hijos, train_hogares1$anos_educ_promedio_hijos)

train_hogares1$anos_educ_promedio_hijos <- ifelse(is.na(train_hogares1$anos_educ_promedio_hijos) & train_hogares1$niños18 == 0, 
                                                  0, train_hogares1$anos_educ_promedio_hijos)

# Analisis de Edad promedio hijos

train_hogares1 <- train_hogares1 %>%
  mutate(edad_promediohijos = ifelse(edad_promediohijos == 0, mean(edad_promediohijos, na.rm = TRUE), edad_promediohijos))

train_hogares1$edad_promediohijos <- ifelse(is.na(train_hogares1$edad_promediohijos), 
                                            0, train_hogares1$edad_promediohijos)

train_hogares1$Prop_Vivienda <- ifelse(is.na(train_hogares1$Prop_Vivienda), 
                                 "Ocupante", train_hogares1$Prop_Vivienda)


# Imputar en el valor minimo para la relación ingreso
promedio_niños <- train_hogares1 %>%
  group_by(Dominio) %>%
  summarize(total_niños = sum(niños18, na.rm = TRUE),
            media_prh = mean(Per_por_Hogar, na.rm = TRUE),
            prom_niños = mean(niños18, na.rm = TRUE))
promedio_niños



# Revisión de stadística descriptivas de variables para el Modelo
Tabla_Stat <- train_hogares1  %>% select(Habit_por_Hogar, 
                                         Dormit, 
                                         Per_Uni_Gasto,
                                         Per_por_Hogar,
                                         Arriendo,
                                         Ing_perc_ug,
                                         Ingreso_Total_Por_Hogar,
                                         edadjefe,
                                         Educjefe,
                                         edadconyugue,
                                         Educconyugue,
                                         tiempotrabajojefe_meses,
                                         horastrabajadasjefe,
                                         htrabaocupados_prop,
                                         htrabaocupados,
                                         niños18,
                                         anos_educ_promedio_hijos,
                                         edad_promediohijos,
                                         Educconyugue,
                                         Prop_Vivienda,
                                         Estrato1)

Tabla_Descript <- as.data.frame(stargazer(data.frame(Tabla_Stat), header=FALSE, type='text', title="Estadisticas Variables Seleccionadas"))
Stat_Tabla <- "C:/Output R/Problem_Set3/Taller_3/Tabla_Stat.xlsx"
write_xlsx(Tabla_Descript, path = Stat_Tabla)


# Variables para las regresiones de predicción de pobreza:

train_hogares1 <- train_hogares1 %>% rename(Sexo_JHogar=sexojefe) #(Variable 1)
train_hogares1 <- train_hogares1 %>% rename(Edad_JHogar=edadjefe) #(Variable 2)
train_hogares1 <- train_hogares1 %>% rename(Pers_por_Hogar=Per_por_Hogar) #(Variable 3)
train_hogares1 <- train_hogares1 %>% rename(Edad_prom_Hijos=edad_promediohijos) #(Variable 4)
train_hogares1 <- train_hogares1 %>% rename(Menores_18Años=niños18) #(Variable 5)
train_hogares1 <- train_hogares1 %>% rename(Estrato=Estrato1)  #(Variable 6)
train_hogares1 <- train_hogares1 %>% rename(Exp_Empresa=tiempotrabajojefe_meses)  #(Variable 7)
train_hogares1 <- train_hogares1 %>% rename(Hrs_Ocupados=htrabaocupados)  #(Variable 8)
train_hogares1 <- train_hogares1 %>% rename(Total_Ocup=total_ocupados)  #(Variable 9)
train_hogares1 <- train_hogares1 %>% rename(Cat_Ocup_JHogar=categocupjefe)  #(Variable 10)
train_hogares1 <- train_hogares1 %>% rename(Posc_Ocup_JHogar=posicionocupacionjefe)  #(Variable 11)
train_hogares1 <- train_hogares1 %>% rename(Educ_JHogar=Educjefe)  #(Variable 12)
train_hogares1 <- train_hogares1 %>% rename(Educ_prom_Hijos=anos_educ_promedio_hijos)  #(Variable 13)
train_hogares1 <- train_hogares1 %>% rename(Educ_Conyugue=Educconyugue)  #(Variable 14)
train_hogares1 <- train_hogares1 %>% rename(Edad_Conyugue=edadconyugue)  #(Variable 15)
train_hogares1 <- train_hogares1 %>% rename(Hab_por_Hogar=Habit_por_Hogar)  #(Variable 16)
train_hogares1 <- train_hogares1 %>% rename(Dormit_Hogar=Dormit)  #(Variable 17)
train_hogares1 <- train_hogares1 %>% rename(Propieadad_Vivienda=Prop_Vivienda)  #(Variable 18)
train_hogares1 <- train_hogares1 %>% rename(Pago_Arriendo=Arriendo)  #(Variable 19)
train_hogares1 <- train_hogares1 %>% rename(Recibe_Subsidios=Subsidio)  #(Variable 20)
train_hogares1 <- train_hogares1 %>% rename(SS_Jefe=SS_Jefe)  #(Variable 21)
train_hogares1 <- train_hogares1 %>% rename(Ingreso_Hogar=Ingreso_Total_Por_Hogar)  #(Variable 22)
train_hogares1 <- train_hogares1 %>% rename(Ingreso_Perc_Hogar=Ing_perc_ug)  #(Variable 23)
train_hogares1 <- train_hogares1 %>% rename(Pobreza=Pobre)  #(Variable 24)
train_hogares1$Edad_JHogar2<- train_hogares1$Edad_JHogar^2


# Imputar en el valor minimo para la relación ingreso
promedio_Hrs_Ocupados <- train_hogares1 %>%
  group_by(Dominio) %>%
  summarize(media_hrs = mean(horastrabajadasjefe, na.rm = TRUE))
promedio_Hrs_Ocupados

###########-------------------------------------------------------------DATA 1---------------------------------------------#######
Data1 <- train_hogares1[ c("id","Dominio", "Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años", 
                                    "Linea_Indigencia", "Linea_Pobreza", "Total_Ocup", "Cat_Ocup_JHogar",
                                    "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar",
                                    "Pago_Arriendo", "SS_Jefe", "Ingreso_Hogar", "Ingreso_Perc_Hogar", "Pobreza")]


Train_Data1 <- "C:/Output R/Problem_Set3/Taller_3/Train_Data1.xlsx"  
write.xlsx(Data1, Train_Data1)

Tabla_Stat <- Data1  %>% select(Hab_por_Hogar, 
                                         Dormit_Hogar, 
                                         Pers_por_Hogar,
                                         Pago_Arriendo,
                                         Ingreso_Perc_Hogar,
                                         Ingreso_Hogar,
                                         Edad_JHogar,
                                         Educ_JHogar,
                                         Total_Ocup,
                                         Menores_18Años,
                                         Educ_prom_Hijos,
                                         SS_Jefe,
                                         Pobreza)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas")

# Gráfico de barras del promedio Ing_perc_ug por Departamento
ggplot(train_hogares1, aes(x = Dominio, y = Pobreza)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Promedio pobreza por Departamento",
       x = "Departamento",
       y = "Pobreza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) 



# Dividimos la muestra en entrenamiento y testeo
set.seed(123)
data_split <- initial_split(Data1, prop = .7)
train_Data1 <- training(data_split)
test_Data1  <- testing(data_split)

# Modelos Logit con todo el conjunto de variables y observaciones de Data1
# Modelo Logit1:
Model1.1 <- glm(Pobreza ~ Sexo_JHogar + Edad_JHogar + Edad_JHogar2 + Pers_por_Hogar + Menores_18Años + 
                  Total_Ocup + Cat_Ocup_JHogar + Posc_Ocup_JHogar + Educ_JHogar + Educ_prom_Hijos + Hab_por_Hogar + 
                  Dormit_Hogar + Pago_Arriendo + SS_Jefe + Ingreso_Hogar, data = train_Data1, family = "binomial")
summary(Model1.1,type="text")
test_Data1 <- test_Data1 %>% mutate(prob_hat=predict(Model1.1,newdata = test_Data1, type = "response")) 
rule <- 1/2 # Bayes Rule
test_Data1 <-  test_Data1  %>% mutate(Pobreza_hat=ifelse(prob_hat>rule,1,0))
Pronost_1.1  <- test_Data1[ c("id", "Pobreza_hat")]
  
# Modelo Logit2:
Model1.2 <- glm(Pobreza ~ Sexo_JHogar + Edad_JHogar + Edad_JHogar2 + Pers_por_Hogar + Menores_18Años + 
                  Total_Ocup + Cat_Ocup_JHogar + Posc_Ocup_JHogar + Educ_JHogar + Educ_prom_Hijos + Hab_por_Hogar + 
                  Dormit_Hogar + Pago_Arriendo + SS_Jefe + Ingreso_Perc_Hogar, data = train_Data1, family = "binomial")
summary(Model1.2,type="text")
test_Data1 <- test_Data1 %>% mutate(prob_hat1=predict(Model1.2,newdata = test_Data1, type = "response"))
test_Data1 <-  test_Data1  %>% mutate(Pobreza_hat1=ifelse(prob_hat1>rule,1,0))
Pronost_1.2  <- test_Data1[ c("id", "Pobreza_hat1")]



# Modelos Logit con todo el conjunto de variables y observaciones de Data1
# Modelo Logit1 con penalización tipo Elastic Net
#Logit_EN <- as.matrix(Data1[, c("Sexo_JHogar", "Edad_JHogar", "Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años","Total_Ocup",
#"Cat_Ocup_JHogar", "Posc_Ocup_JHogar", "Educ_JHogar", "Educ_prom_Hijos", "Hab_por_Hogar", "Dormit_Hogar",
# "Pago_Arriendo", "SS_Jefe", "Ingreso_Hogar")])


###########-------------------------------------------------------------DATA 2---------------------------------------------#######
Data2 <- train_hogares1[ c("id","Dominio", "Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Edad_prom_Hijos", "Menores_18Años", 
                                   "Linea_Indigencia", "Linea_Pobreza", "Exp_Empresa","Hrs_Ocupados","Total_Ocup", "Cat_Ocup_JHogar", 
                                   "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar",
                                   "Pago_Arriendo","SS_Jefe", "Ingreso_Hogar", "Ingreso_Perc_Hogar", "Pobreza")]

Data2 <- Data2[complete.cases(Data2$Hrs_Ocupados), ]
Data2 <- Data2[complete.cases(Data2$Exp_Empresa), ]
Data2 %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

Train_Data2 <- "C:/Output R/Problem_Set3/Taller_3/Train_Data2.xlsx"  
write.xlsx(Data2, Train_Data2)


Tabla_Stat <- Data2  %>% select(Hab_por_Hogar, 
                                         Dormit_Hogar, 
                                         Pers_por_Hogar,
                                         Pago_Arriendo,
                                         Ingreso_Perc_Hogar,
                                         Ingreso_Hogar,
                                         Edad_JHogar,
                                         Educ_JHogar,
                                         Exp_Empresa,
                                         Hrs_Ocupados,
                                         Total_Ocup,
                                         Menores_18Años,
                                         Educ_prom_Hijos,
                                         SS_Jefe,
                                         Pobreza)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas")

Data2 %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# Dividimos la muestra en entrenamiento y testeo
set.seed(123)
data_split <- initial_split(Data2, prop = .7)
train_Data2 <- training(data_split)
test_Data2  <- testing(data_split)

# Modelos Logit con todo el conjunto de variables y observaciones de Data1
# Modelo Logit2.1:
Model2.1 <- glm(Pobreza ~ Sexo_JHogar + Edad_JHogar + Edad_JHogar2 + Pers_por_Hogar + Menores_18Años + Exp_Empresa + Hrs_Ocupados +
                  Total_Ocup + Cat_Ocup_JHogar + Posc_Ocup_JHogar + Educ_JHogar + Educ_prom_Hijos + Hab_por_Hogar +
                  Dormit_Hogar + Pago_Arriendo + SS_Jefe + Ingreso_Hogar, data = train_Data2, family = "binomial")

summary(Model2.1,type="text")
test_Data2 <- test_Data2 %>% mutate(prob_hat2=predict(Model2.1,newdata = test_Data2, type = "response")) 
rule <- 1/2 # Bayes Rule
test_Data2 <-  test_Data2  %>% mutate(Pobreza_hat2=ifelse(prob_hat2>rule,1,0))
Pronost_2.1  <- test_Data2[ c("id", "Pobreza_hat2")]

# Modelo Logit2.2:
Model2.2 <- glm(Pobreza ~ Sexo_JHogar + Edad_JHogar + Edad_JHogar2 + Pers_por_Hogar + Menores_18Años + Exp_Empresa + Hrs_Ocupados +
                  Total_Ocup + Cat_Ocup_JHogar + Posc_Ocup_JHogar + Educ_JHogar + Educ_prom_Hijos + Hab_por_Hogar +
                  Dormit_Hogar + Pago_Arriendo + SS_Jefe + Ingreso_Perc_Hogar, data = train_Data2, family = "binomial")

summary(Model2.2,type="text")
test_Data2 <- test_Data2 %>% mutate(prob_hat3=predict(Model2.2,newdata = test_Data2, type = "response"))
test_Data2 <-  test_Data2  %>% mutate(Pobreza_hat3=ifelse(prob_hat3>rule,1,0))
Pronost_2.2  <- test_Data2[ c("id", "Pobreza_hat3")]

###########-------------------------------------------------------------DATA 3---------------------------------------------#######
Data3 <- train_hogares1[ c("id","Dominio", "Sexo_JHogar", "Edad_JHogar", "Edad_JHogar2", "Pers_por_Hogar", "Edad_prom_Hijos", "Menores_18Años", 
                                    "Linea_Indigencia", "Linea_Pobreza", "Exp_Empresa","Hrs_Ocupados","Total_Ocup", "Cat_Ocup_JHogar", 
                                    "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Educ_Conyugue","Edad_Conyugue", "Hab_por_Hogar","Dormit_Hogar",
                                    "Pago_Arriendo","SS_Jefe", "Ingreso_Hogar", "Ingreso_Perc_Hogar", "Pobreza")]


Data3  <- Data3[complete.cases(Data3$Educ_Conyugue), ]
Data3  <- Data3[complete.cases(Data3$Hrs_Ocupados), ]
Data3  <- Data3[complete.cases(Data3$Exp_Empresa), ]

Train_Data3 <- "C:/Output R/Problem_Set3/Taller_3/Train_Data3.xlsx"  
write.xlsx(Data3, Train_Data3)


Tabla_Stat <- Data3  %>% select(Hab_por_Hogar, 
                                Dormit_Hogar, 
                                Pers_por_Hogar,
                                Pago_Arriendo,
                                Ingreso_Perc_Hogar,
                                Ingreso_Hogar,
                                Edad_JHogar,
                                Educ_JHogar,
                                Exp_Empresa,
                                Hrs_Ocupados,
                                Total_Ocup,
                                Menores_18Años,
                                Educ_prom_Hijos,
                                Educ_Conyugue,
                                Edad_Conyugue,
                                SS_Jefe,
                                Pobreza)
                               

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas")

Data3 %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# Dividimos la muestra en entrenamiento y testeo
set.seed(123)
data_split <- initial_split(Data3, prop = .7)
train_Data3 <- training(data_split)
test_Data3  <- testing(data_split)

# Modelos Logit con todo el conjunto de variables y observaciones de Data1
# Modelo Logit3.1:
Model3.1 <- glm(Pobreza ~ Sexo_JHogar + Edad_JHogar + Edad_JHogar2 + Pers_por_Hogar + Menores_18Años + Exp_Empresa + Hrs_Ocupados +
                Total_Ocup + Cat_Ocup_JHogar + Posc_Ocup_JHogar + Educ_JHogar + Educ_prom_Hijos + Hab_por_Hogar +  Educ_Conyugue +
                Edad_Conyugue + Dormit_Hogar + Pago_Arriendo + SS_Jefe + Ingreso_Hogar, data = train_Data3, family = "binomial")

summary(Model3.1,type="text")
test_Data3 <- test_Data3 %>% mutate(prob_hat4=predict(Model3.1,newdata = test_Data3, type = "response")) 
rule <- 1/2 # Bayes Rule
test_Data3 <-  test_Data3  %>% mutate(Pobreza_hat4=ifelse(prob_hat4>rule,1,0))
Pronost_3.1  <- test_Data3[ c("id", "Pobreza_hat4")]

# Modelo Logit3.2:
Model3.2 <- glm(Pobreza ~ Sexo_JHogar + Edad_JHogar + Edad_JHogar2 + Pers_por_Hogar + Menores_18Años + Exp_Empresa + Hrs_Ocupados +
                  Total_Ocup + Cat_Ocup_JHogar + Posc_Ocup_JHogar + Educ_JHogar + Educ_prom_Hijos + Hab_por_Hogar + Educ_Conyugue +
                  Edad_Conyugue + Dormit_Hogar + Pago_Arriendo + SS_Jefe + Ingreso_Perc_Hogar, data = train_Data3, family = "binomial")

summary(Model3.2,type="text")
test_Data3 <- test_Data3 %>% mutate(prob_hat5=predict(Model3.2,newdata = test_Data3, type = "response"))
test_Data3 <-  test_Data3  %>% mutate(Pobreza_hat5=ifelse(prob_hat5>rule,1,0))
Pronost_3.2  <- test_Data3[ c("id", "Pobreza_hat5")]


Data_l1 <- Data2[ c("Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años", 
                           "Total_Ocup", "Cat_Ocup_JHogar","Exp_Empresa","Hrs_Ocupados",
                           "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar",
                           "Pago_Arriendo", "SS_Jefe", "Ingreso_Perc_Hogar", "Pobreza")]

# Base de datos1
# Lista de columnas categóricas (factores) a convertir a variables dummy
train_Datal1 <- Data_l1 %>% mutate(Sexo_JHogar = case_when(Sexo_JHogar=="Male"~ 1,
                                                              Sexo_JHogar =="Female" ~2 ),
                                      Pobreza = case_when(Pobreza=="si"~ 1,
                                                          Pobreza=="no"~ 0),
                                      SS_Jefe = case_when(SS_Jefe=="Cotiza a un Seguro"~ 1,
                                                          SS_Jefe=="No Cotiza"~2,
                                                          SS_Jefe=="Otro"~9),
                                      Educ_JHogar = case_when(Educ_JHogar=="Ninguno"~1, 
                                                              Educ_JHogar=="Preescolar"~2 ,
                                                              Educ_JHogar=="Educación básica en el ciclo de primaria"~3 ,
                                                              Educ_JHogar=="Educación básica en el ciclo de secundaria"~4,
                                                              Educ_JHogar=="Educación media"~5,
                                                              Educ_JHogar=="Superior o universitaria"~6,
                                                              Educ_JHogar=="No sabe"~9 ),
                                      Cat_Ocup_JHogar = case_when(Cat_Ocup_JHogar=="Obrero o empleado de empresa particular"~1,
                                                                  Cat_Ocup_JHogar=="Obrero o empleado del gobierno"~2,
                                                                  Cat_Ocup_JHogar=="Empleado doméstico"~3,
                                                                  Cat_Ocup_JHogar=="Trabajador por cuenta propia"~4 ,
                                                                  Cat_Ocup_JHogar=="Patrón o empleador"~5,
                                                                  Cat_Ocup_JHogar=="Trabajador familiar sin remuneración"~6,
                                                                  Cat_Ocup_JHogar=="Trabajador sin remuneración en empresas o negocios de otros hogares"~7,
                                                                  Cat_Ocup_JHogar=="Jornalero o peón"~8,
                                                                  Cat_Ocup_JHogar=="Otro"~9),
                                      Posc_Ocup_JHogar = case_when(Posc_Ocup_JHogar=="Trabajando"~1 ,
                                                                   Posc_Ocup_JHogar=="Buscando trabajo"~2 ,
                                                                   Posc_Ocup_JHogar=="Estudiando"~3,
                                                                   Posc_Ocup_JHogar=="Oficios del hogar"~4 ,
                                                                   Posc_Ocup_JHogar=="Incapacitado permanente para trabajar"~5,
                                                                   Posc_Ocup_JHogar=="Otra Actividad"~6))


############---------------------------------Logit con Elastic Net----------------------------------######################

set.seed(123)
data_split <- initial_split(train_Datal1, prop = .7)
train_Logit_EN <- training(data_split)
test_Logit_EN  <- testing(data_split)

y <- train_Logit_EN$Pobreza

train_EN_logit <- train_Logit_EN[ c("Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años", 
                                    "Total_Ocup", "Cat_Ocup_JHogar","Exp_Empresa","Hrs_Ocupados",
                                    "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar",
                                    "Pago_Arriendo", "SS_Jefe", "Ingreso_Perc_Hogar")]
train_matrix <- as.matrix(train_EN_logit)
test_EN_logit <- test_Logit_EN[ c("Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años", 
                                    "Total_Ocup", "Cat_Ocup_JHogar","Exp_Empresa","Hrs_Ocupados",
                                    "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar",
                                    "Pago_Arriendo", "SS_Jefe", "Ingreso_Perc_Hogar")]
test_matrix <- as.matrix(test_EN_logit)

EN_logit <- glmnet(train_matrix, y, family = "binomial", alpha = 0.5) 
# Seleccionar el valor óptimo de lambda
cv_EN_logit <- cv.glmnet(train_matrix, y, family = "binomial", alpha = 0.5)
lambda_opt_en <- cv_EN_logit$lambda.min
lambda_opt_en


# Ajustar el modelo Elastic Net con el valor óptimo de lambda
M_Logit_EN <- glmnet(train_matrix, y, alpha = 0.5, family = "binomial", lambda = lambda_opt_en)

###############--------Pronostico dentro de Muestra------------------------###############
test_Logit_EN <- test_Logit_EN %>% mutate(prob_h=predict(M_Logit_EN, s = lambda_opt_en, newx = test_matrix, type = "response")) 
test_Logit_EN$prob_h <- unlist(test_Logit_EN$prob_h)
test_Logit_EN$prob_h <- as.numeric(test_Logit_EN$prob_h)
test_Logit_EN <-  test_Logit_EN  %>% mutate(Pobreza_hat6=ifelse(prob_h>rule,1,0))
Pronost_4.1  <- test_Logit_EN[ c("Pobreza_hat6")]

############---------------------------------Logit con Ridge----------------------------------######################

# Ajustar un modelo de regresión Ridge
Ridge_logit <- glmnet(train_matrix, y, family = "binomial", alpha = 0)  # alpha = 0 para regresión Ridge

# Seleccionar el valor óptimo de lambda
cv_ridge <- cv.glmnet(train_matrix, y, family = "binomial", alpha = 0)  # alpha = 0 para regresión Ridge
lambda_opt_rgd <- cv_ridge$lambda.min
lambda_opt_rgd

# Ajustar el modelo con el valor óptimo de lambda
M_Logit_rgd <- glmnet(train_matrix, y, family = "binomial", alpha = 0, lambda = lambda_opt_rgd)
test_Logit_EN <- test_Logit_EN %>% mutate(prob_h1=predict(M_Logit_rgd, s = lambda_opt_rgd, newx = test_matrix, type = "response")) 
test_Logit_EN$prob_h1 <- unlist(test_Logit_EN$prob_h1)
test_Logit_EN$prob_h1 <- as.numeric(test_Logit_EN$prob_h1)
test_Logit_EN <-  test_Logit_EN  %>% mutate(Pobreza_hat7=ifelse(prob_h1>rule,1,0))
Pronost_4.2  <- test_Logit_EN[ c("Pobreza_hat7")]

############---------------------------------Logit con Lasso----------------------------------######################

# Ajustar un modelo Logit Lasso
Lasso_logit <- glmnet(train_matrix, y, family = "binomial", alpha = 1)  # alpha = 0 para regresión Ridge

# Seleccionar el valor óptimo de lambda
cv_lasso <- cv.glmnet(train_matrix, y, family = "binomial", alpha = 1)  # alpha = 0 para regresión Ridge
lambda_opt_ls <- cv_lasso$lambda.min
lambda_opt_ls

# Ajustar el modelo con el valor óptimo de lambda
M_Logit_ls <- glmnet(train_matrix, y, family = "binomial", alpha = 1, lambda = lambda_opt_ls)
test_Logit_EN <- test_Logit_EN %>% mutate(prob_h2=predict(M_Logit_ls, s = lambda_opt_ls, newx = test_matrix, type = "response")) 
test_Logit_EN$prob_h2 <- unlist(test_Logit_EN$prob_h2)
test_Logit_EN$prob_h2 <- as.numeric(test_Logit_EN$prob_h2)
test_Logit_EN <-  test_Logit_EN  %>% mutate(Pobreza_hat8=ifelse(prob_h2>rule,1,0))
Pronost_4.2  <- test_Logit_EN[ c("Pobreza_hat8")]


# Convertir en Factor los pronósticos:

test_Data1 <- test_Data1 %>% 
  mutate(Pobreza_hat=factor(Pobreza_hat,levels=c(0,1),labels=c("no","si"))) # Logic1
test_Data1 <- test_Data1 %>% 
  mutate(Pobreza_hat1=factor(Pobreza_hat1,levels=c(0,1),labels=c("no","si"))) # Logic2
test_Data2 <- test_Data2 %>% 
  mutate(Pobreza_hat2=factor(Pobreza_hat2,levels=c(0,1),labels=c("no","si"))) # Logic3
test_Data2 <- test_Data2 %>% 
  mutate(Pobreza_hat3=factor(Pobreza_hat3,levels=c(0,1),labels=c("no","si"))) # Logic4
test_Data3 <- test_Data3 %>% 
  mutate(Pobreza_hat4=factor(Pobreza_hat4,levels=c(0,1),labels=c("no","si"))) # Logic5
test_Data3 <- test_Data3 %>% 
  mutate(Pobreza_hat5=factor(Pobreza_hat5,levels=c(0,1),labels=c("no","si"))) # Logic6
test_Logit_EN <- test_Logit_EN %>% 
  mutate(Pobreza_hat6=factor(Pobreza_hat6,levels=c(0,1),labels=c("no","si"))) # Logic7
test_Logit_EN <- test_Logit_EN %>% 
  mutate(Pobreza=factor(Pobreza,levels=c(0,1),labels=c("no","si"))) # convertir en factor la variable pobreza
test_Logit_EN <- test_Logit_EN %>% 
  mutate(Pobreza_hat7=factor(Pobreza_hat7,levels=c(0,1),labels=c("no","si"))) # Logic7
test_Logit_EN <- test_Logit_EN %>% 
  mutate(Pobreza_hat8=factor(Pobreza_hat8,levels=c(0,1),labels=c("no","si"))) # Logic8

accuracy <- accuracy(test_Data1, truth = Pobreza, estimate = Pobreza_hat)
accuracy1 <- accuracy(test_Data1, truth = Pobreza, estimate = Pobreza_hat1)
accuracy2 <- accuracy(test_Data2, truth = Pobreza, estimate = Pobreza_hat2)
accuracy3 <- accuracy(test_Data2, truth = Pobreza, estimate = Pobreza_hat3)
accuracy4 <- accuracy(test_Data3, truth = Pobreza, estimate = Pobreza_hat4)
accuracy5 <- accuracy(test_Data3, truth = Pobreza, estimate = Pobreza_hat5)
accuracy6 <- accuracy(test_Logit_EN, truth = Pobreza, estimate = Pobreza_hat6)
accuracy7 <- accuracy(test_Logit_EN, truth = Pobreza, estimate = Pobreza_hat7)
accuracy8 <- accuracy(test_Logit_EN, truth = Pobreza, estimate = Pobreza_hat8)

# Crear una tabla tidy con las métricas
tabla1 <- tribble( ~Metric, ~Value, "Accuracy", accuracy$.estimate)
tabla2 <- tribble( ~Metric, ~Value, "Accuracy", accuracy1$.estimate)
tabla3 <- tribble( ~Metric, ~Value, "Accuracy", accuracy2$.estimate)
tabla4 <- tribble( ~Metric, ~Value, "Accuracy", accuracy3$.estimate)
tabla5 <- tribble( ~Metric, ~Value, "Accuracy", accuracy4$.estimate)
tabla6 <- tribble( ~Metric, ~Value, "Accuracy", accuracy5$.estimate)
tabla7 <- tribble( ~Metric, ~Value, "Accuracy", accuracy6$.estimate)
tabla8 <- tribble( ~Metric, ~Value, "Accuracy", accuracy7$.estimate)
tabla9 <- tribble( ~Metric, ~Value, "Accuracy", accuracy8$.estimate)

tabla_resumen <- bind_rows(
  tabla1 %>% mutate(Model = "Logit1"),
  tabla2 %>% mutate(Model = "Logict2"),
  tabla3 %>% mutate(Model = "Logit3"),
  tabla4 %>% mutate(Model = "Logit4"),
  tabla5 %>% mutate(Model = "Logit5"),
  tabla6 %>% mutate(Model = "Logit6"),
  tabla7 %>% mutate(Model = "Logit_EN"),
  tabla8 %>% mutate(Model = "Logit_Ridge"),
  tabla9 %>% mutate(Model = "Logit_Lasso")
)

# Imprime la tabla resumen
print(tabla_resumen)



Test_Datal1 <- "C:/Output R/Problem_Set3/Taller_3/Test_Datal1.xlsx"  
write.xlsx(test_Logit_EN, Test_Datal1)


# Importar los datos de Test
test_personas1 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_personas_1.xlsx")
test_personas2 <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_personas_2.xlsx")
test_hogares <- read.xlsx("https://github.com/chernan77/Data_3/raw/main/test_hogares.xlsx")

test_personas <- rbind(test_personas1, test_personas2)

# Base de datos de test_personas:

test_personas <- test_personas %>%
  mutate(Des = ifelse(Oc == 1 & is.na(Des), 0, Des))
test_personas$Oc <- ifelse(test_personas$Des == 1 & is.na(test_personas$Oc), 0, test_personas$Oc)

test_personas <- test_personas %>% rename(Sexo=P6020)
test_personas <- test_personas %>% rename(Edad=P6040) # ¿Cuantos años tiene cumplido?
test_personas <- test_personas %>% rename(Parent_jh=P6050) # Parentesco con el jefe de hogar
test_personas <- test_personas %>% rename(SS=P6090) # 1:Si, 2: No y 9: No sabe
test_personas <- test_personas %>% rename(R_SS=P6100) # 1:Si, 2: No y 9: No sabe
test_personas <- test_personas %>% rename(Educ=P6210) 
test_personas <- test_personas %>% rename(G_Educ=P6210s1)
test_personas <- test_personas %>% rename(Act_Ocup=P6240)
test_personas <- test_personas %>% rename(Exp_Empresa=P6426)
test_personas <- test_personas %>% rename(Cat_Ocup=P6430)
test_personas <- test_personas %>% rename(Hrs_Trab=P6800)
test_personas <- test_personas %>% rename(Num_Trab=P6870)
test_personas <- test_personas %>% rename(Ocupac=Oc)

# Base de datos de test_personas:
test_personas <- test_personas %>%
  mutate(
    edadjefe = ifelse(Parent_jh == 1, Edad, NA_real_),
    edadconyugue = ifelse(Parent_jh == 2, Edad, NA_real_),
    edadhijos = ifelse(Parent_jh == 3, Edad, NA_real_),
    edadnietos = ifelse(Parent_jh == 4, Edad, NA_real_),
    sexojefe = ifelse(Parent_jh == 1, Sexo, NA_character_),
    sexoconyugue = ifelse(Parent_jh == 2, Sexo, NA_character_),
    Educjefe = ifelse(Parent_jh == 1, Educ, NA_character_),
    Educconyugue = ifelse(Parent_jh == 2, Educ, NA_real_),
    Educhijos = ifelse(Parent_jh == 3, Educ, NA_real_),
    SS_Jefe = ifelse(Parent_jh == 1, SS, NA_character_),
    SS_Conyugue = ifelse(Parent_jh == 2, SS, NA_character_),
    tiempotrabajojefe_meses = ifelse(Parent_jh == 1, Exp_Empresa, NA_real_),
    tiempotrabajoconyugue = ifelse(Parent_jh == 2, Exp_Empresa, NA_real_),
    categocupjefe = ifelse(Parent_jh == 1, Cat_Ocup, NA_character_),
    categocupconyugue = ifelse(Parent_jh == 2, Cat_Ocup, NA_character_),
    categocuphijos = ifelse(Parent_jh == 3, Cat_Ocup, NA_character_),
    posicionocupacionjefe = ifelse(Parent_jh == 1, Act_Ocup, NA_character_),
    posicionocupacionconyugue = ifelse(Parent_jh == 2, Act_Ocup, NA_character_),
    horastrabajadasjefe = ifelse(Parent_jh == 1, Hrs_Trab, NA_real_),
    horastrabajadasconyugue = ifelse(Parent_jh == 2, Hrs_Trab, NA_real_),
    Jefe_Hogar_Mujer = ifelse(Parent_jh == 1 & Sexo == 2, 1, 0))


test_personas <- test_personas %>%
  group_by(id) %>%
  mutate(
    edadhijos = ifelse(Parent_jh == 3, Edad, NA_real_),
    total_ocupados = sum(Ocupac == 1, na.rm = TRUE),
    htrabaocupados = sum(Hrs_Trab, na.rm = TRUE),
    niños6 = sum(edadhijos < 6, na.rm = TRUE),
    niños6a12 = sum(edadhijos >= 6 & edadhijos < 12, na.rm = TRUE),
    niños12a18 = sum(edadhijos >= 12 & edadhijos < 18, na.rm = TRUE),
    niños18 = sum(edadhijos < 18, na.rm = TRUE),
    edad_promediohijos = mean(edadhijos ,na.rm = TRUE) ,
    anos_educ_promedio_hijos = mean(Educhijos, na.rm = TRUE),
    htrabaocupados_prop = htrabaocupados / total_ocupados)


Data_Jefe <- test_personas %>% 
  filter(Parent_jh == 1) %>%  # Filtra solo el jefe de hogar
  select(id, edadjefe, sexojefe, Educjefe, SS_Jefe,categocupjefe,tiempotrabajojefe_meses,
         posicionocupacionjefe,horastrabajadasjefe,total_ocupados,sexoconyugue,
         htrabaocupados,niños6,niños6a12,niños12a18,niños18,
         htrabaocupados_prop,Jefe_Hogar_Mujer, anos_educ_promedio_hijos,edad_promediohijos)
test_hogares <- left_join(test_hogares, Data_Jefe, by = "id")

Data_conyugue <- test_personas %>% 
  filter(Parent_jh == 2) %>%  # Filtra solo el jefe de hogar
  select(id,edadconyugue,Educconyugue,SS_Conyugue,categocupconyugue,tiempotrabajoconyugue,
         posicionocupacionconyugue,horastrabajadasconyugue)
Data_conyugue<- distinct(Data_conyugue, id, .keep_all = TRUE)
test_hogares <- left_join(test_hogares, Data_conyugue, by = "id")

test_hogares <- test_hogares %>%
  mutate(edadconyugue = ifelse(edadconyugue <= 15, mean(edadconyugue, na.rm = TRUE), edadconyugue))

# Predicciones fueta de muestra:
test_hogares <- test_hogares %>% rename(Hab_por_Hogar=P5000)
test_hogares <- test_hogares %>% rename(Dormit_Hogar=P5010)
test_hogares <- test_hogares %>% rename(Propiedad_Vivienda=P5090)
test_hogares <- test_hogares %>% rename(Pag_Arriendo_Est=P5130)
test_hogares <- test_hogares %>% rename(Pag_Arriendo=P5140)
test_hogares <- test_hogares %>%
  mutate(Pago_Arriendo = ifelse(is.na(Pag_Arriendo_Est), Pag_Arriendo, Pag_Arriendo_Est))
test_hogares <- test_hogares %>% rename(Pers_por_Hogar=Nper)
test_hogares <- test_hogares %>% rename(Per_Uni_Gasto=Npersug)
test_hogares <- test_hogares %>% rename(Sexo_JHogar=sexojefe) 
test_hogares <- test_hogares %>% rename(Edad_JHogar=edadjefe)
test_hogares <- test_hogares %>% rename(Edad_prom_Hijos=edad_promediohijos)
test_hogares <- test_hogares %>% rename(Menores_18Años=niños18)
test_hogares <- test_hogares %>% rename(Exp_Empresa=tiempotrabajojefe_meses)  
test_hogares <- test_hogares %>% rename(Hrs_Ocupados=htrabaocupados_prop)
test_hogares <- test_hogares %>% rename(Total_Ocup=total_ocupados)
test_hogares <- test_hogares %>% rename(Cat_Ocup_JHogar=categocupjefe)
test_hogares <- test_hogares %>% rename(Posc_Ocup_JHogar=posicionocupacionjefe)
test_hogares <- test_hogares %>% rename(Educ_JHogar=Educjefe)
test_hogares <- test_hogares %>% rename(Educ_prom_Hijos=anos_educ_promedio_hijos)
test_hogares <- test_hogares %>% rename(Educ_Conyugue=Educconyugue) 
test_hogares <- test_hogares %>% rename(Edad_Conyugue=edadconyugue)
test_hogares <- test_hogares %>% rename(SS_Jefe=SS_Jefe)
test_hogares$Edad_JHogar2<- test_hogares$Edad_JHogar^2

test_hogares <- test_hogares %>%
  mutate(Cat_Ocup_JHogar = ifelse(is.na(Cat_Ocup_JHogar)  & test_hogares$Posc_Ocup_JHogar == 4, 
                                        4, Cat_Ocup_JHogar))
test_hogares <- test_hogares %>%
  mutate(Cat_Ocup_JHogar = ifelse(is.na(Cat_Ocup_JHogar)  & test_hogares$Posc_Ocup_JHogar == 3, 
                                  4, Cat_Ocup_JHogar))
test_hogares <- test_hogares %>%
  mutate(Cat_Ocup_JHogar = ifelse(is.na(Cat_Ocup_JHogar)  & test_hogares$Posc_Ocup_JHogar == 2, 
                                  4, Cat_Ocup_JHogar))
test_hogares <- test_hogares %>%
  mutate(Cat_Ocup_JHogar = ifelse(is.na(Cat_Ocup_JHogar)  & test_hogares$Posc_Ocup_JHogar == 6, 
                                  4, Cat_Ocup_JHogar))
test_hogares <- test_hogares %>%
  mutate(Cat_Ocup_JHogar = ifelse(is.na(Cat_Ocup_JHogar)  & test_hogares$Posc_Ocup_JHogar == 5, 
                                  9, Cat_Ocup_JHogar))
test_hogares <- test_hogares %>%
  mutate(Cat_Ocup_JHogar = ifelse(is.na(Cat_Ocup_JHogar), 9, Cat_Ocup_JHogar))

test_hogares$SS_Jefe <- ifelse(is.na(test_hogares$SS_Jefe), 
                               9, test_hogares$SS_Jefe)

test_hogares$Posc_Ocup_JHogar <- ifelse(is.na(test_hogares$Posc_Ocup_JHogar), 
                                        6, test_hogares$Posc_Ocup_JHogar)

test_hogares$Propiedad_Vivienda <- ifelse(is.na(test_hogares$Propiedad_Vivienda), 
                                          6, test_hogares$Propiedad_Vivienda)

test_hogares$SS_Jefe <- factor(test_hogares$SS_Jefe, levels = c(1, 2, 9), labels = c("Cotiza a un Seguro", "No Cotiza", "Otro"))



test_hogares$Cat_Ocup_JHogar <- factor(test_hogares$Cat_Ocup_JHogar, levels = c(1, 2, 3, 4, 5, 6,7,8, 9), labels = c("Obrero o empleado de empresa particular", "Obrero o empleado del gobierno",
                                                                                                                 "Empleado doméstico","Trabajador por cuenta propia",
                                                                                                                 "Patrón o empleador", "Trabajador familiar sin remuneración",
                                                                                                                 "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                                                                                                 "Jornalero o peón","Otro"))

test_hogares$posicionocupacionconyugue <- factor(test_hogares$posicionocupacionconyugue, levels = c(1, 2, 3, 4, 5, 6), labels = c("Trabajando", "Buscando trabajo", 
                                                                                                                "Estudiando", "Oficios del hogar", 
                                                                                                                "Incapacitado permanente para trabajar", 
                                                                                                                "Otra Actividad"))

test_hogares$Posc_Ocup_JHogar <- factor(test_hogares$Posc_Ocup_JHogar, levels = c(1, 2, 3, 4, 5, 6), labels = c("Trabajando", "Buscando trabajo", 
                                                                                                                "Estudiando", "Oficios del hogar", 
                                                                                                                "Incapacitado permanente para trabajar", 
                                                                                                                "Otra Actividad"))

test_hogares$categocupconyugue <- factor(test_hogares$categocupconyugue, levels = c(1, 2, 3, 4, 5, 6,7,8, 9), labels = c("Obrero o empleado de empresa particular", "Obrero o empleado del gobierno",
                                                                                                                     "Empleado doméstico","Trabajador por cuenta propia",
                                                                                                                     "Patrón o empleador", "Trabajador familiar sin remuneración",
                                                                                                                     "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                                                                                                     "Jornalero o peón","Otro"))
#### Cambiar las Variables de la base de datos test_hogares
test_hogares <- test_hogares %>% mutate(Sexo_JHogar= case_when(Sexo_JHogar==1 ~"Male",
                                                            Sexo_JHogar==2 ~"Female"),
                                        sexoconyugue= case_when(sexoconyugue==1 ~"Male",
                                                                sexoconyugue==2 ~"Female"),
                                        Educ_JHogar= case_when(Educ_JHogar==1 ~"Ninguno",
                                                               Educ_JHogar==2 ~"Preescolar",
                                                               Educ_JHogar==3 ~"Educación básica en el ciclo de primaria",
                                                               Educ_JHogar==4 ~"Educación básica en el ciclo de secundaria",
                                                               Educ_JHogar==5 ~"Educación media",
                                                               Educ_JHogar==6 ~"Superior o universitaria",
                                                               Educ_JHogar==9 ~"No sabe"),
                                          Educ_Conyugue= case_when(Educ_Conyugue==1 ~"Ninguno",
                                                                   Educ_Conyugue==2 ~"Preescolar",
                                                                   Educ_Conyugue==3 ~"Educación básica en el ciclo de primaria",
                                                                   Educ_Conyugue==4 ~"Educación básica en el ciclo de secundaria",
                                                                   Educ_Conyugue==5 ~"Educación media",
                                                                   Educ_Conyugue==6 ~"Superior o universitaria",
                                                                   Educ_Conyugue==9 ~"No sabe"),
                                        sexoconyugue = case_when(sexoconyugue==1 ~"Male",
                                                                    sexoconyugue==2 ~"Female"),
                                        SS_Conyugue= case_when(SS_Conyugue==1 ~"Cotiza a un Seguro",
                                                                 SS_Conyugue==2 ~"No Cotiza",
                                                                 SS_Conyugue==9 ~"Otro"),
                                        Propiedad_Vivienda = case_when(Propiedad_Vivienda==1 ~"Propia Pagada",
                                                                          Propiedad_Vivienda==2 ~"Propia por Pagar",
                                                                          Propiedad_Vivienda==3 ~"En Arriendo",
                                                                          Propiedad_Vivienda==4 ~"En Usufructo",
                                                                          Propiedad_Vivienda==5 ~"Ocupante",
                                                                          Propiedad_Vivienda==6 ~"Otra"))

promedio_exp_emp <- test_hogares %>%
  filter(Exp_Empresa != 0) %>%  # Excluir ceros
  group_by(Dominio) %>%
  summarize(
    media_exp_emp = round(mean(Exp_Empresa, na.rm = TRUE)),
    mediana_exp_emp = median(Exp_Empresa, na.rm = TRUE),
    max_exp_emp = max(Exp_Empresa, na.rm = TRUE),
    min_exp_emp = min(Exp_Empresa, na.rm = TRUE),
    desv_exp_emp = sd(Exp_Empresa, na.rm = TRUE),
    lim_sup_exp_emp = round(media_exp_emp + 2.5*desv_exp_emp),
    moda_exp_emp = as.numeric(names(sort(table(Exp_Empresa), decreasing = TRUE)[1]))
  )
print(promedio_exp_emp)

test_hogares <- left_join(test_hogares, promedio_exp_emp %>% select(Dominio, lim_sup_exp_emp), by = "Dominio")


test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo <= 60000  & test_hogares$Dominio == "RURAL", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "POPAYAN", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "CARTAGENA", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "PASTO", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "QUIBDO", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "VALLEDUPAR", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "TUNJA", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "SANTA MARTA", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 98 & test_hogares$Dominio == "CUCUTA", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo == 99 & test_hogares$Dominio == "BUCARAMANGA", 
                                     mean(test_hogares$Pago_Arriendo, na.rm = TRUE), test_hogares$Pago_Arriendo)

promedio_arriendo <- test_hogares %>%
  group_by(Dominio) %>%
  mutate(
    lower_limit = quantile(Pago_Arriendo, 0.05, na.rm = TRUE),
    upper_limit = quantile(Pago_Arriendo, 0.95, na.rm = TRUE)
  ) %>%
  filter(Pago_Arriendo >= lower_limit & Pago_Arriendo <= upper_limit) %>%
  summarize(
    media_arriendo = round(mean(Pago_Arriendo, na.rm = TRUE)),
    mediana_arriendo = median(Pago_Arriendo, na.rm = TRUE),
    max_arriendo = max(Pago_Arriendo, na.rm = TRUE),
    min_arriendo = min(Pago_Arriendo, na.rm = TRUE),
    desv_arriendo = sd(Pago_Arriendo, na.rm = TRUE),
    lim_sup_arriendo = round(media_arriendo + 3*desv_arriendo),
    lim_inf_arriendo =round(media_arriendo - 1.75*desv_arriendo),
    moda_arriendo = as.numeric(names(sort(table(Pago_Arriendo), decreasing = TRUE)[1]))
  )
print(promedio_arriendo)

test_hogares <- left_join(test_hogares, promedio_arriendo %>% select(Dominio, lim_sup_arriendo), by = "Dominio")
test_hogares <- left_join(test_hogares, promedio_arriendo %>% select(Dominio, lim_inf_arriendo), by = "Dominio")

test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo < test_hogares$lim_inf_arriendo,test_hogares$lim_inf_arriendo,test_hogares$Pago_Arriendo) 
test_hogares$Pago_Arriendo <- ifelse(test_hogares$Pago_Arriendo > test_hogares$lim_sup_arriendo,test_hogares$lim_sup_arriendo,test_hogares$Pago_Arriendo)                              


test_hogares <- test_hogares %>%
  mutate(Exp_Empresa = ifelse(Exp_Empresa >= lim_sup_exp_emp, lim_sup_exp_emp, Exp_Empresa))

test_hogares$Exp_Empresa <- ifelse(is.na(test_hogares$Exp_Empresa), 
                                          0, test_hogares$Exp_Empresa)


# Analisis y depuración de las Horas trabajadas
promedio_Hrs_Ocupados <- test_hogares %>%
  mutate(
    lower_limit = quantile(Hrs_Ocupados, 0.01, na.rm = TRUE),
    upper_limit = quantile(Hrs_Ocupados, 0.99, na.rm = TRUE)
  ) %>%
  filter(Hrs_Ocupados >= lower_limit & Hrs_Ocupados <= upper_limit) %>%
  summarize(
    media_Hrs_Ocupados = round(mean(Hrs_Ocupados)),
    desv_Hrs_Ocupados = round(sd(Hrs_Ocupados)),
  )
print(promedio_Hrs_Ocupados)


hrs_oficial <- 48
umbral_max <- round(hrs_oficial + 2.5*promedio_Hrs_Ocupados$desv_Hrs_Ocupados,0)
umbral_min <- round(hrs_oficial - 2.5*promedio_Hrs_Ocupados$desv_Hrs_Ocupados,0)

test_hogares$Hrs_Ocupados[test_hogares$Hrs_Ocupados > umbral_max] <- umbral_max
test_hogares$Hrs_Ocupados[test_hogares$Hrs_Ocupados < umbral_min] <- umbral_min

test_hogares$Hrs_Ocupados <- ifelse(is.na(test_hogares$Hrs_Ocupados),0, test_hogares$Hrs_Ocupados)

# Analisis de Educación promedio hijos
prom_educ_hijos <-  round(mean(test_hogares$Educ_prom_Hijos, na.rm = TRUE))
test_hogares$Educ_prom_Hijos <- ifelse(is.na(test_hogares$Educ_prom_Hijos) & test_hogares$Menores_18Años >= 1, 
                                         prom_educ_hijos, test_hogares$Educ_prom_Hijos)

test_hogares$Educ_prom_Hijos <- ifelse(is.na(test_hogares$Educ_prom_Hijos) & test_hogares$Menores_18Años == 0, 
                                                  0, test_hogares$Educ_prom_Hijos)



###########------------------------------------------------------------Test DATA 1---------------------------------------------#######
Data1_Test <- test_hogares[ c("id","Dominio", "Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años", 
                           "Li", "Lp", "Total_Ocup", "Cat_Ocup_JHogar","Exp_Empresa","Hrs_Ocupados","SS_Jefe","Pago_Arriendo",
                           "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar")]

Tabla_Stat <- Data1_Test  %>% select(Hab_por_Hogar, 
                                Dormit_Hogar, 
                                Pers_por_Hogar,
                                Pago_Arriendo,
                                Edad_JHogar,
                                Educ_JHogar,
                                Total_Ocup,
                                Menores_18Años,
                                Educ_prom_Hijos,
                                SS_Jefe,
                                Exp_Empresa)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas")



Data1_Test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

Test_Data <- "C:/Output R/Problem_Set3/Taller_3/Test_Data.xlsx"  
write.xlsx(Data1_Test, Test_Data)

#Tabla_1 <- "C:/Output R/Problem_Set3/Taller_3/tabla_1.xlsx"  
#write_xlsx(Pronost_3.1,Tabla_1)
#Tabla_2 <- "C:/Output R/Problem_Set3/Taller_3/tabla_2.xlsx"  
#write_xlsx(Pronost_3.2,Tabla_2)

Data1 <- train_hogares1[ c("id","Dominio", "Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años", 
                           "Linea_Indigencia", "Linea_Pobreza", "Total_Ocup", "Cat_Ocup_JHogar","Exp_Empresa","Hrs_Ocupados",
                           "Posc_Ocup_JHogar","Educ_JHogar", "Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar",
                           "Pago_Arriendo", "SS_Jefe", "Ingreso_Hogar", "Ingreso_Perc_Hogar", "Pobreza")]

data1 <- Data1  %>% select(Sexo_JHogar,
                           Edad_JHogar2,
                           Edad_JHogar,
                           Educ_JHogar,
                           Exp_Empresa,
                           Hrs_Ocupados,
                           SS_Jefe,
                           Posc_Ocup_JHogar,
                           Cat_Ocup_JHogar,
                           Ingreso_Hogar)


# Dividir los datos en conjunto de entrenamiento, validación y prueba
# Eliminar filas con valores faltantes en toda la base de datos
data1 <- na.omit(data1)

# Luego, continúa con tu proceso de división de datos y entrenamiento del modelo
# Dividir los datos en conjunto de entrenamiento, validación y prueba
library(caret)

# Dividir los datos en conjunto de entrenamiento y prueba de manera estratificada
set.seed(123)
index <- createDataPartition(data1$Ingreso_Hogar, p = 0.7, list = FALSE)
train_data <- data1[index, ]
test_data <- data1[-index, ]

# Entrenamiento del modelo de regresión lineal usando el conjunto de entrenamiento
lm_model <- train(
  Ingreso_Hogar ~ ., 
  data = train_data, 
  method = "lm", 
  trControl = trainControl(method = "cv"),
  preProcess = c("center", "scale")
)

# Evaluación del modelo en el conjunto de prueba
predictions <- predict(lm_model, newdata = test_data)

# Calcular el RMSE y MAE usando funciones de caret
rmse <- sqrt(mean((test_data$Ingreso_Hogar - predictions)^2))
mae <- mean(abs(test_data$Ingreso_Hogar - predictions))

# Mostrar los resultados
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# Predicciones fuera de muestra y cálculo de Pobreza
Data1_Test$Ingreso_Hogar <- predict(lm_model, newdata = Data1_Test)

Data1_Test$Ingreso_Perc_Hogar <- Data1_Test$Ingreso_Hogar/Data1_Test$Pers_por_Hogar


Data1_Test <- Data1_Test %>% mutate(prob_hat7=predict(Model2.2,newdata = Data1_Test, type = "response")) 

rule <- 1/2 # Bayes Rule
Data1_Test <-  Data1_Test  %>% mutate(Pobreza_hat7=ifelse(prob_hat7>rule,1,0))
Data1_Test$pobre <- Data1_Test$Pobreza_hat7
Pronost_fm1  <- Data1_Test[ c("id", "pobre")]
Tabla_Pronost_fm_1 <- "C:/Output R/Problem_Set3/Taller_3/Tabla_Pronost_fm1.csv"  
write.csv(x = Pronost_fm1, file = Tabla_Pronost_fm_1, row.names = FALSE)


# Base de datos1
# Lista de columnas categóricas (factores) a convertir a variables dummy
test_Datal1 <- Data1_Test %>% mutate(Sexo_JHogar = case_when(Sexo_JHogar=="Male"~ 1,
                                                             Sexo_JHogar =="Female" ~2 ),
                                      SS_Jefe = case_when(SS_Jefe=="Cotiza a un Seguro"~ 1,
                                                         SS_Jefe=="No Cotiza"~2,
                                                         SS_Jefe=="Otro"~9),
                                     Educ_JHogar = case_when(Educ_JHogar=="Ninguno"~1, 
                                                             Educ_JHogar=="Preescolar"~2 ,
                                                             Educ_JHogar=="Educación básica en el ciclo de primaria"~3 ,
                                                             Educ_JHogar=="Educación básica en el ciclo de secundaria"~4,
                                                             Educ_JHogar=="Educación media"~5,
                                                             Educ_JHogar=="Superior o universitaria"~6,
                                                             Educ_JHogar=="No sabe"~9 ),
                                     Cat_Ocup_JHogar = case_when(Cat_Ocup_JHogar=="Obrero o empleado de empresa particular"~1,
                                                                 Cat_Ocup_JHogar=="Obrero o empleado del gobierno"~2,
                                                                 Cat_Ocup_JHogar=="Empleado doméstico"~3,
                                                                 Cat_Ocup_JHogar=="Trabajador por cuenta propia"~4 ,
                                                                 Cat_Ocup_JHogar=="Patrón o empleador"~5,
                                                                 Cat_Ocup_JHogar=="Trabajador familiar sin remuneración"~6,
                                                                 Cat_Ocup_JHogar=="Trabajador sin remuneración en empresas o negocios de otros hogares"~7,
                                                                 Cat_Ocup_JHogar=="Jornalero o peón"~8,
                                                                 Cat_Ocup_JHogar=="Otro"~9),
                                     Posc_Ocup_JHogar = case_when(Posc_Ocup_JHogar=="Trabajando"~1 ,
                                                                  Posc_Ocup_JHogar=="Buscando trabajo"~2 ,
                                                                  Posc_Ocup_JHogar=="Estudiando"~3,
                                                                  Posc_Ocup_JHogar=="Oficios del hogar"~4 ,
                                                                  Posc_Ocup_JHogar=="Incapacitado permanente para trabajar"~5,
                                                                  Posc_Ocup_JHogar=="Otra Actividad"~6))


Test_Data_l1 <- test_Datal1[ c("Sexo_JHogar", "Edad_JHogar","Edad_JHogar2", "Pers_por_Hogar", "Menores_18Años", 
                             "Total_Ocup", "Cat_Ocup_JHogar","Exp_Empresa","Hrs_Ocupados",
                             "Posc_Ocup_JHogar","Educ_JHogar","Educ_prom_Hijos", "Hab_por_Hogar","Dormit_Hogar",
                             "Pago_Arriendo", "SS_Jefe")]


# Predicciones fuera de muestra y cálculo de Pobreza
Test_Data_l1$Ingreso_Hogar <- predict(lm_model, newdata = Data1_Test)
Test_Data_l1$Ingreso_Perc_Hogar <- Test_Data_l1$Ingreso_Hogar/Test_Data_l1$Pers_por_Hogar
Test_Data_l1$Ingreso_Hogar <- NULL
Datal1_Test_matrix <- as.matrix(Test_Data_l1)



###############--------Pronostico fuera de Muestra Elastic Net------------------------###############
Test_Data_l1 <- Test_Data_l1 %>% mutate(prob_h=predict(M_Logit_EN, newx = Datal1_Test_matrix, type = "response")) 
Test_Data_l1$prob_h <- unlist(Test_Data_l1$prob_h)
Test_Data_l1$prob_h <- as.numeric(Test_Data_l1$prob_h)
Test_Data_l1$id <- Data1_Test$id
rule <- 1/2 # Bayes Rule
Test_Data_l1 <-  Test_Data_l1  %>% mutate(Pobreza_hat9=ifelse(prob_h>rule,1,0))
Test_Data_l1$pobre <- Test_Data_l1$Pobreza_hat9
Pronost_en_1  <- Test_Data_l1[ c("id", "pobre")]
Pronost_en1 <- "C:/Output R/Problem_Set3/Taller_3/Pronost_en1.csv"  
write.csv(x = Pronost_en_1, file = Pronost_en1, row.names = FALSE)

###############--------Pronostico fuera de Muestra Ridge------------------------###############
Test_Data_l1 <- Test_Data_l1 %>% mutate(prob_h1=predict(M_Logit_rgd, newx = Datal1_Test_matrix, type = "response")) 
Test_Data_l1$prob_h1 <- unlist(Test_Data_l1$prob_h1)
Test_Data_l1$prob_h1 <- as.numeric(Test_Data_l1$prob_h1)
Test_Data_l1$id <- Data1_Test$id
rule <- 1/2 # Bayes Rule
Test_Data_l1 <-  Test_Data_l1  %>% mutate(Pobreza_h10=ifelse(prob_h1>rule,1,0))
Test_Data_l1$pobre <- Test_Data_l1$Pobreza_h10
Pronost_rgd_1  <- Test_Data_l1[ c("id", "pobre")]
Pronost_rgd1 <- "C:/Output R/Problem_Set3/Taller_3/Pronost_rgd1.csv"  
write.csv(x = Pronost_rgd_1, file = Pronost_rgd1, row.names = FALSE)


###############--------Pronostico fuera de Muestra Lasso------------------------###############
Test_Data_l1 <- Test_Data_l1 %>% mutate(prob_h2=predict(M_Logit_ls, newx = Datal1_Test_matrix, type = "response")) 
Test_Data_l1$prob_h2 <- unlist(Test_Data_l1$prob_h2)
Test_Data_l1$prob_h2 <- as.numeric(Test_Data_l1$prob_h2)
Test_Data_l1$id <- Data1_Test$id
rule <- 1/2 # Bayes Rule
Test_Data_l1 <-  Test_Data_l1  %>% mutate(Pobreza_h11=ifelse(prob_h2>rule,1,0))
Test_Data_l1$pobre <- Test_Data_l1$Pobreza_h11
Pronost_ls_1  <- Test_Data_l1[ c("id", "pobre")]
Pronost_ls1 <- "C:/Output R/Problem_Set3/Taller_3/Pronost_ls1.csv"  
write.csv(x = Pronost_ls_1, file = Pronost_ls1, row.names = FALSE)

